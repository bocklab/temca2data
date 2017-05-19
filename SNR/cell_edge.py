"""
Script for the paper
"""
import time
import numpy as np
import snr_of_images
import urllib
import zlib
from io import BytesIO
import h5py
import os
import matplotlib.pyplot as plt
import cv2
import json
import requests

np.random.seed(314159)
OCP_server = 'http://cloud.neurodata.io/ocp/ca/'
class OCP_data:
    def __init__(self,key,num):
        info = requests.get(OCP_server+key+'/info/').json()['dataset']
        res = info['resolutions'][0]
        x_size,y_size,z_size = info['imagesize'][str(res)]
        x_off,y_off,z_off = info['neariso_offset'][str(res)]
        ocp_x_rand = np.random.randint(x_off,x_size-2000,num+100)
        ocp_y_rand = np.random.randint(y_off,y_size-2000,num+100)
        ocp_z_rand = np.random.randint(z_off,z_size,num+100)
        count = 0
        for i in range(num+100):
            print(key,': ',count)
            f = urllib.request.urlopen("http://cloud.neurodata.io/ocp/ca/"+key+"/npz/"+str(res)+"/"+str(ocp_x_rand[i])+","+str(ocp_x_rand[i]+2000)+"/"+str(ocp_y_rand[i])+","+str(ocp_y_rand[i]+2000)+"/"+str(ocp_z_rand[i])+","+str(ocp_z_rand[i]+1)+"/")
            zdata = f.read()
            datastr = zlib.decompress ( zdata[:] )
            
            datafobj = BytesIO ( datastr )
            temp_data = np.load (datafobj)
            if np.sum(temp_data[0]==0) > 0.5*len(temp_data[0].flatten()):
                continue
            if count == 0:
                data = temp_data
            else:
                data = np.append(data, temp_data, axis=1)
            count += 1
            if count == num:
                break
            
        self.data = data[0]



#Hacky
class PixelSelector:
     def __init__(self, line, ax0, ax1, ax2, image):
          self.line = line
          self.ax0 = ax0
          self.ax1 = ax1
          self.ax2 = ax2
          self.image = image
          self.sample_text = ax2.text(0,.8,r'${\rm \#~Samples:}~0$')

          self.signal_text = ax2.text(0,0.6,r'${\rm Signal:}~{\rm NaN}$')
          self.background_text = ax2.text(0,0.4,r'${\rm Background:}~{\rm NaN}$')
          self.noise_text = ax2.text(0,0.2,r'${\rm Noise:}~{\rm NaN}$')
          self.snr_text = ax2.text(0,0,r'${\rm SNR:}~{\rm NaN}$')
          self.snr = np.nan
          self.xs = list()
          self.ys = list()
          self.pix_signal = list()
          self.pix_background = list()
          self.count = 0
          self.cid = line.figure.canvas.mpl_connect('button_press_event', self)
          self.update_axis()
          line.figure.canvas.mpl_connect('key_press_event', self.key_press)
          
     def __call__(self, event):
          if event.inaxes!=self.line.axes: return
          if event.button  != 2: return
          self.xs.append(event.xdata)
          self.ys.append(event.ydata)
          if len(self.xs) == 2:
               if event.key == 'shift':
                    self.pix_signal.append(self.get_pixels(self.xs,self.ys))
                    self.ax0.plot(self.xs,self.ys,'r',lw=1,)
                    self.ax1.plot(self.pix_signal[-1],'r-',lw=1)
                    self.snr_calc()
                    self.line.figure.canvas.draw()
               else:
                    self.pix_background.append(self.get_pixels(self.xs,self.ys))
                    self.ax0.plot(self.xs,self.ys,'g',lw=1)
                    self.ax1.plot(self.pix_background[-1],'g-',lw=1)
                    self.snr_calc()
                    self.line.figure.canvas.draw()
               self.xs = list()
               self.ys = list()
               self.count += 1
          if self.count == 2:
               self.update_axis()

     def key_press(self, event):
          if event.key != 'right': return
          self.update_axis()
          
     def update_axis(self):
          im_shape = self.image.shape
          rand_0 = np.random.randint(im_shape[0]-100)
          rand_1 = np.random.randint(im_shape[1]-100)
          self.ax0.set_xlim(rand_0,rand_0+100)
          self.ax0.set_ylim(rand_1+100,rand_1)
          self.line.figure.canvas.draw()
          self.count = 0
          
     def snr_calc(self):
          sig = []
          bg = []
          for i in self.pix_signal:
               sig += i
          for i in self.pix_background:
               bg += i
          if len(bg):
               noise = np.std(bg)
               background = np.mean(bg)
               self.background_text.set_text(r'${\rm Background:}~%4.2f$' %background)
               self.noise_text.set_text(r'${\rm Noise:}~%4.2f$' %noise)

          if len(sig):
               signal = np.mean(sig)
               self.signal_text.set_text(r'${\rm Signal:}~%4.2f$' %signal)

          if len(sig) and len(bg):
               self.snr = np.abs(signal-background)/noise
               self.snr_text.set_text(r'${\rm SNR:}~%4.2f$' %self.snr)

          n_samples = len(self.pix_signal)+len(self.pix_background)
          self.sample_text.set_text(r'${\rm \#~Samples:}~%d$' %n_samples)
     #Uses Bresenham's line algorithm
     def get_pixels(self,xs,ys):
          pix = []
          x_pts = xs.copy()
          y_pts = ys.copy()
          steep = np.abs(y_pts[1]-y_pts[0]) > np.abs(x_pts[1]-x_pts[0])
          if steep:
               x_pts[0],y_pts[0] = y_pts[0],x_pts[0]
               x_pts[1],y_pts[1] = y_pts[1],x_pts[1]

          if x_pts[1] < x_pts[0]:
               neg = True
               x_pts[0],x_pts[1] = x_pts[1],x_pts[0]
               y_pts[0],y_pts[1] = y_pts[1],y_pts[0]
          else:
               neg = False
          deltax = x_pts[1]-x_pts[0]
          deltay = y_pts[1]-y_pts[0]

          error = int(deltax/2)
          if y_pts[0] < y_pts[1]:
               step = 1
          else:
               step = -1

          y = np.int(y_pts[0])
          for x in np.arange(np.floor(x_pts[0]),np.ceil(x_pts[1])+1,dtype=np.int):
               if steep:
                    pix.append(self.image[x,y])
               else:
                    pix.append(self.image[y,x])
               error -= abs(deltay)
               if error < 0:
                    y += step
                    error += deltax
          if neg:
               pix = pix[::-1]
          return pix


def snr_gui(image):

     fig = plt.figure(figsize=(12,8))

     ax0 = plt.subplot2grid((2,7),(0,0),colspan=4,rowspan=3)
     ax0.axis('equal')
     ax0.set_xlabel(r'$x$')
     ax0.set_ylabel(r'$y$')
     ax1 = plt.subplot2grid((2,7),(0,5),colspan=2)
     ax1.set_xlabel(r'${\rm Pixel}$')
     ax1.set_ylabel(r'${\rm Intensity}$')
     ax2 = plt.subplot2grid((2,7),(1,5),colspan=2)
     ax2.axis('off')
     ax2.set_axis_bgcolor('gray')
     temp, = ax0.plot([0],[0])
     ax0.set_aspect('equal')
     ax0.imshow(image,cmap='gray',interpolation='none')
     ps = PixelSelector(temp,ax0,ax1,ax2,image)
     return ps
