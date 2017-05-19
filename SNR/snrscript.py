"""
Script for the paper
"""
import time
import numpy as np
import snr_of_images
import urllib
import zlib
from io import BytesIO
import cv2
import h5py
import os
import matplotlib.pyplot as plt
import json
import requests
from requests.packages.urllib3.exceptions import InsecureRequestWarning

#Following if it gives an InsecureRequestWarning
#requests.packages.urllib3.disable_warnings(InsecureRequestWarning)

sbem_skip = False
atum_skip = False
fbem_skip = False
temca_skip = False
OCP_server = 'http://cloud.neurodata.io/ocp/ca/'
n_comp = 100

class OCP_data:
    def __init__(self,key,num):
        info = requests.get(OCP_server+key+'/info/',verify=False).json()['dataset']
        res = info['resolutions'][0]
        x_size,y_size,z_size = info['imagesize'][str(res)]
        x_off,y_off,z_off = info['neariso_offset'][str(res)]
        ocp_x_rand = np.random.randint(x_off,x_size-2000,num+400)
        ocp_y_rand = np.random.randint(y_off,y_size-2000,num+400)
        ocp_z_rand = np.random.randint(z_off,z_size,num+400)
        count = 0
        self.bad = []
        self.bad_state = []
        for i in range(num+400):
            print(key,': ',count)
            try_count = 0
            while try_count < 10:
                try_count2 = 0
                try:
                    f = requests.get("http://cloud.neurodata.io/ocp/ca/"+key+"/npz/"+str(res)+"/"+str(ocp_x_rand[i])+","+str(ocp_x_rand[i]+2000)+"/"+str(ocp_y_rand[i])+","+str(ocp_y_rand[i]+2000)+"/"+str(ocp_z_rand[i])+","+str(ocp_z_rand[i]+1)+"/",timeout=60,verify=False).content
                except Exception as e:
                    print(e)
                    print(key,', type 1: ',count, "http://cloud.neurodata.io/ocp/ca/"+key+"/npz/"+str(res)+"/"+str(ocp_x_rand[i])+","+str(ocp_x_rand[i]+2000)+"/"+str(ocp_y_rand[i])+","+str(ocp_y_rand[i]+2000)+"/"+str(ocp_z_rand[i])+","+str(ocp_z_rand[i]+1)+"/")
                    try_count2 +=1
                    if try_count2 == 5:
                        raise IOError('Maximum tries to download exceeded')
                    continue
                try:
                    zdata = f#.read()
                    datastr = zlib.decompress ( zdata[:] )
            
                    datafobj = BytesIO ( datastr )
                    temp_data = np.load (datafobj)
                except:
                    try_count +=1
                    print(key,', type 2: ',count, "http://cloud.neurodata.io/ocp/ca/"+key+"/npz/"+str(res)+"/"+str(ocp_x_rand[i])+","+str(ocp_x_rand[i]+2000)+"/"+str(ocp_y_rand[i])+","+str(ocp_y_rand[i]+2000)+"/"+str(ocp_z_rand[i])+","+str(ocp_z_rand[i]+1)+"/")

                    continue
                if len(temp_data) == 0: #data failed to download correctly
                    try_count +=1
                else:
                    break
            if try_count == 10:
                self.bad.append("http://cloud.neurodata.io/ocp/ca/"+key+"/npz/"+str(res)+"/"+str(ocp_x_rand[i])+","+str(ocp_x_rand[i]+2000)+"/"+str(ocp_y_rand[i])+","+str(ocp_y_rand[i]+2000)+"/"+str(ocp_z_rand[i])+","+str(ocp_z_rand[i]+1)+"/")
                self.bad_state.append(np.random.get_state())
                continue
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

        
np.random.seed(20170127)

#sudo mount -t davfs https://segem.rzg.mpg.de/webdav /image/sbem
sbem_snr = np.zeros(n_comp)
count = 0

while count < n_comp:
    x = np.random.permutation(25)[0]+1 #Plus one to avoid some of the edges
    y = np.random.permutation(35)[0]+1
    z = np.random.permutation(42)[0]+1
    if sbem_skip is True:
        count+=1
        if count == n_comp:
            break
        else:
            continue
    im = np.zeros([128*5,128*5])
    for k in range(5):
         for l in range(5):
              #Construct large images by copying over to /tmp and reading the raw
              os.system('cp /image/sbem/datasets/ek0563/raw/color/1/x'+str(x+k).zfill(4)+'/y'+str(y+l).zfill(4)+'/z'+str(z).zfill(4)+'/100527_k0563_mag1_x'+str(x+k).zfill(4)+'_y'+str(y+l).zfill(4)+'_z'+str(z).zfill(4)+'.raw /tmp/tmpim.raw')
              im[l*128:(l+1)*128,k*128:(k+1)*128] = np.memmap('/tmp/tmpim.raw',dtype=np.uint8,shape=(128,128))
    sbem_snr[count] = snr_of_images.SNR(im.astype(np.uint8),mode='im_array',conv=35,hess=200)
    count += 1
    if count == n_comp:
         break

#sbem_snr.sort()
sbem_snr = sbem_snr[sbem_snr < np.inf]



atum_snr = np.zeros(n_comp)
atum= OCP_data('kasthuri11',n_comp+10)
count = 0
for i in range(n_comp+10):
     try:
          atum_snr[count] = snr_of_images.SNR(atum.data[i,:,:],mode='im_array',conv=55,hess=800)
          count += 1
          if count == n_comp:
               break
     except:
          continue
atum_snr = atum_snr[atum_snr < np.inf]


fibsem_snr = np.zeros(n_comp)
fib_random = np.random.permutation(range(1376,7651))[:300]
#fib_sem images have to be downloaded in advance
count = 0
for i,j in enumerate(fib_random):
     if fbem_skip is True:
        break
     try:
          fibsem_snr[count] = snr_of_images.SNR('fib_sem_images/grayscale-xy-'+str(j)+'.png',conv=35,hess=3200)
          im = cv2.imread('fib_sem_images/grayscale-xy-'+str(j)+'.png',cv2.IMREAD_GRAYSCALE)

          count += 1
          if count == n_comp:
               break
     except:
          continue
fibsem_snr = fibsem_snr[fibsem_snr < np.inf]

#aplus, bplus and cplus are the cremi.org files
crop_im = h5py.File('aplus.h5','r')  
temca = crop_im['volumes']['raw']
crop_im = h5py.File('bplus.h5','r')  
temca = np.append(temca,crop_im['volumes']['raw'],axis=0)
crop_im = h5py.File('cplus.h5','r')  
temca = np.append(temca,crop_im['volumes']['raw'],axis=0)

temca_random = np.random.permutation(len(temca))[:150]
temcas = np.zeros(n_comp)
count = 0
for i,j in enumerate(temca_random):
    if temca_skip is True:
            break
    try:
        temcas[count] = snr_of_images.SNR(temca[j,:,:],mode='im_array',conv=55,hess=200)
        cv2.imwrite('/image/Used/temca_'+str(count).zfill(3)+'.tif',temca[j,:,:])

        count +=1 
        if count == n_comp:
             break
    except:
        continue
temcas = temcas[temcas < np.inf]

bock11 = OCP_data('bock11',n_comp+10)
bock_snr = np.zeros(n_comp)
count = 0
for i in range(n_comp+10):
     try:
          bock_snr[count] = snr_of_images.SNR(bock11.data[i,:,:],mode='im_array',conv=55,hess=800)
          cv2.imwrite('/image/Used/bock11_'+str(count).zfill(3)+'.tif',bock11.data[i,:,:])

          count += 1
          if count == n_comp:
               break
     except:
          continue
bock_snr = bock_snr[bock_snr < np.inf]

acardona11 = OCP_data('acardona_0111_8',n_comp+10)
acardona_snr = np.zeros(n_comp)
count = 0
for i in range(n_comp+10):
     try:
          acardona_snr[count] = snr_of_images.SNR(acardona11.data[i,:,:],mode='im_array',conv=55,hess=800)
          cv2.imwrite('/image/Used/acardona11_'+str(count).zfill(3)+'.tif',acardona11.data[i,:,:])

          count += 1
          if count == n_comp:
               break
     except:
          continue
acardona_snr = acardona_snr[acardona_snr < np.inf]



takemura13 = OCP_data('takemura13',n_comp+10)
takemura_snr = np.zeros(n_comp)
count = 0
for i in range(n_comp+10):
     try:
          takemura_snr[count] = snr_of_images.SNR(takemura13.data[i,:,:],mode='im_array',conv=55,hess=800)
          cv2.imwrite('/image/Used/takemura13_'+str(count).zfill(3)+'.tif',takemura13.data[i,:,:])

          count += 1
          if count == n_comp:
               break
     except:
          continue
takemura_snr = takemura_snr[takemura_snr < np.inf]

times = np.array([35,1.2e-2,0.14,0.59])

means = np.array([np.nanmean(temcas),np.nanmean(fibsem_snr),np.nanmean(atum_snr),np.nanmean(sbem_snr),np.nanmean(bock_snr),np.nanmean(acardona_snr),np.nanmean(takemura_snr)])

yerr = np.array([np.nanstd(temcas),np.nanstd(fibsem_snr),np.nanstd(atum_snr),np.nanstd(sbem_snr),np.nanstd(bock_snr),np.nanstd(acardona_snr),np.nanstd(takemura_snr)])

np.savetxt('means_feature.txt',means)
np.savetxt('std_feature.txt',yerr)
b = np.argmin(np.abs(atum_snr-np.mean(atum_snr)))
#np.savetxt('atum_loc.txt',b)

means2 = np.array([ 15.7,  11.1,   9.9,   5.2])
yerr2 = np.array([ 2. ,  2.6,  1.5,  0.8])

f = plt.figure(figsize=(10,4))
ax0 = f.add_subplot(121)
symbol = ['o','x','s','d','>','8','h','+']
colors = ['r','g','b','k','y','c','m','brown']
[ax0.loglog(times[i],means[i],'.',mfc=colors[i],marker=symbol[i],mec=colors[i]) for i in range(len(means))]
[ax0.errorbar(times[i],means[i],yerr=yerr[i],lw=1,fmt='none',ecolor=colors[i]) for i in range(len(means))]
ax0.set_title(r'${\rm a)~Feature~based~S/N}$')
ax0.set_xlim(1e-2,50)
ax0.set_ylim(1,30)
ax0.xlabel(r'${\rm Acquisition~Rate}~[\mu{}m^3~s^{-1}]$')
ax0.ylabel(r'${\rm S/N}$')

ax1 = f.add_subplot(122)
symbol = ['o','x','s','d']
colors = ['r','g','b','k']
[ax1.loglog(times[i],means2[i],'.',mfc=colors[i],marker=symbol[i],mec=colors[i]) for i in range(4)]
[ax1.errorbar(times[i],means2[i],yerr=yerr2[i],lw=1,fmt='none',ecolor=colors[i]) for i in range(len(means))]
ax1.set_title(r'${\rm b)~Cell-edge~S/N}$')
ax1.set_xlim(1e-2,50)
ax1.set_ylim(1,30)
ax1.xlabel(r'${\rm Acquisition~Rate}~[\mu{}m^3~s^{-1}]$')
ax1.ylabel(r'${\rm S/N}$')


