"""
Variety of filters to detect areas inside the images.
Signal rich areas should have a large density, while the areas dominated by noise are
low in density.
"""

import cv2
import numpy as np
import scipy as sp
import scipy.ndimage

def keypoint_density(image,convolve_size,n_pix=10,hess=1600):

    detector = cv2.xfeatures2d.SURF_create(hessianThreshold=hess)
    (kps, features) = detector.detectAndCompute(image,None)
    kps_pts = np.array([i.pt for i in kps])

    
    #Convolve with the gaussian
    kps_digx = np.digitize(kps_pts[:,0],range(image.shape[1]))
    kps_digy = np.digitize(kps_pts[:,1],range(image.shape[0]))
    
    pts_im = np.zeros([image.shape[0],image.shape[1]],dtype=np.float32)
    pts_im[kps_digy,kps_digx] = 1 #Points shouldn't be in the same pixel, though technically possible.
    #Change if this causes issues

    blurred_pts = cv2.GaussianBlur(pts_im,(convolve_size,convolve_size),0)#,np.std(image),np.std(image))
    blurred_pts_low = blurred_pts.copy()
    blurred_pts_low[np.where(image==0)] = np.inf
    blurred_pts_low[0,:] = np.inf
    blurred_pts_low[-1,:] = np.inf
    blurred_pts_low[:,0] = np.inf
    blurred_pts_low[:,-1] = np.inf
    blur_max = sp.ndimage.filters.maximum_filter(blurred_pts_low,size=convolve_size,mode='constant')
    #blur_max[np.where(sp.ndimage.filters.minimum_filter(image,3,mode='constant')<np.mean(image[image>0]))] = np.inf
    blurred_pts_low[np.where(blur_max==np.inf)] = np.inf
    blurred_pts_high = blurred_pts.copy()
    blurred_pts_high[np.where(image==0)] = -np.inf
    blurred_pts_high[0,:] = -np.inf
    blurred_pts_high[-1,:] = -np.inf
    blurred_pts_high[:,0] = -np.inf
    blurred_pts_high[:,-1] = -np.inf
    blur_min = sp.ndimage.filters.minimum_filter(blurred_pts_high,size=convolve_size,mode='constant')
    #blur_min[np.where(sp.ndimage.filters.maximum_filter(image,3,mode='constant')>np.mean(image[image>0]))] = -np.inf
    blurred_pts_high[np.where(blur_min==-np.inf)] = -np.inf

    #Need MIN points, but if pixels near each other, can make them nan or inf??
    pix_low = []
    for i in range(n_pix):
        flat = blurred_pts_low.flatten()
        shuffle = np.random.permutation(len(flat))
        new_pix = np.array(np.unravel_index(shuffle[np.argmin(flat[shuffle])], blurred_pts_low.shape))
        new_pix[0] = new_pix[0]
        new_pix[1] = new_pix[1]
        pix_low.append(new_pix)
        blurred_pts_low[max(new_pix[0]-convolve_size,0):min(new_pix[0]+convolve_size,blurred_pts_low.shape[0]),max(new_pix[1]-convolve_size,0):min(new_pix[1]+convolve_size,blurred_pts_low.shape[1])] += np.inf
    pix_high = []
    for i in range(n_pix):
        flat = blurred_pts_high.flatten()
        shuffle = np.random.permutation(len(flat))
        new_pix = np.array(np.unravel_index(shuffle[np.argmax(flat[shuffle])], blurred_pts_high.shape))
        new_pix[0] = new_pix[0]
        new_pix[1] = new_pix[1]
        pix_high.append(new_pix)
        blurred_pts_high[max(new_pix[0]-convolve_size,0):min(new_pix[0]+convolve_size,blurred_pts_high.shape[0]),max(new_pix[1]-convolve_size,0):min(new_pix[1]+convolve_size,blurred_pts_high.shape[1])] -= np.inf

    return pix_low,pix_high,blurred_pts_low,blurred_pts_high,kps_pts,blurred_pts

def median_subtract(image,convolve_size):

    #median = cv2.medianBlur(image,convolve_size)
    median = cv2.blur(image,(convolve_size,convolve_size))
    return image.astype(np.float32)-median.astype(np.float32)
    
