"""
Loads the images then calculates the SNR
Takes the filename
"""

import cv2
import numpy as np
import scipy as sp
import filters

def SNR(im,mode='file',conv=45,n = 1,npoints=10,hess=400,**kwargs):
    if mode == 'file':
        image = cv2.imread(im,cv2.IMREAD_GRAYSCALE)
    elif mode == 'im_array':
        image = im
        
    med = filters.median_subtract(image,3)
    pix_low,pix_high,blur_low,blur_high,kps_pts,bp = filters.keypoint_density(image,conv,npoints,hess)

    means_low = [np.mean(image[i[0]-n:i[0]+n+1,i[1]-n:i[1]+n+1]) for i in pix_low]
    means_high = [np.mean(image[i[0]-n:i[0]+n+1,i[1]-n:i[1]+n+1]) for i in pix_high]
    stds_low = [np.std(med[i[0]-n:i[0]+n+1,i[1]-n:i[1]+n+1]) for i in pix_low]
    
    
    return np.mean(np.abs(means_high-np.mean(means_low)))/np.mean(stds_low)
