import numpy as np
import math

def accuracy(img, HR_img):
    img = img[:,:,1]
    hr = HR_img[:,:,1]
    imdiff = hr - img
    rmse = np.sqrt(np.mean(pow(imdiff,2)))
    psnr = 20*math.log10(255/rmse)
    return (rmse, psnr)