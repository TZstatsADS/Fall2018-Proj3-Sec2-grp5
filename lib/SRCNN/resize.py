import numpy as np
import cv2
from scipy import misc

def resize(lr_dir, hr_dir):
    img_LR = cv2.imread(lr_dir)
    img_HR = cv2.imread(hr_dir)
    img_LR = misc.imresize(img_LR, np.shape(img_HR), interp = 'bicubic')
    img_HR = misc.imresize(img_HR, np.shape(img_HR))
    return (img_LR, img_HR)