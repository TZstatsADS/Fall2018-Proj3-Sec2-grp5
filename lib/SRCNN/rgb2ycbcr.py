import numpy as np

def rgb2ycbcr(img, mat):
    shape = np.shape(img)
    img = img.reshape((shape[0]*shape[1], 3))
    img = np.dot(img, mat)
    img[:,0] = img[:,0] + 16.
    img[:,1:] = img[:,1:] + 128.
    return img.reshape(shape)