import numpy as np

def normalize(x):
    x = np.float32(x)
    return  (x / 255.)