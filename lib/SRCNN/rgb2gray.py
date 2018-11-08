import cv2

def rbg2gray(img):
    return cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)