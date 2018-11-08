import numpy as np

def segment(img,scale=3,input_size=33,label_size=21,stride=14):
    w, h = np.shape(img)
    pad = (input_size - label_size) // 2
        
    X = []
    y = []    
    for i in range(0, h - input_size + 1, stride):
        for j in range(0, w - input_size + 1, stride):

            sub_img = img[j: j + input_size, i: i + input_size]
            sub_img = sub_img.reshape([1, input_size, input_size, 1])
            X.append(sub_img)

            sub_img_label = img[j + pad: j + pad + label_size, i + pad: i + pad + label_size]
            sub_img_label = sub_img_label.reshape([1, label_size, label_size, 1])
            y.append(sub_img_label)
    
    return (X, y)
            
