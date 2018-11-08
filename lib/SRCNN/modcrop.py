def modcrop(img, modulo):
    size = shape(img)
    size = size - mod(size, modulo)
    img = img[1:size[0], 1:size[1]]
    return img