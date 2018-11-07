install.packages("tensorflow")
library(tensorflow)

# creating environment
install_tensorflow()

# checking installation to see if it worked
sess <- tf$Session()
hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)

# input tensors
data_tf <- tf$placeholder(tf$float32, shape(2, 2))
size_tf <- tf$placeholder(tf$int32)

# constructing transition matrix
seq_tf <- tf$linspace(start = 1.0, stop = 0.0, num = size_tf+1L)
flipseq_tf <- tf$linspace(start = 0.0, stop = 1.0, num = size_tf+1L)
mat1_tf <- tf$concat(list(list(seq_tf), list(flipseq_tf)), axis = 0L)
mat2_tf <- tf$transpose(mat1_tf)

# operation tensors for matrix multiplication
h1_tf <- tf$matmul(data_tf, mat1_tf)
out_tf <- tf$matmul(mat2_tf, h1_tf)

# defining output
tf_output <- out_tf

# the actual bilinear interpolation function
bilinear_interpolation <- function(original_pic, times){
  original_size <- dim(original_pic[, , 1])[1]
  a <- original_size
  b <- times
  new_pic <- array(, c(a*b-b+1, a*b-b+1, 3))
  sess <- tf$Session()
  for(d in 1:3){
    original_channel_data <- original_pic[, , d]
    new_channel_data <- matrix(, nrow = a*b-b+1, ncol = a*b-b+1)
    for (i in 1:(a - 1)){
      for (j in 1:(a - 1)){
        start_x <- i
        end_x <- i + 1
        start_y <- j
        end_y <- j + 1
        this.data <- original_channel_data[start_x:end_x, start_y:end_y]
        out <- sess$run(tf_output, feed_dict = dict(data_tf = this.data,size_tf = b))
        new_channel_data[(b*i-b+1):(b*i+1), (b*j-b+1):(b*j+1)] <- out
      }
      cat("row", i, "finished!", "\n")
    }
    cat("channel", d, "finished!", "\n")
    new_pic[ , , d] <- new_channel_data
  }
  sess$close()
  return(new_pic)
}

##########################

# testing with chicken image
library(jpeg)
chicken <- readJPEG("~/figs/chicken_50x50.jpg")
plot(c(0, 1), c(0, 1))
rasterImage(chicken, 0, 0, 1, 1)

new_pic <- bilinear_interpolation(chicken, 8)

# comparison
par(mfrow=c(1, 2))

plot(c(0, 1), c(0, 1), type = n)
rasterImage(chicken, 0, 0, 1, 1)

plot(c(0, 1), c(0, 1), type = n)
rasterImage(new_pic, 0, 0, 1, 1)

# procressing converted file
writeJPEG(new_pic, target = "~/figs/linear_interpolation_8.jpg")
