#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3


# helper function to get the value for the neighbor 8 pixels - central pixel
get_pixel_value <- function(All_value, Given_Row_Index, Given_Col_Index){
  
  if( Given_Row_Index <= 0 | Given_Row_Index > nrow(All_value) | Given_Col_Index <= 0 | Given_Col_Index > ncol(All_value) ){
    return(NA)
  }
  
  else{
    return(All_value[Given_Row_Index, Given_Col_Index])
  }
  
}


# Helper function for get neighbor and label for sample
Get_Neighbor_and_Label_Pixel <- function(Index, LR_Data, HR_Data, Samples){
  
  Row_Index <- arrayInd(Samples[Index], dim(LR_Data))[1]
  Col_Index <- arrayInd(Samples[Index], dim(LR_Data))[2]
  
  LR_up_left <- get_pixel_value(LR_Data, Row_Index-1, Col_Index-1)
  LR_left <- get_pixel_value(LR_Data, Row_Index, Col_Index-1)
  LR_bottom_left <- get_pixel_value(LR_Data, Row_Index+1, Col_Index-1)
  LR_up <- get_pixel_value(LR_Data, Row_Index-1, Col_Index)
  LR_center <- get_pixel_value(LR_Data, Row_Index, Col_Index)
  LR_bottom <- get_pixel_value(LR_Data, Row_Index+1, Col_Index)
  LR_up_right <- get_pixel_value(LR_Data, Row_Index-1, Col_Index+1)
  LR_right <- get_pixel_value(LR_Data, Row_Index, Col_Index+1)
  LR_bottom_right <- get_pixel_value(LR_Data, Row_Index+1, Col_Index+1)
  
  LR_Neighbor_value <- c(LR_up_left, LR_left, LR_bottom_left, LR_up, LR_bottom, LR_up_right, LR_right, LR_bottom_right)
  
  LR_Neighbor_value <- LR_Neighbor_value - LR_center
  LR_Neighbor_NA_index <- which(is.na(LR_Neighbor_value))
  LR_Neighbor_value[LR_Neighbor_NA_index] <- 0
  
  # next select sub pixel from HR
  HR_1 <- get_pixel_value(HR_Data, 2*Row_Index-1, 2*Col_Index-1)
  HR_2 <- get_pixel_value(HR_Data, 2*Row_Index, 2*Col_Index-1)
  HR_3 <- get_pixel_value(HR_Data, 2*Row_Index-1, 2*Col_Index)
  HR_4 <- get_pixel_value(HR_Data, 2*Row_Index, 2*Col_Index)
  
  sub_pixels <- c(HR_1, HR_2, HR_3, HR_4)
  sub_pixels <- sub_pixels - LR_center
  return(list(LR_Neighbor_value = LR_Neighbor_value, sub_pixels = sub_pixels))
}


# Helper function get feature value
Extract_Feature <- function(LR_Image, HR_Image, Color_Chanel, Sample_Size){
  
  LR_image_data_chanel <- LR_Image[ , , Color_Chanel]
  
  HR_image_data_chanel <- HR_Image[ , , Color_Chanel]
  
  ### step 1. sample n_points from imgLR
  
  Sample_Points <- sample(c(1:length(LR_image_data_chanel)), Sample_Size)
  
  # Sample_Points <- c(1:1000)
  
  Result_LR_Neighbor_value <- matrix(nrow = Sample_Size, ncol = 8)
  
  Result_sub_pixels <- matrix(nrow = Sample_Size, ncol = 4)
  
  ### step 2. for each sampled point in imgLR,
  ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
  ###           tips: padding zeros for boundary points
  ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
  
  # registerDoParallel(cores=2)
  # foreach(index = 1:Sample_Size) %dopar% {
  #   
  #   Result <- Get_Neighbor_and_Label_Pixel(index, LR_image_data_chanel, HR_image_data_chanel, Sample_Points)
  #   
  #   Result_LR_Neighbor_value[index, ] <- Result$LR_Neighbor_value
  #   
  #   Result_sub_pixels[index,] <- Result$sub_pixels
  #   
  # }
  
  for (index in c(1:Sample_Size)) {
    
    Result <- Get_Neighbor_and_Label_Pixel(index, LR_image_data_chanel, HR_image_data_chanel, Sample_Points)
    
    Result_LR_Neighbor_value[index, ] <- Result$LR_Neighbor_value
    
    Result_sub_pixels[index,] <- Result$sub_pixels
    
  }
  
  
  return(list(LR_Neighbor_value = Result_LR_Neighbor_value, sub_pixels = Result_sub_pixels)) 
  
}


feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  # n_files <- 200
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  # nrow(featMat[,,1])
  
  ### read LR/HR image pairs
  Mat_Index <- 1
  for(i in 1:n_files){

    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    LR_image_data <- imageData(imgLR)
    
    HR_image_data <- imageData(imgHR)
    
    ### step 3. repeat above for three channels
    
    Result_Feature_Red <- Extract_Feature(LR_image_data, HR_image_data, 1, n_points)
    
    Result_Feature_LR_Neighbor_value_Red <- Result_Feature_Red$LR_Neighbor_value
    
    Result_Feature_sub_pixels_Red <- Result_Feature_Red$sub_pixels
    
    
    Result_Feature_Green <- Extract_Feature(LR_image_data, HR_image_data, 2, n_points)
    
    Result_Feature_LR_Neighbor_value_Green <- Result_Feature_Green$LR_Neighbor_value
    
    Result_Feature_sub_pixels_Green <- Result_Feature_Green$sub_pixels
    
    
    Result_Feature_Blue <- Extract_Feature(LR_image_data, HR_image_data, 3, n_points)
    
    Result_Feature_LR_Neighbor_value_Blue <- Result_Feature_Blue$LR_Neighbor_value
    
    Result_Feature_sub_pixels_Blue <- Result_Feature_Blue$sub_pixels
    
    # print(Result_Feature_LR_Neighbor_value_Red)
    # print(featMat[Mat_Index:(Mat_Index+n_points), , 1])
    Result_Feature_LR_Neighbor_value_Red
    featMat[Mat_Index:(Mat_Index+n_points-1), , 1] <- Result_Feature_LR_Neighbor_value_Red
    labMat[Mat_Index:(Mat_Index+n_points-1), , 1] <- Result_Feature_sub_pixels_Red
      
    featMat[Mat_Index:(Mat_Index+n_points-1), , 2] <- Result_Feature_LR_Neighbor_value_Green
    labMat[Mat_Index:(Mat_Index+n_points-1), , 2] <- Result_Feature_sub_pixels_Green
      
    featMat[Mat_Index:(Mat_Index+n_points-1), , 3] <- Result_Feature_LR_Neighbor_value_Blue
    labMat[Mat_Index:(Mat_Index+n_points-1), , 3] <- Result_Feature_sub_pixels_Blue
      # nrow(featMat[,,1])
      
      ### read LR/HR image pairs
    Mat_Index <- Mat_Index + n_points
    
    cat("file", i, "\n")

      
      
  }
  return(list(feature = featMat, label = labMat))
}
