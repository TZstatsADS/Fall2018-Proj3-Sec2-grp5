########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3

source("../lib/test_xgboost.R")
# helper function to get the value for pixel
# get_pixel_value <- function(All_value, Given_Row_Index, Given_Col_Index){
#   
#   if( Given_Row_Index <= 0 | Given_Row_Index > nrow(All_value) | Given_Col_Index <= 0 | Given_Col_Index > ncol(All_value) ){
#     return(NA)
#   }
#   
#   else{
#     return(All_value[Given_Row_Index, Given_Col_Index])
#   }
#   
# }

# helper function to get the value for the neighbor 8 pixels - central pixel
get_neighbor_pixel_value <- function(Chanel_Data, Given_Index){
  
  Row_Index <- arrayInd(Given_Index, dim(Chanel_Data))[1]
  Col_Index <- arrayInd(Given_Index, dim(Chanel_Data))[2]
  
  # 5*5
  Fivematrix <- matrix(NA,nrow=5,ncol=5)
  Fivematrix[max(4-Row_Index,1):min(3+nrow(Chanel_Data)-Row_Index,5),max(4-Col_Index,1):min(3+ncol(Chanel_Data)-Col_Index,5)] <- Chanel_Data[max(Row_Index-2,1):min(Row_Index+2,nrow(Chanel_Data)),max(Col_Index-2,1):min(Col_Index+2,ncol(Chanel_Data))]
  
  # 9*9
  # Ninematrix <- matrix(NA,nrow=9,ncol=9)
  #  Ninematrix[max(6-Row_Index,1):min(5+nrow(LR_image_data_chanel)-Row_Index,9),max(6-Col_Index,1):min(5+ncol(LR_image_data_chanel)-Col_Index+2,9)] 
  #  <- LR_image_data_chanel[max(Row_Index-4,1):min(Row_Index+4,nrow(LR_image_data_chanel)),max(Col_Index-4,1):min(Col_Index+4,ncol(LR_image_data_chanel))]
  
  
  Fivematrix <- Fivematrix-Fivematrix[3,3]
  
  
  
 # LR_Neighbor_value <- LR_Neighbor_value - LR_center
  
  LR_Neighbor_value <- as.vector(Fivematrix) 
  
  LR_Neighbor_NA_index <- which(is.na(LR_Neighbor_value))
  LR_Neighbor_value[LR_Neighbor_NA_index] <- 0
  
  LR_Neighbor_value <- LR_Neighbor_value[-13]
  # if(Given_Index == 500){
  #   print("======")
  #   print(LR_Neighbor_value)
  #   
  # }
  return(LR_Neighbor_value)
  
}



superResolution2<- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  Total_MSE <- c()
  Total_PSNR <- c()
  
  ### read LR/HR image pairs
  for(j in 1:n_files){
    
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", j), ".jpg"))
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", j), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 24, 3))
    
    
    Red_Chanel_Image_Data <- imageData(imgLR)[ , , 1]
    Green_Chanel_Image_Data <- imageData(imgLR)[ , , 2]
    Blue_Chanel_Image_Data <- imageData(imgLR)[ , , 3]
    
    
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    
    for (Index in c(1:length(Red_Chanel_Image_Data))) {
      
      featMat[Index, , 1] <- get_neighbor_pixel_value(Red_Chanel_Image_Data, Index)
      
      
      
      featMat[Index, , 2] <- get_neighbor_pixel_value(Green_Chanel_Image_Data, Index)
      
      
      
      featMat[Index, , 3] <- get_neighbor_pixel_value(Blue_Chanel_Image_Data, Index)
      
      
      
    }
    
    ### step 2. apply the modelList over featMat
    predMat <- test_xgboost(modelList, featMat)
    
    # print(predMat)
    
    New_Image_Data <- array(data = predMat, c(dim(imgLR)[1]*dim(imgLR)[2], 4, 3))
    
    HR_Image_Data <- array(data = NA, c(dim(imgLR)[1]*2 , dim(imgLR)[2]*2, 3))
    
    print(dim(HR_Image_Data))
    
    
    for (index in c(1:nrow(New_Image_Data[,,1]))) {
      
      Row_i <- arrayInd(index, dim(Red_Chanel_Image_Data))[1]
      Col_i <- arrayInd(index, dim(Red_Chanel_Image_Data))[2]

      
      # In Red Channel
      HR_Image_Data[2*Row_i-1, 2*Col_i-1,1] <- New_Image_Data[index, 1, 1] + Red_Chanel_Image_Data[ Row_i, Col_i]
      HR_Image_Data[2*Row_i, 2*Col_i,1-1] <- New_Image_Data[index, 2, 1] + Red_Chanel_Image_Data[ Row_i, Col_i]
      HR_Image_Data[2*Row_i-1, 2*Col_i,1] <- New_Image_Data[index, 3, 1] + Red_Chanel_Image_Data[ Row_i, Col_i]
      HR_Image_Data[2*Row_i, 2*Col_i,1] <- New_Image_Data[index, 4, 1] + Red_Chanel_Image_Data[ Row_i, Col_i]
      
      # Green
      HR_Image_Data[2*Row_i-1, 2*Col_i-1,2] <- New_Image_Data[index, 1, 2] + Green_Chanel_Image_Data[Row_i, Col_i]
      HR_Image_Data[2*Row_i, 2*Col_i-1,2] <- New_Image_Data[index, 2, 2] + Green_Chanel_Image_Data[Row_i, Col_i]
      HR_Image_Data[2*Row_i-1, 2*Col_i,2] <- New_Image_Data[index, 3, 2] + Green_Chanel_Image_Data[Row_i, Col_i]
      HR_Image_Data[2*Row_i, 2*Col_i,2] <- New_Image_Data[index, 4, 2] + Green_Chanel_Image_Data[Row_i, Col_i]
      
      # Blue
      HR_Image_Data[2*Row_i-1, 2*Col_i-1,3] <- New_Image_Data[index, 1, 3] + Blue_Chanel_Image_Data[Row_i, Col_i]
      HR_Image_Data[2*Row_i-1, 2*Col_i-1,3] <- New_Image_Data[index, 2, 3] + Blue_Chanel_Image_Data[Row_i, Col_i]
      HR_Image_Data[2*Row_i-1, 2*Col_i,3] <- New_Image_Data[index, 3, 3] + Blue_Chanel_Image_Data[Row_i, Col_i]
      HR_Image_Data[2*Row_i, 2*Col_i,3] <- New_Image_Data[index, 4, 3] + Blue_Chanel_Image_Data[Row_i, Col_i]
      
    }
    
    # HR_Image <- Image(predMat, dim=c(dim(imgLR)[1]*2, dim(imgLR)[2]*2, 3), colormode='Color')
    HR_Image <- Image(HR_Image_Data, colormode='Color')
    
    True_HR_Image_Data <- imageData(readImage(paste0("../data/train_set/HR/",  "img", "_", sprintf("%04d", j), ".jpg")))
    
    MSE <- mean((True_HR_Image_Data - HR_Image_Data)^2)
    
    Total_MSE <- c(Total_MSE, MSE)
    
    PSNR <- 20*log10(1) - 10*log10(MSE)
    
    Total_PSNR <- c(Total_PSNR, PSNR)
    
    # print(HR_Image)
    ### step 3. recover high-resolution from predMat and save in HR_dir
    writeImage(HR_Image, pathHR)
    
    cat("Image", j, "Done !\n")
    print(Sys.time())
  }
  
  print("Mean MSE : \n")
  print(mean(Total_MSE))
  print("Mean PSNR : \n")
  print(mean(Total_PSNR))
  
}

