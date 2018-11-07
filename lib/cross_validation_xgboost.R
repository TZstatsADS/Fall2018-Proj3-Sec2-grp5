########################
### Cross Validation ###
########################

### Author: Chengliang Tang
### Project 3

cv.function_xgboost <- function(X.train, y.train, d, K){
  
  n <- dim(y.train)[1]
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X.train[s != i, ,]
    train.label <- y.train[s != i, ,]
    test.data <- X.train[s == i, ,]
    test.label <- y.train[s == i, ,]
    
    par <- list(depth=d)
    cat("k=", i, "\n")
    fit <- train_xgboost(train.data, train.label, par)
    cat(i, "Train finish","\n")
    pred <- test_xgboost(fit, test.data)  
    cv.error[i] <- mean((pred - test.label)^2)  
    
  }			
  return(c(mean(cv.error),sd(cv.error)))
}
