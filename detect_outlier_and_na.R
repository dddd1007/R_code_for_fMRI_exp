detect_outlier_and_na <- function(vector, return_if_none = NULL){
  sd <- sd(vector)
  mean <- mean(vector)
  upper_sd <- mean + 3*sd
  lower_sd <- mean - 3*sd
  
  mark <- c(which(lower_sd > vector), which(vector > upper_sd), is.na(vector))
  
  if(length(mark) >= 1){
    return(mark)
  }else{
    return(return_if_none)
  }
}