detect_outlier_and_na <- function(vector, return_if_none = 65535){
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

remove_outlier_by_RT <- function(input_data_frame){
  data_without_NA <- na.omit(input_data_frame)
  
  data_without_outlier_NA <- data_without_NA[!detect_outlier_and_na(data_without_NA$RT),]
  
  return(data_without_outlier_NA)
}