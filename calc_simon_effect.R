# 计算单个被试 simon 效应的函数
calc_simon_effect <- function(data = NA, exclude_outlier = T){
  
  # extract the data when the sub hit the right button
  
  if(exclude_outlier){ 
    
    ## 建立处理特殊值的函数
    treat_dataframe_outlier_in_RT <- function(data){
      sd <- sd(data$RT)
      mean <- mean(data$RT)
      upper_sd <- mean + 3*sd
      lower_sd <- mean - 3*sd
      
      mark <- c(which(lower_sd > data$RT), which(data$RT > upper_sd))
      
      if(length(mark) > 0){
        data <- data[-mark,]
      }
      
      return(data)
    }
    
    ## 按条件（con、inc）处理特殊值
    data %>% 
      split(.$Code) %>% 
      map_dfr(treat_dataframe_outlier_in_RT) -> data
  }
  
  # sep the RT between con and inc
  con_data <- data$RT[which(data$Code == "con")]
  inc_data <- data$RT[which(data$Code == "inc")]
  
  simon_effect <- (mean(inc_data) - mean(con_data))
  
  simon_effect_table <- data.frame(simon_effect = simon_effect, con_mean = mean(con_data), inc_mean = mean(inc_data))
  return(simon_effect_table)
}