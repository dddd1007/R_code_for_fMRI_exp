# 本函数用于找出单个被试的switch trials
# 参数x为单个被试的data.frame，其中Code列中的标记为con或inc而非01
# mark1为需要标记的第一种mark，为从inc到con
# mark2为需要标记的第二种mark，为从con到inc
# 

mark_switch_trials <- function(x, mark1 = 1, mark2 = 2){
  mark_vector <- vector(mode = "numeric", length = nrow(x))
  check_vector <- c(2:nrow(x)) #从2开始检查这些行数
  check_code_vector <- x$Code
  
  for(i in check_vector){
    if(check_code_vector[i] == check_code_vector[i-1]){
      mark_vector[i] <- 0
    }else{
      
      if (check_code_vector[i] == "con") {
        mark_vector[i] <- mark1
      }else if(check_code_vector[i] == "inc"){
        mark_vector[i] <- mark2
      }
        
    }
  }
  
  mark_vector[1] <- 0
  
  result_dataframe <- cbind(x, switch_mark = mark_vector)
  return(result_dataframe)
}