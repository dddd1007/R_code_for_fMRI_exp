# The input data must be as this:
# - code is "con" or "inc"
# - hit is 1 and icorrect is 0

calc_simon_acc_rate <- function(code_vector, acc_vector){
  
  acc_vector <- as.character(acc_vector)
  # sd <- sd(rt_vector)
  # mean <- mean(rt_vector)
  # upper_sd <- mean + 3*sd
  # lower_sd <- mean - 3*sd
  # 
  # mark <- c(which(lower_sd > rt_vector), which(rt_vector > upper_sd))
  # 
  # if(length(mark) > 0){
  #   code_vector <- code_vector[-mark]
  #   acc_vector <- acc_vector[-mark]
  # }
  
  con_num <- which(code_vector == "con")
  inc_num <- which(code_vector == "inc")
  
  acc_vector[which(acc_vector == "hit")] <- 1
  acc_vector[which(acc_vector == "incorrect" | acc_vector == "miss")] <- 0
  acc_vector <- as.numeric(as.character(acc_vector))
  
  con_acc_rate <- sum(acc_vector[con_num]) /  length(acc_vector[con_num])
  inc_acc_rate <- sum(acc_vector[inc_num]) /  length(acc_vector[inc_num])
  simon_acc_rate <- inc_acc_rate - con_acc_rate
  
  con_error_rate <- 1 - con_acc_rate
  inc_error_rate <- 1 - inc_acc_rate
  simon_error_rate <- inc_error_rate - con_error_rate
  
  acc_rate_table <- data.frame(simon_acc_rate = simon_acc_rate, con_acc = con_acc_rate, inc_acc = inc_acc_rate)
  error_rate_table <- data.frame(simon_error_rate = simon_error_rate, con_error_rate = con_error_rate, inc_error_rate = inc_error_rate)
  
  return(error_rate_table)
}