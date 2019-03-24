# 本函数在原有logfile导入的数据框基础上增加block和实验条件两列，并直接修改code
modify_data <- function(raw_data, block_list = block_list, condition_list = condition_list){
  raw_data %>% 
    filter(Type == "hit" | Type == "miss" | Type == "incorrect") %>%
    cbind(block = block_list, condition = condition_list) -> raw_data
  raw_data$Code %>% 
    str_replace_all("red_left", "con") %>% 
    str_replace_all("red_right", "inc") %>% 
    str_replace_all("green_left", "inc") %>% 
    str_replace_all("green_right", "con") -> raw_data$Code
  modified_data <- raw_data
  modified_data$Code <- str_c(raw_data$Code, raw_data$condition, sep = "_")
  modified_data <- select(modified_data, Code, Type, RT, onset)
  return(modified_data)
}