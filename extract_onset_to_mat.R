# 本脚本用于生成对应log文件的onset文件
# 输入变量为: 
# sub_num       被试编号(如sub01)
# logfile_dir   读取log文件的文件夹路径
# mat_dir       生成mat文件的路径

# 配置分析环境
library(tidyverse)

# 计算PE
calc_CPE <- function(Code, p){
  if (Code == 1) {
    CEP = 1 - p
  }else{
    CEP = p
  }
  
  return(CEP)
}

# 抽取onset点
mark_outliner <- function(data){
  sd <- sd(data)
  mean <- mean(data)
  upper_sd <- mean + 3*sd
  lower_sd <- mean - 3*sd
  
  mark <- c(which(lower_sd > data), which(data > upper_sd))
  outlier_factor <- vector(mode = "numeric", 
                           length = length(data))
  outlier_factor[mark] <- 1
  return(outlier_factor)
}

mutate_outlier <- function(x) {
  cbind(x, outlier = mark_outliner(x$RT))
}

extract_onset <- function(logdata_frame){
  logdata_frame %>% 
    filter(Type == "hit" | Type == "miss" | Type == "incorrect") %>% 
    select(final_code, onset, Type, RT, CPE) -> foo
  
  if (nrow(filter(foo, Type == "hit")) >0) {
    foo %>% 
      filter(Type == "hit") %>% 
      split(.$final_code) %>% 
      map(mutate_outlier) %>% 
      reduce(rbind) -> bar
    bar$final_code <- as.character(bar$final_code)
  }else{
    bar <- NULL
  }
  
  
  if (sum(foo$Type != "hit") > 0) {
    bar2 <- foo %>% 
      filter(Type != "hit") %>% 
      cbind(outlier = 1)
  }else{
    bar2 <- NULL
  }
  
  bar <- rbind(bar, bar2)
  
  for (i in 1:nrow(bar)) {
    if (bar[i,]$Type != "hit" | bar[i,]$outlier == 1) {
      bar[i,]$final_code <- "error"
    }
  }
  
  bar$Num <- 1:nrow(bar)
  
  total_list <- list()
  
  total_list[[1]] <- bar %>% 
    select(final_code, onset, Num) %>% 
    spread(final_code, onset, fill = NA)
  total_list[[2]] <- bar %>% 
    select(final_code, CPE, Num) %>% 
    spread(final_code, CPE, fill = NA)
  
  return(total_list)
}

# 读取相应的被试数据并生成对应的 mat 文件的函数
extract_onset_to_mat <- function(sub_num = "sub01", logfile_dir, mat_dir, pe_dir, total_nscan = 1088){
  
  log_filenames <- dir(logfile_dir,full.names = T)
  log_filenames <- log_filenames[str_detect(log_filenames, "log")] %>% 
    .[!str_detect(., "intro|end|break")]
  focus_log <- log_filenames[str_detect(log_filenames, sub_num)]
  
  # read the logfiles
  all_log_list <- list()
  j = 1
  for (i in focus_log) {
    all_log_list[[j]] <- read_logfile_to_dataframe(i)
    j = j + 1
  }
  
  sub_data <- do.call(rbind.data.frame, all_log_list)
  
  session_list <- sort(c(rep(1:6, 160)))
  sub_data %>% 
    arrange(time_stamp) %>% 
    cbind(session = session_list) -> sub_data
  
  # if (nrow(sub_data) == total_nscan) {
  #   print("The log file is correct.")
  # }else{
  #   print("Please check the log file!")
  # }
  
  # 整合最终的code
  print("Init the final code...")
  # dir(pe_dir) %>% 
  #   .[stringr::str_detect(., sub_num)] -> p_filename
  # p_value <- read.csv(paste0(pe_dir,"/",p_filename))$p
  # sub_data <- cbind(sub_data, p_value)
  # sub_data %>% 
  #   mutate(CPE = calc_CPE(Code, p_value)) -> sub_data
  
  sub_data %>% 
    mutate(final_code = paste(Code, prop, condition, sep = "_")) -> sub_data
  
  # 按条件生成数据
  
  ## 非稳定条件
  volatility_data <- filter(sub_data, condition == "v")
  volatility_session_num <- unique(volatility_data$session)
  volatility_session <- paste0("session", volatility_session_num)
  volatility_list <- rbind(volatility_session_num, volatility_session, 
                           condition_name = c("v1", "v2", "v3", "v4"))
  

    for (i in 1:length(volatility_session_num)) {
      sub_data %>% 
        filter(session == volatility_session_num[i]) -> bar

        extract_onset(bar) -> foo
      assign(volatility_list[3,i], foo[[1]])
      assign(paste0(volatility_list[3,i], "_PE"), foo[[2]])
    }

    ## 稳定条件
    s_20_data <- filter(sub_data, logfile_name == "s_20")
    s_80_data <- filter(sub_data, logfile_name == "s_80")
    stable_session_num <- unique(c(s_20_data$session, s_80_data$session))
    stable_session <- paste0("session", stable_session_num)
    stable_list <- rbind(stable_session_num, stable_session, 
                         condition_name = c("s_20_1", "s_20_2", "s_80_1", "s_80_2"))
  
    for (i in 1:length(stable_session_num)) {
      sub_data %>% 
        filter(session == stable_session_num[i]) -> bar
      if (nrow(bar) == 0) {
        next()
      }
      extract_onset(bar) -> foo
      assign(stable_list[3,i], foo[[1]])
      assign(paste0(stable_list[3,i], "_PE"), foo[[2]])
    }
    
    session_table <- cbind(volatility_list, stable_list)
    onset_table <- cbind(v1, v2, v3, v4,s_20_1, s_20_2, s_80_1, s_80_2)
    PE_table <- cbind(v1_PE, v2_PE, v3_PE, v4_PE, s_20_1_PE, s_20_2_PE, s_80_1_PE, s_80_2_PE)
    
    print("Writing the MAT file.")
    print("Writing the MAT file..")
    print("Writing the MAT file...")
    R.matlab::writeMat(paste0(mat_dir,"/",sub_num,"_onset.MAT"), session = session_table, 
                       v1 = v1, v2 = v2, v3 = v3, s_part1 = s_20_1, s_part2 = s_20_2, s_part3 = s_80_1)
    print("Done.")
    print(paste0("MAT file location: ", " ", paste0(mat_dir,"/",sub_num,"_onset.MAT")))

}