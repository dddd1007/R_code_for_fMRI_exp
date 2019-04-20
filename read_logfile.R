# 本脚本从第三个类江界峰实验中的logfile获取数据
# 由于在这次实验中, 我们重复使用了多个scenario, 且各自独立获取. 因此我们排序的
# 依据, 便包括了被试编号及记录时间. 即记录时间较早的为较早的run.

read_logfile <- function(logfile = NA, end_code = "jitter") {
  if (is.na(logfile)) {
    logfile <- file.choose()
  }
  
  source("check_first_onset.R")
  temp_text_file <- readLines(logfile)
  
  index_message <- which(stringr::str_detect(temp_text_file, "Subject"))[[1]]
  end_message <- which(stringr::str_detect(temp_text_file, end_code))[[sum(str_detect(temp_text_file, end_code))]]
  index_RT <- which(stringr::str_detect(temp_text_file,"RT"))[[1]]
  time_stamp <- which(stringr::str_detect(temp_text_file,"Logfile written"))[[1]]
  #scenario <- which(stringr::str_detect(temp_text_file,"scenario"))[[1]]
  #subject <- scenario + 1
  
  message_table <- read.delim(textConnection(temp_text_file[(index_message):(end_message+2)]), sep = "\t") %>% 
    filter(Event.Type == "Picture") %>% 
    select(Subject, Trial)
  RT_table <- read.csv(textConnection(c(temp_text_file[index_RT], temp_text_file[(index_RT + 2):length(temp_text_file)])), sep = "\t")%>% 
    filter(Event.Type == "Picture")
  time_stamp <- which(stringr::str_detect(temp_text_file,"Logfile written"))[[1]] %>% 
    temp_text_file[.] %>% 
    str_split(" - ") %>% 
    unlist() %>% 
    .[2] %>% 
    lubridate::mdy_hms() %>% 
    as.numeric()
  
  # calc the onset / TR
  # I found that sometime I press the reset butten before running the experiment programme,
  # it may cause the first zero onset is not the real one.
  # so I need to fix it.
  
  if(check_first_onset(logfile)){
    zero_onset_loc <- 1
  }else{
    zero_onset_loc <- 2
  }
  zero_onset <- read.delim(textConnection(temp_text_file[(index_message):(end_message+2)]), sep = "\t") %>% 
    filter(Code == 30, Trial == 0)  %>% 
    .$Time %>% 
    .[zero_onset_loc]
  
  RT_table$RT <- RT_table$RT / 10
  
  # 添加修饰列表
  logfile_name <- stringr::str_extract(logfile, "v_[28]0|s[12]_part[123]")
  block_list <- c(rep(1,40), rep(2,40), rep(3,40), rep(4,40))
  
  if (logfile_name == "s1_part1") {
    prop <- c(rep(80, 120),rep(20,40))
    condition <- "s"
  }else if (logfile_name == "s1_part2") {
    prop <- c(rep(20, 80), rep(80,80))
    condition <- "s"
  }else if (logfile_name == "s1_part3") {
    prop <- c(rep(80, 40), rep(20,120))
    condition <- "s"
  }else if (logfile_name == "s2_part1") {
    prop <- c(rep(20, 120), rep(80,40))
    condition <- "s"
  }else if (logfile_name == "s2_part2") {
    prop <- c(rep(80, 80), rep(20,80))
    condition <- "s"
  }else if (logfile_name == "s2_part3") {
    prop <- c(rep(20, 40), rep(80,120))
    condition <- "s"
  }else if (logfile_name == "v_20") {
    prop <- c(rep(20, 40), rep(80, 40), 
              rep(20, 40), rep(80, 40))
    condition <- "v"
  }else if (logfile_name == "v_80") {
    prop <- c(rep(80, 40), rep(20, 40), 
              rep(80, 40), rep(20, 40))
    condition <- "v"    
  }
  
  result_table <- RT_table %>% 
    cbind(message_table) %>% 
    filter(Type != "other") %>% 
    cbind(logfile_name, prop, condition, block = block_list, time_stamp)
  
  result_table$Code %>% 
    str_replace_all("red_left", "con") %>% 
    str_replace_all("red_right", "inc") %>% 
    str_replace_all("green_left", "inc") %>% 
    str_replace_all("green_right", "con") -> result_table$Code
  
  result_table$Time <- result_table$Time - as.numeric(as.character(zero_onset))
  result <- mutate(result_table, onset = Time / 20000)
  
  result <- cbind(result, filename = logfile)
  
  #time_stamp <- str_split(temp_text_file[time_stamp], "\"")[[1]][[4]] %>% 
  #  as.POSIXct() %>% 
  #  as.numeric()
  #scenario <- str_split(temp_text_file[scenario], "\"")[[1]][[4]] %>% 
  #  str_remove(".sce")
  
  return(result)
}