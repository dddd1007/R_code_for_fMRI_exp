# 因为实验操作问题，有 Prensentation 的 Log 文件记录的并非全部是 dummey scan 的时间，
# 而是第一个pulse记录的是按复位按钮的信号。本函数旨在判断第一个pulse是否是dummey scan的信号，
# 原理是检测第一个pulse和第二个pulse之间的之间是否是恰好是2s
# 如果非2s，则返回FALSE

check_first_onset <- function(logfile = NA) {
    if(is.na(logfile)) {logfile <- file.choose()} 
    temp_info <- readLines(logfile)
    index_message <- which(stringr::str_detect(temp_info, "Subject"))[[1]]
    pulse_location <- which(stringr::str_detect(temp_info, "Pulse"))
    pulse_table <- read.delim(textConnection(temp_info[c(index_message, pulse_location[1:4])]))
    dummey_time_first <- pulse_table$Time[2] - pulse_table$Time[1]
    
    if(dummey_time_first > 21000 | dummey_time_first < 19000){
        warning(paste0("The first diff time is ",dummey_time_first))
        result <- FALSE # Show the first pulse is not a dummey scan
    }else{
        result <- TRUE  # The first pulse is a dummey scan pulse
    }
    
    return(result)
}
