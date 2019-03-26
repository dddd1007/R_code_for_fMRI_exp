read_all_logfile_in_folder <- function(logdir, exclude = "intro|break"){
  all_file <- dir(logdir)
  all_file <- all_file[!str_detect(all_file, exclude)]
  
  all_data_list <- list()
  for (i in 1:length(all_file)) {
    path <- paste0(logdir, "/", all_file[i])
    all_data_list[[i]] <- read_logfile_to_dataframe(path)
  }
  
  all_data_dataframe <- do.call(rbind, all_data_list) %>% 
    arrange(time_stamp)
  return(all_data_dataframe)
}