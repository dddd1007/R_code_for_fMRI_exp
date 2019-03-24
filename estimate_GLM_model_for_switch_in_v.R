estimate_GLM_model_for_switch_in_v <- function(data = all_sub_with_switch_trials, switch = switch, code = code){
  lm_full_model_list <- list()
  sub_names <- unique(data$Subject)
  for (i in 1:length(sub_names)) {
    subject_i <- sub_names[i]
    
    data %>% 
      filter(condition == "V", switch_mark == switch, Code == code) %>% 
      filter(Subject == subject_i) %>% 
      lm(RT ~ CPE, data = .) %>% 
      summary() -> foo
    foo$coefficients %>% 
      reshape2::melt() -> bar
    lm_full_model_list[[i]] <- cbind(bar, subject = as.character(subject_i))
  }
  lm_full_model <- do.call(rbind.data.frame, lm_full_model_list)

  return(lm_full_model)
}