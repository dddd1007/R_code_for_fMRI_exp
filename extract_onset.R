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
    select(final_code, onset, Type, RT) -> foo
  
  foo %>% 
    filter(Type == "hit") %>% 
    split(.$final_code) %>% 
    map(mutate_outlier) %>% 
    reduce(rbind) -> bar
  bar$final_code <- as.character(bar$final_code)
  
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
  bar %>% 
    select(final_code, onset, Num) %>% 
    spread(final_code, onset, fill = NA) %>% 
    return()
}