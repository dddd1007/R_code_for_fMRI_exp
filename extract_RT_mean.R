# Extract Simon Effect

all_sub_without_na_outliers %>% 
  filter(Code == "con") %>% 
  group_by(sub, block) %>% 
  summarise(rt_con_mean = mean(RT)/10) %>% 
  spread(block, rt_con_mean) %>% 
  write.csv("../data_analysis_project2/output/rt_con_mean.csv")

all_sub_without_na_outliers %>% 
  filter(Code == "inc") %>% 
  group_by(sub, block) %>% 
  summarise(rt_inc_mean = mean(RT)/10) %>% 
  spread(block, rt_inc_mean) %>% 
  write.csv("../data_analysis_project2/output/rt_inc_mean.csv")