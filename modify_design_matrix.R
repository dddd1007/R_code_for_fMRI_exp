# 本函数旨在解决每次生成 contrast matrix 时需要添加其他的列（如头动XYZ等）
# 时手工处理比较麻烦的问题
# 希望可以输入较为简单csv文件后，直接导出相应已经添加好多余列的文件
#
# input:
# csv_file: 根据条件已经填入1，-1等的设计矩阵
# col_num_by_condition: 为一个数值向量每种条件的列数，程序会在条件内的
#                       每一列增加两列空白列，在条件后增加六列头动列
#
# output: 一个CSV文件，可以直接粘贴到 SPM 的 contrast 条件当中

modify_design_matrix <- function(csv_file, col_num_by_condition) {
  split_list <- list()
  modified_list <- list()
  
  matrix_data <- read_csv(csv_file)
  
  for(i in 1:length(col_num_by_condition)){
    end_num <- col_num_by_condition[i]
    
    if(i == 1){
      start_num <- 1
    }else{
      start_num <- (col_num_by_condition[i-1] + 1)
    }
    
    split_list[[i]] <- matrix_data[start_num, end_num]
  }
  
  for (i in 1:length(split_list)) {
    modified_list[[i]] <- add_other_signal_col(split_list[[i]])
  }
  
  result <- do.call(cbind, modified_list)
  
  return(result)
}

add_other_signal_col <- function(input_data){
  foo_list <- list()
  for (i in 1:ncol(input_data)) {
    foo_list[[i]] <- cbind(cbind(input_data[,i],0),0)
  }
  foo <- do.call(cbind, foo_list)
  foo <- cbind(foo, matrix(0, nrow = nrow(foo), ncol = 6))
  return(foo)
}
