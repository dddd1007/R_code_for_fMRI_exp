#' @title spm_code_generator
#' @description This function could generate the SPM contrast code by rule and session conditions
#' @param condition_vector The vector which describe the experiment sessions
#'        contrast_list A list show the condition of contrast. Exp. "(inc - con)*(80 - 20)*(v - s)"
#' @return A data frame contain the contrast code

spm_code_generator <- function(condition_vector, contrast_list){
    result_list <- list()
    
    for (i in 1:length(contrast_list)) {
        contrast_condition <- contrast_list[[i]][1]
        result_list[[i]] <- match_code(condition_vector, contrast_condition)
    }
    
    result <- do.call(rbind, result_list)
    rownames(result) <- unlist(contrast_list)
    colnames(result) <- condition_vector
    
    return(result)
}

match_code <- function(condition_vector, contrast_condition){
    ## contrast
    require(magrittr)

    interaction_level <- stringr::str_count(contrast_condition, "\\(")

    if(interaction_level == 3){
        condition_elements <- stringr::str_split(contrast_condition, "\\*")

        stable_elements <- condition_elements[[1]][1] %>% 
            stringr::str_remove("\\(") %>%
            stringr::str_remove("\\)") %>% 
            stringr::str_remove_all(" ") %>% 
            stringr::str_split("-")
        prop_elements <- condition_elements[[1]][2] %>% 
            stringr::str_remove("\\(") %>%
            stringr::str_remove("\\)") %>% 
            stringr::str_remove_all(" ") %>% 
            stringr::str_split("-")
        contigency_elements <- condition_elements[[1]][3] %>% 
            stringr::str_remove("\\(") %>%
            stringr::str_remove("\\)") %>% 
            stringr::str_remove_all(" ") %>% 
            stringr::str_split("-")
               
        stable_condition <- c(rep(stable_elements[[1]][1], 4), 
                              rep(stable_elements[[1]][2], 4))
        prop_condition <- c(rep(c(rep(prop_elements[[1]][1], 2), rep(prop_elements[[1]][2], 2)), 2))
        contigency_condition <- c(rep(contigency_elements[[1]], 4))
        symbols_condition <- c(1,-1,-1,1,-1,1,1,-1)
        
        check_table <- data.frame(stable_condition, prop_condition, contigency_condition, symbols_condition)
    }
       
    check_if_satisfy <- function(condition_item, check_vector){
            match1 <- stringr::str_detect(condition_item, as.character(check_vector$stable_condition))
            match2 <- stringr::str_detect(condition_item, as.character(check_vector$prop_condition))
            match3 <- stringr::str_detect(condition_item, as.character(check_vector$contigency_condition))
            
            count <- sum(match1, match2, match3)
            
            if (count == 3) {
                result <- TRUE
            }else{
                result <- FALSE
            }
            
            return(result)
        }
        
        result_vector <- vector(length = length(condition_vector), mode = "numeric")
        
        for (i in 1:nrow(check_table)) {
            for (j in 1:length(condition_vector)) {
                if (check_if_satisfy(condition_item = condition_vector[j],
                                     check_vector = check_table[i,])) {
                    result_vector[j] <- check_table[i,]$symbols_condition
                }
            }
        }
    return(result_vector)
}
