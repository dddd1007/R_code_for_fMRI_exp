calc_CPE <- function(Code, p){
  if (Code == 1) {
    CEP = 1 - p
  }else{
    CEP = p
  }
  
  return(CEP)
}