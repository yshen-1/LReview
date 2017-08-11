trim_trailing_whitespace <- function(x){
  gsub("^\\s+|\\s+$","",x)
}
