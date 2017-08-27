readFile <- function(file_path, file_type) {
    file_contents <- readLines(file_path, encoding="UTF-8")
    attr(file_contents, "file_type") <- file_type
    return(file_contents)
}

readFiles <- function(...){
  arguments <- list(...)
  char_args <- unlist(arguments[unlist(lapply(arguments, is.character))])
  filtered_args <- char_args[stringr::str_detect(char_args, "\\.((csv)|(bib)|(txt))$")]
  if (length(filtered_args)==0) {
      print("Please input a valid file (bib)")
      return(NA)
  }
  output <- vector("list", length(filtered_args))
  for (i in 1:length(filtered_args)) {
    file_name <- filtered_args[i]
    extension <- stringr::str_extract(file_name, "...$")
    output[[i]] <- readFile(file_name, extension)
  }
  return(output)
}
