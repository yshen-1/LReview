readFile <- function(file_path, file_type) {
    file_contents <- readLines(file_path, encoding="UTF-8")
    attr(file_contents, "file_type") <- file_type
    return(file_contents)
}

list_str_detect <- function(list_el, regex){
  return(stringr::str_detect(list_el[2], regex))
}

readFilesShiny <- function(...){
  arguments <- list(...)
  char_args <- arguments[unlist(lapply(arguments, is.character))]
  filtered_args <- char_args[unlist(lapply(char_args, list_str_detect, regex = "\\.((csv)|(bib)|(txt))$"))]
  if (length(filtered_args)==0) {
      print("Please input a valid file (bib)")
      return(NA)
  }
  output <- vector("list", length(filtered_args))
  for (i in 1:length(filtered_args)) {
    file_name <- filtered_args[[i]][2]
    extension <- stringr::str_extract(file_name, "...$")
    output[[i]] <- readFile(filtered_args[[i]][1], extension)
  }
  return(output)
}
