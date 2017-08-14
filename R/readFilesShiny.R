#' Load a sequence of ISI or SCOPUS Export files into a large character object
#'
#' It loads a sequence of SCOPUS and Thomson Reuters' ISI Web of Knowledge export files and create a large character vector from it.
#'
#'
#' @param ... is a sequence of names of files downaloaded from ISI WOS.(in plain text or bibtex format) or SCOPUS Export file (exclusively in bibtex format).
#' @return a character vector of length the number of lines read.
#'
#' @examples
#' # ISI or SCOPUS Export files can be read using \code{\link{readFiles}} function:
#'
#' # largechar <- readFiles('filename1.txt','filename2.txt','filename3.txt')
#'
#' # filename1.txt, filename2.txt and filename3.txt are ISI or SCOPUS Export file 
#' # in plain text or bibtex format.
#'
#' D <- readFiles('http://www.bibliometrix.org/datasets/bibliometrics_articles.txt')
#'
#' @seealso \code{\link{convert2df}} for converting SCOPUS of ISI Export file into a dataframe
#' 
#' @export
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
      print("Please input a valid file (csv, txt, bib)")
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
