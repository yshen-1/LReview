library(tibble)

is_not_empty_string <- function(x){
  x!=""
}

bib2df <- function(bib_file) {
  full_file <- stringr::str_c(bib_file, collapse="")
  raw_entries <- unlist(stringr::str_split(full_file, "@article")[1])
  entries <- raw_entries[sapply(raw_entries, is_not_empty_string)]
  test_entry <- entries[1]
  file_conn <- file("~/Rbibliometricsdev/bibliometrix0.2/diag/entry_test.txt")
  writeLines(test_entry, file_conn)
  close(file_conn)
}
