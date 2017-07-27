library(tibble)

is_not_empty_string <- function(x){
  x!=""
}

trim_trailing_whitespace <- function(x){
  gsub("^\\s+|\\s+$","",x)
}
trim_trailing_braces <- function(x) {
  gsub("^\\{+|\\}+$", "", x)
}

bib2df <- function(bib_file) {
  full_file <- stringr::str_c(bib_file, collapse="")
  rawest_entries <- unlist(stringr::str_split(full_file, "@(?i)article")[1])
  raw_entries <- rawest_entries[sapply(rawest_entries, is_not_empty_string)]
  is_entry <- stringr::str_detect(raw_entries, ",([^={},!@#$%&;\\\\~]*)=")
  entries <- raw_entries[is_entry]
  headers <- stringr::str_match_all(entries, ",([^={},!@#$%&;\\\\~]*)=") #Regex must be ",(NO EQUALS SIGNS or {})=" 
  tibble_vars <- stringr::str_to_lower(trim_trailing_whitespace(headers[[1]][,2]))
  parser_fields <- trim_trailing_whitespace(headers[[1]][,2])
  test_entry <- entries[1]
  #print(trim_trailing_braces(stringr::str_match(test_entry, "Abstract\\s*=\\s*\\{([^=]*)\\}")[,2]))
  for (i in 1:length(parser_fields)){
    field_regex <- stringr::str_c(parser_fields[i], "\\s*=\\s*\\{([^=]*)\\}", collapse="")
    field_entry <- trim_trailing_braces(stringr::str_match(test_entry, field_regex)[,2])
    print(stringr::str_c(parser_fields[i], ":", collapse=""))
    print(field_entry)
  }
}
