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

parse_entry_fields <- function(x) {
  char_division_matrix <- matrix(, nrow=nrow(x), ncol=2)
  entry_length <- attr(x, "entry_length")
  fields <- nrow(char_division_matrix)
  for (i in 1:fields) {
    if (i == fields) {
      char_division_matrix[i,1] <- (x[i,2]+1)
      char_division_matrix[i,2] <- (entry_length-2)
    } else {
      char_division_matrix[i,1] <- (x[i,2] +1 )
      char_division_matrix[i,2] <- (x[i+1,1] -1 )
    }
  }
  return(char_division_matrix)
}

bib2df <- function(bib_file) {
  full_file <- stringr::str_c(bib_file, collapse="")
  rawest_entries <- unlist(stringr::str_split(full_file, "@(?i)article")[1])
  raw_entries <- rawest_entries[sapply(rawest_entries, is_not_empty_string)]
  parser_field_regex <- ",([^={},!@#$%&;\\\\~]*)="
  is_entry <- stringr::str_detect(raw_entries, parser_field_regex)
  entries <- raw_entries[is_entry]
  headers <- stringr::str_match_all(entries, parser_field_regex) #Regex must be ",(NO EQUALS SIGNS or {})=" 
  parser_field_locations <- stringr::str_locate_all(pattern=parser_field_regex, entries)
  tibble_vars <- stringr::str_to_lower(trim_trailing_whitespace(headers[[1]][,2]))
  parser_fields <- trim_trailing_whitespace(headers[[1]][,2])
  entry_lengths <- stringr::str_length(entries)
  for (i in 1:length(parser_field_locations)) {
    attr(parser_field_locations[[i]], "entry_length") <- entry_lengths[i]
  }
  entry_locations <- lapply(parser_field_locations, parse_entry_fields)
  print(length(entries))
  print(entry_locations)
}
