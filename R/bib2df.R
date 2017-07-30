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

enhanced_substr <- function(x,y) {
  output <- substr(x, y[1], y[2])
  filtered_output <- trim_trailing_braces(trim_trailing_whitespace(output))
  return(filtered_output)
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
  output_data <- data.frame(matrix(ncol=length(tibble_vars), nrow=length(entries)))
  colnames(output_data) <- tibble_vars
  for (i in 1:ncol(output_data)) {
    #print(output_data[i])
    data_entries_locas <- lapply(entry_locations, `[`,i,)
    new_data_col <- mapply(enhanced_substr, x=entries, y=data_entries_locas, SIMPLIFY=FALSE)
    filtered_col <- unlist(unname(new_data_col))
    output_data[i] <- filtered_col
  }
  print(output_data["unique-id"])
  #print(lapply(entry_locations, `[`,1,))
}
