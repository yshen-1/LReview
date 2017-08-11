is_not_empty_string <- function(x){
  x!=""
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

vectorized_substr<- function(target_str, start_end_vect){
  output <- vector("list", length=nrow(start_end_vect))
  for (i in 1:nrow(start_end_vect)){
    output[i] <- enhanced_substr(target_str, c(start_end_vect[i,1], start_end_vect[i,2]))
  }
  return(output)
}

enhanced_str_detect <- function(parsed_entry_matrix, regex_expr) {
  return(stringr::str_detect(parsed_entry_matrix[,1], regex_expr))
}

is_zerolen_int <- function(x) {
  return (length(x)==0)
}

data_matrix_extraction <- function(x,y){
  return(x[y,2])
}

bib2df <- function(bib_file) {
  full_file <- stringr::str_c(bib_file, collapse="")
  bibtex_entrytypes <- c("article", "book", "booklet", "conference", "inbook", "incollection", "inproceedings", "manual", "mastersthesis", "misc", "phdthesis", "proceedings", "techreport", "unpublished")
  
  bibtex_entrytype_regex <- stringr::str_c(bibtex_entrytypes, collapse="|(?i)")
  full_bibtexentrytype_regex <- stringr::str_c("@((?i)", bibtex_entrytype_regex, ")", collapse="")
  rawest_entries <- unlist(stringr::str_split(full_file, full_bibtexentrytype_regex)[1])
  raw_entries <- rawest_entries[sapply(rawest_entries, is_not_empty_string)]
  parser_field_regex <- ",\\s*([^={},!@#$%&:/;\\\\~\\s]*)\\s*=\\s*\\{"
  is_entry <- stringr::str_detect(raw_entries, parser_field_regex)
  entries <- raw_entries[is_entry]
  headers <- stringr::str_match_all(entries, parser_field_regex) #Regex must be ",(NO EQUALS SIGNS or {})=" 
  parser_field_locations <- stringr::str_locate_all(pattern=parser_field_regex, entries)
  number_of_vars <- lapply(headers, nrow)
  var_defining_value <- which.max(number_of_vars)
  tibble_vars <- stringr::str_to_lower(trim_trailing_whitespace(headers[[var_defining_value]][,2]))
  parser_fields <- trim_trailing_whitespace(headers[[var_defining_value]][,2])
  entry_lengths <- stringr::str_length(entries)
  for (i in 1:length(parser_field_locations)) {
    attr(parser_field_locations[[i]], "entry_length") <- entry_lengths[i]
  }
  entry_locations <- lapply(parser_field_locations, parse_entry_fields)
  output_data <- data.frame(matrix(ncol=length(tibble_vars), nrow=length(entries)))
  colnames(output_data) <- tibble_vars
  parsed_entries <- vector("list", length=length(entries))
  for (i in 1:length(entries)) {
    entry_matrix <- matrix(, nrow=nrow(entry_locations[[i]]), ncol=2)
    entry_matrix[,1] <- unlist(vectorized_substr(entries[i], parser_field_locations[[i]]))
    entry_matrix[,2] <- unlist(vectorized_substr(entries[i], entry_locations[[i]]))
    parsed_entries[[i]] <- entry_matrix
    rm(entry_matrix)
  }
  #for (i in 1:1){
  for (i in 1:length(tibble_vars)) {
    entry_regex <- stringr::str_c(",\\s*(?i)", tibble_vars[i], "\\s*=", collapse="")
    entries_with_data <- lapply(parsed_entries, FUN=enhanced_str_detect, entry_regex)
    data_locations <- lapply(entries_with_data, FUN=which)
    missing_data_indices <- sapply(data_locations, is_zerolen_int)
    data_locations[missing_data_indices] <- NA 
    has_data_indices <- !(unlist(lapply(data_locations, is.na)))
    data_matrices <- parsed_entries[has_data_indices]
    data_indices <- data_locations[has_data_indices]
    data_for_tibble <- mapply(data_matrix_extraction, x=data_matrices, y=data_indices, SIMPLIFY=FALSE)
    data_locations[has_data_indices] <- data_for_tibble
    data_to_write <- unlist(data_locations)
    output_data[i] <- data_to_write
  }
  return(tibble::as_tibble(output_data))
}
