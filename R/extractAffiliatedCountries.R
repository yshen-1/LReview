extractAffiliatedCountries <- function(tidied_df){
  #Use set_separator() function on affiliation column first.
  #sep is "   " for wosrecs, ";" for scopus
  sep <- attr(tidied_df$affiliation, "sep")
  affiliation_entries <- stringr::str_split(tidied_df$affiliation, sep)
  split_entries <- lapply(affiliation_entries, stringr::str_match_all, ", ([^,]*)$")
  countries_col <- vector("list", length=length(tidied_df$affiliation))
  for (i in 1:length(split_entries)){
    countries <- unique(unlist(lapply(split_entries[[i]], function(x){ x[,2]})))
    stripped_countries <- gsub("\\.", "", countries)
    data_entry <- stringr::str_c(stripped_countries, collapse=sep)
    countries_col[[i]] <- data_entry
  }
  return(dplyr::mutate(tidied_df, countries_collab=unlist(countries_col)))
}
