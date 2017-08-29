extract_affiliated_countries <- function(tidied_df){
  #Use set_separator() function on affiliation column first.
  #sep is "   " for wosrecs, ";" for scopus
  sep <- attr(tidied_df$affiliation, "sep")
  if (sep=="") {return(tidied_df)}
  affiliation_entries <- stringr::str_split(tidied_df$affiliation, sep)
  split_entries <- lapply(affiliation_entries, stringr::str_match_all, ", ([^,]*)$")
  countries_col <- vector("list", length=length(tidied_df$affiliation))
  for (i in 1:length(split_entries)){
    if (is.na(tidied_df$affiliation[i])){
      countries_col[[i]] <- NA
    } else {
      raw_countries <- unique(unlist(lapply(split_entries[[i]], function(x){ x[,2]})))
      if (identical(raw_countries, character(0))){
        print("No country affiliation found.")
        countries_col[[i]] <- NA
      } else {
        tidier_countries <- unlist(lapply(stringr::str_match_all(raw_countries, "([^0123456789]*)$"), function(x){x[,2]}))
        countries <- trim_trailing_whitespace(tidier_countries[sapply(tidier_countries, function(x){x!=""})])
        stripped_countries <- unique(gsub("\\.", "", countries))
        data_entry <- stringr::str_c(stripped_countries, collapse=sep)
        if ((identical(data_entry, character(0))) || (data_entry=="")){
          countries_col[[i]] <- NA
        } else {
          countries_col[[i]] <- data_entry
        }
      }
    }
  }
  return(dplyr::mutate(tidied_df, countries_collab=unlist(countries_col)))
}
