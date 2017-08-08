#Tidies up author columns to format used by bibliometrix,
#converts columns containing numbers to dbl type
tidydf <- function(list_of_dfs) {
  tidy_output <- vector("list", length=length(list_of_dfs))
  for (i in 1:length(list_of_dfs)) {
    target_df <- list_of_dfs[[i]]
    output_df <- target_df
    column_names <- attr(target_df, "names")
    has_author <- any(stringr::str_detect(column_names, "^(?i)author$"))
    if (has_author) {
      which_column_author <- which(stringr::str_detect(column_names, "^(?i)author$"))
      tidier_column <- lapply(target_df[[which_column_author]], stringr::str_replace_all, "\\s*and\\s*", ";")
      tidiest_column <- lapply(tidier_column, stringr::str_replace_all, "\\s*,\\s*", " ")
      output_df[which_column_author] <- unlist(tidiest_column)
    }
    nonnumeric_regex <- "[A-Za-z$&+,:;=?@#|'<>.^*()%!-]"
    for (j in 1:ncol(target_df)){
      current_col <- target_df[[j]]
      has_nonnumbers <- stringr::str_detect(current_col, nonnumeric_regex)
      has_numbers <- !has_nonnumbers
      total_entries <- length(current_col)
      numeric_entries <- sum(has_numbers, na.rm=TRUE)
      if ((numeric_entries/total_entries) > 0.7){
        output_df[[j]] <- as.numeric(current_col)
      }
    }
    tidy_output[[i]] <- output_df 
  }
  return(tidy_output)
}
