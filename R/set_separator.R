#Sets data separators for each column
#list_of_seps is list of vectors (each entry in each vector corresponds to a column in a df)

set_separator <- function(tidy_df, column_name, separator){
  if (separator!=""){
    current_column <- tidy_df[[column_name]]
    split_column <- stringr::str_split(current_column, separator)
    trimmed_column <- lapply(split_column, trim_trailing_whitespace)
    space_replaced_column <- lapply(trimmed_column, replace_internal_spaces)
    reconstructed_column <- lapply(space_replaced_column, stringr::str_c, collapse=separator)
    new_column <- unlist(reconstructed_column)
    attr(new_column, "sep") <- separator
    tidied_df <- tidy_df
    tidied_df[[column_name]] <- new_column
    return(tidied_df)
  } else {
    current_column <- tidy_df[[column_name]]
    attr(current_column, "sep") <- separator
    tidied_df <- tidy_df
    tidied_df[[column_name]] <- current_column
    return(tidied_df)
  }
}
