mod_iselement <- function(v, el) {
  return(is.element(el, v))
}
network_analysis <- function(tidied_up_df, column_name, row1, row2){
  #ONLY USE ON COLUMNS AFTER set_separator() tidy function has been used.
  sliced_df <- tidied_up_df[row1:row2,]
  target_col <- sliced_df[[column_name]]
  sep <- attr(tidied_up_df[[column_name]], "sep")
  if ((sep=="") || (nrow(sliced_df)==0)){
    return(NULL)
  }
  split_col <- stringr::str_split(target_col, sep)
  labels <- unique(unlist(lapply(split_col, unique)))
  ids <- 1:length(labels)
  nodes <- data.frame(id=ids, label=labels)
  from <- vector("integer", length=length(labels)^3)
  edge_matrix <- matrix(, ncol=2, nrow=10*length(labels)*(length(labels)-1)) 
  starting_row <- 1
  for (i in 1:length(labels)){
    current_term <- labels[i]
    contains_term <- unlist(lapply(split_col, mod_iselement, current_term))
    relevant_col_entries <- unlist(split_col[contains_term])
    words_to <- relevant_col_entries[-which(relevant_col_entries==current_term)]
    for (j in 1:length(words_to)) {
      current_word <- words_to[j]
      to_index <- which(labels==current_word)
      edge_matrix[starting_row,] <- c(i, to_index) 
      starting_row <- starting_row+1
    }
  }
  final_edge_matrix <- na.omit(edge_matrix)
  edges <- data.frame(final_edge_matrix)
  colnames(edges) <- c("from", "to")
  return(list(nodes=nodes, edges=edges))
}
