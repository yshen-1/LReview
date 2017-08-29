convert2df_shiny<-function(file_list){
  primary_output <- vector("list", length(file_list))
  for (i in 1:length(file_list)) {
    print("File complete")
    subject_file <- file_list[[i]]
    primary_output[[i]] <- switch(attr(subject_file, "file_type"), bib=bib2df(subject_file)) 
  }   
  return(primary_output) 
}
