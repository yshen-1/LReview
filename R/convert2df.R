source("readFiles.R") #Delete later
source("bib2df.R")
convert2df<-function(file_list){
  primary_output <- vector("list", length(file_list))
  for (i in 1:length(file_list)) {
    print("File complete")
    subject_file <- file_list[[i]]
    primary_output[[i]] <- switch(attr(subject_file, "file_type"), bib=bib2df(subject_file), csv=csv2df(subject_file), txt=txt2df(subject_file)) 
  }   
  invisible(primary_output) #make visible later
}


#TEST SECTION

test_object <- readFiles("~/Ranalysis/SmallScaleTests/scopus.bib")
convert2df(test_object)
