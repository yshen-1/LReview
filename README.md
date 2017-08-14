# Bibliometrix 0.2 Description

Bibliometrix 0.2 is a R package based off of `bibliometrix` and is intended to 
mimic the functions `bibliometrix` provides in a more modern/user friendly manner.

# Sample Workflow
1. dataFiles <- bibliometrix0.2::readFiles("pathtoFile1", "pathtoFile2", ...)
2. parsedDataFiles <- bibliometrix0.2::convert2df(dataFiles)
3. tidiedDataframes <- bibliometrix0.2::tidydf(parsedDataFiles)

## For a coupling network analysis:
1. dataForAnalysis <- tidiedDataframes[[i]]
2. tidiedData <- set_separator(dataForAnalysis, "column_name_for_analysis", "column_separator")
3. nodesEdgesList <- networkAnalysis(tidiedData, "column_name_for_analysis", data_row_1, data_row_2)
4. visNetwork::visNetwork(nodesEdgesList$nodes, nodesEdgesList$edges, width="100%")
