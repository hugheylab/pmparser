library(RCurl)
library(stringr)

getPubmedXml = function(startFile = 1, maxFiles = NULL, skipBaseline = FALSE, skipUpdates = FALSE){
  urlBaseline = "ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/"
  urlUpdates = "ftp://ftp.ncbi.nlm.nih.gov/pubmed/updatefiles/"
  baseline = getURL(url = urlBaseline)
  write(baseline, file = "data/baseline.txt")
  updates = getURL(url = urlUpdates)
  write(updates, file = "data/update_files.txt")
  baselineFileList = stringr::str_extract_all(baseline, pattern = "pubmed20n.*\\.gz(?=\n)")
  updateFileList = stringr::str_extract_all(updates, pattern = "pubmed20n.*\\.gz(?=\n)")
  for(baselineFile in baselineFileList[[1]]){
    if((is.null(maxFiles) || i <= maxFiles) && !skipBaseline){
      if(!file.exists(str_c("data/", baselineFile))){
        download.file(str_c(urlBaseline, baselineFile), str_c("data/", baselineFile))
      }
    }
    i = i + 1
  }
  for(updateFile in updateFileList[[1]]){
    if((is.null(maxFiles) || i <= maxFiles) && !skipUpdates){
      if(!file.exists(str_c("data/", updateFile))){
        download.file(str_c(urlUpdates, updateFile), str_c("data/", updateFile))
      }
    }
    i = i + 1
  }
}
