library(RCurl)
library(stringr)

getPubmedXml = function(startFile = 1, maxFiles = NULL, skipBaseline = FALSE, skipUpdates = FALSE){
  urlBaseline = "ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/"
  urlUpdates = "ftp://ftp.ncbi.nlm.nih.gov/pubmed/updatefiles/"
  baseline = RCurl::getURL(url = urlBaseline)
  write(baseline, file = file.path("data", "baseline.txt"))
  updates = RCurl::getURL(url = urlUpdates)
  write(updates, file = file.path("data", "baseline.txt"))
  baselineFileList = stringr::str_extract_all(baseline, pattern = "pubmed20n.*\\.gz(?=\n)")
  updateFileList = stringr::str_extract_all(updates, pattern = "pubmed20n.*\\.gz(?=\n)")
  for(baselineFile in baselineFileList[[1]]){
    if((is.null(maxFiles) || i <= maxFiles) && !skipBaseline){
      if(!file.exists(file.path("data", baselineFile))){
        download.file(str_c(urlBaseline, baselineFile), file.path("data", baselineFile))
      }
    }
    i = i + 1
  }
  for(updateFile in updateFileList[[1]]){
    if((is.null(maxFiles) || i <= maxFiles) && !skipUpdates){
      if(!file.exists(file.path("data", updateFile))){
        download.file(str_c(urlUpdates, updateFile), file.path("data", updateFile))
      }
    }
    i = i + 1
  }
}
