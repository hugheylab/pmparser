# This function grabs and downloads all files from the FTP server to the data subfolder.

getPubmedXml = function(startFile = 1, # Which indexed file number to start with downloading (compared against i)
                        lastFile = NULL, # Which indexed file number to stop downloading at
                        skipBaseline = FALSE, # Indicates if you wish to skip the baseline downloads all together
                        skipUpdates = FALSE, # Indicates if you wish to skip the update downloads all together
                        dataDir = 'data' #Directory to save the results into
                        ){

  # Set the url variables to the FTP server folder links
  urlBaseline = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/'
  urlUpdates = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/updatefiles/'

  # Returns back the file list for both URLs and writes them to their respective .txt files
  baseline = RCurl::getURL(url = urlBaseline)
  write(baseline, file = file.path(dataDir, 'baseline.txt'))
  updates = RCurl::getURL(url = urlUpdates)
  write(updates, file = file.path(dataDir, 'updates.txt'))

  # Extracts a string list of file names via regex
  baselineFileList = stringr::str_extract_all(baseline, pattern = 'pubmed20n.*\\.gz(?=\n)')
  updateFileList = stringr::str_extract_all(updates, pattern = 'pubmed20n.*\\.gz(?=\n)')

  # Iterates over baseline files and checks to decide to download or not
  for (baselineFile in baselineFileList[[1]]){
    if ((is.null(startFile) || i >= startFile) && (is.null(lastFile) || i <= lastFile) && !skipBaseline){
      if (!file.exists(file.path(dataDir, baselineFile))){
        download.file(str_c(urlBaseline, baselineFile), file.path(dataDir, baselineFile))}}
    i = i + 1}

  # Iterates over update files and checks to decide to download or not
  for (updateFile in updateFileList[[1]]){
    if ((is.null(startFile) || i >= startFile) && (is.null(lastFile) || i <= lastFile) && !skipUpdates){
      if (!file.exists(file.path(dataDir, updateFile))){
        download.file(str_c(urlUpdates, updateFile), file.path(dataDir, updateFile))}}
    i = i + 1}
}
