
#' @export
# This function compares the checksum of 2 files in a directory.
checkSumCompare = function(dataDir, # Directory the files are located in
                           md5File, # File to use to read downloaded MD5 sum
                           baseFile = NULL # File to calculate and compare against, if null uses same file name and path as md5File but removes '.md5' from end of filename
){
  if(is.null(baseFile) || baseFile == ''){
    baseFile = sub('.md5','', md5File)}

  md5Calculated = tools::md5sum(file.path(dataDir, baseFile))
  md5Downloaded = readr::read_file(file.path(dataDir, md5File))
  md5Downloaded = sub('.*= ', '', md5Downloaded)
  md5Downloaded = sub('\n', '', md5Downloaded)

  return(md5Downloaded == md5Calculated[[1]])
}


# This function grabs and downloads all files from the FTP server to the data subfolder.
#' @export
getPubmedXml = function(startFile = 1, # Which indexed file number to start with downloading (compared against i)
                        lastFile = NULL, # Which indexed file number to stop downloading at
                        skipBaseline = FALSE, # Indicates if you wish to skip the baseline downloads all together
                        skipUpdates = FALSE, # Indicates if you wish to skip the update downloads all together
                        dataDir = 'data', # Directory to save the results into
                        dbname = NULL # Database name to update, if null skips database step
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
  baselineFileList = stringr::str_extract_all(baseline, pattern = 'pubmed20n.*\\.gz.*(?=\n)')
  updateFileList = stringr::str_extract_all(updates, pattern = 'pubmed20n.*\\.gz.*(?=\n)')
  i = 1

  downloadedFiles = c()

  # Iterates over baseline files and checks to decide to download or not
  for (baselineFile in baselineFileList[[1]]){
    if ((is.null(startFile) || i >= startFile) && (is.null(lastFile) || i <= lastFile) && !skipBaseline){
      if (!file.exists(file.path(dataDir, baselineFile))){
        download.file(stringr::str_c(urlBaseline, baselineFile), file.path(dataDir, baselineFile))}
        if(!endsWith(baselineFile, '.xml.gz')){
          compare = checkSumCompare(dataDir, baselineFile)
          if(!compare){
            View(stringr::str_c('COMPARE NOT TRUE FOR MD5 SUM: ', baselineFile))}
          else {
            downloadedFiles = c(downloadedFiles, sub('.md5','', baselineFile))}}}
    if(!endsWith(baselineFile, '.xml.gz')){
      i = i + 1}}

  # Iterates over update files and checks to decide to download or not
  for (updateFile in updateFileList[[1]]){
    if ((is.null(startFile) || i >= startFile) && (is.null(lastFile) || i <= lastFile) && !skipUpdates){
      if (!file.exists(file.path(dataDir, updateFile))){
        download.file(stringr::str_c(urlBaseline, updateFile), file.path(dataDir, updateFile))}
      if(!endsWith(updateFile, '.xml.gz')){
        compare = checkSumCompare(dataDir, updateFile)
        if(!compare){
          View(stringr::str_c('COMPARE NOT TRUE FOR MD5 SUM: ', updateFile))}
        else {
          downloadedFiles = c(downloadedFiles, sub('.md5','', updateFile))}}}
    if(!endsWith(updateFile, '.xml.gz')){
      i = i + 1}}

  if(!is.null(dbname)){

    # Sys.setenv(PGHOST = "pubmed-parsing-test.choxmvghdfxg.us-east-2.rds.amazonaws.com")
    # Sys.setenv(PGPORT = "5432")
    # Sys.setenv(PGUSER = "postgres")
    # Sys.setenv(PGPASSWORD = "password")
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname)
    d = data.table::data.table(xml_file = unique(downloadedFiles),
                   processed = FALSE,
                   datetime_download = Sys.time(),
                   datetime_processed = NULL)
    d = d[!endsWith(xml_file, 'md5')]
    DBI::dbWriteTable(con, 'xml_processed', d,
                      overwrite = FALSE, append = TRUE)}

}

