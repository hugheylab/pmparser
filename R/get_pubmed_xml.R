

# This function returns a data table of all files on pubmed database
#' @export
getFileList = function(dataDir,
                       getDownloaded = FALSE
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

  fileTable = data.table(url = urlBaseline,
                         xml_file = unique(baselineFileList[[1]]),
                         md5_file = unique(stringr::str_c(baselineFileList[[1]], '.md5')),
                         type = 'baseline',
                         xml_download_succeeded = '',
                         md5_download_succeeded = '',
                         checksums_match = '')
  fileTable = rbind(fileTable, data.table(url = urlUpdates,
                                          xml_file = unique(updateFileList[[1]]),
                                          md5_file = unique(stringr::str_c(updateFileList[[1]], '.md5')),
                                          type = 'update',
                                          xml_download_succeeded = '',
                                          md5_download_succeeded = '',
                                          checksums_match = ''))
  if(!getDownloaded){
    fileTable = fileTable[!file.exists(file.path(dataDir, xml_file))]}

  if(length(fileTable) == 0L){
    stop('ERROR: File table returned from pubmed is empty.')}

  return(fileTable)

}

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
                        dbname = NULL, # Database name to update, if null skips database step
                        getDownloaded = FALSE
                        ){

  fileTable = getFileList(dataDir = dataDir)

  if(is.null(startFile)){
    startFile = 1}

  if(startFile > 1 || !is.null(lastFile)){
    if(is.null(lastFile)){
      lastFile = length(fileTable)}

    fileTable = fileTable[i = startFile:lastFile]}

  if(skipBaseline){
    fileTable = fileTable[type != 'baseline']}

  if(skipUpdates){
    fileTable = fileTable[type != 'update']}

  downloadedFiles = c()

  # Iterates over baseline files and checks to decide to download or not
  for (i in 1:nrow(fileTable)){
    file = fileTable[i,]
    download.file(stringr::str_c(file$url, file$xml_file), file.path(dataDir, file$xml_file))
    file$xml_download_succeeded = TRUE
    download.file(stringr::str_c(file$url, file$md5_file), file.path(dataDir, file$md5_file))
    file$md5_download_succeeded = TRUE

    compare = checkSumCompare(dataDir, file$md5_file)
    file$checksums_match = compare
    fileTable[i,] = file}

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

  return(fileTable)

}

