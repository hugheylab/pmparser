getRemoteFilenames = function(url, pattern) {
  raw = RCurl::getURL(url)
  filenames = unlist(stringr::str_extract_all(raw, pattern))
  d = data.table(xml_filename = filenames,
                 md5_filename = paste0(filenames, '.md5'))
  return(d)}


#' @export
getPubmedFileInfo = function(
  localDir = NULL, remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/',
  subDirs = c('baseline', 'updatefiles'), tableSuffix = NULL,
  dbname = NULL, ...) {

  # if localDir is not NULL, skip files already downloaded
  # if dbname is not NULL, skip files already processed

  pattern = 'pubmed.*\\.xml\\.gz'

  dRemote = foreach(subDir = subDirs, .combine = rbind) %do% {
    dNow = getRemoteFilenames(paste0(remoteDir, subDir, '/'),
                              paste0(pattern, '(?=\n)'))
    dNow[, sub_dir := subDir]}
  setcolorder(dRemote, 'sub_dir')

  if (is.null(localDir)) {
    dLocal = data.table(sub_dir = as.character(), xml_filename = as.character())
  } else {
    dLocal = foreach(subDir = subDirs, .combine = rbind) %do% {
      dNow = data.table(
        sub_dir = subDir,
        xml_filename = dir(file.path(localDir, subDir), paste0(pattern, '$')))}}

  if (is.null(dbname)) {
    dProcessed = data.table(xml_filename = as.character())
  } else {
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)
    dProcessed = DBI::dbReadTable(con, paste_('xml_processed', tableSuffix))
    dProcessed = data.table::setDT(dProcessed)[, .(xml_filename)]}

  dKeep = dRemote[!dLocal, on = c('sub_dir', 'xml_filename')]
  dKeep = dKeep[!dProcessed, on = 'xml_filename']
  setattr(dKeep, 'remoteDir', remoteDir)
  return(dKeep)}


getProvidedMd5s = function(filepaths) {
  x1 = sapply(filepaths, readLines, USE.NAMES = FALSE)
  x2 = trimws(gsub('.*=', '', x1))
  return(x2)}


checkPubmedFiles = function(xmlFilepaths, md5Filepaths) {
  d = data.table(md5_computed = tools::md5sum(file.path(xmlFilepaths)),
                 md5_provided = getProvidedMd5s(md5Filepaths))
  d[, md5_match := md5_computed == md5_provided]
  return(d[])}


#' @export
getPubmedFiles = function(fileInfo, localDir, checkMd5 = TRUE) {
  remoteDir = attr(fileInfo, 'remoteDir')

  cols = c('xml_filename', 'md5_filename')
  d = foreach(f = iterators::iter(fileInfo, by = 'row'), .combine = rbind) %dopar% {
    x = foreach(col = cols, .combine = cbind) %do% {
      utils::download.file(paste0(remoteDir, f$sub_dir, '/', f[[col]]),
                           file.path(localDir, f$sub_dir, f[[col]]))}}

  d = data.table::as.data.table(d)
  setnames(d, c('xml_download', 'md5_download'))
  fileInfo = cbind(fileInfo, d)

  if (isTRUE(checkMd5)) {
    fileInfo[(xml_download == 0) & (md5_download == 0),
             c('md5_computed', 'md5_provided', 'md5_match') :=
               checkPubmedFiles(file.path(localDir, sub_dir, xml_filename),
                                file.path(localDir, sub_dir, md5_filename))]}

  return(fileInfo[])}
