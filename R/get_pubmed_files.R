getRemoteFilenames = function(url, pattern) {
  raw = RCurl::getURL(url)
  filenames = unlist(stringr::str_extract_all(raw, pattern))
  d = data.table(xml_filename = filenames,
                 md5_filename = paste0(filenames, '.md5'))
  return(d)}


getPubmedFileInfo = function(
  localDir = NULL, remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/',
  subDirs = c('baseline', 'updatefiles'), tableSuffix = NULL, con = NULL) {

  # if localDir is not NULL, skip files already downloaded
  # if con is not NULL, skip files already processed

  pattern = 'pubmed.*\\.xml\\.gz'

  dRemote = foreach(subDir = subDirs, .combine = rbind) %do% {
    dNow = getRemoteFilenames(paste0(remoteDir, subDir, '/'),
                              paste0(pattern, '(?=\n)'))
    dNow[, sub_dir := subDir]}

  if (is.null(localDir)) {
    dLocal = data.table(sub_dir = as.character(), xml_filename = as.character(),
                        xml_download = as.integer())
  } else {
    dLocal = foreach(subDir = subDirs, .combine = rbind) %do% {
      dNow = data.table(
        sub_dir = subDir,
        xml_filename = dir(file.path(localDir, subDir), paste0(pattern, '$')),
        xml_download = 0L)}}

  if (is.null(con)) {
    dDb = data.table(xml_filename = as.character(), processed = as.integer())
  } else {
    dDb = DBI::dbReadTable(con, paste_('xml_processed', tableSuffix))
    data.table::setDT(dDb)
    dDb = dDb[, .(xml_filename, processed = 0L)]}

  d = merge(dRemote, dLocal, by = c('sub_dir', 'xml_filename'), all.x = TRUE)
  d = merge(d, dDb, by = 'xml_filename', all.x = TRUE)
  setcolorder(d, 'sub_dir')
  return(d)}


getProvidedMd5s = function(filepaths) {
  x1 = sapply(filepaths, readLines, USE.NAMES = FALSE)
  x2 = trimws(gsub('.*=', '', x1))
  return(x2)}


checkPubmedFiles = function(xmlFilepaths, md5Filepaths) {
  d = data.table(md5_computed = tools::md5sum(file.path(xmlFilepaths)),
                 md5_provided = getProvidedMd5s(md5Filepaths))
  d[, md5_match := md5_computed == md5_provided]
  return(d[])}


getPubmedFiles = function(
  fileInfo, localDir, remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/',
  checkMd5 = TRUE) {

  for (subDir in unique(fileInfo$sub_dir)) {
    if (!dir.exists(file.path(localDir, subDir)))
      dir.create(file.path(localDir, subDir))}

  fileInfo = data.table(fileInfo)
  fileInfo[, md5_download := NA_integer_]

  # download all md5 files since it'll be quick and we know they're right
  d = foreach(f = iterators::iter(fileInfo, by = 'row'), .combine = rbind) %dopar% {
    x = foreach(pre = c('xml', 'md5'), .combine = cbind) %do% {
      col = paste_(pre, 'filename')
      down = paste_(pre, 'download')
      if (is.na(f[[down]])) {
        utils::download.file(paste0(remoteDir, f$sub_dir, '/', f[[col]]),
                             file.path(localDir, f$sub_dir, f[[col]]))
      } else {
        0}}}

  d = data.table::as.data.table(d)
  setnames(d, c('xml_download', 'md5_download'))
  fileInfo = cbind(fileInfo[, !c('xml_download', 'md5_download')], d)

  if (isTRUE(checkMd5)) {
    fileInfo[(xml_download == 0) & (md5_download == 0),
             c('md5_computed', 'md5_provided', 'md5_match') :=
               checkPubmedFiles(file.path(localDir, sub_dir, xml_filename),
                                file.path(localDir, sub_dir, md5_filename))]}

  return(fileInfo[])}
