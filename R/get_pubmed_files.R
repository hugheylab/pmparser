getRemoteFilenames = function(url, pattern) {
  raw = RCurl::getURL(url)
  x = strsplit(raw, getOSLineDelim())[[1L]]
  m = regexpr(glue('{pattern}$'), x)
  filenames = regmatches(x, m)
  d = data.table(
    xml_filename = filenames, md5_filename = glue('{filenames}.md5'))
  return(d)}


getPubmedFileInfo = function(
  localDir = NULL, remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/',
  subDirs = c('baseline', 'updatefiles'), tableSuffix = NULL, con = NULL) {

  sub_dir = subDir = . = xml_filename = NULL
  pattern = 'pubmed.*\\.xml\\.gz'

  dRemote = foreach(subDir = subDirs, .combine = rbind) %do% {
    dNow = getRemoteFilenames(glue('{remoteDir}/{subDir}/'), pattern)
    dNow[, sub_dir := subDir]}

  dEmpty = data.table(
    sub_dir = as.character(), xml_filename = as.character(),
    xml_download = as.integer())

  if (is.null(localDir)) {
    dLocal = dEmpty
  } else {
    dLocal = foreach(subDir = subDirs, .combine = rbind) %do% {
      filenames = dir(file.path(localDir, subDir), glue('{pattern}$'))

      if (length(filenames) == 0) {
        dNow = dEmpty
      } else {
        dNow = data.table(
          sub_dir = subDir, xml_filename = filenames, xml_download = 0L)}}}

  if (is.null(con)) {
    dDb = data.table(xml_filename = as.character(), processed = as.integer())
  } else {
    dDb = DBI::dbReadTable(con, paste_('xml_processed', tableSuffix))
    data.table::setDT(dDb)
    dDb = dDb[, .(xml_filename, processed = 0L)]}

  d = merge(dRemote, dDb, by = 'xml_filename', all.x = TRUE)
  d = merge(d, dLocal, by = c('sub_dir', 'xml_filename'), all.x = TRUE)
  setcolorder(d, 'sub_dir')
  return(d)}


getProvidedMd5s = function(filepaths) {
  x1 = sapply(filepaths, readLines, USE.NAMES = FALSE)
  x2 = trimws(gsub('.*=', '', x1))
  return(x2)}


checkPubmedFiles = function(xmlFilepaths, md5Filepaths) {
  md5_match = md5_computed = md5_provided = NULL
  d = data.table(md5_computed = tools::md5sum(file.path(xmlFilepaths)),
                 md5_provided = getProvidedMd5s(md5Filepaths))
  d[, md5_match := md5_computed == md5_provided]
  return(d[])}


getPubmedFiles = function(
  fileInfo, localDir, remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/',
  downloadMd5 = TRUE) {

  md5_download = xml_download = f = sub_dir = xml_filename = md5_filename =
    md5_match = NULL

  for (subDir in unique(fileInfo$sub_dir)) {
    if (!dir.exists(file.path(localDir, subDir)))
      dir.create(file.path(localDir, subDir))}

  fileInfo = data.table(fileInfo)

  # default to download md5 files since it's quick and we know they're right
  if (isTRUE(downloadMd5)) {
    fileInfo[, md5_download := NA_integer_]
  } else { # trick for testing
    fileInfo[, md5_download := xml_download]}

  # download md5 files
  fTmp = fileInfo[is.na(md5_download)]
  col = 'md5_filename'
  r = foreach(f = iterators::iter(fTmp, by = 'row'), .combine = c) %dopar% {
    remoteDir;
    download(glue('{remoteDir}/{f$sub_dir}/{f[[col]]}'),
             file.path(localDir, f$sub_dir, f[[col]]))}
  fileInfo[is.na(md5_download), md5_download := r]

  # check md5 sums
  fileInfo[, `:=`(md5_computed = '', md5_provided = '', md5_match = FALSE)]
  fileInfo[md5_download == 0 & xml_download == 0,
           c('md5_computed', 'md5_provided', 'md5_match') :=
             checkPubmedFiles(file.path(localDir, sub_dir, xml_filename),
                              file.path(localDir, sub_dir, md5_filename))]

  # download xml files
  fTmp = fileInfo[!(md5_match)]
  col = 'xml_filename'
  r = foreach(f = iterators::iter(fTmp, by = 'row'), .combine = c) %dopar% {
    remoteDir;
    download(glue('{remoteDir}{f$sub_dir}/{f[[col]]}'),
             file.path(localDir, f$sub_dir, f[[col]]))}
  fileInfo[!(md5_match), xml_download := r]

  # check md5 sums again
  fileInfo[md5_download == 0 & xml_download == 0,
           c('md5_computed', 'md5_provided', 'md5_match') :=
             checkPubmedFiles(file.path(localDir, sub_dir, xml_filename),
                              file.path(localDir, sub_dir, md5_filename))]

  return(fileInfo[])}
