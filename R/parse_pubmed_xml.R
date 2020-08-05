#' @importFrom data.table data.table := setcolorder setnames setattr
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom xml2 xml_find_all xml_find_first xml_text xml_attr xml_length xml_integer
NULL


globalVariables(c(
  '.', '.N', 'd', 'affiliation', 'author_pos', 'filenameNow', 'i', 'id_type',
  'm', 'pmid', 'pub_date', 'step', 'source', 'identifier', 'status', 'y',
  'parseFunc', 'collective_name', 'person_pos', 'affiliation_pos', 'affil_idx',
  'person_idx', 'n_affil_ids', 'n_person_ids', 'n_total_ids', 'id_pos', 'f',
  'md5_computed', 'md5_provided', 'md5_match', 'subDir', 'col', 'group', 'name',
  'xml_filename', 'md5_filename', 'xml_download', 'md5_download', 'version',
  'published_date', 'sub_dir', 'sourceName', 'targetName', 'accession_number',
  '..cols', 'tableName'))


parsePubmedXmlCore = function(xmlDir, filename, steps = 'all', logPath = NULL,
                              tableSuffix = NULL, dbtype = 'postgres',
                              dbname = NULL, ...) {

  parseFuncs = getParseFuncs(steps)
  writeLogFile(logPath, data.table(filename, 'start', 0, NA))

  # create separate connection for each parallel process
  con = if (is.null(dbname)) NULL else connect(dbtype, dbname, ...)

  rawXml = xml2::read_xml(file.path(xmlDir, filename))
  writeLogFile(logPath, data.table(filename, 'read_xml', 0, NA))

  step = 'pmid_status'
  conNow = if (step %in% names(parseFuncs)) con else NULL
  res = tryCatch({parsePmidStatus(rawXml, filename, conNow, tableSuffix)},
                error = function(e) e)
  msg = if (is.character(res)) res else NA_character_
  writeLogFile(logPath, data.table(filename, step, is.character(res), msg))

  # assuming pmid_status never fails
  pmXml = res[[1L]]
  dPmid = res[[2L]][status != 'Deleted', !'status']
  if (isEmpty(tableSuffix)) dPmid[, xml_filename := NULL]

  idx = !(names(parseFuncs) %in% step)

  r = foreach(parseFunc = parseFuncs[idx], step = names(parseFuncs)[idx]) %do% {
    res = tryCatch({parseFunc(pmXml, dPmid, con, tableSuffix)},
                   error = function(e) e)
    msg = if (is.character(res)) res else NA_character_
    writeLogFile(logPath, data.table(filename, step, is.character(res), msg))}

  d = data.table(
    xml_filename = filename,
    pmparser_version = getPkgVersion(),
    datetime_processed = Sys.time())

  appendTable(con, paste_('xml_processed', tableSuffix), d)
  disconnect(con)

  writeLogFile(logPath, data.table(filename, 'finish', 0, NA))
  invisible()}


#' Parse PubMed XML files
#'
#' describe shit here
#'
#' @param xmlDir Path to directory containing the xml or xml.gz files.
#' @param xmlFiles Character vector of file names or a data.frame with columns
#'   `xml_filename` and `step` TODO
#' @param logPath Path to the log file to create. The log file is a
#'   tab-delimited file with columns `datetime`, `xml_filename`, `step`,
#'   `status`, and `message`. A `status` of 0 indicates success, 1 indicates
#'   an error, in which case `message` contains the error message.
#' @param tableSuffix String to append to the table names.
#' @param overwrite Logical indicating whether to overwrite existing tables.
#' @param dbtype String indicating type of database, either 'postgres',
#'   'mariadb', 'mysql', or 'sqlite'. Only used if `dbname` is not `NULL`.
#' @param dbname Name of the database in which to create the tables.
#' @param ... Other arguments passed to [DBI::dbConnect()].
#'
#' @return `NULL`, invisibly.
#'
#' @export
parsePubmedXml = function(xmlDir, xmlFiles = NULL, logPath = NULL,
                          tableSuffix = NULL, overwrite = FALSE,
                          dbtype = 'postgres', dbname = NULL, ...) {

  xmlInfo = getXmlInfo(xmlDir, xmlFiles, tableSuffix)

  writeEmptyTables(tableSuffix, overwrite, dbtype, dbname, ...)
  dLog = data.table(
    xml_filename = 'all', step = 'start', status = 0, message = NA)
  writeLogFile(logPath, dLog, append = FALSE)

  r = foreach(filenameNow = unique(xmlInfo$xml_filename)) %dopar% {
    steps = xmlInfo[xml_filename == filenameNow]$step
    parsePubmedXmlCore(
      xmlDir, filenameNow, steps, logPath, tableSuffix, dbtype, dbname, ...)}

  writeLogFile(logPath, data.table('all', 'finish', 0, NA))
  invisible()}
