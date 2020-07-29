#' @importFrom data.table data.table := setcolorder setnames setattr
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom xml2 xml_find_all xml_find_first xml_text xml_attr xml_length xml_integer
NULL


globalVariables(c('.', '.N', 'd', 'affiliation', 'author_pos', 'filenameNow',
                  'i', 'id_type', 'm', 'pmid', 'pub_date', 'pubmed', 'step',
                  'source', 'identifier', 'status', 'y', 'stepFunc',
                  'collective_name', 'person_pos', 'affiliation_pos',
                  'affil_idx', 'person_idx', 'n_affil_ids', 'n_person_ids',
                  'n_total_ids', 'id_pos', 'md5_computed', 'md5_provided',
                  'md5_match', 'subDir', 'group', 'f', 'col', 'xml_filename',
                  'md5_filename', 'xml_download', 'md5_download', 'name',
                  'published_date', 'sub_dir', 'sourceName', 'targetName'))


parsePubmedXmlCore = function(xmlDir, filename, steps = 'all', logPath = NULL,
                              tableSuffix = NULL, dbname = NULL, ...) {

  stepFuncs = getStepFuncs(steps)
  dLog = data.table(
    xml_filename = filename, step = 'start', status = 0, messsage = NA)
  writeLogFile(logPath, dLog)

  # create separate connection for each parallel process
  con = if (is.null(dbname)) NULL else
    DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)

  rawXml = xml2::read_xml(file.path(xmlDir, filename))
  writeLogFile(logPath, data.table(filename, 'read_xml', 0, NA))

  step = 'pmid_status'
  conNow = if (step %in% names(stepFuncs)) con else NULL
  res = tryCatch({getPmidStatus(rawXml, filename, conNow, tableSuffix)},
                error = function(e) e)
  msg = if (is.character(res)) res else NA
  writeLogFile(logPath, data.table(filename, step, is.character(res), msg))

  # assuming pmid_status never fails
  pmXml = res[[1L]]
  pmids = res[[2L]][status != 'Deleted']$pmid
  filenameNow = if (isEmpty(tableSuffix)) NULL else filename
  idx = !(names(stepFuncs) %in% step)

  r = foreach(stepFunc = stepFuncs[idx], step = names(stepFuncs)[idx]) %do% {
    res = tryCatch({stepFunc(pmXml, pmids, filenameNow, con, tableSuffix)},
                   error = function(e) e)
    msg = if (is.character(res)) res else NA
    writeLogFile(logPath, data.table(filename, step, is.character(res), msg))}

  d = data.table(xml_filename = filename, datetime_processed = Sys.time())
  appendTable(con, paste_('xml_processed', tableSuffix), d)

  writeLogFile(logPath, data.table(filename, 'finish', 0, NA))
  invisible()}


#' @export
parsePubmedXml = function(xmlDir, xmlFiles = NULL, logPath = NULL,
                          tableSuffix = NULL, overwrite = FALSE,
                          dbname = NULL, ...) {

  xmlInfo = getXmlInfo(xmlDir, xmlFiles, tableSuffix)

  writeEmptyTables(tableSuffix, overwrite, dbname, ...)
  dLog = data.table(
    xml_filename = 'all', step = 'start', status = 0, message = NA)
  writeLogFile(logPath, dLog, append = FALSE)

  r = foreach(filenameNow = unique(xmlInfo$xml_filename)) %dopar% {
    steps = xmlInfo[xml_filename == filenameNow]$step
    parsePubmedXmlCore(
      xmlDir, filenameNow, steps, logPath, tableSuffix, dbname, ...)}

  writeLogFile(logPath, data.table('all', 'finish', 0, NA))
  invisible()}
