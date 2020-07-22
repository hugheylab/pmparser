#' @importFrom data.table data.table := setcolorder setnames setattr
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom xml2 xml_find_all xml_find_first xml_text xml_attr xml_length xml_integer
NULL


globalVariables(c('.', '.N', 'd', 'affiliation', 'author_pos', 'filenameNow',
                  'i', 'id_type', 'm', 'pmid', 'pub_date', 'pubmed', 'step',
                  'source', 'identifier', 'status', 'y', 'stepFunc',
                  'collective_name', 'person_pos', 'affiliation_pos',
                  'affil_idx', 'person_idx', 'n_affil_ids', 'n_person_ids',
                  'n_total_ids', 'id_pos', 'md5_calculated', 'md5_provided',
                  'md5_match', 'subDir', 'group', 'f', 'nem', 'xml_filename',
                  'md5_filename', 'xml_download', 'md5_download'))


processPubmedXmlCore = function(xmlDir, filename, steps = 'all', logPath = NULL,
                                tableSuffix = '', dbname = NULL, ...) {

  stepFuncs = getStepFuncs(steps)
  writeLogFile(logPath, data.table(xml_filename = filename, step = 'start',
                                   status = 0))

  if (is.null(dbname)) {
    con = NULL
  } else {
    # create separate connection for each parallel process
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)}

  rawXml = xml2::read_xml(file.path(xmlDir, filename))
  writeLogFile(logPath, data.table(filename, 'read_xml', 0))

  step = 'pmid_status'
  conNow = if (step %in% names(stepFuncs)) con else NULL
  ex = tryCatch({stepFuncs[[step]](rawXml, conNow, tableSuffix)},
                error = function(e) NULL)
  writeLogFile(logPath, data.table(filename, step, is.null(ex)))

  pmXml = ex[[1L]]
  pmids = ex[[2L]][status != 'Deleted']$pmid
  idx = !(names(stepFuncs) %in% step)

  r = foreach(stepFunc = stepFuncs[idx], step = names(stepFuncs)[idx]) %do% {
    ex = tryCatch({stepFunc(pmXml, pmids, con, tableSuffix)},
                  error = function(e) NULL)
    writeLogFile(logPath, data.table(filename, step, is.null(ex)))}

  writeLogFile(logPath, data.table(filename, 'finish', 0))
  invisible()}


#' @export
processPubmedXml = function(xmlDir, xmlFiles = NULL, logPath = NULL,
                            tableSuffix = '', overwrite = FALSE, dbname = NULL,
                            ...) {

  xmlInfo = getXmlInfo(xmlFiles, tableSuffix)

  writeEmptyTables(tableSuffix, overwrite, dbname, ...)
  writeLogFile(logPath, data.table(xml_filename = 'all', step = 'start',
                                   status = 0),
               append = FALSE)

  r = foreach(filenameNow = unique(xmlInfo$xml_filename)) %dopar% {
    steps = xmlInfo[xml_filename == filenameNow]$step
    processPubmedXmlCore(xmlDir, filenameNow, steps, logPath, tableSuffix,
                         dbname, ...)}

  if (!is.null(dbname)) {
    # TODO: move this into the core function? currently ignores overwrite
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)
    d = data.table(xml_filename = unique(xmlInfo$xml_filename),
                   datetime_processed = Sys.time())
    DBI::dbWriteTable(con, paste0('xml_processed', tableSuffix), d,
                      overwrite = TRUE)}

  writeLogFile(logPath, data.table('all', 'finish', 0))
  invisible()}
