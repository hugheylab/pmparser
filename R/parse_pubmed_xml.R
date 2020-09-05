#' @importFrom data.table data.table := setcolorder setnames setattr
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom glue glue glue_sql
#' @importFrom xml2 xml_find_all xml_find_first xml_text xml_attr xml_length xml_integer
NULL


globalVariables(c(
  '.', '.N', 'd', 'affiliation', 'author_pos', 'filenameNow', 'i', 'id_type',
  'm', 'pmid', 'pub_date', 'step', 'source', 'identifier', 'status', 'y',
  'parseFunc', 'collective_name', 'person_pos', 'affiliation_pos', 'affil_idx',
  'person_idx', 'n_affil_ids', 'n_person_ids', 'n_total_ids', 'id_pos', 'f',
  'md5_computed', 'md5_provided', 'md5_match', 'subDir', 'pre', 'group', 'name',
  'xml_filename', 'md5_filename', 'xml_download', 'md5_download', 'version',
  'published_date', 'sub_dir', 'sourceName', 'targetName', 'accession_number',
  '..cols', 'tableName', 'copyright', 'descriptor_pos', 'processed', 'N',
  'equal_contrib', 'sample_base', 'pub_year', 'pub_month', 'pub_month_tmp1',
  'pub_month_tmp2', 'pub_day', 'pub_day_tmp'))


parsePubmedXmlCore = function(
  xmlDir, filename, steps = 'all', logPath = NULL, tableSuffix = NULL,
  dbtype = 'postgres', dbname = NULL, ...) {

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
  err = inherits(res, 'error')
  msg = if (err) trimws(as.character(res)) else NA_character_
  writeLogFile(logPath, data.table(filename, step, err, msg))

  # assuming pmid_status never fails
  pmXml = res[[1L]]
  dPmid = res[[2L]][status != 'Deleted', !'status']
  if (isEmpty(tableSuffix)) dPmid[, xml_filename := NULL]

  idx = !(names(parseFuncs) %in% step)

  r = foreach(parseFunc = parseFuncs[idx], step = names(parseFuncs)[idx]) %do% {
    res = tryCatch({parseFunc(pmXml, dPmid, con, tableSuffix)},
                   error = function(e) e)
    err = inherits(res, 'error')
    msg = if (err) trimws(as.character(res)) else NA_character_
    writeLogFile(logPath, data.table(filename, step, err, msg))}

  d = data.table(
    xml_filename = filename,
    pmparser_version = getPkgVersion(),
    datetime_processed = Sys.time())

  appendTable(con, paste_('xml_processed', tableSuffix), d)
  disconnect(con)

  writeLogFile(logPath, data.table(filename, 'finish', 0, NA))
  invisible()}


parsePubmedXml = function(
  xmlDir, xmlFiles = NULL, logPath = NULL, tableSuffix = NULL,
  overwrite = FALSE, dbtype = 'postgres', dbname = NULL, ...) {

  xmlInfo = getXmlInfo(xmlDir, xmlFiles, tableSuffix)

  if (dbtype == 'sqlite' && foreach::getDoParWorkers() > 1) {
    warning(glue('Parsing of XML files cannot run in parallel if using an \\
                  sqlite database. Parsing will run sequentially.'),
            immediate. = TRUE)}
  doOp = getDoOp(dbtype)

  writeEmptyTables(tableSuffix, overwrite, dbtype, dbname, ...)
  dLog = data.table(
    xml_filename = 'all', step = 'start', status = 0, message = NA)
  writeLogFile(logPath, dLog, append = FALSE)

  feo = foreach(filenameNow = unique(xmlInfo$xml_filename),
                .options.future = list(scheduling = Inf))

  r = doOp(feo, {
    steps = xmlInfo[xml_filename == filenameNow]$step
    parsePubmedXmlCore(
      xmlDir, filenameNow, steps, logPath, tableSuffix, dbtype, dbname, ...)})

  writeLogFile(logPath, data.table('all', 'finish', 0, NA))
  invisible()}
