#' @importFrom data.table data.table := setcolorder setnames
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom xml2 xml_find_all xml_find_first xml_text xml_attr xml_length xml_integer
NULL


globalVariables(c('.', '.N', 'd', 'affiliation', 'author_pos', 'filename',
                  'filenameNow', 'i', 'id_type', 'm', 'pmid', 'pub_date',
                  'pubmed', 'step', 'y', 'source', 'identifier', 'xml_file',
                  'status', 'stepFunc', 'collective_name', 'person_pos',
                  'affiliation_pos', 'affil_idx', 'person_idx', 'n_affil_ids',
                  'n_person_ids', 'n_total_ids', 'id_pos'))


processPubmedXmlCore = function(xmlDir, filename, steps = 'all', logPath = NULL,
                                tableSuffix = '', dbname = NULL, ...) {

  stepFuncs = getStepFuncs(steps)

  writeLogFile(logPath, data.table(filename = filename, step = 'start', status = 0))
  # create separate connection for each parallel process
  if (is.null(dbname)) {
    con = NULL
  } else {
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)}

  x0 = xml2::read_xml(file.path(xmlDir, filename))
  pmXml = xml_find_all(x0, './/PubmedArticle')
  writeLogFile(logPath, data.table(filename, 'read_xml', 0))

  step = 'deleted'
  if (step %in% names(stepFuncs)) {
    ex = tryCatch({stepFuncs[[step]](x0, filename, con, tableSuffix)},
                  error = function(e) NULL)
    writeLogFile(logPath, data.table(filename, step, is.null(ex)))}

  step = 'article_ids'
  conNow = if (step %in% names(stepFuncs)) con else NULL
  ex = tryCatch({stepFuncs[[step]](pmXml, filename, conNow, tableSuffix)},
                error = function(e) NULL)
  writeLogFile(logPath, data.table(filename, step, is.null(ex)))

  if (!is.null(ex)) {
    pmids = ex$pmid
    idx = !(names(stepFuncs) %in% c('deleted', 'article_ids'))

    r = foreach(stepFunc = stepFuncs[idx], step = names(stepFuncs)[idx]) %do% {
      ex = tryCatch({stepFunc(pmXml, pmids, con, tableSuffix)},
                    error = function(e) NULL)
      writeLogFile(logPath, data.table(filename, step, is.null(ex)))}}

  writeLogFile(logPath, data.table(filename, 'finish', 0))
  invisible()}


#' @export
processPubmedXml = function(xmlDir, xmlFiles = NULL, logPath = NULL, tableSuffix = '',
                            overwrite = FALSE, dbname = NULL, ...) {

  if(is.null(xmlFiles)){
    xmlFiles = list.files(file.path(xmlDir))
    xmlFiles = xmlFiles[endsWith(xmlFiles, '.xml.gz')]
  }

  xmlInfo = getXmlInfo(xmlFiles, tableSuffix)
  stopifnot(all(file.exists(file.path(xmlDir, xmlInfo$filename))))

  writeEmptyTables(tableSuffix, overwrite, dbname, ...)
  writeLogFile(logPath, data.table(filename = 'all', step = 'start', status = 0),
               append = FALSE)

  r = foreach(filenameNow = unique(xmlInfo$filename)) %dopar% {
    steps = xmlInfo[filename == filenameNow]$step
    processPubmedXmlCore(xmlDir, filenameNow, steps, logPath, tableSuffix,
                         dbname, ...)}

  if (!is.null(dbname)) {
    # TODO: move this into the core function? currently ignores overwrite
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)
    d = data.table(xml_file = unique(xmlInfo$filename),
                   datetime_processed = Sys.time())
    DBI::dbWriteTable(con, paste0('xml_processed', tableSuffix), d,
                      overwrite = TRUE)}

  writeLogFile(logPath, data.table('all', 'finish', 0))
  invisible()}
