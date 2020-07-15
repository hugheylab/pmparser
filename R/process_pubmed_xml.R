processPubmedXmlCore = function(xmlDir, filename, steps = 'all', logPath = NULL,
                                tableSuffix = '', dbname = NULL, ...) {

  if ('all' %in% steps) {
    stepsKeep = c('deleted', 'article_ids', 'medline', 'titles_journals',
                  'pub_types', 'pub_dates', 'mesh_terms', 'comments',
                  'abstracts', 'authors_affiliations')
  } else {
    stepsKeep = steps}

  writeLogFile(logPath, data.table(filename = filename, step = 'start', status = 0))
  # create separate connection for each parallel process
  if (is.null(dbname)) {
    con = NULL
  } else {
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)}

  x0 = xml2::read_xml(file.path(xmlDir, filename))
  pmXml = xml_find_all(x0, './/PubmedArticle')
  writeLogFile(logPath, data.table(filename, 'read_xml', 0))

  if ('deleted' %in% stepsKeep) {
    ex = tryCatch({getDeleted(x0, filename, con, tableSuffix)},
                  error = function(e) NULL)
    writeLogFile(logPath, data.table(filename, 'deleted', is.null(ex)))}

  conNow = if ('article_ids' %in% stepsKeep) con else NULL
  ex = tryCatch({getArticleIds(pmXml, filename, conNow, tableSuffix)},
                error = function(e) NULL)
  writeLogFile(logPath, data.table(filename, 'article_ids', is.null(ex)))

  if (!is.null(ex)) {
    pmids = ex$pmid
    getFuncs = c(medline = getMedlineStatus,
                 titles_journals = getTitlesJournals,
                 pub_types = getPubTypes,
                 pub_dates = getPubDates,
                 mesh_terms = getMeshTerms,
                 comments = getComments,
                 abstracts = getAbstracts,
                 authors_affiliations = getAuthorsAffiliations)
    getFuncs = getFuncs[names(getFuncs) %in% stepsKeep]

    r = foreach(getFunc = getFuncs, step = names(getFuncs)) %do% {
      ex = tryCatch({getFunc(pmXml, pmids, con, tableSuffix)},
                    error = function(e) NULL)
      writeLogFile(logPath, data.table(filename, step, is.null(ex)))}}

  writeLogFile(logPath, data.table(filename, 'finish', 0))
  invisible(0)}


#' @export
processPubmedXml = function(xmlDir, xmlFiles, logPath = NULL, tableSuffix = '',
                            overwrite = FALSE, dbname = NULL, ...) {

  xmlFiles = unique(xmlFiles)

  if (is.character(xmlFiles)) {
    xmlInfo = data.table(filename = xmlFiles, step = 'all')

  } else if (is.data.frame(xmlFiles)) {
    stopifnot(sort(colnames(xmlFiles)) == c('filename', 'step'),
              tableSuffix != '')
    xmlInfo = data.table(xmlFiles)

  } else {
    stop(paste('xmlFiles must be a character vector of filenames',
               'or a data.frame with columns filename and step.'))}

  stopifnot(all(file.exists(file.path(xmlDir, xmlInfo$filename))))

  if (!is.null(dbname)) {
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)
    tableNames = c('deleted', 'article_ids', 'medline', 'titles_journals',
                   'pub_types', 'pub_dates', 'mesh_terms', 'comments',
                   'abstracts', 'authors', 'affiliations', 'author_identifiers',
                   'affiliation_identifiers')

    tablesExist = sapply(tableNames, function(x) DBI::dbExistsTable(con, tableName))
    stopifnot(!any(tablesExist) || isTRUE(overwrite))

    for (tableName in tableNames) {
      DBI::dbWriteTable(con, paste0(tableName, tableSuffix),
                        getEmptyDt(tableName), overwrite = TRUE)}}

  writeLogFile(logPath, data.table(filename = 'all', step = 'start', status = 0),
               append = FALSE)

  r = foreach(filenameNow = unique(xmlInfo$filename)) %dopar% {
    steps = xmlInfo[filename == filenameNow]$step
    processPubmedXmlCore(xmlDir, filenameNow, steps, logPath, tableSuffix,
                         dbname, ...)}

  if (!is.null(con)) {
    d = data.table(xml_file = unique(xmlInfo$filename),
                   datetime_processed = Sys.time())
    DBI::dbWriteTable(con, paste0('xml_processed', tableSuffix), d,
                      overwrite = TRUE)}

  writeLogFile(logPath, data.table('all', 'finish', 0))
  invisible(0)}
