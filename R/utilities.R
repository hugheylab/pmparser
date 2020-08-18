getFailed = function(logPath) {
  d = data.table::fread(logPath, sep = '\t', na.strings = '', logical01 = TRUE)
  d = d[(status)][order(xml_filename)]
  return(d)}


getMissing = function(con, tableSuffix, dFile) {
  dProc = DBI::dbReadTable(con, paste_('xml_processed', tableSuffix))
  data.table::setDT(dProc)
  dMissing = data.table::fsetdiff(
    dFile[, .(xml_filename)], dProc[, .(xml_filename)])}


writeLogFile = function(logPath, x = NULL, append = TRUE, ...) {
  if (is.null(logPath)) {
    return(invisible())}
  y = data.table(datetime = Sys.time(), x)
  data.table::fwrite(
    y, logPath, append = append, sep = '\t', logical01 = TRUE, ...)}


connect = function(dbtype, dbname, ...) {
  dbtype = match.arg(dbtype, c('postgres', 'mariadb', 'mysql', 'sqlite'))
  drv = switch(dbtype,
               postgres = RPostgres::Postgres(),
               mariadb = RMariaDB::MariaDB(),
               mysql = RMariaDB::MariaDB(),
               sqlite = RSQLite::SQLite())
  return(DBI::dbConnect(drv, dbname = dbname, ...))}


disconnect = function(con) {
  if (!is.null(con)) DBI::dbDisconnect(con)}


appendTable = function(con, tableName, d) {
  if (is.null(con) || nrow(d) == 0L) {
    return(invisible())}
  DBI::dbAppendTable(con, tableName, d)}


getXmlInfo = function(xmlDir, xmlFiles, tableSuffix) {

  if (is.null(xmlFiles)) {
    xmlFiles = list.files(xmlDir, 'xml\\.gz$')
    xmlInfo = data.table(xml_filename = unique(xmlFiles), step = 'all')

  } else if (is.character(xmlFiles)) {
    xmlInfo = data.table(xml_filename = unique(xmlFiles), step = 'all')

  } else if (is.data.frame(xmlFiles)) {
    stopifnot(all(c('xml_filename', 'step') %in% colnames(xmlFiles)),
              !isEmpty(tableSuffix))
    xmlInfo = unique(data.table(xmlFiles)[, .(xml_filename, step)])

  } else {
    stop(paste('xmlFiles must be NULL, a character vector of filenames,',
               'or a data.frame with columns xml_filename and step.'))}

  stopifnot(all(file.exists(file.path(xmlDir, xmlInfo$xml_filename))))
  return(xmlInfo)}


getParseFuncs = function(steps = 'all') {
  parseFuncs = c(
    pmid_status = parsePmidStatus,
    article_id = parseArticleId,
    title_journal = parseTitleJournal,
    pub_type = parsePubType,
    pub_date = parsePubDate,
    mesh = parseMesh,
    keyword = parseKeyword,
    grant = parseGrant,
    chemical = parseChemical,
    data_bank = parseDataBank,
    comment = parseComment,
    abstract = parseAbstract,
    author = parseAuthorAffiliation,
    investigator = parseInvestigatorAffiliation)

  if ('all' %in% steps) {
    x = parseFuncs
  } else {
    x = parseFuncs[names(parseFuncs) %in% steps]}
  return(x)}


setColumn = function(d, value, colname = 'xml_filename') {
  if (!is.null(value)) {
    data.table::set(d, j = colname, value = value)}}


isEmpty = function(x) {
  return(is.null(x) || all(x == ''))}


paste_ = function(...) {
  x = list(...)
  if (isEmpty(x[[length(x)]])) {
    y = do.call(paste, c(x[-length(x)], list(sep = '_')))
  } else {
    y = paste(..., sep = '_')}
  return(y)}


runStatement = function(con, q) {
  q = trimws(q)
  if (startsWith(q, 'select count')) { # dry run
    res = DBI::dbSendQuery(con, q)
    n = DBI::dbFetch(res)[[1L]]
    DBI::dbClearResult(res)
  } else if (grepl('^(delete|insert)', q)) { # for reals
    n = DBI::dbExecute(con, q)
  } else {
    stop('Statement must start with "select count", "delete", or "insert".')}
  return(n)}


getPkgVersion = function(pkgName = 'pmparser') {
  as.character(utils::packageVersion(pkgName))}


getReadme = function(remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/',
                     filename = 'README.txt', con = NULL) {
  if (startsWith(remoteDir, 'ftp://')) {
    txt = RCurl::getURL(paste(remoteDir, filename, sep = '/'))
  } else { # was going to use for testing
    path = file.path(remoteDir, filename)
    txt = readChar(path, file.info(path)$size)}
  dReadme = data.table(text = txt)
  if (!is.null(con)) DBI::dbWriteTable(con, 'readme', dReadme, overwrite = TRUE)
  return(dReadme)}


isTesting = function() identical(Sys.getenv('TESTTHAT'), 'true')


#' Get Postgres connection parameters
#'
#' This is a helper function to get parameters from a .pgpass file. See
#' [here](https://www.postgresql.org/docs/9.6/libpq-pgpass.html) for details.
#'
#' @param path Path to .pgpass file. The file's first line should be
#'   \preformatted{# hostname:port:database:username:password}
#'
#' @return A data.table with one row for each set of parameters.
#'
#' @examples
#' pg = getPgParams(system.file('extdata', 'pgpass', package = 'pmparser'))
#'
#' @seealso [modifyPubmedDb()]
#'
#' @export
getPgParams = function(path = '~/.pgpass') {
  x1 = readLines(path)
  x2 = strsplit(trimws(gsub('#', '', x1)), ':')
  x3 = data.table::rbindlist(lapply(x2[-1L], as.list))
  setnames(x3, x2[[1L]])
  x3}

#' Copy of withr::local_file function, but allows additional parameters.
#'
#' Allows more parameters to be provided to the unlink step in the withr::local_file function.
#'
#' @param file `[named list]`\cr Files to create.
#' @param .local_envir `[environment]`\cr The environment to use for scoping.
#'
#'
#' @export
local_folder = function(file, ..., .local_envir = parent.frame()) {
    file_nms <- rlang::names2(file)
    unnamed <- file_nms == ""
    file_nms[unnamed] <- as.character(file[unnamed])
    withr::defer(unlink(file_nms, ...), envir = .local_envir)

    invisible(file)
  }
