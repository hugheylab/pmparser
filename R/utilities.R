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


download = function(url, destfile, n = 3L) {
  i = 1L
  x = 1L
  while (i <= n && !identical(x, 0L)) {
    x = tryCatch({utils::download.file(url, destfile)}, error = function(e) e)
    if (!identical(x, 0L)) Sys.sleep(stats::runif(1L, 1, 2))
    i = i + 1L}

  if (inherits(x, 'error')) stop(x)
  if (x != 0L) stop(glue('Download of {url} failed {n} times. Ruh-roh.'))
  x}


connect = function(dbtype, dbname, ...) {
  dbtype = match.arg(dbtype, c('postgres', 'mariadb', 'mysql', 'sqlite', 'clickhouse', 'bigquery'))
  pkgName = switch(dbtype,
                   postgres = 'RPostgres',
                   mariadb = 'RMariaDB',
                   mysql = 'RMariaDB',
                   sqlite = 'RSQLite',
                   clickhouse = 'RClickhouse',
                   bigquery = 'bigrquery')

  if (!requireNamespace(pkgName, quietly = TRUE)) {
    stop(glue('To use dbtype "{dbtype}", install the {pkgName} package.'))}

  drv = switch(dbtype,
               postgres = RPostgres::Postgres(),
               mariadb = RMariaDB::MariaDB(),
               mysql = RMariaDB::MariaDB(),
               sqlite = RSQLite::SQLite(),
               clickhouse = RClickhouse::clickhouse(),
               bigquery = bigrquery::bigquery())

  return(DBI::dbConnect(drv, dbname = dbname, ...))}


disconnect = function(con) if (!is.null(con)) DBI::dbDisconnect(con)


getDoOp = function(dbtype) if (dbtype == 'sqlite') `%do%` else `%dopar%`


appendTable = function(con, tableName, d) {
  if (is.null(con) || nrow(d) == 0L) return(invisible())
  # for some reason dbWriteTable is faster than dbAppendTable
  DBI::dbWriteTable(con, tableName, d, append = TRUE)}


getXmlInfo = function(xmlDir, xmlFiles, tableSuffix) {

  if (is.null(xmlFiles)) {
    xmlFiles = list.files(xmlDir, 'xml\\.gz$')
    xmlInfo = data.table(xml_filename = xmlFiles, step = 'all')

  } else if (is.character(xmlFiles)) {
    xmlInfo = data.table(xml_filename = unique(xmlFiles), step = 'all')

  } else if (is.data.frame(xmlFiles)) {
    stopifnot(all(c('xml_filename', 'step') %in% colnames(xmlFiles)),
              !isEmpty(tableSuffix))
    xmlInfo = unique(data.table(xmlFiles)[, .(xml_filename, step)])

  } else {
    stop(glue('xmlFiles must be NULL, a character vector of filenames, \\
               or a data.frame with columns xml_filename and step.'))}

  stopifnot(all(file.exists(file.path(xmlDir, xmlInfo$xml_filename))))
  return(xmlInfo)}


getParseFuncs = function(steps = 'all') {
  parseFuncs = c(
    pmid_status = parsePmidStatus,
    article_id = parseArticleId,
    article = parseArticle,
    journal = parseJournal,
    pub_type = parsePubType,
    pub_history = parsePubHistory,
    mesh = parseMesh,
    keyword = parseKeyword,
    grant = parseGrant,
    chemical = parseChemical,
    data_bank = parseDataBank,
    comment = parseComment,
    abstract = parseAbstract,
    author = parseAuthor,
    investigator = parseInvestigator)

  x = if ('all' %in% steps) parseFuncs else
    parseFuncs[names(parseFuncs) %in% steps]
  return(x)}


setColumn = function(d, value, colname = 'xml_filename') {
  if (!is.null(value)) data.table::set(d, j = colname, value = value)}


isEmpty = function(x) is.null(x) || all(x == '')


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
  txt = RCurl::getURL(glue('{remoteDir}/{filename}'))
  dReadme = data.table(text = txt)
  if (!is.null(con)) DBI::dbWriteTable(con, 'readme', dReadme, overwrite = TRUE)
  return(dReadme)}


isTesting = function() identical(Sys.getenv('TESTTHAT'), 'true')


names2 = function(x) {
  nms = names(x)
  if (is.null(nms)) {
    rep('', length(x))
  } else {
    nms[is.na(nms)] = ''
    nms}}


#' Get Postgres connection parameters
#'
#' This is a helper function to get parameters from a .pgpass file. See
#' [here](https://www.postgresql.org/docs/9.6/libpq-pgpass.html) for details.
#'
#' @param path Path to .pgpass file.
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
  x1 = trimws(readLines(path))
  x2 = strsplit(x1[!startsWith(x1, '#')], ':')
  x3 = data.table::rbindlist(lapply(x2, as.list))
  setnames(x3, c('hostname', 'port', 'database', 'username', 'password'))
  x3}


writeTableInChunks = function(path, con, nRowsPerChunk, overwrite, tableName) {
  # get total number of lines
  n = R.utils::countLines(path)

  # read first row to get data types, but don't send it to database
  d = data.table::fread(path, nrows = 1L)

  # will be inefficient if file is compressed
  if (!(DBI::dbExistsTable(con, tableName) && !isTRUE(overwrite))) {
    createTable(con, tableName, d)
  } else if(DBI::dbExistsTable(con, tableName)) {
    if (isTRUE(overwrite)) {
      DBI::dbRemoveTable(con, tableName)
      createTable(con, tableName, d)
    } else {
      stop(glue('Table {tableName} exists and overwrite is not TRUE.'))}}




  # append in chunks, fread handles last chunk where nrows > remaining rows
  for (i in seq(1L, n - 1L, nRowsPerChunk)) {
    dNow = data.table::fread(path, skip = i, nrows = nRowsPerChunk)
    setnames(dNow, colnames(d))
    appendTable(con, tableName, dNow)}

  invisible()}


getClickhouseDataTypes = function(d, nullable = TRUE) {
  r = c('logical', 'integer', 'numeric', 'character', 'Date', 'POSIXct')
  ch = c('UInt8', 'Int32', 'Float64', 'String', 'Date', 'DateTime')
  rIdx = lapply(d, function(x) inherits(x, r, which = TRUE))
  dataTypesTmp = sapply(rIdx, function(i) ch[i == 1L])

  if (!isTRUE(nullable)) {
    return(dataTypesTmp)
  } else {
    dataTypes = glue('Nullable({dataTypesTmp})')
    dataTypes[dataTypesTmp == 'DateTime'] = 'DateTime'
    dataTypes[dataTypesTmp == 'Date'] = 'Date'
    return(dataTypes)}}


createTable = function(con, tableName, d) {
  # writes 0 rows
  if (inherits(con, 'ClickhouseConnection')) {
    createTableClickhouse(con, tableName, d)
  } else if (inherits(con, 'BigQueryConnection')) {
    bigrquery::bq_table_create(bigrquery::bq_table(con@project, con@dataset, tableName), d)
  } else {
    DBI::dbCreateTable(con, tableName, d)}
  invisible(0L)}


createTableClickhouse = function(con, tableName, d, nullable = TRUE) {
  dataTypes = getClickhouseDataTypes(d, nullable = nullable)
  q = glue(
    'create table {tableName} ({z}) ENGINE = MergeTree() order by tuple()',
    z = paste(colnames(d), dataTypes, collapse = ', '))
  DBI::dbExecute(con, q)}

setNAToSpecial = function(d) {
  naDateVal = as.Date('2100-01-01')
  priorDateVal = as.Date('2075-01-01')
  if (nrow(d) == 1L && any(is.na(d))){
    columns = colnames(d)
    for(column in columns){
      if (is.na(d[[column]])){
        if (is.logical(d[[column]])) val = 0
        else if (is.integer(d[[column]])) val = -1L
        else if (is.numeric(d[[column]])) val = -1
        else if (inherits(d[[column]], 'POSIXct')) val = naDateVal
        else if (inherits(d[[column]], 'Date')) val = naDateVal
        else val = as.character(NA)
        d[, (column) := val]}}
  } else {
    for (j in 1:ncol(d)) {
      if (inherits(d[[j]], 'Date')) {
        data.table::set(
          d, i = which(is.na(d[[j]])), j = j, value = naDateVal)
        data.table::set(
          d, i = which(d[[j]] < as.Date('1970-01-01')), j = j, value = priorDateVal)}}}
  return(invisible(d))
}
