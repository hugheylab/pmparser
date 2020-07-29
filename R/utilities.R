#' @export
getFailed = function(logPath) {
  d = data.table::fread(logPath, sep = '\t', na.strings = '', logical01 = TRUE)
  d = d[(status), .(xml_filename, step, message)][order(xml_filename)]
  return(d)}


writeLogFile = function(logPath, x = NULL, append = TRUE, ...) {
  if (is.null(logPath)) {
    return(invisible())}
  y = data.table(datetime = Sys.time(), x)
  data.table::fwrite(y, logPath, append = append, sep = '\t', logical01 = TRUE,
                     ...)}


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
    stopifnot(all(c('filename', 'step') %in% colnames(xmlFiles)),
              !isEmpty(tableSuffix))
    xmlInfo = unique(data.table(xmlFiles)[, .(xml_filename, step)])

  } else {
    stop(paste('xmlFiles must be NULL, a character vector of filenames,',
               'or a data.frame with columns xml_filename and step.'))}

  stopifnot(all(file.exists(file.path(xmlDir, xmlInfo$xml_filename))))
  return(xmlInfo)}


getStepFuncs = function(steps = 'all') {
  stepFuncs = c(
    pmid_status = getPmidStatus,
    article_id = getArticleId,
    title_journal = getTitleJournal,
    pub_type = getPubType,
    pub_date = getPubDate,
    mesh_term = getMeshTerm,
    keyword = getKeyword,
    grant = getGrant,
    chemical = getChemical,
    comment = getComment,
    abstract = getAbstract,
    author = getAuthorAffiliation,
    investigator = getInvestigatorAffiliation)

  if ('all' %in% steps) {
    x = stepFuncs
  } else {
    x = stepFuncs[names(stepFuncs) %in% steps]}
  return(x)}


setXmlFilename = function(d, filename) {
  if (!is.null(filename)) {
    d[, xml_filename := filename]}}


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
