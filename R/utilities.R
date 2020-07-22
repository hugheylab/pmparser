#' @export
getFailed = function(logPath) {
  d = data.table::fread(logPath, na.strings = '', logical01 = TRUE)
  d = d[(status), .(filename, step)][order(filename)]
  return(d)}


writeLogFile = function(logPath, x = NULL, append = TRUE, logical01 = TRUE, ...) {
  if (is.null(logPath)) {
    return(invisible())}
  y = data.table(datetime = Sys.time(), x)
  data.table::fwrite(y, logPath, append = append, logical01 = logical01, ...)}


appendTable = function(con, tableName, d) {
  if (is.null(con) || nrow(d) == 0L) {
    return(invisible())}
  DBI::dbAppendTable(con, tableName, d)}


getXmlInfo = function(xmlFiles, tableSuffix) {
  if (is.character(xmlFiles)) {
    xmlInfo = data.table(filename = unique(xmlFiles), step = 'all')

  } else if (is.data.frame(xmlFiles)) {
    stopifnot(all(c('filename', 'step') %in% colnames(xmlFiles)),
              tableSuffix != '')
    xmlInfo = unique(data.table(xmlFiles)[, .(filename, step)])

  } else {
    stop(paste('xmlFiles must be a character vector of filenames',
               'or a data.frame with columns filename and step.'))}

  return(xmlInfo)}


getStepFuncs = function(steps = 'all') {
  stepFuncs = c(
    pmid_status = getPmidStatus,
    article_id = getArticleId,
    title_journal = getTitleJournal,
    pub_type = getPubType,
    pub_date = getPubDate,
    mesh_term = getMeshTerm,
    comment = getComment,
    abstract = getAbstract,
    author = getAuthorAffiliation,
    investigator = getInvestigatorAffiliation)

  if ('all' %in% steps) {
    x = stepFuncs
  } else {
    x = stepFuncs[names(stepFuncs) %in% steps]}
  return(x)}
