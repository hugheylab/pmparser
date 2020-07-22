#' @export
getFailed = function(logPath) {
  d = data.table::fread(logPath, na.strings = '', logical01 = TRUE)
  d = d[(status), .(xml_filename, step)][order(xml_filename)]
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


getXmlInfo = function(xmlDir, xmlFiles, tableSuffix) {

  if (is.null(xmlFiles)) {
    xmlFiles = list.files(xmlDir, pattern = '.*.xml.gz')
    xmlInfo = data.table(xml_filename = unique(xmlFiles), step = 'all')

  } else if (is.character(xmlFiles)) {
    xmlInfo = data.table(xml_filename = unique(xmlFiles), step = 'all')

  } else if (is.data.frame(xmlFiles)) {
    stopifnot(all(c('filename', 'step') %in% colnames(xmlFiles)),
              tableSuffix != '')
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
    comment = getComment,
    abstract = getAbstract,
    author = getAuthorAffiliation,
    investigator = getInvestigatorAffiliation)

  if ('all' %in% steps) {
    x = stepFuncs
  } else {
    x = stepFuncs[names(stepFuncs) %in% steps]}
  return(x)}
