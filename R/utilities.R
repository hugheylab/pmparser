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


getXmlInfo = function(xmlDir, xmlFiles, tableSuffix) {

  if(is.null(xmlFiles)){
    xmlFiles = list.files(xmlDir, pattern = '.*.xml.gz')
    xmlFiles = xmlFiles[endsWith(xmlFiles, '.xml.gz')]
    xmlInfo = data.table(filename = unique(xmlFiles), step = 'all')}

  else if (is.character(xmlFiles)) {
    xmlInfo = data.table(filename = unique(xmlFiles), step = 'all')

  } else if (is.data.frame(xmlFiles)) {
    stopifnot(all(c('filename', 'step') %in% colnames(xmlFiles)),
              tableSuffix != '')
    xmlInfo = unique(data.table(xmlFiles)[, .(filename, step)])

  } else {
    stop(paste('xmlFiles must be null, a character vector of filenames, ',
               'or a data.frame with columns filename and step.'))}

  stopifnot(all(file.exists(file.path(xmlDir, xmlInfo$filename))))
  return(xmlInfo)}


getStepFuncs = function(steps = 'all') {
  stepFuncs = c(
    deleted = getDeleted,
    article_ids = getArticleIds,
    medline = getMedlineStatus,
    titles_journals = getTitlesJournals,
    pub_types = getPubTypes,
    pub_dates = getPubDates,
    mesh_terms = getMeshTerms,
    comments = getComments,
    abstracts = getAbstracts,
    authors = getAuthorsAffiliations,
    investigators = getInvestigatorsAffiliations)

  if ('all' %in% steps) {
    x = stepFuncs
  } else {
    x = stepFuncs[names(stepFuncs) %in% steps]}
  return(x)}
