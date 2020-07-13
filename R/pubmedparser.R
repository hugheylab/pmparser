#' @importFrom data.table data.table := setcolorder
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom xml2 xml_find_all xml_find_first xml_text xml_attr xml_length xml_integer
NULL


globalVariables(c('.', '.N', 'd', 'affiliation', 'author_pos', 'filename',
                  'filenameNow', 'i', 'id_type', 'm', 'pmid', 'pub_date',
                  'pubmed', 'step', 'y', 'orcid', 'source', 'xml_file',
                  'status', 'getFunc'))


writeLogFile = function(logPath, x = NULL, append = TRUE, logical01 = TRUE, ...) {
  if (is.null(logPath)) {
    return(NULL)}
  y = data.table(datetime = Sys.time(), x)
  data.table::fwrite(y, logPath, append = append, logical01 = logical01, ...)}


appendTable = function(con, tableName, d) {
  if (is.null(con) || nrow(d) == 0L) {
    return(NULL)}
  DBI::dbAppendTable(con, tableName, d)}


#' @export
getDeleted = function(xmlRaw, filename, con, tableSuffix = '',
                      tableName = 'deleted') {
  x1 = xml_find_all(xml_find_all(xmlRaw, './/DeleteCitation'), './/PMID')
  x2 = data.table(filename = filename, pmid = xml_integer(x1))
  appendTable(con, paste0(tableName, tableSuffix), x2)
  return(x2)}


#' @export
getArticleIds = function(pmXml, filename, con, tableSuffix = '',
                         tableName = 'article_ids') {
  x1 = xml_find_first(pmXml, './/ArticleIdList') # assuming this comes before refs
  n_ids = xml_length(x1)

  x2 = xml_find_all(x1, './/ArticleId')
  x3 = data.table(
    index = rep.int(1:length(pmXml), n_ids),
    id = xml_text(x2),
    id_type = xml_attr(x2, 'IdType'))

  x6 = data.table::dcast(
    x3[id_type %in% c('pubmed', 'doi', 'pmc')],
    index ~ id_type, value.var = 'id', fill = NA,
    fun.aggregate = function(x) x[1L]) # 1 out of 30e6 articles

  x6[, pmid := as.integer(pubmed)]
  x6[, pubmed := NULL]
  x6[, xml_file := filename]

  # beware a pmid may have multiple doi versions
  appendTable(con, paste0(tableName, tableSuffix), x6)
  return(x6)}


#' @export
getMedlineStatus = function(pmXml, pmids, con, tableSuffix = '',
                            tableName = 'medline') {
  x1 = data.table(
    pmid = pmids,
    status = xml_attr(xml_find_first(pmXml, 'MedlineCitation'), 'Status'))
  appendTable(con, paste0(tableName, tableSuffix), x1)
  return(x1)}


#' @export
getPubDates = function(pmXml, pmids, con, tableSuffix = '',
                       tableName = 'pub_dates') {
  x1 = xml_find_first(pmXml, './/History')
  n_hist = xml_length(x1)
  x2 = xml_find_all(x1, './/PubMedPubDate')

  x4 = data.table(
    pmid = rep.int(pmids, n_hist),
    pub_status = xml_attr(x2, 'PubStatus'),
    y = xml_text(xml_find_all(x2, './/Year')),
    m = xml_text(xml_find_all(x2, './/Month')),
    d = xml_text(xml_find_all(x2, './/Day')))

  x4[, pub_date := data.table::as.IDate(sprintf('%s-%s-%s', y, m, d))]
  x4[, c('y', 'm', 'd') := NULL]

  x4 = unique(x4)
  appendTable(con, tableName, x4)
  return(x4)}


#' @export
getTitlesJournals = function(pmXml, pmids, con, tableSuffix = '',
                             tableName = 'titles_journals') {
  x1 = xml_find_first(pmXml, './/Journal')
  x2 = data.table(
    pmid = pmids,
    title = xml_text(xml_find_first(pmXml, './/ArticleTitle')),
    journal_full = xml_text(xml_find_first(x1, './/Title')),
    journal_abbrev = xml_text(xml_find_first(x1, './/ISOAbbreviation')))
  x2 = unique(x2)
  appendTable(con, paste0(tableName, tableSuffix), x2)
  return(x2)}


#' @export
getPubTypes = function(pmXml, pmids, con, tableSuffix = '',
                       tableName = 'pub_types') {
  x1 = xml_find_first(pmXml, './/PublicationTypeList')
  x2 = xml_find_all(x1, './/PublicationType')
  x3 = data.table(
    pmid = rep.int(pmids, xml_length(x1)),
    pub_type = xml_text(x2),
    pub_type_id = xml_attr(x2, 'UI'))
  x3 = unique(x3)
  appendTable(con, paste0(tableName, tableSuffix), x3)
  return(x3)}


#' @export
getMeshTerms = function(pmXml, pmids, con, tableSuffix = '',
                        tableName = 'mesh_terms') {
  x1 = xml_find_first(pmXml, './/MeshHeadingList')
  n = xml_length(x1)
  x2 = xml_find_all(x1[n > 0], './/DescriptorName')
  x3 = data.table(
    pmid = rep.int(pmids, n),
    term = xml_text(x2),
    term_id = xml_attr(x2, 'UI'),
    major_topic = xml_attr(x2, 'MajorTopicYN'))
  x3 = unique(x3)
  appendTable(con, paste0(tableName, tableSuffix), x3)
  return(x3)}


#' @export
getComments = function(pmXml, pmids, con, tableSuffix = '',
                       tableName = 'comments') {
  x1 = xml_find_first(pmXml, './/CommentsCorrectionsList')
  n = xml_length(x1)
  x2 = xml_find_all(x1[n >0], './/CommentsCorrections')
  x3 = data.table(
    pmid = rep.int(pmids, n),
    ref_type = xml_attr(x2, 'RefType'),
    ref_pmid = xml_integer(xml_find_first(x2, './/PMID')))
  x3 = unique(x3)
  appendTable(con, paste0(tableName, tableSuffix), x3)
  return(x3)}


#' @export
getAbstracts = function(pmXml, pmids, con, tableSuffix = '',
                        tableName = 'abstracts') {
  if (is.null(idx)) {
    idx = 1:length(pmXml)}

  x1 = xml_find_first(pmXml, './/Abstract')

  x2 = data.table(
    pmid = pmids,
    copyright = xml_text(xml_find_first(x1, './/CopyrightInformation')))

  x3 = xml_length(x1) - !is.na(x2$copyright)
  x4 = xml_find_all(xml_find_all(pmXml, './/Abstract'), './/AbstractText')

  x5 = data.table(
    pmid = rep.int(pmids, x3),
    text = xml_text(x4),
    label = xml_attr(x4, 'Label'),
    nlm_category = xml_attr(x4, 'NlmCategory'))

  x6 = merge(x5, x2, by = 'pmid')
  x6 = unique(x6)
  appendTable(con, paste0(tableName, tableSuffix), x6)
  return(x6)}


#' @export
getAuthorsAffiliations = function(pmXml, pmids, con, tableSuffix = '',
                                  tableNames = c('authors', 'affiliations')) {
  x2 = xml_find_all(pmXml, './/Author')
  n_authors = xml_length(xml_find_first(pmXml, './/AuthorList'))

  # will miss orcid ids that aren't first, but way faster than the alternative,
  # and ISNI and GRID ids refer to institutions not authors anyway
  x3 = xml_find_first(x2, './/Identifier')

  dAuthors = data.table(
    pmid = rep.int(pmids, n_authors),
    last_name = xml_text(xml_find_first(x2, './/LastName')),
    fore_name = xml_text(xml_find_first(x2, './/ForeName')),
    initials = xml_text(xml_find_first(x2, './/Initials')),
    suffix = xml_text(xml_find_first(x2, './/Suffix')),
    orcid = xml_text(x3),
    source = xml_attr(x3, 'Source'),
    collective_name = xml_text(xml_find_first(x2, './/CollectiveName')))

  dAuthors[source != 'ORCID', orcid := NA]
  dAuthors[, source := NULL]

  dAuthors[, author_pos := 1:.N, by = pmid]
  setcolorder(dAuthors, c('pmid', 'author_pos'))

  x4 = xml_find_all(x2, './/AffiliationInfo', flatten = FALSE) # new xml2 syntax
  n_affiliations = sapply(x4, length)

  dAffils = dAuthors[rep.int(1:nrow(dAuthors), n_affiliations),
                     .(pmid, author_pos)]
  dAffils[, affiliation := unlist(lapply(x4, xml_text))]

  appendTable(con, paste0(tableNames[1L], tableSuffix), dAuthors)
  appendTable(con, paste0(tableNames[2L], tableSuffix), dAffils)
  return(list(dAuthors, dAffils))}


#' @export
getEmptyDt = function(tableName) {
  ac = as.character()
  ai = as.integer()
  d = switch(
    tableName,
    deleted = data.table(filename = ac, pmid = ai),
    article_ids = data.table(filename = ac, index = ai, doi = ac, pmc = ac,
                             pmid = ai),
    medline = data.table(pmid = ai, status = ac),
    titles_journals = data.table(pmid = ai, title = ac, journal_full = ac,
                                 journal_abbrev = ac),
    pub_types = data.table(pmid = ai, pub_type = ac, pub_type_id = ac),
    pub_dates = data.table(pmid = ai, pub_status = ac,
                           pub_date = data.table::as.IDate(ai)),
    mesh_terms = data.table(pmid = ai, term = ac, term_id = ac,
                            major_topic = ac),
    comments = data.table(pmid = ai, ref_type = ac, ref_pmid = ai),
    abstracts = data.table(pmid = ai, text = ac, label = ac, nlm_category = ac,
                           copyright = ac),
    authors = data.table(pmid = ai, author_pos = ai, last_name = ac,
                         fore_name = ac, initials = ac, collective_name = ac),
    affiliations = data.table(pmid = ai, author_pos = ai, affiliation = ac))
  return(d)}


#' @export
getFailed = function(logPath) {
  d = data.table::fread(logPath, na.strings = '', logical01 = TRUE)
  d = d[(status), .(filename, step)][order(filename)]
  return(d)}


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
                   'abstracts', 'authors', 'affiliations')

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

