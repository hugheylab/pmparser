#' @importFrom data.table data.table := setcolorder
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom xml2 xml_find_all xml_find_first xml_text xml_attr xml_length xml_integer
NULL


globalVariables(c('.', '.N', 'd', 'affiliation', 'author_pos', 'filename',
                  'filenameNow', 'i', 'id_type', 'm', 'pmid', 'pub_date',
                  'pubmed', 'step', 'y', 'source', 'identifier',
                  'xml_file', 'status', 'getFunc'))


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
                                  tableNames = c('authors', 'affiliations',
                                                 'author_identifiers',
                                                 'affiliation_identifiers')) {
  timeStart = Sys.time()
  x2 = xml_find_all(pmXml, './/Author')
  n_authors = xml_length(xml_find_first(pmXml, './/AuthorList'))

  # orcid ids seem to always come first if they exist
  # x3 = xml_find_first(x2, './/Identifier')

  # get authors
  dAuthors = data.table(
    pmid = rep.int(pmids, n_authors),
    last_name = xml_text(xml_find_first(x2, './/LastName')),
    fore_name = xml_text(xml_find_first(x2, './/ForeName')),
    initials = xml_text(xml_find_first(x2, './/Initials')),
    suffix = xml_text(xml_find_first(x2, './/Suffix')),
    # orcid = xml_text(x3),
    # source = xml_attr(x3, 'Source'),
    collective_name = xml_text(xml_find_first(x2, './/CollectiveName')))

  # dAuthors[source != 'ORCID', orcid := NA]
  # dAuthors[, source := NULL]

  dAuthors[, author_pos := 1:.N, by = pmid]
  setcolorder(dAuthors, c('pmid', 'author_pos'))

  # get affiliations
  x4 = xml_find_all(x2, './/Affiliation', flatten = FALSE)
  n_affiliations = sapply(x4, length)

  dAffils = dAuthors[rep.int(1:nrow(dAuthors), n_affiliations),
                     .(pmid, author_pos)]
  dAffils[, affiliation_pos := 1:.N, by = .(pmid, author_pos)]
  dAffils[, affiliation := unlist(lapply(x4, xml_text))]

  # get affiliation identifiers
  # have to know which identifier belongs to which affiliation
  x5 = xml_find_all(x2, './/AffiliationInfo')
  x6 = xml_find_all(x5, './/Identifier', flatten = FALSE)
  n_affil_ids = sapply(x6, length)

  dAffilIds = data.table(
    affil_idx = rep.int(1:length(x6), n_affil_ids),
    source = unlist(lapply(x6, function(x) xml_attr(x, 'Source'))),
    identifier = unlist(lapply(x6, xml_text)))

  dAffils[, affil_idx := 1:.N]
  dAffilIds = merge(dAffilIds,
                    dAffils[, .(affil_idx, pmid, author_pos, affiliation_pos)],
                    by = 'affil_idx')
  dAffils[, affil_idx := NULL]
  dAffilIds[, affil_idx := NULL]
  setcolorder(dAffilIds, c('pmid', 'author_pos', 'affiliation_pos',
                           'source', 'identifier'))

  # get author identifiers
  # have to exclude affiliation identifiers
  # currently just orcid, so could be much simpler, but this should be robust
  x7 = xml_find_all(x2, './/Identifier', flatten = FALSE)
  n_total_ids = sapply(x7, length)

  dAllIds = data.table(
    author_idx = rep.int(1:length(x7), n_total_ids),
    source = unlist(lapply(x7, function(x) xml_attr(x, 'Source'))),
    identifier = unlist(lapply(x7, xml_text)))

  dAuthors[, author_idx := 1:.N]
  dAllIds = merge(dAllIds, dAuthors[, .(author_idx, pmid, author_pos)],
                  by = 'author_idx')

  dAuthors[, author_idx := NULL]
  dAllIds[, author_idx := NULL]

  x8 = dAffilIds[, .(n_affil_ids = .N), by = .(pmid, author_pos)]
  x9 = dAllIds[, .(n_total_ids = .N), by = .(pmid, author_pos)]
  x10 = merge(x9, x8, by = c('pmid', 'author_pos'), all.x = TRUE)
  x10[is.na(n_affil_ids), n_affil_ids := 0]
  x10[, n_author_ids := n_total_ids - n_affil_ids]
  x11 = x10[n_author_ids > 0,
            .(id_pos = 1:n_author_ids),
            by = .(pmid, author_pos)]

  dAllIds[, id_pos := 1:.N, by = .(pmid, author_pos)]
  dAuthorIds = merge(dAllIds, x11, by = c('pmid', 'author_pos', 'id_pos'))
  dAuthorIds[, id_pos := NULL]

  # append to db
  appendTable(con, paste0(tableNames[1L], tableSuffix), dAuthors)
  appendTable(con, paste0(tableNames[2L], tableSuffix), dAffils)
  appendTable(con, paste0(tableNames[3L], tableSuffix), dAuthorIds)
  appendTable(con, paste0(tableNames[4L], tableSuffix), dAffilIds)
  r = list(authors = dAuthors, affiliations = dAffils,
           author_identifiers = dAuthorIds, affiliations = dAffilIds)
  return(r)}


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
    affiliations = data.table(pmid = ai, author_pos = ai, affiliation = ac),
    author_identifiers = data.table(pmid = ai, author_pos = ai, source = ac,
                                    identifier = ac),
    affiliation_identifiers = data.table(pmid = ai, author_pos = ai,
                                         affiliation_pos = ai, source = ac,
                                         identifier = ac))
  return(d)}


#' @export
getFailed = function(logPath) {
  d = data.table::fread(logPath, na.strings = '', logical01 = TRUE)
  d = d[(status), .(filename, step)][order(filename)]
  return(d)}
