#' @export
getDeleted = function(xmlRaw, filename, con, tableSuffix = '',
                      tableName = 'deleted') {
  x1 = xml_find_all(xml_find_all(xmlRaw, './/DeleteCitation'), './/PMID')
  if (length(x1) == 0) {
    x2 = data.table(filename = as.character(), pmid = as.integer())
  } else {
    x2 = data.table(filename = filename, pmid = xml_integer(x1))
    appendTable(con, paste0(tableName, tableSuffix), x2)}
  return(x2)}


#' @export
getArticleIds = function(pmXml, filename, con, tableSuffix = '',
                         tableName = 'article_ids') {
  x1 = xml_find_first(pmXml, './/ArticleIdList') # assuming this comes before refs
  nIds = xml_length(x1)

  x2 = xml_find_all(x1, './/ArticleId')
  x3 = data.table(
    index = rep.int(1:length(pmXml), nIds),
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
  nHist = xml_length(x1)
  x2 = xml_find_all(x1, './/PubMedPubDate')

  x4 = data.table(
    pmid = rep.int(pmids, nHist),
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
