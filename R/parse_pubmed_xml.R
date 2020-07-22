#' @export
getPmidStatus = function(rawXml, con, tableSuffix = '',
                         tableName = 'pmid_status'){#, filename = NULL) {

  x1 = xml_find_all(xml_find_all(rawXml, './/DeleteCitation'), './/PMID')
  x2 = data.table(pmid = xml_integer(x1))
  x2[, status := 'Deleted']

  pmXml = xml_find_all(rawXml, './/PubmedArticle')
  x3 = data.table(
    pmid = xml_integer(xml_find_first(pmXml, './/PMID')),
    status = xml_attr(xml_find_first(pmXml, 'MedlineCitation'), 'Status'))

  x4 = rbind(x2, x3)
  # if (!is.null(filename)) {
  #   x4[, xml_filename := filename]}

  appendTable(con, paste0(tableName, tableSuffix), x4)
  return(list(pmXml, x4))}


#' @export
getArticleId = function(pmXml, pmids, con, tableSuffix = '',
                        tableName = 'article_id') {
  x1 = xml_find_first(pmXml, './/ArticleIdList') # assuming this comes before refs
  nIds = xml_length(x1)

  x2 = xml_find_all(x1, './/ArticleId')
  x3 = data.table(
    pmid = rep.int(pmids, nIds),
    id_type = xml_attr(x2, 'IdType'),
    id_value = xml_text(x2))
  x4 = x3[id_type %in% c('doi', 'pmc')]

  appendTable(con, paste0(tableName, tableSuffix), x4)
  return(x4)}


#' @export
getPubDate = function(pmXml, pmids, con, tableSuffix = '',
                      tableName = 'pub_date') {
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
getTitleJournal = function(pmXml, pmids, con, tableSuffix = '',
                           tableName = 'title_journal') {
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
getPubType = function(pmXml, pmids, con, tableSuffix = '',
                      tableName = 'pub_type') {
  x1 = xml_find_first(pmXml, './/PublicationTypeList')
  x2 = xml_find_all(x1, './/PublicationType')
  x3 = data.table(
    pmid = rep.int(pmids, xml_length(x1)),
    type_name = xml_text(x2),
    type_id = xml_attr(x2, 'UI'))
  x3 = unique(x3)
  appendTable(con, paste0(tableName, tableSuffix), x3)
  return(x3)}


#' @export
getMeshTerm = function(pmXml, pmids, con, tableSuffix = '',
                       tableName = 'mesh_term') {
  x1 = xml_find_first(pmXml, './/MeshHeadingList')
  n = xml_length(x1)
  x2 = xml_find_all(x1[n > 0], './/DescriptorName')
  x3 = data.table(
    pmid = rep.int(pmids, n),
    term_name = xml_text(x2),
    term_id = xml_attr(x2, 'UI'),
    major_topic = xml_attr(x2, 'MajorTopicYN'))
  x3 = unique(x3)
  appendTable(con, paste0(tableName, tableSuffix), x3)
  return(x3)}


#' @export
getComment = function(pmXml, pmids, con, tableSuffix = '',
                      tableName = 'comment') {
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
getAbstract = function(pmXml, pmids, con, tableSuffix = '',
                       tableName = 'abstract') {
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
