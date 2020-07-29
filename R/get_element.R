#' @export
getPmidStatus = function(rawXml, filename, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_all(xml_find_all(rawXml, './/DeleteCitation'), './/PMID')
  x2 = data.table(pmid = xml_integer(x1)) # could have zero rows
  x2[, status := 'Deleted']

  pmXml = xml_find_all(rawXml, './/PubmedArticle')
  x3 = data.table(
    pmid = xml_integer(xml_find_first(pmXml, './/PMID')),
    status = xml_attr(xml_find_first(pmXml, 'MedlineCitation'), 'Status'))

  x4 = rbind(x2, x3)
  setXmlFilename(x4, filename)

  appendTable(con, paste_('pmid_status', tableSuffix), x4)
  return(list(pmXml, x4))}


#' @export
getArticleId = function(pmXml, pmids, filename = NULL, con = NULL,
                        tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/ArticleIdList') # assume this comes before refs
  nIds = xml_length(x1)

  x2 = xml_find_all(x1, './/ArticleId')
  x3 = data.table(
    pmid = rep.int(pmids, nIds),
    id_type = xml_attr(x2, 'IdType'),
    id_value = xml_text(x2))
  x4 = x3[id_type %in% c('doi', 'pmc')]

  setXmlFilename(x4, filename)
  appendTable(con, paste_('article_id', tableSuffix), x4)
  return(x4)}


#' @export
getPubDate = function(pmXml, pmids, filename = NULL, con = NULL,
                      tableSuffix = NULL) {
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

  setXmlFilename(x4, filename)
  appendTable(con, paste_('pub_date', tableSuffix), x4)
  return(x4)}


#' @export
getTitleJournal = function(pmXml, pmids, filename = NULL, con = NULL,
                           tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/Journal')
  x2 = data.table(
    pmid = pmids,
    title = xml_text(xml_find_first(pmXml, './/ArticleTitle')),
    journal_full = xml_text(xml_find_first(x1, './/Title')),
    journal_abbrev = xml_text(xml_find_first(x1, './/ISOAbbreviation')))

  setXmlFilename(x2, filename)
  appendTable(con, paste_('title_journal', tableSuffix), x2)
  return(x2)}


#' @export
getPubType = function(pmXml, pmids, filename = NULL, con = NULL,
                      tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/PublicationTypeList')
  x2 = xml_find_all(x1, './/PublicationType')
  x3 = data.table(
    pmid = rep.int(pmids, xml_length(x1)),
    type_name = xml_text(x2),
    type_id = xml_attr(x2, 'UI'))

  setXmlFilename(x3, filename)
  appendTable(con, paste_('pub_type', tableSuffix), x3)
  return(x3)}


#' @export
getMeshTerm = function(pmXml, pmids, filename = NULL, con = NULL,
                       tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/MeshHeadingList')
  n = xml_length(x1)
  x2 = xml_find_all(x1[n > 0], './/DescriptorName')

  x3 = data.table(
    pmid = rep.int(pmids, n),
    term_name = xml_text(x2),
    term_id = xml_attr(x2, 'UI'),
    major_topic = xml_attr(x2, 'MajorTopicYN'))

  setXmlFilename(x3, filename)
  appendTable(con, paste_('mesh_term', tableSuffix), x3)
  return(x3)}


#' @export
getKeyword = function(pmXml, pmids, filename = NULL, con = NULL,
                      tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/KeywordList')
  n = xml_length(x1)

  x2 = data.table(
    pmid = pmids[n > 0],
    list_owner = xml_attr(x1[n > 0], 'Owner'))

  x3 = xml_find_all(x1[n > 0], './/Keyword')

  x4 = data.table(
    pmid = rep.int(pmids, n),
    keyword_name = trimws(xml_text(x3)),
    major_topic = xml_attr(x3, 'MajorTopicYN'))

  r = list(x2, x4)
  names(r) = c(paste_('keyword_list', tableSuffix),
               paste_('keyword_item', tableSuffix)) # consistent with grant_item

  for (i in 1:length(r)) {
    setXmlFilename(r[[i]], filename)
    appendTable(con, names(r)[i], r[[i]])}

  return(r)}


#' @export
getGrant = function(pmXml, pmids, filename = NULL, con = NULL,
                    tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/GrantList')
  n = xml_length(x1)

  x2 = data.table(
    pmid = pmids[n > 0],
    complete = xml_attr(x1[n > 0], 'CompleteYN'))

  x3 = xml_find_all(x1[n > 0], './/Grant')

  x4 = data.table(
    pmid = rep.int(pmids, n),
    grant_id = xml_text(xml_find_first(x3, './/GrantID')),
    acronym = xml_text(xml_find_first(x3, './/Acronym')),
    agency = xml_text(xml_find_first(x3, './/Agency')),
    country = xml_text(xml_find_first(x3, './/Country')))

  r = list(x2, x4)
  names(r) = c(paste_('grant_list', tableSuffix),
               paste_('grant_item', tableSuffix)) # avoid reserved word

  for (i in 1:length(r)) {
    setXmlFilename(r[[i]], filename)
    appendTable(con, names(r)[i], r[[i]])}

  return(r)}


#' @export
getChemical = function(pmXml, pmids, filename = NULL, con = NULL,
                       tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/ChemicalList')
  n = xml_length(x1)

  x2 = xml_find_all(x1[n > 0], './/Chemical')
  x3 = xml_find_first(x2, './/NameOfSubstance')

  x4 = data.table(
    pmid = rep.int(pmids, n),
    registry_number = xml_text(xml_find_first(x2, './/RegistryNumber')),
    substance_name = xml_text(x3),
    substance_ui = xml_attr(x3, 'UI'))

  setXmlFilename(x4, filename)
  appendTable(con, paste_('chemical', tableSuffix), x4)
  return(x4)}


#' @export
getComment = function(pmXml, pmids, filename = NULL, con = NULL,
                      tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/CommentsCorrectionsList')
  n = xml_length(x1)
  x2 = xml_find_all(x1[n >0], './/CommentsCorrections')

  x3 = data.table(
    pmid = rep.int(pmids, n),
    ref_type = xml_attr(x2, 'RefType'),
    ref_pmid = xml_integer(xml_find_first(x2, './/PMID')))

  setXmlFilename(x3, filename)
  appendTable(con, paste_('comment', tableSuffix), x3)
  return(x3)}


#' @export
getAbstract = function(pmXml, pmids, filename = NULL, con = NULL,
                       tableSuffix = NULL) {
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

  setXmlFilename(x6, filename)
  appendTable(con, paste_('abstract', tableSuffix), x6)
  return(x6)}
