#' Parse elements from a PubMed XML file
#'
#' Elements are parsed according to the MEDLINE®PubMed® XML Element
#' Descriptions and their Attributes
#' [here](https://www.nlm.nih.gov/bsd/licensee/elements_descriptions.html).
#'
#' @param rawXml An xml document obtained by loading a PubMed XML file using
#'   [xml2::read_xml()].
#' @param filename A string or NULL. If not NULL, a column `xml_filename` is
#'   added to the generated table(s).
#' @param pmXml An xml nodeset derived from `rawXml`, such as that returned by
#'   `parsePmidStatus()`, where each node corresponds to a PMID.
#' @param pmids Integer vector of PMIDs for `pmXml`.
#' @param con Connection to the database, created using [DBI::dbConnect()].
#' @param tableSuffix String to append to the table names.
#'
#' @return `parsePmidStatus()` returns a list of two objects: the first is an xml
#'   nodeset in which each node corresponds to a PMID, the second is a
#'   data.table with columns `pmid` and `status`. The latter is parsed from the
#'   DeleteCitation and MedlineCitation sections.
#'
#'   `parseArticleId()` returns a data.table with columns `pmid`, `id_type`, and
#'   `id_value`, parsed from the ArticleIdList section. Only `id_type`s "doi"
#'   and "pmc" are retained.
#'
#'   `parsePubDate()` returns a data.table with columns `pmid`, `pub_status`,
#'   and `pub_date`, parsed from the History section.
#'
#'   `parseTitleJournal()` returns a data.table with columns `pmid`, `title`,
#'   `journal_full`, and `journal_abbrev`, parsed from the Journal section.
#'
#'   `parsePubType()` returns a data.table with columns `pmid`, `type_name`, and
#'   `type_id`, parsed from the PublicationTypeList section.
#'
#'   `parseMeshTerm()` returns a data.table with columns `pmid`, `term_name`,
#'   `term_id`, and `major_topic`, parsed from the MeshHeadingList section.
#'
#'   `parseKeyword()` returns a list of two data.tables: the first has columns
#'   `pmid` and `list_owner`, the second has columns `pmid`, `keyword_name`,
#'   and `major_topic`. Both data.tables are parsed from the KeywordList
#'   section.
#'
#'   `parseGrant()` returns a list of two data.tables: the first has columns
#'   `pmid` and `complete`, the second has columns `pmid`, `grant_id`,
#'   `acronym`, `agency`, and `country`. Both data.tables are parsed from the
#'   GrantList section.
#'
#'   `parseChemical()` returns a data.table with columns `pmid`,
#'   `registry_number`, `substance_name`, and `substance_ui`, parsed from the
#'   ChemicalList section.
#'
#'   `parseDataBank()` returns a data.table with columns `pmid`,
#'   `data_bank_name`, and `accession_number`, parsed from the DataBankList
#'   section.
#'
#'   `parseComment()` returns a data.table with columns `pmid`, `ref_type`, and
#'   `ref_pmid`, parsed from the CommentsCorrectionsList section.
#'
#'   `parseAbstract()` returns a data.table with columns `pmid`, `text`, `label`,
#'   `nlm_category`, and `copyright`, parsed from the Abstract section.
#'
#'   `parseAuthorAffiliation()` returns a list of data.tables parsed from the
#'   AuthorList section. The first data.table is for authors and has columns
#'   `pmid`, `author_pos`, `last_name`, `fore_name`, `initials`, `suffix`,
#'   and `collective_name`. The second data.table is for affiliations and has
#'   columns `pmid`, `author_pos`, `affiliation_pos`, and `affiliation`. The
#'   third data.table is for author identifiers and has columns `pmid`,
#'   `author_pos`, `source`, and `identifier`. The fourth data.table is for
#'   author affiliation identifiers and has columns `pmid`, `author_pos`,
#'   `affiliation_pos`, `source`, and `identifier`.
#'
#'   `parseInvestigatorAffiliation()` returns a list of data.tables identical to
#'   those returned by `parseAuthorAffiliation()`, except parsed from the
#'   InvestigatorList section, with column names containing "investigator"
#'   instead of "author", and lacking a column in the first data.table for
#'   `collective_name`.
#'
#' @name parseElement
NULL


#' @rdname parseElement
#' @export
parsePmidStatus = function(rawXml, filename, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_all(xml_find_all(rawXml, './/DeleteCitation'), './/PMID')
  x2 = data.table(pmid = xml_integer(x1)) # could have zero rows
  x2[, status := 'Deleted']

  pmXml = xml_find_all(rawXml, './/PubmedArticle')
  x3 = data.table(
    pmid = xml_integer(xml_find_first(pmXml, './/PMID')),
    status = xml_attr(xml_find_first(pmXml, 'MedlineCitation'), 'Status'))

  x4 = rbind(x2, x3)
  setColumn(x4, filename)

  appendTable(con, paste_('pmid_status', tableSuffix), x4)
  return(list(pmXml, x4))}


#' @rdname parseElement
#' @export
parseArticleId = function(pmXml, pmids, filename = NULL, con = NULL,
                          tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/ArticleIdList') # assume this comes before refs
  nIds = xml_length(x1)

  x2 = xml_find_all(x1, './/ArticleId')
  x3 = data.table(
    pmid = rep.int(pmids, nIds),
    id_type = xml_attr(x2, 'IdType'),
    id_value = xml_text(x2))
  x4 = x3[id_type %in% c('doi', 'pmc')]

  setColumn(x4, filename)
  appendTable(con, paste_('article_id', tableSuffix), x4)
  return(x4)}


#' @rdname parseElement
#' @export
parsePubDate = function(pmXml, pmids, filename = NULL, con = NULL,
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

  setColumn(x4, filename)
  appendTable(con, paste_('pub_date', tableSuffix), x4)
  return(x4)}


#' @rdname parseElement
#' @export
parseTitleJournal = function(pmXml, pmids, filename = NULL, con = NULL,
                             tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/Journal')
  x2 = data.table(
    pmid = pmids,
    title = xml_text(xml_find_first(pmXml, './/ArticleTitle')),
    journal_full = xml_text(xml_find_first(x1, './/Title')),
    journal_abbrev = xml_text(xml_find_first(x1, './/ISOAbbreviation')))

  setColumn(x2, filename)
  appendTable(con, paste_('title_journal', tableSuffix), x2)
  return(x2)}


#' @rdname parseElement
#' @export
parsePubType = function(pmXml, pmids, filename = NULL, con = NULL,
                        tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/PublicationTypeList')
  x2 = xml_find_all(x1, './/PublicationType')
  x3 = data.table(
    pmid = rep.int(pmids, xml_length(x1)),
    type_name = xml_text(x2),
    type_id = xml_attr(x2, 'UI'))

  setColumn(x3, filename)
  appendTable(con, paste_('pub_type', tableSuffix), x3)
  return(x3)}


#' @rdname parseElement
#' @export
parseMeshTerm = function(pmXml, pmids, filename = NULL, con = NULL,
                         tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/MeshHeadingList')
  n = xml_length(x1)
  x2 = xml_find_all(x1[n > 0], './/DescriptorName')

  x3 = data.table(
    pmid = rep.int(pmids, n),
    term_name = xml_text(x2),
    term_id = xml_attr(x2, 'UI'),
    major_topic = xml_attr(x2, 'MajorTopicYN'))

  setColumn(x3, filename)
  appendTable(con, paste_('mesh_term', tableSuffix), x3)
  return(x3)}


#' @rdname parseElement
#' @export
parseKeyword = function(pmXml, pmids, filename = NULL, con = NULL,
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
    setColumn(r[[i]], filename)
    appendTable(con, names(r)[i], r[[i]])}

  return(r)}


#' @rdname parseElement
#' @export
parseGrant = function(pmXml, pmids, filename = NULL, con = NULL,
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
    setColumn(r[[i]], filename)
    appendTable(con, names(r)[i], r[[i]])}

  return(r)}


#' @rdname parseElement
#' @export
parseChemical = function(pmXml, pmids, filename = NULL, con = NULL,
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

  setColumn(x4, filename)
  appendTable(con, paste_('chemical', tableSuffix), x4)
  return(x4)}


#' @rdname parseElement
#' @export
parseDataBank = function(pmXml, pmids, filename = NULL, con = NULL,
                         tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/DataBankList')
  nBanksPerPmid = xml_length(x1)

  x2 = xml_find_all(pmXml[nBanksPerPmid > 0], './/DataBank')
  x3 = xml_find_all(x2, './/AccessionNumber', flatten = FALSE)
  nAccsPerBank = sapply(x3, length)

  x4 = data.table(
    pmid = rep.int(pmids, nBanksPerPmid),
    data_bank_name = xml_text(xml_find_first(x2, './/DataBankName')))

  x5 = x4[rep.int(1:.N, nAccsPerBank)]
  x5[, accession_number := unlist(lapply(x3, xml_text))]

  setColumn(x5, filename)
  appendTable(con, paste_('data_bank', tableSuffix), x5)
  return(x5)}


#' @rdname parseElement
#' @export
parseComment = function(pmXml, pmids, filename = NULL, con = NULL,
                        tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/CommentsCorrectionsList')
  n = xml_length(x1)
  x2 = xml_find_all(x1[n > 0], './/CommentsCorrections')

  x3 = data.table(
    pmid = rep.int(pmids, n),
    ref_type = xml_attr(x2, 'RefType'),
    ref_pmid = xml_integer(xml_find_first(x2, './/PMID')))

  setColumn(x3, filename)
  appendTable(con, paste_('comment', tableSuffix), x3)
  return(x3)}


#' @rdname parseElement
#' @export
parseAbstract = function(pmXml, pmids, filename = NULL, con = NULL,
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

  setColumn(x6, filename)
  appendTable(con, paste_('abstract', tableSuffix), x6)
  return(x6)}
