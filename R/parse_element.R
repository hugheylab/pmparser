#' Parse elements from a PubMed XML file
#'
#' Elements are parsed according to the MEDLINE®PubMed® XML Element
#' Descriptions and their Attributes
#' [here](https://www.nlm.nih.gov/bsd/licensee/elements_descriptions.html).
#'
#' @param rawXml An xml document obtained by loading a PubMed XML file using
#'   [xml2::read_xml()].
#' @param filename A string that will be added to a column `xml_filename`.
#' @param pmXml An xml nodeset derived from `rawXml`, such as that returned by
#'   `parsePmidStatus()`, where each node corresponds to a PMID.
#' @param dPmid A data.table with one row for each node of `pmXml`, should have
#'   columns `pmid`, `version`, and possibly `xml_filename`.
#' @param con Connection to the database, created using [DBI::dbConnect()].
#' @param tableSuffix String to append to the table names.
#'
#' @return `parsePmidStatus()` returns a list of two objects. The first is an
#'   xml nodeset in which each node corresponds to a PubmedArticle in the
#'   `rawXml` object. The second is a data.table with columns `pmid`, `version`,
#'   `xml_filename`, and `status`, in which each row corresponds to a
#'   PubmedArticle in the `rawXml` object or a deleted pmid. The `status` column
#'   is parsed from the DeleteCitation and MedlineCitation sections.
#'
#'   The following functions return a data.table or list of data.tables with
#'   columns from `dPmid` plus the columns specified.
#'
#'   `parseArticleId()`: a data.table with columns `id_type` and `id_value`,
#'   parsed from the ArticleIdList section. Only `id_type`s "doi"
#'   and "pmc" are retained.
#'
#'   `parsePubDate()`: a data.table with columns  `pub_status` and `pub_date`,
#'   parsed from the History section.
#'
#'   `parseTitleJournal()`: a data.table with columns `title`, `journal_full`,
#'   and `journal_abbrev`, parsed from the Journal section.
#'
#'   `parsePubType()`: a data.table with columns `type_name` and `type_id`,
#'   parsed from the PublicationTypeList section.
#'
#'   `parseMeshTerm()`: a data.table with columns `term_name`, `term_id`, and
#'   `major_topic`, parsed from the MeshHeadingList section.
#'
#'   `parseKeyword()`: a list of two data.tables parsed from the KeywordList
#'   section. The first with column `list_owner`, the second with columns
#'   `keyword_name` and `major_topic`.
#'
#'   `parseGrant()`: a list of two data.tables parsed from the GrantList
#'   section. The first has column `complete`, the second has columns
#'   `grant_id`, `acronym`, `agency`, and `country`.
#'
#'   `parseChemical()`: a data.table with columns `registry_number`,
#'   `substance_name`, and `substance_ui`, parsed from the ChemicalList section.
#'
#'   `parseDataBank()`: a data.table with columns `data_bank_name` and
#'   `accession_number`, parsed from the DataBankList section.
#'
#'   `parseComment()`: a data.table with columns `ref_type` and `ref_pmid`,
#'   parsed from the CommentsCorrectionsList section.
#'
#'   `parseAbstract()`: a list of two data.tables parsed from the Abstract
#'   section. The first has column `copyright`. The second has columns `text`,
#'   `label`, and `nlm_category`.
#'
#'   `parseAuthorAffiliation()`: a list of data.tables parsed from the
#'   AuthorList section. The first is for authors and has columns `author_pos`,
#'   `last_name`, `fore_name`, `initials`, `suffix`, and `collective_name`. The
#'   second is for affiliations and has columns `author_pos`, `affiliation_pos`,
#'   and `affiliation`. The third is for author identifiers and has columns
#'   `author_pos`, `source`, and `identifier`. The fourth is for author
#'   affiliation identifiers and has columns `author_pos`, `affiliation_pos`,
#'   `source`, and `identifier`.
#'
#'   `parseInvestigatorAffiliation()`: a list of data.tables identical to
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
  x2[, `:=`(version = 999, # ugly, but should work
            xml_filename = filename,
            status = 'Deleted')]

  pmXml = xml_find_all(rawXml, './/PubmedArticle')
  x0 = xml_find_first(pmXml, './/PMID')

  x3 = data.table(
    pmid = xml_integer(x0),
    version = as.integer(xml_attr(x0, 'Version')),
    xml_filename = filename,
    status = xml_attr(xml_find_first(pmXml, 'MedlineCitation'), 'Status'))

  x4 = rbind(x3, x2)
  appendTable(con, paste_('pmid_status', tableSuffix), x4)
  return(list(pmXml, x4))}


#' @rdname parseElement
#' @export
parseArticleId = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/ArticleIdList') # assume this comes before refs
  nIds = xml_length(x1)

  x2 = xml_find_all(x1, './/ArticleId')
  x3 = data.table(
    dPmid[rep.int(1:.N, nIds)],
    id_type = xml_attr(x2, 'IdType'),
    id_value = xml_text(x2))
  x4 = x3[id_type %in% c('doi', 'pmc')]

  appendTable(con, paste_('article_id', tableSuffix), x4)
  return(x4)}


#' @rdname parseElement
#' @export
parsePubDate = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/History')
  nHist = xml_length(x1)
  x2 = xml_find_all(x1, './/PubMedPubDate')

  x4 = data.table(
    dPmid[rep.int(1:.N, nHist)],
    pub_status = xml_attr(x2, 'PubStatus'),
    y = xml_text(xml_find_all(x2, './/Year')),
    m = xml_text(xml_find_all(x2, './/Month')),
    d = xml_text(xml_find_all(x2, './/Day')))

  x4[, pub_date := data.table::as.IDate(sprintf('%s-%s-%s', y, m, d))]
  x4[, c('y', 'm', 'd') := NULL]

  appendTable(con, paste_('pub_date', tableSuffix), x4)
  return(x4)}


#' @rdname parseElement
#' @export
parseTitleJournal = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/Journal')
  x2 = data.table(
    dPmid,
    title = xml_text(xml_find_first(pmXml, './/ArticleTitle')),
    journal_full = xml_text(xml_find_first(x1, './/Title')),
    journal_abbrev = xml_text(xml_find_first(x1, './/ISOAbbreviation')))

  appendTable(con, paste_('title_journal', tableSuffix), x2)
  return(x2)}


#' @rdname parseElement
#' @export
parsePubType = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/PublicationTypeList')
  x2 = xml_find_all(x1, './/PublicationType')

  x3 = data.table(
    dPmid[rep.int(1:.N, xml_length(x1))],
    type_name = xml_text(x2),
    type_id = xml_attr(x2, 'UI'))

  appendTable(con, paste_('pub_type', tableSuffix), x3)
  return(x3)}


#' @rdname parseElement
#' @export
parseMeshTerm = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/MeshHeadingList')
  n = xml_length(x1)
  x2 = xml_find_all(x1[n > 0], './/DescriptorName')

  x3 = data.table(
    dPmid[rep.int(1:.N, n)],
    term_name = xml_text(x2),
    term_id = xml_attr(x2, 'UI'),
    major_topic = xml_attr(x2, 'MajorTopicYN'))

  appendTable(con, paste_('mesh_term', tableSuffix), x3)
  return(x3)}


#' @rdname parseElement
#' @export
parseKeyword = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/KeywordList')
  n = xml_length(x1)

  x2 = data.table(
    dPmid[n > 0],
    list_owner = xml_attr(x1[n > 0], 'Owner'))

  x3 = xml_find_all(x1[n > 0], './/Keyword')

  x4 = data.table(
    dPmid[rep.int(1:.N, n)],
    keyword_name = trimws(xml_text(x3)),
    major_topic = xml_attr(x3, 'MajorTopicYN'))

  r = list(x2, x4)
  names(r) = c(paste_('keyword_list', tableSuffix),
               paste_('keyword_item', tableSuffix)) # consistent with grant_item

  for (i in 1:length(r)) {
    appendTable(con, names(r)[i], r[[i]])}

  return(r)}


#' @rdname parseElement
#' @export
parseGrant = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/GrantList')
  n = xml_length(x1)

  x2 = data.table(
    dPmid[n > 0],
    complete = xml_attr(x1[n > 0], 'CompleteYN'))

  x3 = xml_find_all(x1[n > 0], './/Grant')

  x4 = data.table(
    dPmid[rep.int(1:.N, n)],
    grant_id = xml_text(xml_find_first(x3, './/GrantID')),
    acronym = xml_text(xml_find_first(x3, './/Acronym')),
    agency = xml_text(xml_find_first(x3, './/Agency')),
    country = xml_text(xml_find_first(x3, './/Country')))

  r = list(x2, x4)
  names(r) = c(paste_('grant_list', tableSuffix),
               paste_('grant_item', tableSuffix)) # avoid reserved word

  for (i in 1:length(r)) {
    appendTable(con, names(r)[i], r[[i]])}

  return(r)}


#' @rdname parseElement
#' @export
parseChemical = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/ChemicalList')
  n = xml_length(x1)

  x2 = xml_find_all(x1[n > 0], './/Chemical')
  x3 = xml_find_first(x2, './/NameOfSubstance')

  x4 = data.table(
    dPmid[rep.int(1:.N, n)],
    registry_number = xml_text(xml_find_first(x2, './/RegistryNumber')),
    substance_name = xml_text(x3),
    substance_ui = xml_attr(x3, 'UI'))

  appendTable(con, paste_('chemical', tableSuffix), x4)
  return(x4)}


#' @rdname parseElement
#' @export
parseDataBank = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/DataBankList')
  nBanksPerPmid = xml_length(x1)

  x2 = xml_find_all(x1[nBanksPerPmid > 0], './/DataBank')
  x3 = xml_find_all(x2, './/AccessionNumber', flatten = FALSE)
  nAccsPerBank = sapply(x3, length)

  x4 = data.table(
    dPmid[rep.int(1:.N, nBanksPerPmid)],
    data_bank_name = xml_text(xml_find_first(x2, './/DataBankName')))

  x5 = x4[rep.int(1:.N, nAccsPerBank)]
  x5[, accession_number := unlist(lapply(x3, xml_text))]

  appendTable(con, paste_('data_bank', tableSuffix), x5)
  return(x5)}


#' @rdname parseElement
#' @export
parseComment = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/CommentsCorrectionsList')
  n = xml_length(x1)
  x2 = xml_find_all(x1[n > 0], './/CommentsCorrections')

  x3 = data.table(
    dPmid[rep.int(1:.N, n)],
    ref_type = xml_attr(x2, 'RefType'),
    ref_pmid = xml_integer(xml_find_first(x2, './/PMID')))

  appendTable(con, paste_('comment', tableSuffix), x3)
  return(x3)}


#' @rdname parseElement
#' @export
parseAbstract = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_first(pmXml, './/Abstract')
  x2 = data.table(
    dPmid,
    copyright = xml_text(xml_find_first(x1, './/CopyrightInformation')))

  x3 = xml_length(x1) - !is.na(x2$copyright)
  x4 = xml_find_all(xml_find_all(pmXml, './/Abstract'), './/AbstractText')

  x5 = data.table(
    dPmid[rep.int(1:.N, x3)],
    text = xml_text(x4),
    label = xml_attr(x4, 'Label'),
    nlm_category = xml_attr(x4, 'NlmCategory'))

  r = list(x2[!is.na(copyright)], x5)
  names(r) = c(paste_('abstract_copyright', tableSuffix),
               paste_('abstract', tableSuffix))

  for (i in 1:length(r)) {
    appendTable(con, names(r)[i], r[[i]])}

  return(r)}
