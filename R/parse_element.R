#' Parse elements from a PubMed XML file
#'
#' Elements are parsed according to the MEDLINE®PubMed® XML Element
#' Descriptions and their Attributes
#' [here](https://www.nlm.nih.gov/bsd/licensee/elements_descriptions.html).
#' These functions should not normally be called directly, as they are called by
#' [modifyPubmedDb()].
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
#'   `parseArticle()`: a data.table with columns `title`, `pub_date`, and
#'   `pub_model`, parsed from the Article section.
#'
#'   `parsePubHistory()`: a data.table with columns `pub_status` and `pub_date`,
#'   parsed from the History section.
#'
#'   `parseJournal()`: a data.table with columns `journal_name`, `journal_iso`,
#'   `pub_date`, `pub_year`, `pub_month`, `pub_day`, `medline_date`, `volume`,
#'   `issue`, and `cited_medium`, parsed from the Journal section.
#'
#'   `parsePubType()`: a data.table with columns `type_name` and `type_id`,
#'   parsed from the PublicationTypeList section.
#'
#'   `parseMesh()`: a list of three data.tables parsed mostly from the
#'   MeshHeadingList section. The first has column `indexing_method` (parsed
#'   from the MedlineCitation section), the second has columns `descriptor_pos`,
#'   `descriptor_name`, `descriptor_ui`, and `descriptor_major_topic`, the
#'   third has columns `descriptor_pos`, `qualifier_name`, `qualifier_ui`, and
#'   `qualifier_major_topic`.
#'
#'   `parseKeyword()`: a list of two data.tables parsed from the KeywordList
#'   section. The first has column `list_owner`, the second has columns
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
#'   `parseAuthor()`: a list of data.tables parsed from the AuthorList section.
#'   The first is for authors and has columns `author_pos`, `last_name`,
#'   `fore_name`, `initials`, `suffix`, `valid`, `equal_contrib`, and
#'   `collective_name`. The second is for affiliations and has columns
#'   `author_pos`, `affiliation_pos`, and `affiliation`. The third is for author
#'   identifiers and has columns `author_pos`, `source`, and `identifier`. The
#'   fourth is for author affiliation identifiers and has columns `author_pos`,
#'   `affiliation_pos`, `source`, and `identifier`. The fifth is for the author
#'   list itself and has a column `complete`.
#'
#'   `parseInvestigator()`: a list of data.tables similar to those returned by
#'   `parseAuthor()`, except parsed from the InvestigatorList section, with
#'   column names containing "investigator" instead of "author", and where the
#'   first data.table lacks columns for `equal_contrib` and `collective_name`
#'   and the fifth data.table does not exist.
#'
#'   `parseOther()`: a list of data.tables parsed from the OtherAbstract and
#'   OtherID sections. The first has columns `text`, `type`, and `language`. The
#'   second has columns `source` and `id_value`.
#'
#' @examples
#' library('data.table')
#' library('xml2')
#'
#' filename = 'pubmed20n1016.xml.gz'
#' rawXml = read_xml(system.file('extdata', filename, package = 'pmparser'))
#'
#' pmidStatusList = parsePmidStatus(rawXml, filename)
#' pmXml = pmidStatusList[[1L]]
#' dPmidRaw = pmidStatusList[[2L]]
#' dPmid = dPmidRaw[status != 'Deleted', !'status']
#'
#' dArticleId = parseArticleId(pmXml, dPmid)
#' dArticle = parseArticle(pmXml, dPmid)
#' dJournal = parseJournal(pmXml, dPmid)
#' dPubType = parsePubType(pmXml, dPmid)
#' dPubHistory = parsePubHistory(pmXml, dPmid)
#' meshRes = parseMesh(pmXml, dPmid)
#' keywordRes = parseKeyword(pmXml, dPmid)
#' grantRes = parseGrant(pmXml, dPmid)
#' dChemical = parseChemical(pmXml, dPmid)
#' dDataBank = parseDataBank(pmXml, dPmid)
#' dComment = parseComment(pmXml, dPmid)
#' abstractRes = parseAbstract(pmXml, dPmid)
#' authorRes = parseAuthor(pmXml, dPmid)
#' investigatorRes = parseInvestigator(pmXml, dPmid)
#' otherRes = parseOther(pmXml, dPmid)
#'
#' @seealso [getCitation()], [modifyPubmedDb()]
#'
#' @name parseElement
NULL


#' @rdname parseElement
#' @export
parsePmidStatus = function(rawXml, filename, con = NULL, tableSuffix = NULL) {
  x1 = xml_find_all(xml_find_all(rawXml, './/DeleteCitation'), './/PMID')
  x2 = data.table(pmid = xml_integer(x1)) # could have zero rows
  x2[, `:=`(version = 999L, # ugly, but should work
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
  .N = id_type = NULL
  stopifnot(length(pmXml) == nrow(dPmid))
  x1 = xml_find_first(pmXml, './/ArticleIdList') # assume this comes before refs
  nIds = xml_length(x1)

  x2 = xml_find_all(x1[nIds > 0], './/ArticleId')
  x3 = data.table(
    dPmid[rep.int(seq_len(.N), nIds)],
    id_type = xml_attr(x2, 'IdType'),
    id_value = xml_text(x2))
  x4 = x3[id_type %in% c('doi', 'pmc')]

  appendTable(con, paste_('article_id', tableSuffix), x4)
  return(x4)}


#' @rdname parseElement
#' @export
parseArticle = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  pub_date = y = m = d = NULL
  stopifnot(length(pmXml) == nrow(dPmid))
  x1 = xml_find_first(pmXml, './/Article')
  idx = xml_length(x1) > 0

  x2 = data.table(
    dPmid[idx],
    title = xml_text(xml_find_first(pmXml[idx], './/ArticleTitle')),
    pub_model = xml_attr(x1[idx], 'PubModel'))

  x3 = xml_find_first(x1, './/ArticleDate')
  idx3 = xml_length(x3) > 0

  x4 = data.table(
    dPmid[idx3],
    y = xml_text(xml_find_first(x3[idx3], './/Year')),
    m = xml_text(xml_find_first(x3[idx3], './/Month')),
    d = xml_text(xml_find_first(x3[idx3], './/Day')))

  x4[, pub_date := as.Date(sprintf('%s-%s-%s', y, m, d))]
  # x4[, pub_date := as.Date(glue('{y}-{m}-{d}'), .envir = x4)]
  x4[, c('y', 'm', 'd') := NULL]

  x5 = merge(x2, x4, by = colnames(dPmid), all.x = TRUE, sort = FALSE)
  setcolorder(x5, c(colnames(dPmid), 'title', 'pub_date', 'pub_model'))

  appendTable(con, paste_('article', tableSuffix), x5)
  return(x5)}


#' @rdname parseElement
#' @export
parsePubHistory = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  .N = pub_date = y = m = d = NULL
  stopifnot(length(pmXml) == nrow(dPmid))
  x1 = xml_find_first(pmXml, './/History')
  nHist = xml_length(x1)
  x2 = xml_find_all(x1[nHist > 0], './/PubMedPubDate')

  x4 = data.table(
    dPmid[rep.int(seq_len(.N), nHist)],
    pub_status = xml_attr(x2, 'PubStatus'),
    y = xml_text(xml_find_all(x2, './/Year')),
    m = xml_text(xml_find_all(x2, './/Month')),
    d = xml_text(xml_find_all(x2, './/Day')))

  x4[, pub_date := as.Date(sprintf('%s-%s-%s', y, m, d))]
  x4[, c('y', 'm', 'd') := NULL]

  appendTable(con, paste_('pub_history', tableSuffix), x4)
  return(x4)}


#' @rdname parseElement
#' @export
parseJournal = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  medline_date = pub_month_tmp1 = pub_month = pub_day_tmp = pub_day = pub_year =
    pub_month_tmp2 = pub_date = date_idx = NULL
  stopifnot(length(pmXml) == nrow(dPmid))
  x1 = xml_find_first(pmXml, './/Journal')
  idx = xml_length(x1) > 0

  x2 = data.table(
    dPmid[idx],
    journal_name = xml_text(xml_find_first(x1[idx], './/Title')),
    journal_iso = xml_text(xml_find_first(x1[idx], './/ISOAbbreviation')),
    pub_year = xml_text(xml_find_first(x1[idx], './/Year')),
    pub_month = xml_text(xml_find_first(x1[idx], './/Month')),
    pub_day = xml_text(xml_find_first(x1[idx], './/Day')),
    medline_date = xml_text(xml_find_first(x1[idx], './/MedlineDate')),
    volume = xml_text(xml_find_first(x1[idx], './/Volume')),
    issue = xml_text(xml_find_first(x1[idx], './/Issue')),
    cited_medium = xml_attr(xml_find_first(x1[idx], 'CitedMedium')))

  x2[is.na(pub_year), date_idx := regexpr('[0-9]{4}', medline_date)]
  x2[is.na(pub_year) & date_idx > 0,
     pub_year := as.integer(substr(medline_date, date_idx, date_idx + 3L))]
  x2[, date_idx := NULL]

  monthNums = sprintf('%.2d', 1:12)
  dMonth = data.table(
    pub_month_tmp1 = c('jan', 'feb', 'mar', 'apr', 'may', 'jun',
                       'jul', 'aug', 'sep', 'oct', 'nov', 'dec', monthNums),
    pub_month_tmp2 = rep(monthNums, 2))

  x2[, pub_month_tmp1 := tolower(pub_month)]
  x3 = merge(x2, dMonth, by = 'pub_month_tmp1', all.x = TRUE, sort = FALSE)

  x3[, pub_day_tmp := pub_day]
  x3[is.na(pub_day_tmp), pub_day_tmp := '01']
  x3[!is.na(pub_year) & !is.na(pub_month_tmp2),
     pub_date := as.Date(
       sprintf('%s-%s-%s', pub_year, pub_month_tmp2, pub_day_tmp))]

  x3[, c('pub_month_tmp1', 'pub_month_tmp2', 'pub_day_tmp') := NULL]
  setcolorder(x3, c(colnames(dPmid), 'journal_name', 'journal_iso', 'pub_date'))
  appendTable(con, paste_('journal', tableSuffix), x3)
  return(x3)}


#' @rdname parseElement
#' @export
parsePubType = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  .N = NULL
  stopifnot(length(pmXml) == nrow(dPmid))
  x1 = xml_find_first(pmXml, './/PublicationTypeList')
  n = xml_length(x1)
  x2 = xml_find_all(x1[n > 0], './/PublicationType')

  x3 = data.table(
    dPmid[rep.int(seq_len(.N), xml_length(x1))],
    type_name = xml_text(x2),
    type_id = xml_attr(x2, 'UI'))

  appendTable(con, paste_('pub_type', tableSuffix), x3)
  return(x3)}


#' @rdname parseElement
#' @export
parseMesh = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  indexing_method = .N = descriptor_pos = pmid = NULL
  stopifnot(length(pmXml) == nrow(dPmid))
  ai = as.integer()
  ac = as.character()

  x1 = xml_find_first(pmXml, './/MeshHeadingList')
  nDescPerPmid = xml_length(x1)

  x7 = data.table(
    dPmid,
    indexing_method = xml_attr(
      xml_find_first(pmXml, 'MedlineCitation'), 'IndexingMethod'))
  x7 = x7[!is.na(indexing_method)]

  x2 = xml_find_all(x1[nDescPerPmid > 0], './/DescriptorName')
  x3 = data.table(
    dPmid[rep.int(seq_len(.N), nDescPerPmid)],
    descriptor_name = xml_text(x2),
    descriptor_ui = xml_attr(x2, 'UI'),
    descriptor_major_topic = xml_attr(x2, 'MajorTopicYN'))

  if (nrow(x3) > 0) {
    x3[, descriptor_pos := seq_len(.N), by = pmid]
  } else {
    x3[, descriptor_pos := ai]}
  setcolorder(x3, c(colnames(dPmid), 'descriptor_pos'))

  x4 = xml_find_all(x1[nDescPerPmid > 0], './/MeshHeading')
  x5 = xml_find_all(x4, './/QualifierName', flatten = FALSE)
  nQualPerDesc = lengths(x5)

  descPos = unlist(lapply(nDescPerPmid[nDescPerPmid > 0], function(n) 1:n))

  if (length(nQualPerDesc) > 0 && sum(nQualPerDesc) > 0) {
    x6 = data.table(
      x3[rep.int(seq_len(.N), nQualPerDesc), colnames(dPmid), with = FALSE],
      descriptor_pos = rep.int(descPos, nQualPerDesc),
      qualifier_name = unlist(lapply(x5, xml_text)),
      qualifier_ui = unlist(lapply(x5, function(x) xml_attr(x, 'UI'))),
      qualifier_major_topic = unlist(lapply(x5, function(x)
        xml_attr(x, 'MajorTopicYN'))))
  } else {
    x6 = data.table(
      x3[0L, colnames(dPmid), with = FALSE], descriptor_pos = ai,
      qualifier_name = ac, qualifier_ui = ac, qualifier_major_topic = ac)}

  r = list(x7, x3, x6)
  names(r) = paste_(
    c('mesh_list', 'mesh_descriptor', 'mesh_qualifier'), tableSuffix)

  for (i in seq_len(length(r))) appendTable(con, names(r)[i], r[[i]])
  return(r)}


#' @rdname parseElement
#' @export
parseKeyword = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  .N = NULL
  stopifnot(length(pmXml) == nrow(dPmid))
  x1 = xml_find_first(pmXml, './/KeywordList')
  n = xml_length(x1)

  x2 = data.table(
    dPmid[n > 0],
    list_owner = xml_attr(x1[n > 0], 'Owner'))

  x3 = xml_find_all(x1[n > 0], './/Keyword')

  x4 = data.table(
    dPmid[rep.int(seq_len(.N), n)],
    keyword_name = trimws(xml_text(x3), whitespace = '[ \t\r\n\uFEFF]'),
    major_topic = xml_attr(x3, 'MajorTopicYN'))

  r = list(x2, x4)
  # consistent with grant_item
  names(r) = paste_(c('keyword_list', 'keyword_item'), tableSuffix)

  for (i in seq_len(length(r))) appendTable(con, names(r)[i], r[[i]])
  return(r)}


#' @rdname parseElement
#' @export
parseGrant = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  .N = NULL
  stopifnot(length(pmXml) == nrow(dPmid))
  x1 = xml_find_first(pmXml, './/GrantList')
  n = xml_length(x1)

  x2 = data.table(
    dPmid[n > 0],
    complete = xml_attr(x1[n > 0], 'CompleteYN'))

  x3 = xml_find_all(x1[n > 0], './/Grant')

  x4 = data.table(
    dPmid[rep.int(seq_len(.N), n)],
    grant_id = xml_text(xml_find_first(x3, './/GrantID')),
    acronym = xml_text(xml_find_first(x3, './/Acronym')),
    agency = xml_text(xml_find_first(x3, './/Agency')),
    country = xml_text(xml_find_first(x3, './/Country')))
  x4 = unique(x4) # funding info has some duplicates

  r = list(x2, x4)
  # avoid reserved word
  names(r) = paste_(c('grant_list', 'grant_item'), tableSuffix)

  for (i in seq_len(length(r))) appendTable(con, names(r)[i], r[[i]])
  return(r)}


#' @rdname parseElement
#' @export
parseChemical = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  .N = NULL
  stopifnot(length(pmXml) == nrow(dPmid))
  x1 = xml_find_first(pmXml, './/ChemicalList')
  n = xml_length(x1)

  x2 = xml_find_all(x1[n > 0], './/Chemical')
  x3 = xml_find_first(x2, './/NameOfSubstance')

  x4 = data.table(
    dPmid[rep.int(seq_len(.N), n)],
    registry_number = xml_text(xml_find_first(x2, './/RegistryNumber')),
    substance_name = xml_text(x3),
    substance_ui = xml_attr(x3, 'UI'))

  appendTable(con, paste_('chemical', tableSuffix), x4)
  return(x4)}


#' @rdname parseElement
#' @export
parseDataBank = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  .N = accession_number = NULL
  stopifnot(length(pmXml) == nrow(dPmid))
  x1 = xml_find_first(pmXml, './/DataBankList')
  nBanksPerPmid = xml_length(x1)

  if (sum(nBanksPerPmid) == 0) {
    return(data.table(dPmid[0L], accession_number = as.character()))}

  x2 = xml_find_all(x1[nBanksPerPmid > 0], './/DataBank')
  x3 = xml_find_all(x2, './/AccessionNumber', flatten = FALSE)
  nAccsPerBank = lengths(x3)

  x4 = data.table(
    dPmid[rep.int(seq_len(.N), nBanksPerPmid)],
    data_bank_name = xml_text(xml_find_first(x2, './/DataBankName')))

  x5 = x4[rep.int(seq_len(.N), nAccsPerBank)]
  x5[, accession_number := unlist(lapply(x3, xml_text))]

  appendTable(con, paste_('data_bank', tableSuffix), x5)
  return(x5)}


#' @rdname parseElement
#' @export
parseComment = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  .N = NULL
  stopifnot(length(pmXml) == nrow(dPmid))
  x1 = xml_find_first(pmXml, './/CommentsCorrectionsList')
  n = xml_length(x1)
  x2 = xml_find_all(x1[n > 0], './/CommentsCorrections')

  x3 = data.table(
    dPmid[rep.int(seq_len(.N), n)],
    ref_type = xml_attr(x2, 'RefType'),
    ref_pmid = xml_integer(xml_find_first(x2, './/PMID')))

  appendTable(con, paste_('comment', tableSuffix), x3)
  return(x3)}


#' @rdname parseElement
#' @export
parseAbstract = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  .N = abstract_pos = copyright = pmid = NULL
  stopifnot(length(pmXml) == nrow(dPmid))
  x1 = xml_find_first(pmXml, './/Abstract')
  x2 = data.table(
    dPmid,
    copyright = xml_text(xml_find_first(x1, './/CopyrightInformation')))

  x3 = xml_length(x1) - !is.na(x2$copyright)
  x4 = xml_find_all(xml_find_all(pmXml, './/Abstract'), './/AbstractText')

  x5 = data.table(
    dPmid[rep.int(seq_len(.N), x3)],
    text = xml_text(x4),
    label = xml_attr(x4, 'Label'),
    nlm_category = xml_attr(x4, 'NlmCategory'))
  if (nrow(x5) > 0) {
    x5[, abstract_pos := seq_len(.N), by = pmid]
  } else {
    x5[, abstract_pos := as.integer()]}

  r = list(x2[!is.na(copyright)], x5)
  names(r) = c(paste_('abstract_copyright', tableSuffix),
               paste_('abstract', tableSuffix))

  for (i in seq_len(length(r))) appendTable(con, names(r)[i], r[[i]])
  return(r)}


#' @rdname parseElement
#' @export
parseOther = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  .N = pmid = NULL
  stopifnot(length(pmXml) == nrow(dPmid))

  x1 = xml_find_first(pmXml, './/OtherAbstract')
  idx = xml_length(x1) > 0
  x2 = data.table(
    dPmid[idx],
    text = xml_text(xml_find_first(x1[idx], './/AbstractText')),
    type = xml_attr(x1[idx], 'Type'),
    language = xml_attr(x1[idx], 'Language'))

  x3 = xml_find_all(pmXml, './/OtherID', flatten = FALSE)
  nIdsPerPmid = lengths(x3)
  x4 = data.table(
    dPmid[rep.int(seq_len(.N), nIdsPerPmid)],
    source = unlist(lapply(x3, function(x) xml_attr(x, 'Source'))),
    id_value = unlist(lapply(x3, xml_text)))

  r = list(x2, x4)
  names(r) = c(paste_('other_abstract', tableSuffix),
               paste_('other_id', tableSuffix))

  for (i in seq_len(length(r))) appendTable(con, names(r)[i], r[[i]])
  return(r)}
