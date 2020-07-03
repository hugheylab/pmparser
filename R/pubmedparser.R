#' @importFrom data.table data.table := setcolorder
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom xml2 xml_find_all xml_find_first xml_text xml_attr xml_length
NULL


globalVariables(c('.', '.N', 'd', 'V1', 'affiliation', 'author_pos', 'filename',
                  'filenameNow', 'i', 'id_type', 'm', 'pmid', 'pub_date',
                  'pubmed', 'split_list', 'step', 'y', 'orcid', 'source'))


#' @export
writeLogFile = function(logPath, txt = NULL, append = TRUE) {
  if (is.null(logPath)) {
    invisible(NULL)
  } else {
    data.table::fwrite(list(txt), logPath, append = append, quote = FALSE)}}


#' @export
getArticleIds = function(pmXml, filename, con, tableName = 'article_ids') {
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
  x6[, filename := filename]
  setcolorder(x6, 'filename')

  # beware a pmid may have multiple doi versions
  if (!is.null(con)) {
    DBI::dbAppendTable(con, tableName, x6)}
  return(x6)}


#' @export
getPubDates = function(pmXml, pmids, con, tableName = 'pub_dates') {
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
  if (!is.null(con)) {
    DBI::dbAppendTable(con, tableName, x4)}
  return(x4)}


#' @export
getTitlesJournals = function(pmXml, pmids, con, tableName = 'titles_journals') {
  x1 = xml_find_first(pmXml, './/Journal')
  x2 = data.table(
    pmid = pmids,
    title = xml_text(xml_find_first(pmXml, './/ArticleTitle')),
    journal_full = xml_text(xml_find_first(x1, './/Title')),
    journal_abbrev = xml_text(xml_find_first(x1, './/ISOAbbreviation')))
  x2 = unique(x2)
  if (!is.null(con)) {
    DBI::dbAppendTable(con, tableName, x2)}
  return(x2)}


#' @export
getPubTypes = function(pmXml, pmids, con, tableName = 'pub_types') {
  x1 = xml_find_first(pmXml, './/PublicationTypeList')
  x2 = xml_find_all(x1, './/PublicationType')
  x3 = data.table(
    pmid = rep.int(pmids, xml_length(x1)),
    pub_type = xml_text(x2),
    pub_type_id = xml_attr(x2, 'UI'))
  x3 = unique(x3)
  if (!is.null(con)) {
    DBI::dbAppendTable(con, tableName, x3)}
  return(x3)}


#' @export
getMeshTerms = function(pmXml, pmids, con, tableName = 'mesh_terms') {
  x1 = xml_find_first(pmXml, './/MeshHeadingList')
  n = xml_length(x1)
  x2 = xml_find_all(x1[n > 0], './/DescriptorName')
  x3 = data.table(
    pmid = rep.int(pmids, n),
    term = xml_text(x2),
    term_id = xml_attr(x2, 'UI'),
    major_topic = xml_attr(x2, 'MajorTopicYN'))
  x3 = unique(x3)
  if (nrow(x3) > 0 & !is.null(con)) {
    DBI::dbAppendTable(con, tableName, x3)}
  return(x3)}


#' @export
getAbstracts = function(pmXml, pmids, con, tableName = 'abstracts') {
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
  if (nrow(x6) > 0 & !is.null(con)) {
    DBI::dbAppendTable(con, tableName, x6)}
  return(x6)}


#' @export
getAuthorsAffiliations = function(pmXml, pmids, con, idx = NULL,
                                  tableNames = c('authors', 'affiliations')) {
  if (is.null(idx)) {
    idx = 1:length(pmXml)}

  x2 = xml_find_all(pmXml[idx], './/Author')
  n_authors = xml_length(xml_find_first(pmXml[idx], './/AuthorList'))

  # will miss orcid ids that aren't first, but way faster than the alternative,
  # and ISNI and GRID ids refer to institutions not authors anyway
  x3 = xml_find_first(x2, './/Identifier')

  dAuthors = data.table(
    pmid = rep.int(pmids[idx], n_authors),
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

  affiliations = xml_text(xml_find_all(x2, './/AffiliationInfo'))
  # because affiliations are not in their own list, I can't see a way around the
  # slow foreach loop to know how many affiliations per author
  n_affiliations = foreach(i = 1:length(x2), .combine = c) %do% {
    length(xml_find_all(x2[i], './/AffiliationInfo'))}

  dAffils = dAuthors[rep.int(1:nrow(dAuthors), n_affiliations), .(pmid, author_pos)]
  dAffils[, affiliation := affiliations]

  # TODO: should I call unique on these?
  if (!is.null(con)) {
    DBI::dbAppendTable(con, tableNames[1L], dAuthors)
    DBI::dbAppendTable(con, tableNames[2L], dAffils)}
  return(list(dAuthors, dAffils))}


#' @export
getEmptyDt = function(tableName) {
  ac = as.character()
  ai = as.integer()
  d = switch(
    tableName,
    article_ids = data.table(filename = ac, index = ai, doi = ac, pmc = ac,
                             pmid = ai),
    titles_journals = data.table(pmid = ai, title = ac, journal_full = ac,
                                 journal_abbrev = ac),
    pub_types = data.table(pmid = ai, pub_type = ac, pub_type_id = ac),
    pub_dates = data.table(pmid = ai, pub_status = ac,
                           pub_date = data.table::as.IDate(ai)),
    mesh_terms = data.table(pmid = ai, term = ac, term_id = ac,
                            major_topic = ac),
    abstracts = data.table(pmid = ai, text = ac, label = ac, nlm_category = ac,
                           copyright = ac),
    authors = data.table(pmid = ai, author_pos = ai, last_name = ac,
                         fore_name = ac, initials = ac, collective_name = ac),
    affiliations = data.table(pmid = ai, author_pos = ai, affiliation = ac))
  return(d)}


#' @export
processPubmedXml = function(inputDir, dbName, logPath = NULL, idx = NULL,
                            overwrite = TRUE) {
  filenames = list.files(inputDir, '.xml.gz$')

  if (!is.null(dbName) && isTRUE(overwrite)) {
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbName)
    tableNames = c('article_ids', 'titles_journals', 'pub_types', 'pub_dates',
                   'mesh_terms', 'abstracts', 'authors', 'affiliations')
    for (tableName in tableNames) {
      DBI::dbWriteTable(con, tableName, getEmptyDt(tableName), overwrite = TRUE)}}

  exitNames = c('finished', 'failed')
  writeLogFile(logPath, sprintf('%s started', Sys.time()), append = FALSE)

  r = foreach(filename = filenames) %dopar% {
    # create separate connection for each process
    if (is.null(dbName)) {
      con = NULL
    } else {
      con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbName)}

    x0 = xml2::read_xml(file.path(inputDir, filename))
    pmXml = xml_find_all(x0, './/PubmedArticle')

    ex = tryCatch({getArticleIds(pmXml, filename, con)}, error = function(e) NULL)
    writeLogFile(logPath, sprintf('%s %s article_ids for %s',
                                  Sys.time(), exitNames[is.null(ex) + 1L], filename))

    if (!is.null(ex)) {
      pmids = ex$pmid

      ex = tryCatch({getTitlesJournals(pmXml, pmids, con)}, error = function(e) NULL)
      writeLogFile(logPath, sprintf('%s %s titles_journals for %s',
                                    Sys.time(), exitNames[is.null(ex) + 1L], filename))

      ex = tryCatch({getPubTypes(pmXml, pmids, con)}, error = function(e) NULL)
      writeLogFile(logPath, sprintf('%s %s pub_types for %s',
                                    Sys.time(), exitNames[is.null(ex) + 1L], filename))

      ex = tryCatch({getPubDates(pmXml, pmids, con)}, error = function(e) NULL)
      writeLogFile(logPath, sprintf('%s %s pub_dates for %s',
                                    Sys.time(), exitNames[is.null(ex) + 1L], filename))

      ex = tryCatch({getMeshTerms(pmXml, pmids, con)}, error = function(e) NULL)
      writeLogFile(logPath, sprintf('%s %s mesh_terms for %s',
                                    Sys.time(), exitNames[is.null(ex) + 1L], filename))

      ex = tryCatch({getAbstracts(pmXml, pmids, con)}, error = function(e) NULL)
      writeLogFile(logPath, sprintf('%s %s abstracts for %s',
                                    Sys.time(), exitNames[is.null(ex) + 1L], filename))

      ex = tryCatch({getAuthorsAffiliations(pmXml, pmids, con, idx)}, error = function(e) NULL)
      writeLogFile(logPath, sprintf('%s %s authors and affiliations for %s',
                                    Sys.time(), exitNames[is.null(ex) + 1L], filename))
    }
  }

  writeLogFile(logPath, sprintf('%s finished', Sys.time()))
  invisible(0)}


#' @export
getFailed = function(logPath) {
  d = data.table::fread(logPath, sep = NULL, header = FALSE)[2:(.N - 1)]
  d = d[stringr::str_detect(V1, ' failed ')]
  d[, split_list := stringr::str_split(V1, ' ')]
  d[, `:=`(step = sapply(split_list, function(x) x[4]),
           filename = sapply(split_list, function(x) x[length(x)]))]
  d = d[order(filename), .(step, filename)]
  return(d)}


#' @export
reprocessPubmedSingle = function(inputDir, filename, steps, dbName,
                                 logPath, idx, tableNames) {
  exitNames = c('finished', 'failed')
  if (is.null(dbName)) {
    con = NULL
  } else {
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbName)}

  x0 = xml2::read_xml(file.path(inputDir, filename))
  pmXml = xml_find_all(x0, './/PubmedArticle')

  ex = tryCatch({getArticleIds(pmXml, filename, NULL)}, error = function(e) NULL)
  writeLogFile(logPath, sprintf('%s %s article_ids for %s',
                                Sys.time(), exitNames[is.null(ex) + 1L], filename))

  if (!is.null(ex)) {
    pmids = ex$pmid

    if ('abstracts' %in% steps) {
      ex = tryCatch({getAbstracts(pmXml, pmids, con, tableNames['abstracts'])},
                    error = function(e) NULL)
      writeLogFile(logPath, sprintf('%s %s abstracts for %s',
                                    Sys.time(), exitNames[is.null(ex) + 1L], filename))}

    if ('authors' %in% steps) {
      ex = tryCatch({getAuthorsAffiliations(pmXml, pmids, con, idx,
                                            tableNames[c('authors', 'affiliations')])},
                    error = function(e) NULL)
      writeLogFile(logPath, sprintf('%s %s authors and affiliations for %s',
                                    Sys.time(), exitNames[is.null(ex) + 1L], filename))}
  }

  invisible(0)}


#' @export
reprocessPubmedXml = function(inputDir, dbName, logPath, idx, dFailed) {
  steps = c('abstracts', 'authors', 'affiliations')
  tableNames = paste0(steps, '_2')
  names(tableNames) = steps

  if (!is.null(dbName)) {
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbName)
    ac = as.character()
    ai = as.integer()

    abstracts = data.table(pmid = ai, text = ac, label = ac, nlm_category = ac,
                           copyright = ac)
    authors = data.table(pmid = ai, author_pos = ai, last_name = ac,
                         fore_name = ac, initials = ac, collective_name = ac)
    affiliations = data.table(pmid = ai, author_pos = ai, affiliation = ac)

    DBI::dbWriteTable(con, tableNames['abstracts'], abstracts, overwrite = TRUE)
    DBI::dbWriteTable(con, tableNames['authors'], authors, overwrite = TRUE)
    DBI::dbWriteTable(con, tableNames['affiliations'], affiliations, overwrite = TRUE)
  }

  writeLogFile(logPath, sprintf('%s started', Sys.time()), append = FALSE)
  r = foreach(filenameNow = unique(dFailed$filename)) %dopar% {
    steps = dFailed[filename == filenameNow]$step
    reprocessPubmedSingle(inputDir, filenameNow, steps, dbName,
                          logPath, idx, tableNames)}

  writeLogFile(logPath, sprintf('%s finished', Sys.time()))
  invisible(0)}
