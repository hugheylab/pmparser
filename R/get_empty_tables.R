getEmptyTables = function(tableSuffix) {
  ac = as.character()
  ai = as.integer()
  dBase = data.table(pmid = ai, version = ai)

  r = list(
    pmid_status = data.table(xml_filename = ac, status = ac),

    article_id = data.table(id_type = ac, id_value = ac),

    title_journal = data.table(title = ac, journal_full = ac,
      journal_abbrev = ac),

    pub_type = data.table(type_name = ac, type_id = ac),

    pub_date = data.table(pub_status = ac, pub_date = data.table::as.IDate(ai)),

    mesh_term = data.table(term_name = ac, term_id = ac, major_topic = ac),

    keyword_list = data.table(list_owner = ac),

    keyword_item = data.table(keyword_name = ac, major_topic = ac),

    grant_list = data.table(complete = ac),

    grant_item = data.table(
      grant_id = ac, acronym = ac, agency = ac, country = ac),

    chemical = data.table(
      registry_number = ac, substance_name = ac, substance_ui = ac),

    data_bank = data.table(
      data_bank_name = ac, accession_number = ac),

    comment = data.table(ref_type = ac, ref_pmid = ai),

    abstract = data.table(
      text = ac, label = ac, nlm_category = ac, copyright = ac),

    author = data.table(
      author_pos = ai, last_name = ac, fore_name = ac, initials = ac,
      suffix = ac, collective_name = ac),

    author_affiliation = data.table(
      author_pos = ai, affiliation_pos = ai, affiliation = ac),

    author_identifier = data.table(
      author_pos = ai, source = ac, identifier = ac),

    author_affiliation_identifier = data.table(
      author_pos = ai, affiliation_pos = ai, source = ac, identifier = ac),

    investigator = data.table(
      investigator_pos = ai, last_name = ac, fore_name = ac, initials = ac,
      suffix = ac),

    investigator_affiliation = data.table(
      investigator_pos = ai, affiliation_pos = ai, affiliation = ac),

    investigator_identifier = data.table(
      investigator_pos = ai, source = ac, identifier = ac),

    investigator_affiliation_identifier = data.table(
      investigator_pos = ai, affiliation_pos = ai, source = ac,
      identifier = ac),

    xml_processed = data.table(
      xml_filename = ac, pmparser_version = ac,
      datetime_processed = as.POSIXct(ac)))

  for (tableName in setdiff(names(r), 'xml_processed')) {
    r[[tableName]] = cbind(dBase, r[[tableName]])}

  names(r) = paste_(names(r), tableSuffix)

  if (!isEmpty(tableSuffix)) {
    tableNames = names(r)[!grepl('^(pmid_status|xml_processed)', names(r))]
    for (tableName in tableNames) {
      r[[tableName]][, xml_filename := ac]
      setcolorder(r[[tableName]], c('pmid', 'version', 'xml_filename'))}}

  return(r)}


writeEmptyTables = function(tableSuffix = NULL, overwrite = FALSE,
                            dbtype = 'postgres', dbname = NULL, ...) {
  if (is.null(dbname)) {
    return(invisible())}

  con = connect(dbtype, dbname, ...)
  emptyTables = getEmptyTables(tableSuffix)

  tablesExist = sapply(names(emptyTables),
                       function(x) DBI::dbExistsTable(con, x))
  stopifnot(!any(tablesExist) || isTRUE(overwrite))

  for (i in 1:length(emptyTables)) {
    DBI::dbWriteTable(con, names(emptyTables)[i],
                      emptyTables[[i]], overwrite = TRUE)}

  disconnect(con)
  invisible()}
