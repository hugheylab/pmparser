getParsingTables = function(tableSuffix, tableNames = NULL) {
  xml_filename = NULL
  ac = as.character()
  ai = as.integer()

  r = list(
    pmid_status = data.table(xml_filename = ac, status = ac),

    article_id = data.table(id_type = ac, id_value = ac),

    article = data.table(title = ac, pub_date = as.Date(ac), pub_model = ac),

    journal = data.table(
      journal_name = ac, journal_iso = ac, pub_date = as.Date(ac),
      pub_year = ac, pub_month = ac, pub_day = ac, medline_date = ac,
      volume = ac, issue = ac, cited_medium = ac),

    pub_type = data.table(type_name = ac, type_id = ac),

    pub_history = data.table(pub_status = ac, pub_date = as.Date(ac)),

    mesh_list = data.table(indexing_method = ac),

    mesh_descriptor = data.table(
      descriptor_pos = ac, descriptor_name = ac, descriptor_ui = ac,
      descriptor_major_topic = ac),

    mesh_qualifier = data.table(
      descriptor_pos = ac, qualifier_name = ac, qualifier_ui = ac,
      qualifier_major_topic = ac),

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

    abstract_copyright = data.table(copyright = ac),

    abstract = data.table(
      text = ac, label = ac, nlm_category = ac, abstract_pos = ai),

    author = data.table(
      author_pos = ai, last_name = ac, fore_name = ac, initials = ac,
      suffix = ac, valid = ac, equal_contrib = ac, collective_name = ac),

    author_affiliation = data.table(
      author_pos = ai, affiliation_pos = ai, affiliation = ac),

    author_identifier = data.table(
      author_pos = ai, source = ac, identifier = ac),

    author_affiliation_identifier = data.table(
      author_pos = ai, affiliation_pos = ai, source = ac, identifier = ac),

    author_list = data.table(complete = ac),

    investigator = data.table(
      investigator_pos = ai, last_name = ac, fore_name = ac, initials = ac,
      suffix = ac, valid = ac),

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

  dBase = data.table(pmid = ai, version = ai)
  for (tableName in setdiff(names(r), 'xml_processed')) {
    r[[tableName]] = cbind(dBase, r[[tableName]])}

  if (!isEmpty(tableSuffix)) {
    tNames = setdiff(names(r), c('pmid_status', 'xml_processed'))
    for (tableName in tNames) {
      r[[tableName]][, xml_filename := ac]
      setcolorder(r[[tableName]], c('pmid', 'version', 'xml_filename'))}}

  names(r) = paste_(names(r), tableSuffix)
  if (!is.null(tableNames)) r = r[names(r) %in% tableNames]
  return(r)}


createParsingTables = function(
  tableSuffix = NULL, overwrite = FALSE, dbtype = 'postgres', dbname = NULL,
  tableNames = NULL, ...) {

  if (is.null(dbname)) return(invisible())

  con = connect(dbtype, dbname, ...)
  parTables = getParsingTables(tableSuffix, tableNames)

  tableExists = sapply(
    names(parTables), function(x) DBI::dbExistsTable(con, x))
  stopifnot(!any(tableExists) || isTRUE(overwrite))

  for (i in seq_len(length(parTables))) {
    if (tableExists[i]) DBI::dbRemoveTable(con, names(parTables)[i])
    createTable(con, names(parTables)[i], parTables[[i]])}

  disconnect(con)
  invisible()}
