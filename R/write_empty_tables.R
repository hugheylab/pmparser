getEmptyTables = function(tableSuffix) {
  ac = as.character()
  ai = as.integer()

  r = list(
    pmid_status = data.table(pmid = ai, status = ac, xml_filename = ac),

    article_id = data.table(pmid = ai, id_type = ac, id_value = ac),

    title_journal = data.table(
      pmid = ai, title = ac, journal_full = ac, journal_abbrev = ac),

    pub_type = data.table(pmid = ai, type_name = ac, type_id = ac),

    pub_date = data.table(
      pmid = ai, pub_status = ac, pub_date = data.table::as.IDate(ai)),

    mesh_term = data.table(
      pmid = ai, term_name = ac, term_id = ac, major_topic = ac),

    comment = data.table(pmid = ai, ref_type = ac, ref_pmid = ai),

    abstract = data.table(
      pmid = ai, text = ac, label = ac, nlm_category = ac, copyright = ac),

    author = data.table(
      pmid = ai, author_pos = ai, last_name = ac, fore_name = ac, initials = ac,
      suffix = ac, collective_name = ac),

    author_affiliation = data.table(
      pmid = ai, author_pos = ai, affiliation_pos = ai, affiliation = ac),

    author_identifier = data.table(
      pmid = ai, author_pos = ai, source = ac, identifier = ac),

    author_affiliation_identifier = data.table(
      pmid = ai, author_pos = ai, affiliation_pos = ai, source = ac,
      identifier = ac),

    investigator = data.table(
      pmid = ai, investigator_pos = ai, last_name = ac, fore_name = ac,
      initials = ac, suffix = ac),

    investigator_affiliation = data.table(
      pmid = ai, investigator_pos = ai, affiliation_pos = ai, affiliation = ac),

    investigator_identifier = data.table(
      pmid = ai, investigator_pos = ai, source = ac, identifier = ac),

    investigator_affiliation_identifier = data.table(
      pmid = ai, investigator_pos = ai, affiliation_pos = ai, source = ac,
      identifier = ac),

    xml_processed = data.table(
      xml_filename = ac, datetime_processed = as.POSIXct(ac)))

  names(r) = paste_(names(r), tableSuffix)

  if (!is.null(tableSuffix) && tableSuffix != '') {
    tableNames = setdiff(names(r), c('pmid_status', 'xml_processed'))
    for (tableName in tableNames) {
      r[[tableName]][, xml_filename := ac]}}

  return(r)}


#' @export
writeEmptyTables = function(tableSuffix = NULL, overwrite = FALSE,
                            dbname = NULL, ...) {
  if (is.null(dbname)) {
    return(invisible())}

  con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)
  emptyTables = getEmptyTables(tableSuffix)

  tablesExist = sapply(names(emptyTables),
                       function(x) DBI::dbExistsTable(con, x))
  stopifnot(!any(tablesExist) || isTRUE(overwrite))

  for (i in 1:length(emptyTables)) {
    DBI::dbWriteTable(con, paste_(names(emptyTables)[i], tableSuffix),
                      emptyTables[[i]], overwrite = TRUE)}

  invisible()}
