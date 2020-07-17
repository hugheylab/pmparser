#' @export
getEmptyTables = function() {
  ac = as.character()
  ai = as.integer()

  r = list(
    deleted = data.table(filename = ac, pmid = ai),

    article_ids = data.table(
      filename = ac, index = ai, doi = ac, pmc = ac, pmid = ai),

    medline = data.table(pmid = ai, status = ac),

    titles_journals = data.table(
      pmid = ai, title = ac, journal_full = ac, journal_abbrev = ac),

    pub_types = data.table(pmid = ai, pub_type = ac, pub_type_id = ac),

    pub_dates = data.table(
      pmid = ai, pub_status = ac, pub_date = data.table::as.IDate(ai)),

    mesh_terms = data.table(
      pmid = ai, term = ac, term_id = ac, major_topic = ac),

    comments = data.table(pmid = ai, ref_type = ac, ref_pmid = ai),

    abstracts = data.table(
      pmid = ai, text = ac, label = ac, nlm_category = ac, copyright = ac),

    authors = data.table(
      pmid = ai, author_pos = ai, last_name = ac, fore_name = ac, initials = ac,
      suffix = ac, collective_name = ac),

    author_affiliations = data.table(
      pmid = ai, author_pos = ai, affiliation_pos = ai, affiliation = ac),

    author_identifiers = data.table(
      pmid = ai, author_pos = ai, source = ac, identifier = ac),

    author_affiliation_identifiers = data.table(
      pmid = ai, author_pos = ai, affiliation_pos = ai, source = ac,
      identifier = ac),

    investigators = data.table(
      pmid = ai, investigator_pos = ai, last_name = ac, fore_name = ac,
      initials = ac, suffix = ac),

    investigator_affiliations = data.table(
      pmid = ai, investigator_pos = ai, affiliation_pos = ai, affiliation = ac),

    investigator_identifiers = data.table(
      pmid = ai, investigator_pos = ai, source = ac, identifier = ac),

    investigator_affiliation_identifiers = data.table(
      pmid = ai, investigator_pos = ai, affiliation_pos = ai, source = ac,
      identifier = ac))

  return(r)}


#' @export
writeEmptyTables = function(tableSuffix = '', overwrite = FALSE, dbname = NULL, ...) {
  if (is.null(dbname)) {
    return(invisible())}

  con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)
  emptyTables = getEmptyTables()
  tablesExist = sapply(names(emptyTables), function(x) DBI::dbExistsTable(con, x))
  stopifnot(!any(tablesExist) || isTRUE(overwrite))

  for (i in 1:length(emptyTables)) {
    DBI::dbWriteTable(con, paste0(names(emptyTables)[i], tableSuffix),
                      emptyTables[[i]], overwrite = TRUE)}

  invisible()}
