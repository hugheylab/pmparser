foreach::registerDoSEQ()

refDir = 'pubmed_sample'
localDir = tempfile()
dbtype = 'sqlite'
nFiles = 1L
nCitations = Inf

withr::local_file(localDir)
if (dir.exists(localDir)) unlink(localDir, recursive = TRUE)
dir.create(localDir)
x = file.copy(list.files(refDir, include.dirs = TRUE, full.names = TRUE),
              localDir, recursive = TRUE, copy.date = TRUE)

dbBase = 'pmdb_sample_'

byCols = list(
  abstract = NULL,
  abstract_copyright = NULL,
  article = 'pmid',
  article_id = NULL,
  author = c('pmid', 'author_pos'),
  author_affiliation = c('pmid', 'author_pos', 'affiliation_pos'),
  author_affiliation_identifier = NULL,
  author_identifier = NULL,
  author_list = 'pmid',
  chemical = NULL,
  comment = NULL,
  data_bank = NULL,
  grant_item = NULL,
  grant_list = NULL,
  investigator = c('pmid', 'investigator_pos'),
  investigator_affiliation = c('pmid', 'investigator_pos', 'affiliation_pos'),
  investigator_affiliation_identifier = NULL,
  investigator_identifier = NULL,
  keyword_item = NULL,
  keyword_list = NULL,
  mesh_descriptor = NULL,
  mesh_list = 'pmid',
  mesh_qualifier = NULL,
  pmid_status = 'pmid',
  pub_history = NULL,
  pub_type = NULL,
  journal = 'pmid',
  xml_processed = 'xml_filename')

test_that('modifyPubmedDb create is unique', {
  mode = 'create'
  dbname = file.path(localDir, glue('{dbBase}{mode}_obs.db'))

  modifyPubmedDb(
    localDir = localDir, dbname = dbname, dbtype = dbtype,
    nFiles = nFiles, nCitations = nCitations, mode = mode)

  con = withr::local_db_connection(connect(dbtype, dbname))

  for (tableName in names(byCols)) {
    d = data.table::setDT(DBI::dbReadTable(con, tableName))
    by = if (is.null(byCols[[tableName]])) colnames(d) else byCols[[tableName]]
    expect_equal(anyDuplicated(d, by = by), 0L, label = tableName)}
})

test_that('modifyPubmedDb update is unique', {
  mode = 'update'
  dbname = file.path(localDir, glue('{dbBase}create.db'))

  modifyPubmedDb(
    localDir = localDir, dbname = dbname, dbtype = dbtype,
    nFiles = nFiles, nCitations = nCitations, mode = mode)

  con = withr::local_db_connection(connect(dbtype, dbname))

  for (tableName in names(byCols)) {
    d = data.table::setDT(DBI::dbReadTable(con, tableName))
    by = if (is.null(byCols[[tableName]])) colnames(d) else byCols[[tableName]]
    expect_equal(anyDuplicated(d, by = by), 0L, label = tableName)}
})
