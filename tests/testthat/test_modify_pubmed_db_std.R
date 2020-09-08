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

test_that('modifyPubmedDb create matches standard', {
  mode = 'create'
  dbnameObs = file.path(localDir, glue('{dbBase}{mode}_obs.db'))
  dbnameExp = file.path(refDir, glue('{dbBase}{mode}.db'))

  modifyPubmedDb(
    localDir = localDir, dbname = dbnameObs, dbtype = dbtype,
    nFiles = nFiles, nCitations = nCitations, mode = mode)

  conExp = withr::local_db_connection(connect(dbtype, dbnameExp))
  conObs = withr::local_db_connection(connect(dbtype, dbnameObs))
  tableNames = DBI::dbListTables(conExp)
  tableNames = setdiff(tableNames, c('xml_processed', 'citation_version'))

  for (tableName in tableNames) {
    expect_equal(DBI::dbReadTable(conObs, tableName),
                 DBI::dbReadTable(conExp, tableName),
                 label = tableName)}
})

test_that('modifyPubmedDb update matches standard', {
  mode = 'update'
  dbnameObs = file.path(localDir, glue('{dbBase}create.db'))
  dbnameExp = file.path(refDir, glue('{dbBase}{mode}.db'))

  modifyPubmedDb(
    localDir = localDir, dbname = dbnameObs, dbtype = dbtype,
    nFiles = nFiles, nCitations = nCitations, mode = mode)

  conExp = withr::local_db_connection(connect(dbtype, dbnameExp))
  conObs = withr::local_db_connection(connect(dbtype, dbnameObs))
  tableNames = DBI::dbListTables(conExp)
  tableNames = setdiff(tableNames, c('xml_processed', 'citation_version'))

  for (tableName in tableNames) {
    expect_equal(DBI::dbReadTable(conObs, tableName),
                 DBI::dbReadTable(conExp, tableName),
                 label = tableName)}
})
