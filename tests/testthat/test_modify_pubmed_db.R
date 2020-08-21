foreach::registerDoSEQ()

refDir = 'pubmed_sample'
localDir = tempdir()
dbtype = 'sqlite'
nFiles = 1L
nCitations = Inf

local_file(localDir, recursive = TRUE)
if (dir.exists(localDir)) unlink(localDir, recursive = TRUE)
dir.create(localDir)
x = file.copy(list.files(refDir, include.dirs = TRUE, full.names = TRUE),
              localDir, recursive = TRUE, copy.date = TRUE)

dbBase = 'pmdb_sample_'

test_that('modifyPubmedDb create', {
  mode = 'create'
  dbnameObs = file.path(localDir, paste0(dbBase, mode, '_obs.db'))
  dbnameExp = file.path(refDir, paste0(dbBase, mode, '.db'))

  modifyPubmedDb(
    localDir = localDir, dbname = dbnameObs, dbtype = dbtype,
    nFiles = nFiles, nCitations = nCitations, mode = mode)

  conExp = withr::local_db_connection(connect(dbtype, dbnameExp))
  conObs = withr::local_db_connection(connect(dbtype, dbnameObs))
  tableNames = DBI::dbListTables(conExp)
  tableNames = setdiff(tableNames, c('xml_processed', 'citation_version'))

  for (tableName in setdiff(tableNames, 'xml_processed')) {
    expect_equal(DBI::dbReadTable(conObs, tableName),
                 DBI::dbReadTable(conExp, tableName))}
})

test_that('modifyPubmedDb update', {
  mode = 'update'
  dbnameObs = file.path(localDir, paste0(dbBase, 'create.db'))
  dbnameExp = file.path(refDir, paste0(dbBase, mode, '.db'))

  modifyPubmedDb(
    localDir = localDir, dbname = dbnameObs, dbtype = dbtype,
    nFiles = nFiles, nCitations = nCitations, mode = mode)

  conExp = withr::local_db_connection(connect(dbtype, dbnameExp))
  conObs = withr::local_db_connection(connect(dbtype, dbnameObs))
  tableNames = DBI::dbListTables(conExp)
  tableNames = setdiff(tableNames, c('xml_processed', 'citation_version'))

  for (tableName in tableNames) {
    expect_equal(DBI::dbReadTable(conObs, tableName),
                 DBI::dbReadTable(conExp, tableName))}
})
