refDir = 'parsing_tables'
tableSuffix = 'dejavu'

test_that('getParsingTables with no tableSuffix', {
  parTablesObs = getParsingTables(NULL)
  parTablesExp = readRDS(file.path(refDir, 'get_parsing_tables_no_suffix.rds'))

  expect_equal(parTablesObs, parTablesExp)
})

test_that('getParsingTables with tableSuffix', {
  parTablesObs = getParsingTables(tableSuffix)
  parTablesExp = readRDS(
    file.path(refDir, 'get_parsing_tables_with_suffix.rds'))

  expect_equal(parTablesObs, parTablesExp)
})

test_that('createParsingTables with no tableSuffix', {

  dbtype = 'sqlite'
  dbnameObs = 'create_parsing_tables_no_suffix_obs.db'
  dbnameExp = file.path(refDir, 'create_parsing_tables_no_suffix.db')

  withr::local_file(dbnameObs)

  createParsingTables(dbtype = dbtype, dbname = dbnameObs)

  conExp = withr::local_db_connection(connect(dbtype, dbnameExp))
  conObs = withr::local_db_connection(connect(dbtype, dbnameObs))

  expect_equal(DBI::dbListTables(conExp), DBI::dbListTables(conObs))
})

test_that('createParsingTables with tableSuffix', {

  dbtype = 'sqlite'
  dbnameObs = 'create_parsing_tables_with_suffix_obs.db'
  dbnameExp = file.path(refDir, 'create_parsing_tables_with_suffix.db')

  withr::local_file(dbnameObs)

  createParsingTables(
    tableSuffix = tableSuffix, dbtype = dbtype, dbname = dbnameObs)

  conExp = withr::local_db_connection(connect(dbtype, dbnameExp))
  conObs = withr::local_db_connection(connect(dbtype, dbnameObs))

  expect_equal(DBI::dbListTables(conExp), DBI::dbListTables(conObs))
})
