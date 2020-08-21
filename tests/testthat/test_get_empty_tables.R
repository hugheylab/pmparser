refDir = 'empty_tables'

test_that('getEmptyTables no tableSuffix', {
  emptyTablesObs = getEmptyTables(NULL)
  emptyTablesExp = readRDS(file.path(refDir, 'get_empty_tables_no_suffix.rds'))

  expect_equal(emptyTablesObs, emptyTablesExp)
})

test_that('getEmptyTables with tableSuffix', {
  emptyTablesObs = getEmptyTables('test')
  emptyTablesExp = readRDS(file.path(refDir, 'get_empty_tables_with_suffix.rds'))

  expect_equal(emptyTablesObs, emptyTablesExp)
})

test_that('writeEmptyTables with tableSuffix', {

  dbtype = 'sqlite'
  dbnameObs = 'write_empty_tables_no_suffix_obs.db'
  dbnameExp = file.path(refDir, 'write_empty_tables_no_suffix.db')

  withr::local_file(dbnameObs)

  writeEmptyTables(dbtype = dbtype, dbname = dbnameObs)

  conExp = withr::local_db_connection(connect(dbtype, dbnameExp))
  conObs = withr::local_db_connection(connect(dbtype, dbnameObs))

  expect_equal(DBI::dbListTables(conExp), DBI::dbListTables(conObs))
})

test_that('writeEmptyTables with tableSuffix', {

  dbtype = 'sqlite'
  dbnameObs = 'write_empty_tables_with_suffix_obs.db'
  dbnameExp = file.path(refDir, 'write_empty_tables_with_suffix.db')
  tableSuffix = 'test'

  withr::local_file(dbnameObs)

  writeEmptyTables(tableSuffix = tableSuffix, dbtype = dbtype, dbname = dbnameObs)

  conExp = withr::local_db_connection(connect(dbtype, dbnameExp))
  conObs = withr::local_db_connection(connect(dbtype, dbnameObs))

  expect_equal(DBI::dbListTables(conExp), DBI::dbListTables(conObs))
})
