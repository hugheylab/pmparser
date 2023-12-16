logPath = 'cutter.log'
withr::local_file(logPath)

dStart = data.table(xml_filename = 'all', step = 'start', status = 0)
writeLogFile(logPath, dStart, append = FALSE)
writeLogFile(logPath, data.table('freedom.xml.gz', 'grant', 1))

test_that('writeLogFile', {
  dLog = data.table::fread(logPath, sep = '\t', na.strings = '')
  expect_equal(colnames(dLog), c('datetime', colnames(dStart)))
  expect_equal(nrow(dLog), 2L)
  expect_null(writeLogFile(NULL, 'qwfp'))
})

test_that('getFailed', {
  dFailed = getFailed(logPath)
  expect_equal(colnames(dFailed), c('datetime', colnames(dStart)))
  expect_equal(nrow(dFailed), 1L)
})

test_that('getMissing', {
  refDir = 'pubmed_sample'
  dbname = 'pmdb_sample_update.db'
  dbtype = 'sqlite'
  con = connect(dbtype, file.path(refDir, dbname))
  dFile = data.table(xml_filename = c(
      dir(file.path(refDir, 'updatefiles'), '\\.gz$'), 'freedom.xml.gz'))
  dFailed = getMissing(con = con, tableSuffix = NULL, dFile = dFile)
  expect_equal(dFailed, dFile[.N])
})

test_that('getPgParams', {
  pg = getPgParams('pgpass')
  expect_s3_class(pg, 'data.table')
  expect_equal(nrow(pg), 2L)
})
