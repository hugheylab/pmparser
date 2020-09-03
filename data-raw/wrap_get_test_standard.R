foreach::registerDoSEQ() # to ensure reproducibility, do not parallelize

localDir = file.path('tests', 'testthat', 'pubmed_sample')
tmpDir = tempfile()

nPmidsPerStep = 2L
emptyXmlPath = file.path('data-raw', 'pubmed20n0000.xml')
offset = 1L
nCitations = 100L

set.seed(-1984)
pmparser:::getTestStandard(
  localDir, tmpDir, nPmidsPerStep, emptyXmlPath, offset, nCitations)

unlink(file.path(localDir, 'logs'), recursive = TRUE)
unlink(tmpDir, recursive = TRUE)

########################################

localDir = file.path('tests', 'testthat', 'empty_tables')
dbtype = 'sqlite'

tableSuffix = NULL
emptyTables = pmparser:::getEmptyTables(tableSuffix)
saveRDS(emptyTables, file.path(localDir, 'get_empty_tables_no_suffix.rds'))

pmparser:::writeEmptyTables(
  tableSuffix, overwrite = TRUE, dbtype = dbtype,
  dbname = file.path(localDir, 'write_empty_tables_no_suffix.db'))

tableSuffix = 'dejavu'
emptyTables = pmparser:::getEmptyTables(tableSuffix)
saveRDS(emptyTables, file.path(localDir, 'get_empty_tables_with_suffix.rds'))

pmparser:::writeEmptyTables(
  tableSuffix, overwrite = TRUE, dbtype = dbtype,
  dbname = file.path(localDir, 'write_empty_tables_with_suffix.db'))
