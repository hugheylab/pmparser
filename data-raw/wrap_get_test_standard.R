# before running this script, run "Clean and Rebuild"
foreach::registerDoSEQ() # to ensure reproducibility, do not parallelize

options(timeout = 3600)
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

localDir = file.path('tests', 'testthat', 'parsing_tables')
dbtype = 'sqlite'

tableSuffix = NULL
parTables = pmparser:::getParsingTables(tableSuffix)
saveRDS(parTables, file.path(localDir, 'get_parsing_tables_no_suffix.rds'))

pmparser:::createParsingTables(
  tableSuffix, overwrite = TRUE, dbtype = dbtype,
  dbname = file.path(localDir, 'create_parsing_tables_no_suffix.db'))

tableSuffix = 'dejavu'
parTables = pmparser:::getParsingTables(tableSuffix)
saveRDS(parTables, file.path(localDir, 'get_parsing_tables_with_suffix.rds'))

pmparser:::createParsingTables(
  tableSuffix, overwrite = TRUE, dbtype = dbtype,
  dbname = file.path(localDir, 'create_parsing_tables_with_suffix.db'))
