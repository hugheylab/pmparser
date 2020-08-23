localDir = file.path('tests', 'testthat', 'pubmed_sample')
tmpDir = tempfile() # file.path(parentDir, 'pubmed_tmp')

nPmidsPerStep = 2L
emptyXmlPath = file.path('data-raw', 'pubmed20n0000.xml')
offset = 1L
nCitations = 100L

set.seed(-1984)
pmparser:::getTestStandard(
  localDir, tmpDir, nPmidsPerStep, emptyXmlPath, offset, nCitations)

# unlink(file.path(localDir, 'logs'), recursive = TRUE)
# unlink(tmpDir, recursive = TRUE)
