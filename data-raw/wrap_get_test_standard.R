parentDir = 'data-raw'
localDir = file.path(parentDir, 'pubmed_sample')
tmpDir = file.path(parentDir, 'pubmed_tmp')

nPmidsPerStep = 2L
emptyXmlPath = file.path(parentDir, 'pubmed20n0000.xml')
offset = 1L
nCitations = 100L

set.seed(-1984)
pmparser:::getTestStandard(
  localDir, tmpDir, nPmidsPerStep, emptyXmlPath, offset, nCitations)

# unlink(file.path(localDir, 'logs'), recursive = TRUE)
# unlink(tmpDir, recursive = TRUE)
