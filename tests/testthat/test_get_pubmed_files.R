context('pubmedparser')

test_that('getPubmedFileInfo', {
  localDir = NULL
  remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/'
  subDirs = c('baseline', 'updatefiles')
  tableSuffix = NULL
  con = NULL

  fileInfo = getPubmedFileInfo(localDir, remoteDir, subDirs, tableSuffix, con)
})
