context('pmparser')

test_that('getPubmedFileInfo', {
  localDir = NULL
  remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/'
  subDirs = c('baseline', 'updatefiles')
  tableSuffix = NULL
  con = NULL

  expect_true(all.equal(getPubmedFileInfo(localDir, remoteDir, subDirs, tableSuffix, con), fileInfoExpected, check.attributes = FALSE))

})

test_that('getPubmedFiles', {
  fileInfo = head(fread('file_info_expected.csv'), 5)
  localDir = 'data'
  remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/'
  checkMd5 = TRUE

  fileInfoExpected = fread('file_info_downloaded.c')

  expect_true(all.equal(getPubmedFiles(fileInfo, localDir, remoteDir, checkMd5), fileInfoExpected, check.attributes = FALSE))

})
