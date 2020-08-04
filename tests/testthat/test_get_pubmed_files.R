context('pmparser')

test_that('getPubmedFileInfo', {
  localDir = NULL
  remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/'
  subDirs = c('baseline', 'updatefiles')
  tableSuffix = NULL
  con = NULL

  fileInfoExpected = fread('file_info_expected.csv')[sub_dir == 'baseline']
  fileInfo = getPubmedFileInfo(localDir, remoteDir, subDirs, tableSuffix, con)[sub_dir == 'baseline']

  expect_equal(fileInfo, fileInfoExpected)

})

test_that('getPubmedFiles', {
  fileInfo = head(fread('file_info_expected.csv'), 5)
  localDir = 'data'
  remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/'
  checkMd5 = TRUE

  if(!dir.exists('data')){
    dir.create('data')}

  fileInfoExpected = fread('file_info_downloaded.csv')
  fileInfo = getPubmedFiles(fileInfo, localDir, remoteDir, checkMd5)

  expect_equal(fileInfo, fileInfoExpected)

})
