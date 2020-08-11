context('get_pubmed_files')

test_that('getPubmedFileInfo', {
  localDir = NULL
  remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/'
  subDirs = c('baseline', 'updatefiles')
  tableSuffix = NULL
  con = NULL

  fileInfoExpected = data.table::fread('file_info_expected.csv')
  fileInfoExpected = fileInfoExpected[sub_dir == 'baseline']
  fileInfoExpected[, `:=`(xml_download = as.numeric(xml_download),
                          processed = as.numeric(processed))]

  fileInfo = getPubmedFileInfo(localDir, remoteDir, subDirs, tableSuffix, con)
  fileInfo = fileInfo[sub_dir == 'baseline']

  expect_equivalent(fileInfo, fileInfoExpected)
})

test_that('getPubmedFiles', {
  fileInfoOrig = data.table::fread('file_info_expected.csv')[1:2]
  remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/'
  localDir = 'dataTest'
  checkMd5 = TRUE

  if (!dir.exists(localDir)) dir.create(localDir)

  fileInfoExpected = data.table::fread('file_info_downloaded.csv')

  foreach::registerDoSEQ()
  fileInfo = getPubmedFiles(fileInfoOrig, localDir, remoteDir, checkMd5)
  fileInfo[, md5_match := as.numeric(md5_match)]

  unlink(localDir, recursive = TRUE)
  expect_equivalent(fileInfo, fileInfoExpected)
})
