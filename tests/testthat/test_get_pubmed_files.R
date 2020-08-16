foreach::registerDoSEQ()

test_that('getPubmedFileInfo', {
  localDir = 'pubmed_ref'
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

  expect_equal(fileInfo, fileInfoExpected, check.attributes = FALSE)
})

test_that('getPubmedFiles', {
  refDir = 'pubmed_ref'
  fileInfoOrig = data.table::fread('file_info_expected.csv')[1:2]
  fileInfoExpected = data.table::fread('file_info_downloaded.csv')

  localDir = 'pubmed'
  remoteDir = 'ftp://ftp.ncbi.nlm.nih.gov/pubmed/'
  downloadMd5 = FALSE

  if (!dir.exists(localDir)) dir.create(localDir)
  file.copy(list.files(refDir, include.dirs = TRUE, full.names = TRUE),
            localDir, recursive = TRUE, copy.date = TRUE)

  fileInfo = getPubmedFiles(
    fileInfoOrig, localDir, remoteDir, downloadMd5)
  fileInfo[, md5_match := as.numeric(md5_match)]

  unlink(localDir, recursive = TRUE)
  expect_equal(fileInfo, fileInfoExpected, check.attributes = FALSE)
})
