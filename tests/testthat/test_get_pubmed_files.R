foreach::registerDoSEQ()

refDir = 'pubmed_sample'
localDir = tempfile()

test_that('getPubmedFileInfo', {
  fileInfoExp = data.table::fread(
    file.path(refDir, 'file_info_predown_baseline.csv'))
  fileInfoExp = fileInfoExp[sub_dir == 'baseline']
  fileInfoExp[, `:=`(xml_download = as.numeric(xml_download),
                     processed = as.numeric(processed))]

  fileInfo = getPubmedFileInfo(refDir)
  fileInfo = fileInfo[sub_dir == 'baseline']
  expect_equal(fileInfo, fileInfoExp, check.attributes = FALSE)
})

test_that('getPubmedFiles', {
  withr::local_file(localDir)
  if (dir.exists(localDir)) unlink(localDir, recursive = TRUE)
  dir.create(localDir)
  x = file.copy(list.files(refDir, include.dirs = TRUE, full.names = TRUE),
                localDir, recursive = TRUE, copy.date = TRUE)

  fileInfoExp = data.table::fread(file.path(refDir, 'file_info_postdown.csv'))
  dFileAll = data.table::fread(file.path(refDir, 'file_info_predown_all.csv'))
  idx = getTestStandardIndex(dFileAll, offset = 1L)
  dFilePre = dFileAll[idx]

  fileInfo = getPubmedFiles(dFilePre, localDir, downloadMd5 = FALSE)
  fileInfo[, md5_match := as.numeric(md5_match)]
  expect_equal(fileInfo, fileInfoExp, check.attributes = FALSE)
})
