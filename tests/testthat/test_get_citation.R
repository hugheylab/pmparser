refDir = 'citation_info'

test_that('getCitationInfo', {
  citationInfo = getCitationInfo()
  colsExp = c('is_link_only', 'name', 'supplied_md5',
              'computed_md5', 'id', 'download_url', 'size')
  expect_s3_class(citationInfo, 'data.table')
  expect_equal(colnames(citationInfo), colsExp)
  expect_equal(nrow(citationInfo), 1L)
})

test_that('getCitation', {
  filename = 'open_citation_collection.zip'
  filenameExp = 'citation_info_collection.rds'
  dCitationObs = getCitation(
    localDir = refDir, filename = filename, nrows = 50L, checkMd5 = FALSE)
  dCitationExp = readRDS(file.path(refDir, filenameExp))
  expect_equal(dCitationObs, dCitationExp)
})
