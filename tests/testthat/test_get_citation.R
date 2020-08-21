refDir = 'citation_info'

test_that('getCitationInfo', {
  filenameCols = 'citation_info_columns.rds'

  citationInfoObs = getCitationInfo()
  citationInfoColsExp = readRDS(file.path(refDir, filenameCols))

  expect_s3_class(citationInfoObs, 'data.table')
  expect_equal(citationInfoColsExp, colnames(citationInfoObs))
  expect_equal(1, nrow(citationInfoObs))
})

test_that('getCitation', {
  filename = 'open_citation_collection.zip'
  filenameExp = 'citation_info_collection.rds'

  dCitationObs = getCitation(localDir = refDir, filename = filename, nrows = 50, checkMd5 = FALSE)
  dCitationExp = readRDS(file.path(refDir, filenameExp))

  expect_equal(dCitationExp, dCitationObs, check.attributes = FALSE)
})
