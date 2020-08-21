foreach::registerDoSEQ()

refDir = 'citation_info'

test_that('getCitationInfo', {
  filename = 'citation_info.rds'

  citationInfoObs = getCitationInfo()
  citationInfoExp = readRDS(file.path(refDir, filename))

  expect_equal(citationInfoExp, citationInfoObs, check.attributes = FALSE)
})

test_that('getCitation', {
  filename = 'open_citation_collection.zip'
  filenameExp = 'citation_info_collection.rds'

  dCitationObs = getCitation(localDir = refDir, filename = filename, nrows = 50, checkMd5 = FALSE)
  dCitationExp = readRDS(file.path(refDir, filenameExp))

  expect_equal(dCitationExp, dCitationObs, check.attributes = FALSE)
})
