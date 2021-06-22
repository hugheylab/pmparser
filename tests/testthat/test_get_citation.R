refDir = 'citation_info'

test_that('getCitationInfo', {
  citationInfo = getCitationInfo()
  colsExp = c('id', 'name', 'size', 'is_link_only', 'download_url',
              'supplied_md5', 'computed_md5')
  expect_s3_class(citationInfo, 'data.table')
  expect_equal(colnames(citationInfo), colsExp)
  expect_equal(nrow(citationInfo), 1L)
})

test_that('getCitation', {
  refDir = 'pubmed_sample'
  filename = 'open_citation_collection.zip'
  nrows = 50L

  dCitationExp = data.table::fread(
    cmd = glue('unzip -p {file.path(refDir, filename)}'), nrows = nrows)
  setnames(dCitationExp, c('citing_pmid', 'cited_pmid'))

  dCitationObs = getCitation(
    localDir = refDir, filename = filename, nrows = nrows, checkMd5 = FALSE)
  expect_equal(dCitationObs, dCitationExp)
})
