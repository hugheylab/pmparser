context('modify_pubmed_db')

test_that('modifyPubmedDb', {
  localDir = 'data'

  foreach::registerDoSEQ()

  expect_visible(modifyPubmedDb(localDir, 'modify_pubmed_db_testing.db', 'sqlite', nCitations = 0, mode = 'create'))
  expect_visible(modifyPubmedDb(localDir, 'modify_pubmed_db_testing.db', 'sqlite', nFiles = 1, nCitations = 0, mode = 'update'))
})
