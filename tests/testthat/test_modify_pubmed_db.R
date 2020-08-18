context('modify_pubmed_db')

localDir = 'data/testfiles/updatefiles'
dbname = 'modify_pubmed_db_test_db.db'
dbtype = 'sqlite'
nFiles = 1
retry = TRUE
nCitations = 0

foreach::registerDoSEQ()

test_that('modifyPubmedDb create', {

  # modifyPubmedDb(localDir = localDir, dbname = dbname, dbtype = dbtype, nFiles = nFiles, retry = retry, nCitations = nCitations, mode = 'create')

})

test_that('modifyPubmedDb update', {

  # modifyPubmedDb(localDir = localDir, dbname = dbname, dbtype = dbtype, nFiles = nFiles, retry = retry, nCitations = nCitations, mode = 'update')

})
