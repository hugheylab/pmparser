context('parse_element')

xmlDir = 'data/testfiles'
fileName = 'pubmed20n1016_test.xml'
conNow = NULL
tableSuffix = NULL

rawXml = xml2::read_xml(file.path(xmlDir, fileName))

res = parsePmidStatus(rawXml, fileName, conNow, tableSuffix)

pmXml = res[[1L]]
dPmid = res[[2L]][status != 'Deleted', !'status']

foreach::registerDoSEQ()

test_that('parsePmidStatus', {

  pmidStatuses = res

  expectedOutput = data.table::fread('pmid_status_output.csv')

  expect_equivalent(pmidStatuses[[2]], expectedOutput)

})

test_that('parseArticleId', {

  calcOutput = parseArticleId(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = data.table::fread('article_id_output.csv')

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parsePubDate', {

  calcOutput = parsePubDate(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = data.table::fread('pub_date_output.csv')

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parseTitleJournal', {

  calcOutput = parseTitleJournal(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = data.table::fread('title_journal_output.csv')

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parsePubType', {

  calcOutput = parsePubType(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = data.table::fread('pub_type_output.csv')

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parseMesh', {

  calcOutput = parseMesh(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = data.table::fread('mesh_output.csv')

  # Commented out since current example doesn't have mesh terms
  # expect_equivalent(calcOutput, expectedOutput)

})

test_that('parseKeyword', {

  calcOutput = parseKeyword(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = data.table::fread('keyword_output.csv')

  expect_equivalent(calcOutput[[1]], expectedOutput)

})
