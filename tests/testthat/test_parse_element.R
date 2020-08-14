context('parse_element')

xmlDir = 'data/testfiles'
fileName = 'pubmed20n1016.xml.gz'
conNow = NULL
tableSuffix = NULL

rawXml = xml2::read_xml(file.path(xmlDir, fileName))

res = parsePmidStatus(rawXml, fileName, conNow, tableSuffix)

pmXml = res[[1L]]
dPmid = res[[2L]][status != 'Deleted', !'status']

foreach::registerDoSEQ()

test_that('parsePmidStatus', {

  pmidStatuses = res

  expectedOutput = readRDS(file.path('data/output', 'pmid_status.rds'))

  expect_equivalent(pmidStatuses, expectedOutput)

})

test_that('parseArticleId', {

  calcOutput = parseArticleId(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = readRDS(file.path('data/output', 'article_id.rds'))

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parsePubDate', {

  calcOutput = parsePubDate(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = readRDS(file.path('data/output', 'pub_date.rds'))

  expectedOutput[,pub_date := data.table::as.IDate(pub_date)]

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parseTitleJournal', {

  calcOutput = parseTitleJournal(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = readRDS(file.path('data/output', 'title_journal.rds'))

  expectedOutput[, title := stringr::str_replace_all(title,'""', '"')]

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parsePubType', {

  calcOutput = parsePubType(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = readRDS(file.path('data/output', 'pub_type.rds'))

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parseMesh', {

  calcOutput = parseMesh(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = readRDS(file.path('data/output', 'mesh.rds'))

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parseKeyword', {

  calcOutput = parseKeyword(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = readRDS(file.path('data/output', 'keyword.rds'))

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parseGrant', {

  calcOutput = parseGrant(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = readRDS(file.path('data/output', 'grant.rds'))

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parseChemical', {

  calcOutput = parseChemical(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = readRDS(file.path('data/output', 'chemical.rds'))

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parseDataBank', {

  calcOutput = parseDataBank(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = readRDS(file.path('data/output', 'data_bank.rds'))

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parseComment', {

  calcOutput = parseComment(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = readRDS(file.path('data/output', 'comment.rds'))

  expect_equivalent(calcOutput, expectedOutput)

})

test_that('parseAbstract', {

  calcOutput = parseAbstract(pmXml, dPmid, conNow, tableSuffix)

  expectedOutput = readRDS(file.path('data/output', 'abstract.rds'))

  expect_equivalent(calcOutput, expectedOutput)

})
