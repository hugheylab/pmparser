foreach::registerDoSEQ()

refDir = 'pubmed_sample'
subDir = 'updatefiles'
fileBase = 'pubmed23n1167' # updatefiles

parsedExp = readRDS(file.path(refDir, glue('{fileBase}.rds')))
xmlPath = file.path(refDir, subDir, glue('{fileBase}.xml.gz'))
rawXml = xml2::read_xml(xmlPath)

pmidStatus = parsePmidStatus(rawXml, 'sorry for the convenience')
pmXml = pmidStatus[[1L]]
dPmid = pmidStatus[[2L]][status != 'Deleted', !'status']

test_that('parsePmidStatus', {
  expect_equal(pmidStatus[[2L]], parsedExp$pmid_status)
})

test_that('parseArticleId', {
  parsedObs = parseArticleId(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$article_id)
})

test_that('parseArticle', {
  parsedObs = parseArticle(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$article)
})

test_that('parsePubHistory', {
  parsedObs = parsePubHistory(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$pub_history)
})

test_that('parseJournal', {
  parsedObs = parseJournal(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$journal)
})

test_that('parsePubType', {
  parsedObs = parsePubType(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$pub_type)
})

test_that('parseMesh', {
  parsedObs = parseMesh(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$mesh)
})

test_that('parseKeyword', {
  parsedObs = parseKeyword(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$keyword)
})

test_that('parseGrant', {
  parsedObs = parseGrant(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$grant)
})

test_that('parseChemical', {
  parsedObs = parseChemical(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$chemical)
})

test_that('parseDataBank', {
  parsedObs = parseDataBank(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$data_bank)
})

test_that('parseComment', {
  parsedObs = parseComment(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$comment)
})

test_that('parseAbstract', {
  parsedObs = parseAbstract(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$abstract)
})

test_that('parseAuthor', {
  parsedObs = parseAuthor(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$author)
})

test_that('parseInvestigator', {
  parsedObs = parseInvestigator(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$investigator)
})

test_that('parseOther', {
  parsedObs = parseOther(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$other)
})
