foreach::registerDoSEQ()

refDir = 'pubmed_sample'
subDir = 'updatefiles'
fileBase = 'pubmed20n1016'

parsedExp = readRDS(file.path(refDir, paste0(fileBase, '.rds')))
xmlPath = file.path(refDir, subDir, paste0(fileBase, '.xml.gz'))
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

test_that('parsePubDate', {
  parsedObs = parsePubDate(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$pub_date)
})

test_that('parseTitleJournal', {
  parsedObs = parseTitleJournal(pmXml, dPmid)
  expect_equal(parsedObs, parsedExp$title_journal)
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
