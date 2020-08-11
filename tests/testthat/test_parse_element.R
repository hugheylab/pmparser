context('parse_element')

xmlDir = 'data/testfiles'
fileName = 'pubmed20n1016_test.xml'
conNow = NULL
tableSuffix = NULL

rawXml = xml2::read_xml(file.path(xmlDir, fileName))

test_that('parsePmidStatus', {

  foreach::registerDoSEQ()

  pmidStatuses = parsePmidStatus(rawXml, fileName, conNow, tableSuffix)

  pmidStatusesExpected = data.table::fread('pmid_status_output.csv')

  expect_equivalent(pmidStatuses[[2]], pmidStatusesExpected)

})
