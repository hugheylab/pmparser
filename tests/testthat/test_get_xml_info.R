xmlDir = file.path('pubmed_sample', 'baseline')
xmlFiles = list.files(xmlDir, 'xml\\.gz$')

test_that('getXmlInfo NULL', {
  xmlInfo = getXmlInfo(xmlDir, NULL)
  xmlInfoExp = data.table(xml_filename = xmlFiles, step = 'all')
  expect_equal(xmlInfo, xmlInfoExp)
})

test_that('getXmlInfo char ok', {
  xmlInfo = getXmlInfo(xmlDir, xmlFiles)
  xmlInfoExp = data.table(xml_filename = xmlFiles, step = 'all')
  expect_equal(xmlInfo, xmlInfoExp)
})

test_that('getXmlInfo char not ok', {
  expect_error(getXmlInfo(xmlDir, c(xmlFiles, 'poison.xml.gz')))
})

test_that('getXmlInfo num', {
  expect_error(getXmlInfo(xmlDir, c(1, 1, 2, 3, 5)))
})

test_that('getXmlInfo data.table ok', {
  xmlInfoExp = data.table(xml_filename = xmlFiles, step = c('mesh', 'author'))
  xmlInfo = getXmlInfo(xmlDir, xmlInfoExp, tableSuffix = 'blackwater')
  expect_equal(xmlInfo, xmlInfoExp)
})

test_that('getXmlInfo data.table not ok empty', {
  xmlInfoExp = data.table(xml_filename = xmlFiles, step = c('mesh', 'author'))
  expect_error(getXmlInfo(xmlDir, xmlInfoExp, tableSuffix = NULL))
})

test_that('getXmlInfo data.table not ok cols', {
  xmlInfoExp = data.table(filename = xmlFiles, step = c('mesh', 'author'))
  expect_error(getXmlInfo(xmlDir, xmlInfoExp, tableSuffix = 'blackwater'))
})
