ns = readLines(system.file('NAMESPACE', package = 'pmparser'))
nExportedFuncs = sum(startsWith(ns, 'export(parse'))

test_that('getParseFuncs all', {
  nReturnedFuncs = length(getParseFuncs())
  expect_equal(nExportedFuncs, nReturnedFuncs)
})

test_that('getParseFuncs all select', {
  steps = c('all', 'mesh', 'author')
  parseFuncs = getParseFuncs(steps)
  nReturnedFuncs = length(parseFuncs)
  expect_equal(nExportedFuncs, nReturnedFuncs)
})

test_that('getParseFuncs select', {
  steps = c('mesh', 'author')
  parseFuncs = getParseFuncs(steps)
  nReturnedFuncs = length(parseFuncs)
  expect_equal(length(steps), nReturnedFuncs)
  expect_equal(steps, names(parseFuncs))
})
