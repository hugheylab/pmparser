filename = 'pubmed_no_rows.xml'
rawXml = xml2::read_xml(filename)

pmidStatusList = parsePmidStatus(rawXml, filename)
pmXml = pmidStatusList[[1L]]
dPmidRaw = pmidStatusList[[2L]]
dPmid = dPmidRaw[status != 'Deleted', !'status']

parseFuncs = getParseFuncs()
parseFuncs = parseFuncs[names(parseFuncs) != 'pmid_status']

test_that('parseElement returns no rows', {
  for (i in seq_len(length(parseFuncs))) {
    res = parseFuncs[[i]](pmXml, dPmid)
    if (data.table::is.data.table(res)) {
      expect_equal(nrow(res), 0L, label = names(parseFuncs)[i])
    } else {
      for (j in seq_len(length(res))) {
        expect_equal(nrow(res[[j]]), 0L,
                     label = paste(names(parseFuncs)[i], names(res)[j]))}}}
})
