saveParseFuncsOutput = function(steps = 'all', fileName, xmlDir = NULL, saveDir = NULL, fileSuffix = NULL, save = TRUE) {
  parseFuncs = getParseFuncs(steps)

  con = NULL
  tableSuffix = NULL

  rawXml = xml2::read_xml(file.path(xmlDir, fileName))

  step = 'pmid_status'
  res = parsePmidStatus(rawXml, fileName, con, tableSuffix)
  if(save) saveRDS(res, file.path(saveDir, paste0(step, fileSuffix, '.rds')))

  pmXml = res[[1L]]
  dPmid = res[[2L]][status != 'Deleted', !'status']

  idx = !(names(parseFuncs) %in% step)

  r = foreach(parseFunc = parseFuncs[idx], step = names(parseFuncs)[idx]) %do% {
    res = tryCatch({parseFunc(pmXml, dPmid, con, tableSuffix)},
                   error = function(e) e)
    err = 'error' %in% class(res)
    msg = if (err) trimws(as.character(res)) else NA_character_
    if(save) saveRDS(res, file.path(saveDir, paste0(step, fileSuffix, '.rds')))
    res}

  return(r)

}
