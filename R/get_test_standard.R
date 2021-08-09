parseAll = function(rawXmlPath) {
  status = parseFunc = NULL
  rawXml = xml2::read_xml(rawXmlPath)
  pmidStatus = parsePmidStatus(rawXml, 'sorry for the convenience')
  pmXml = pmidStatus[[1L]]
  dPmid = pmidStatus[[2L]][status != 'Deleted', !'status']

  parseFuncs = getParseFuncs()
  parseFuncs = parseFuncs[names(parseFuncs) != 'pmid_status']
  res = foreach(parseFunc = parseFuncs) %do% parseFunc(pmXml, dPmid)
  names(res) = names(parseFuncs)

  res = c(pmid_status = list(pmidStatus), res)
  res$deleted = xml_find_first(rawXml, './/DeleteCitation')
  res}


getSamplePmid = function(parsed, nPmidsPerStep) {
  .N = pmid = N = . = step = NULL
  d1 = parsed$pmid_status[[2L]][, .N, by = pmid][N > 1]
  d1 = d1[sample.int(min(.N, nPmidsPerStep)), .(pmid)]
  d1[, step := 'pmid_status']

  idx = names(parsed) != 'pmid_status'
  feo = foreach(res = parsed[idx], step = names(parsed)[idx], .combine = rbind)
  d2 = feo %do% {
    res = if (data.table::is.data.table(res)) res else res[[1L]]
    pmidsNow = unique(res$pmid)
    idx = sample.int(length(pmidsNow), min(length(pmidsNow), nPmidsPerStep))
    dNow = data.table(pmid = pmidsNow[idx])
    dNow[, step := step]}

  rbind(d1, d2)}


getSampleXml = function(parsed, dSample, emptyXmlPath) {
  status = pmid = NULL
  pmXml = parsed$pmid_status[[1L]]
  dPmid = parsed$pmid_status[[2L]][status != 'Deleted', !'status']
  rawXml = xml2::read_xml(emptyXmlPath)

  pmids = unique(dSample$pmid)
  idx = dPmid[pmid %in% pmids, which = TRUE]
  for (i in idx) xml2::xml_add_child(rawXml, pmXml[[i]])

  if (length(parsed$deleted) > 0) xml2::xml_add_child(rawXml, parsed$deleted)
  rawXml}


getTestStandardInput = function(localDir, tmpDir, offset) {
  sub_dir = .N = sample_base = NULL
  if (!dir.exists(localDir)) dir.create(localDir, recursive = TRUE)
  if (!dir.exists(tmpDir)) dir.create(tmpDir, recursive = TRUE)

  dFileRaw = getPubmedFileInfo()
  dFile = rbind(dFileRaw[sub_dir == 'baseline'][.N - offset],
                dFileRaw[sub_dir != 'baseline'][1L + offset])

  dFile = getPubmedFiles(dFile, tmpDir, downloadMd5 = FALSE)
  sampleBase = c(dFileRaw[sub_dir == 'baseline'][.N]$xml_filename,
                 dFileRaw[sub_dir != 'baseline'][1L]$xml_filename)
  dFile[, sample_base := gsub('\\.xml.*$', '', sampleBase)][]}


getTestStandardXml = function(
  localDir, tmpDir, dFile, nPmidsPerStep, emptyXmlPath) {

  f = NULL
  r = foreach(f = iterators::iter(dFile, by = 'row')) %do% {
    rawXmlPath = file.path(tmpDir, f$sub_dir, f$xml_filename)
    parsed = parseAll(rawXmlPath)
    dSample = getSamplePmid(
      parsed[names(parsed) != 'deleted'], nPmidsPerStep)
    sampleXml = getSampleXml(parsed, dSample, emptyXmlPath)

    nowDir = file.path(localDir, f$sub_dir)
    xmlPath = file.path(nowDir, glue('{f$sample_base}.xml.gz'))
    if (!dir.exists(nowDir)) dir.create(nowDir, recursive = TRUE)
    xml2::write_xml(sampleXml, xmlPath)

    tx = glue('MD5({f$sample_base}.xml.gz)= {tools::md5sum(xmlPath)}')
    writeLines(tx, glue('{xmlPath}.md5'))}

  invisible()}


getTestStandardParsed = function(localDir, dFile) {
  f = NULL
  r = foreach(f = iterators::iter(dFile, by = 'row')) %do% {
    xmlPath = file.path(localDir, f$sub_dir, glue('{f$sample_base}.xml.gz'))
    parsed = parseAll(xmlPath)
    parsed$pmid_status = parsed$pmid_status[[2L]]
    saveRDS(parsed[names(parsed) != 'deleted'],
            file.path(localDir, glue('{f$sample_base}.rds')))}
  invisible()}


getTestStandardIndex = function(dFile, offset) {
  sub_dir = NULL
  nBaseline = nrow(dFile[sub_dir == 'baseline'])
  idx = c(nBaseline - offset, nBaseline,
          nBaseline + 1L, nBaseline + offset + 1L)}


getTestStandardCsv = function(localDir, offset) {
  sub_dir = xml_download = xml_filename = md5_filename = NULL
  dFileAll = getPubmedFileInfo(localDir)
  data.table::fwrite(dFileAll, file.path(localDir, 'file_info_predown_all.csv'))

  data.table::fwrite(
    dFileAll[sub_dir == 'baseline'],
    file.path(localDir, 'file_info_predown_baseline.csv'))

  idx = getTestStandardIndex(dFileAll, offset)
  dFilePre = dFileAll[idx]

  dFilePost = getPubmedFiles(dFilePre, localDir, downloadMd5 = FALSE)
  data.table::fwrite(dFilePost, file.path(localDir, 'file_info_postdown.csv'),
                     logical01 = TRUE)

  dFileRemove = dFilePost[!dFilePre[xml_download == 0], on = 'xml_filename']
  unlink(dFileRemove[, file.path(localDir, sub_dir, xml_filename)])
  unlink(dFileRemove[, file.path(localDir, sub_dir, md5_filename)])
  invisible()}


getTestStandardCitation = function(localDir, tmpDir, nrows) {
  zipName = formals(getCitation)$filename
  zipPath = file.path(localDir, zipName)
  csvPath = glue('{tools::file_path_sans_ext(zipPath)}.csv')
  withr::local_file(csvPath)

  dCitation = getCitation(tmpDir, nrows = nrows)
  dTmp = data.table::fread(
    cmd = glue('unzip -p {file.path(tmpDir, zipName)} | head -n 2'))

  setnames(dCitation, colnames(dTmp))
  data.table::fwrite(dCitation, csvPath)
  if (file.exists(zipPath)) unlink(zipPath) # prevent appending to zip file
  utils::zip(zipPath, csvPath, flags = '-j9X') # ignore directory tree
  invisible()}


getTestStandardDb = function(localDir, dbnamePre, nFiles) {
  withr::local_envvar(c('TESTTHAT' = 'true'))
  dbCreate = file.path(localDir, glue('{dbnamePre}_create.db'))
  dbUpdate = file.path(localDir, glue('{dbnamePre}_update.db'))
  if (file.exists(dbCreate)) unlink(dbCreate)
  if (file.exists(dbUpdate)) unlink(dbUpdate)

  dbtype = 'sqlite'
  modifyPubmedDb(localDir = localDir, dbname = dbCreate, dbtype = dbtype,
                 nFiles = nFiles, mode = 'create')
  file.copy(dbCreate, dbUpdate, overwrite = TRUE)
  modifyPubmedDb(localDir = localDir, dbname = dbUpdate, dbtype = dbtype,
                 nFiles = nFiles, mode = 'update')
  invisible()}


getTestStandard = function(
  localDir, tmpDir, nPmidsPerStep, emptyXmlPath, offset, nCitations) {
  dFile = getTestStandardInput(localDir, tmpDir, offset)
  getTestStandardXml(localDir, tmpDir, dFile, nPmidsPerStep, emptyXmlPath)
  getTestStandardParsed(localDir, dFile)
  getTestStandardCsv(localDir, offset)
  getTestStandardCitation(localDir, tmpDir, nCitations)
  getTestStandardDb(localDir, 'pmdb_sample', 1L)
  invisible()}
