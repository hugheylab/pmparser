#' Create or update a PubMed database
#'
#' This function downloads PubMed/MEDLINE XML files, parses them, and adds the
#' information to the database, then downloads the NIH Open Citation Collection
#' and adds it to the database. Only the most recent version of each PMID is
#' retained. Parsing of XML files will use a parallel backend if one is
#' registered, such as with [doParallel::registerDoParallel()].
#'
#' @param localDir Directory in which to download the files from PubMed.
#' @param dbname Name of database.
#' @param dbtype Type of database, either 'postgres', 'mariadb', 'mysql', or
#'   'sqlite'. Make sure to install the corresponding DBI driver package first:
#'   RPostgres, RMariaDB (for both 'mariadb' and 'mysql'), or RSQLite. Due to
#'   the large size of the database, SQLite is recommended only for small-scale
#'   testing.
#' @param nFiles Maximum number of xml files to parse that are not already in
#'   the database. This should not normally be changed from the default.
#' @param retry Logical indicating whether to retry parsing steps that fail.
#' @param nCitations Maximum number of rows of the citation file to read. This
#'   should not normally be changed from the default.
#' @param mode String indicating whether to create the database using the
#'   baseline files or to update the database using the update files.
#' @param ... Other arguments passed to [DBI::dbConnect()].
#'
#' @return `NULL`, invisibly. Tab-delimited log files will be created in a logs
#'   folder in `localDir`.
#'
#' @examples
#' \dontrun{
#' modifyPubmedDb('.', 'pmdb', mode = 'create')
#' }
#'
#' @seealso [parsePmidStatus()], [getCitation()], [getPgParams()]
#'
#' @export
modifyPubmedDb = function(
  localDir, dbname, dbtype = c('postgres', 'mariadb', 'mysql', 'sqlite'),
  nFiles = Inf, retry = TRUE, nCitations = Inf, mode = c('create', 'update'),
  ...) {

  processed = .N = md5_match = xml_filename = . = step = NULL
  testing = isTesting()
  con = connect(dbtype, dbname, ...)

  # not using rng, so unclear why future gives warnings
  optVal = getOption( 'future.rng.onMisuse')
  options(future.rng.onMisuse = 'ignore')

  mode = match.arg(mode)
  if (mode == 'create') {
    subDir = 'baseline'
    tableSuffix = ''
    conTmp = NULL
  } else {
    subDir = 'updatefiles'
    tableSuffix = 'update'
    conTmp = con}

  logDir = file.path(localDir, 'logs')
  if (!dir.exists(logDir)) dir.create(logDir)

  f = glue('{mode}_db_{format(Sys.time(), "%Y%m%d_%H%M%S")}.log')
  logPath = file.path(logDir, f)
  writeLogFile(logPath, data.table(step = 'start'), append = FALSE)

  if (mode == 'create') r = getReadme(con = con)

  # download files
  writeLogFile(logPath, data.table('get pubmed file info'))
  fileInfo = getPubmedFileInfo(localDir, subDirs = subDir, con = conTmp)
  fileInfo = fileInfo[is.na(processed)]

  if (nrow(fileInfo) == 0) {
    message('Database is already up-to-date.')
    return(invisible())}

  if (mode == 'create') {
    fileInfo = fileInfo[max(1, min(.N, .N - nFiles + 1)):.N] # take the last
  } else {
    fileInfo = fileInfo[1:max(1, min(.N, nFiles))]} # take the earliest

  if (nrow(fileInfo) > 1 && !foreach::getDoParRegistered()) {
    message('No parallel backend is registered. This could take a while.')}

  writeLogFile(logPath, data.table('get pubmed files'))
  fileInfo = getPubmedFiles(fileInfo, localDir, downloadMd5 = !testing)
  fileInfoKeep = fileInfo[(md5_match)]

  if (nrow(fileInfoKeep) != nrow(fileInfo)) {
    files = paste(fileInfo[!(md5_match), xml_filename], collapse = '\n')
    w = sprintf(paste('The following xml files did not match their md5 sums',
                      'and will not be processed:\n%s'), files)
    warning(w, .immediate = TRUE)}

  if (nrow(fileInfoKeep) == 0) {
    message('Nothing to do. Have a splendid day.')
    return(invisible())}

  # process files
  logName1 = glue('parse_{subDir}_{format(Sys.time(), "%Y%m%d_%H%M%S")}.log')

  writeLogFile(logPath, data.table('parse xml files'))
  parsePubmedXml(
    xmlDir = file.path(localDir, subDir), xmlFiles = fileInfoKeep$xml_filename,
    logPath = file.path(logDir, logName1), tableSuffix = tableSuffix,
    overwrite = TRUE, dbtype = dbtype, dbname = dbname, ...)

  dFailed = getFailed(file.path(logDir, logName1))
  dMissing = getMissing(con, tableSuffix, fileInfoKeep)
  dRetry = rbind(dFailed[, .(xml_filename, step)], dMissing[, step := 'all'])

  if (isTRUE(retry) && nrow(dRetry) > 0) {

    logName2 = glue('parse_{subDir}_{format(Sys.time(), "%Y%m%d_%H%M%S")}.log')
    retrySuffix = paste_(tableSuffix, 'retry')

    # retry failed steps
    writeLogFile(logPath, data.table('retry parsing xml files'))
    parsePubmedXml(
      xmlDir = file.path(localDir, subDir), xmlFiles = dRetry,
      logPath = file.path(logDir, logName2), tableSuffix = retrySuffix,
      overwrite = TRUE, dbtype = dbtype, dbname = dbname, ...)

    # add retry tables to first try tables
    writeLogFile(logPath, data.table('add second try to first try'))
    addSourceToTarget(
      sourceSuffix = retrySuffix, targetSuffix = tableSuffix, dryRun = FALSE,
      dbtype = dbtype, dbname = dbname, ...)}

  if (mode == 'create') {
    writeLogFile(logPath, data.table('drop unneeded pmid versions'))
    deleteOldPmidVersions(
      tableSuffix = tableSuffix, dryRun = FALSE, dbtype = dbtype,
      dbname = dbname, ...)
    dropPmidVersionColumn(tableSuffix = tableSuffix, con = con)
  } else {
    writeLogFile(logPath, data.table('add updates to main tables'))
    addSourceToTarget(
      sourceSuffix = tableSuffix, targetSuffix = '', dryRun = FALSE,
      dbtype = dbtype, dbname = dbname, ...)}

  if (nCitations > 0) {
    writeLogFile(logPath, data.table('get citation table'))
    r = getCitation(
      localDir = localDir, nrows = nCitations, tableSuffix = '',
      overwrite = TRUE, con = con, checkMd5 = !testing)}

  dd = data.table::fread(
    system.file('extdata', 'data_dictionary.csv', package = 'pmparser'))
  DBI::dbWriteTable(con, 'data_dictionary', dd, overwrite = TRUE)
  writeLogFile(logPath, data.table('add data dictionary'))

  dMissing = getMissing(con, '', fileInfoKeep)
  if (nrow(dMissing) > 0) {
    path = glue('{tools::file_path_sans_ext(logPath)}_missing.csv')
    data.table::fwrite(dMissing, path)
    writeLogFile(logPath, data.table(glue('finish: see {basename(path)}')))
  } else {
    writeLogFile(logPath, data.table('finish: good to go'))}

  options(future.rng.onMisuse = optVal)
  disconnect(con)
  invisible()}


addSourceToTarget = function(
  sourceSuffix, targetSuffix, dryRun, dbtype, dbname, ...) {

  targetName = sourceName = NULL
  stopifnot(!isEmpty(sourceSuffix))

  targetEmpty = getParsingTables(targetSuffix)
  sourceEmpty = getParsingTables(sourceSuffix)

  # create source table of pmid, xml_filename to keep
  con = connect(dbtype, dbname, ...)
  sourceKeep = glue('pmid_status_{sourceSuffix}_keep')
  if (DBI::dbExistsTable(con, sourceKeep)) {
    DBI::dbRemoveTable(con, sourceKeep)}

  # window functions are for wizards
  q = glue(
    'create table {sourceKeep} as with ranked_pmid_status as
    (select *, row_number() over
    (partition by pmid order by version desc, xml_filename desc) as rn
    from pmid_status_{sourceSuffix})
    select pmid, version, xml_filename
    from ranked_pmid_status where rn = 1') # glue_sql doesn't work for these
  n = DBI::dbExecute(con, q)

  deleteStart = c('delete', 'select count(*)')
  insertBase = c('insert into %s select %s', 'select count(*)')

  # special treatment for xml_processed tables
  targetNow = names(targetEmpty)[startsWith(names(targetEmpty), 'xml_processed')]
  sourceNow = names(sourceEmpty)[startsWith(names(sourceEmpty), 'xml_processed')]

  q = glue('{deleteStart[1 + dryRun]} from {targetNow} where
           xml_filename in (select xml_filename from {sourceNow})')
  nDelete = runStatement(con, q)

  insertStart = if (isTRUE(dryRun)) insertBase[2L] else
    sprintf(insertBase[1L], targetNow,
            paste0(colnames(targetEmpty[[targetNow]]), collapse = ', '))

  q = glue('{insertStart} from {sourceNow}')
  nInsert = runStatement(con, q)
  disconnect(con)

  d1 = data.table(source_name = sourceNow, target_name = targetNow,
                  nrows_delete = nDelete, nrows_insert = nInsert)

  # loop over remaining tables
  feo = foreach(targetName = setdiff(names(targetEmpty), targetNow),
                sourceName = setdiff(names(sourceEmpty), sourceNow),
                .combine = rbind, .options.future = list(scheduling = Inf))

  doOp = getDoOp(dbtype)
  d2 = doOp(feo, {
    con = connect(dbtype, dbname, ...) # required if in dopar
    deleteStart; sourceKeep; # so glue works in dopar

    # drop rows in target tables, use subquery to conform to sql standard
    q = glue('{deleteStart[1L + dryRun]} from {targetName}
             where pmid in (select pmid from {sourceName})')
    nDelete = runStatement(con, q)

    # append source rows to target tables
    insertStart = if (isTRUE(dryRun)) insertBase[2L] else
      sprintf(insertBase[1L], targetName,
              paste0('a.', DBI::dbListFields(con, targetName), collapse = ', '))

    q = glue('{insertStart} from {sourceName} as a inner join {sourceKeep} as b
             on a.pmid = b.pmid and a.version = b.version
             and a.xml_filename = b.xml_filename')
    nInsert = runStatement(con, q)
    disconnect(con)

    dNow = data.table(source_name = sourceName, target_name = targetName,
                      nrows_delete = nDelete, nrows_insert = nInsert)})

  con = connect(dbtype, dbname, ...)
  DBI::dbRemoveTable(con, sourceKeep)
  if (isFALSE(dryRun)) { # with great power comes great responsibility
    for (sourceName in names(sourceEmpty))
      DBI::dbRemoveTable(con, sourceName)}

  disconnect(con)
  d = rbind(d1, d2)
  setattr(d, 'dryRun', dryRun)
  return(d)}
