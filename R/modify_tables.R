#' @export
modifyTables = function(localDir, dbname, nFiles = Inf, retry = TRUE,
                        nCitations = Inf, mode = c('create', 'update'), ...) {

  mode = match.arg(mode)
  if (mode == 'create') { # run on Jan 1
    subDir = 'baseline'
    tableSuffix = ''
    dbnameTmp = NULL
  } else { # run on 10th day of each month
    subDir = 'updatefiles'
    tableSuffix = 'update'
    dbnameTmp = dbname}

  # download files
  fileInfo = getPubmedFileInfo(subDirs = subDir, dbname = dbnameTmp, ...)
  if (nrow(fileInfo) == 0) {
    cat('Database is already up-to-date.\n')
    return(invisible())}

  fileInfo = fileInfo[max(1, min(.N, .N - nFiles + 1)):.N] # take most recent
  fileInfo = getPubmedFiles(fileInfo, localDir)

  # process files
  logName1 = sprintf('%s_%s.log', subDir, format(Sys.time(), '%Y%m%d_%H%M%S'))

  processPubmedXml(
    xmlDir = file.path(localDir, subDir), xmlFiles = fileInfo$xml_filename,
    logPath = file.path(localDir, logName1), tableSuffix = tableSuffix,
    overwrite = TRUE, dbname = dbname, ...)

  dFailed = getFailed(file.path(localDir, logName1))

  if (isTRUE(retry) && nrow(dFailed) > 0) {

    logName2 = sprintf('%s_%s.log', subDir, format(Sys.time(), '%Y%m%d_%H%M%S'))
    retrySuffix = paste_(tableSuffix, 'retry')

    # retry failed steps
    processPubmedXml(
      xmlDir = file.path(localDir, subDir), xmlFiles = dFailed,
      logPath = file.path(localDir, logName2), tableSuffix = retrySuffix,
      overwrite = TRUE, dbname = dbname, ...)

    # add retry tables to first try tables
    addSourceToTarget(
      sourceSuffix = retrySuffix, targetSuffix = tableSuffix,
      dryRun = FALSE, dbname = dbname, ...)}

  if (mode == 'update') {
    # add update tables to main tables
    addSourceToTarget(
      sourceSuffix = tableSuffix, targetSuffix = '',
      dryRun = FALSE, dbname = dbname, ...)}

  if (nCitations > 0) {
    r = getCitation(
      localDir, nrows = nCitations, tableSuffix = '',
      overwrite = TRUE, dbname = dbname, ...)}

  invisible()}


#' @export
addSourceToTarget = function(sourceSuffix, targetSuffix, dryRun, dbname, ...) {
  stopifnot(!is.null(sourceSuffix) || sourceSuffix != '')

  con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)
  targetEmpty = getEmptyTables(targetSuffix)
  sourceEmpty = getEmptyTables(sourceSuffix)

  # create source table of pmid, xml_filename to keep
  sourceKeep = sprintf('pmid_status_%s_keep', sourceSuffix)
  if (DBI::dbExistsTable(con, sourceKeep)) {
    DBI::dbRemoveTable(con, sourceKeep)}

  q = sprintf(
    paste('create table %s as',
          'select pmid, max(xml_filename) as xml_filename',
          'from pmid_status_%s',
          'group by pmid'),
    sourceKeep, sourceSuffix)
  n = DBI::dbExecute(con, q)

  deleteStart = c('delete', 'select count(*)')
  insertBase = c('insert into %s select %s', 'select count(*)')

  # special treatment for xml_processed tables
  targetNow = names(targetEmpty)[startsWith(names(targetEmpty), 'xml_processed')]
  sourceNow = names(sourceEmpty)[startsWith(names(sourceEmpty), 'xml_processed')]

  q = sprintf('%s from %s where xml_filename in (select xml_filename from %s)',
              deleteStart[1 + dryRun], targetNow, sourceNow)
  nDelete = runStatement(con, q)

  insertStart = if (isTRUE(dryRun)) insertBase[2L] else
    sprintf(insertBase[1L], targetNow,
            paste0(colnames(targetEmpty[[targetNow]]), collapse = ', '))

  q = sprintf('%s from %s', insertStart, sourceNow)
  nInsert = runStatement(con, q)

  d1 = data.table(source_name = sourceNow, target_name = targetNow,
                  nrows_delete = nDelete, nrows_insert = nInsert)

  # loop over remaining tables
  feo = foreach(targetName = setdiff(names(targetEmpty), targetNow),
                sourceName = setdiff(names(sourceEmpty), sourceNow),
                .combine = rbind)

  d2 = feo %do% {
    # drop rows in target tables, use subquery to conform to sql standard
    q = sprintf('%s from %s where pmid in (select pmid from %s)',
                deleteStart[1 + dryRun], targetName, sourceName)
    nDelete = runStatement(con, q)

    # append source rows to target tables
    insertStart = if (isTRUE(dryRun)) insertBase[2L] else
      sprintf(insertBase[1L], targetName,
              paste0('a.', colnames(targetEmpty[[targetName]]), collapse = ', '))

    q = sprintf(paste('%s from %s as a inner join %s as b',
                      'on a.pmid = b.pmid and a.xml_filename = b.xml_filename'),
                insertStart, sourceName, sourceKeep)
    nInsert = runStatement(con, q)

    dNow = data.table(source_name = sourceName, target_name = targetName,
                      nrows_delete = nDelete, nrows_insert = nInsert)}

  if (isFALSE(dryRun)) { # with great power comes great responsibility
    for (sourceName in c(sourceKeep, names(sourceEmpty)))
      DBI::dbRemoveTable(con, sourceName)}

  d = rbind(d1, d2)
  setattr(d, 'dryRun', dryRun)
  return(d)}
