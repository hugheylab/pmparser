deleteOldPmidVersions = function(
  tableSuffix, dryRun, dbtype, dbname, tableNames = NULL, ...) {

  tableName = NULL
  parTables = getParsingTables(tableSuffix, tableNames)
  tableKeep = paste_('pmid_status_keep', tableSuffix)

  con = connect(dbtype, dbname, ...)
  if (DBI::dbExistsTable(con, tableKeep)) {
    DBI::dbRemoveTable(con, tableKeep)}

  tableNow = names(parTables)[startsWith(names(parTables), 'pmid_status')]

  q = glue(
    'create table {tableKeep} as with ranked_pmid_status as
    (select *, row_number() over (partition by pmid order by version desc) as rn
    from {tableNow}) select {cols} from ranked_pmid_status where rn = 1',
    cols = paste(DBI::dbListFields(con, tableNow), collapse = ', '))
  n = DBI::dbExecute(con, q)
  disconnect(con)

  qStart = if (isTRUE(dryRun)) 'select count(*)' else 'delete'
  idx = !grepl('^(pmid_status|xml_processed)', names(parTables))

  feo = foreach(tableName = names(parTables)[idx], .combine = rbind,
                .options.future = list(scheduling = Inf))

  doOp = getDoOp(dbtype)
  d = doOp(feo, {
    con = connect(dbtype, dbname, ...)
    qStart
    tableName
    tableKeep # so glue works in dopar
    q = glue('{qStart} from {tableName} as a where not exists
             (select 1 from {tableKeep} as b
             where a.pmid = b.pmid and a.version = b.version)')
    n = runStatement(con, q)
    disconnect(con)
    dNow = data.table(table_name = tableName, nrow_delete = n)})

  con = connect(dbtype, dbname, ...)
  if (isTRUE(dryRun)) {
    DBI::dbRemoveTable(con, tableKeep)
  } else {
    DBI::dbRemoveTable(con, tableNow)
    if (inherits(con, 'BigQueryConnection')) {
      q = glue('create table {`tableNow`} as select * from {`tableKeep`}')
      n = DBI::dbExecute(con, q)
      q = glue('drop table {`tableKeep`}')
      DBI::dbExecute(con, q)
    } else {
      q = glue_sql('alter table {`tableKeep`} rename to {`tableNow`}',
                   .con = con)
      n = DBI::dbExecute(con, q)}}

  disconnect(con)
  setattr(d, 'dryRun', dryRun)
  return(d)}


dropPmidVersionColumn = function(tableSuffix, con, ...) {
  parTables = getParsingTables(tableSuffix, ...)
  idx = !grepl('^(pmid_status|xml_processed)', names(parTables))

  if (inherits(con, 'SQLiteConnection')) { # thanks, sqlite
    for (tableName in names(parTables)[idx]) {
      cols = setdiff(DBI::dbListFields(con, tableName), 'version')
      tableTmp = paste_(tableName, 'tmp')

      q = glue('create table {tableTmp} as select {cols} from {tableName}',
               cols = paste(cols, collapse = ', '))
      x = DBI::dbExecute(con, q)
      DBI::dbRemoveTable(con, tableName)

      q = glue_sql('alter table {`tableTmp`} rename to {`tableName`}',
                   .con = con)
      x = DBI::dbExecute(con, q)}

  } else if (inherits(con, 'BigQueryConnection')) {
    for (tableName in names(parTables)[idx]) {
      q = glue_sql('create or replace table {`tableName`}
                   as select * except(version) from {`tableName`}',
                   .con = con)
      x = DBI::dbExecute(con, q)}
  } else {
    for (tableName in names(parTables)[idx]) {
      q = glue_sql('alter table {`tableName`} drop column version', .con = con)
      x = DBI::dbExecute(con, q)}}

  invisible()}
