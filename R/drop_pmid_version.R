deleteOldPmidVersions = function(tableSuffix, dryRun, dbtype, dbname, ...) {
  emptyTables = getEmptyTables(tableSuffix)
  tableKeep = paste_('pmid_status_keep', tableSuffix)

  con = connect(dbtype, dbname, ...)
  if (DBI::dbExistsTable(con, tableKeep)) {
    DBI::dbRemoveTable(con, tableKeep)}

  tableNow = names(emptyTables)[startsWith(names(emptyTables), 'pmid_status')]

  if(dbtype == 'clickhouse'){
    q = glue(
      'create table {tableKeep} as with ranked_pmid_status as
      (select *, rowNumberInAllBlocks() from (select pmid order by version desc) as rn
      from {tableNow}) select {cols} from ranked_pmid_status where rn = 1',
      cols = paste(DBI::dbListFields(con, tableNow), collapse = ', '))
    } else {
    q = glue(
      'create table {tableKeep} as with ranked_pmid_status as
      (select *, row_number() over (partition by pmid order by version desc) as rn
      from {tableNow}) select {cols} from ranked_pmid_status where rn = 1',
      cols = paste(DBI::dbListFields(con, tableNow), collapse = ', '))}
  n = DBI::dbExecute(con, q)
  disconnect(con)

  qStart = if (isTRUE(dryRun)) 'select count(*)' else 'delete'
  idx = !grepl('^(pmid_status|xml_processed)', names(emptyTables))

  doOp = getDoOp(dbtype)
  d = doOp(foreach(tableName = names(emptyTables)[idx], .combine = rbind), {
    con = connect(dbtype, dbname, ...)
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
    q = glue_sql('alter table {`tableKeep`} rename to {`tableNow`}', .con = con)
    n = DBI::dbExecute(con, q)}

  disconnect(con)
  setattr(d, 'dryRun', dryRun)
  return(d)}


dropPmidVersionColumn = function(tableSuffix, con) {
  emptyTables = getEmptyTables(tableSuffix)
  idx = !grepl('^(pmid_status|xml_processed)', names(emptyTables))

  if (inherits(con, 'SQLiteConnection')) { # thanks, sqlite
    for (tableName in names(emptyTables)[idx]) {
      cols = setdiff(DBI::dbListFields(con, tableName), 'version')
      tableTmp = paste_(tableName, 'tmp')

      q = glue('create table {tableTmp} as select {cols} from {tableName}',
               cols = paste(cols, collapse = ', '))
      x = DBI::dbExecute(con, q)
      DBI::dbRemoveTable(con, tableName)

      q = glue_sql('alter table {`tableTmp`} rename to {`tableName`}',
                   .con = con)
      x = DBI::dbExecute(con, q)}

  } else {
    for (tableName in names(emptyTables)[idx]) {
      q = glue_sql('alter table {`tableName`} drop column version', .con = con)
      x = DBI::dbExecute(con, q)}}

  invisible()}
