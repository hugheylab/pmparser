deleteOldPmidVersions = function(tableSuffix, dryRun, dbtype, dbname, ...) {
  parTables = getParsingTables(tableSuffix)
  tableKeep = paste_('pmid_status_keep', tableSuffix)

  con = connect(dbtype, dbname, ...)
  if (DBI::dbExistsTable(con, tableKeep)) {
    DBI::dbRemoveTable(con, tableKeep)}

  tableNow = names(parTables)[startsWith(names(parTables), 'pmid_status')]

  if (dbtype == 'clickhouse'){
    q = glue(
      'create table {tableKeep} engine = MergeTree() order by tuple() as
        select pmid, version, xml_filename, status from
        	(select pmid, max(assumeNotNull(version)) as version from {tableNow}
        		group by pmid) as a
        	inner join
        	(select pmid, version, xml_filename, status from {tableNow}) as b
        	on a.pmid = b.pmid and a.version = b.version order by pmid',
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
  idx = !grepl('^(pmid_status|xml_processed)', names(parTables))

  doOp = getDoOp(dbtype)
  d = doOp(foreach(tableName = names(parTables)[idx], .combine = rbind), {
    con = connect(dbtype, dbname, ...)
    if (dbtype == 'clickhouse'){
      q = glue("alter table {tableName} delete where concat(toString(pmid), ':', toString(version)) not in
               (select concat(toString(pmid), ':', toString(version)) from {tableKeep})")
    } else {
      q = glue('{qStart} from {tableName} as a where not exists
               (select 1 from {tableKeep} as b
               where a.pmid = b.pmid and a.version = b.version)')}
    n = runStatement(con, q)
    disconnect(con)
    dNow = data.table(table_name = tableName, nrow_delete = n)})

  con = connect(dbtype, dbname, ...)
  if (isTRUE(dryRun)) {
    DBI::dbRemoveTable(con, tableKeep)
  } else {
    DBI::dbRemoveTable(con, tableNow)
    if (dbtype =='clickhouse'){
      q = glue_sql('rename table {`tableKeep`} to {`tableNow`}', .con = con)
    } else{
      q = glue_sql('alter table {`tableKeep`} rename to {`tableNow`}', .con = con)}
    n = DBI::dbExecute(con, q)}

  disconnect(con)
  setattr(d, 'dryRun', dryRun)
  return(d)}


dropPmidVersionColumn = function(tableSuffix, con) {
  parTables = getParsingTables(tableSuffix)
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

  }
  else if (inherits(con, 'ClickhouseConnection')) {
    dbInfo = DBI::dbGetInfo(con)
    dbname = dbInfo$dbname
    # Removing Clickhouse version column drop to ensure data is the same.

    # q = ''
    # for (tableName in names(parTables)[idx]) {
    #   q = glue('{`q`}\nalter table {`tableName`} drop column version;')}
    # x = DBI::dbGetQuery(con, q)
    for (tableName in names(parTables)[idx]) {
      tNameDB = paste0(dbname, '.', tableName)
      system2('clickhouse-client', args = c('--query', sprintf('"alter table %s drop column version;"', tNameDB)))
    }
  } else {
    for (tableName in names(parTables)[idx]) {
      q = glue_sql('alter table {`tableName`} drop column version', .con = con)
      x = DBI::dbExecute(con, q)}}

  invisible()}
