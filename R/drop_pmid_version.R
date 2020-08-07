deleteOldPmidVersions = function(tableSuffix, dryRun, con) {
  emptyTables = getEmptyTables(tableSuffix)
  tableKeep = paste_('pmid_status_keep', tableSuffix)

  if (DBI::dbExistsTable(con, tableKeep)) {
    DBI::dbRemoveTable(con, tableKeep)}

  tableNow = names(emptyTables)[startsWith(names(emptyTables), 'pmid_status')]

  q = sprintf(paste('create table %s as with ranked_pmid_status as',
                    '(select *, row_number() over',
                    '(partition by pmid order by version desc) as rn',
                    'from %s)',
                    'select %s from ranked_pmid_status where rn = 1'),
              tableKeep, tableNow,
              paste(DBI::dbListFields(con, tableNow), collapse = ', '))
  n = DBI::dbExecute(con, q)

  qStart = if (isTRUE(dryRun)) 'select count(*)' else 'delete'
  idx = !grepl('^(pmid_status|xml_processed)', names(emptyTables))

  d = foreach(tableName = names(emptyTables)[idx], .combine = rbind) %do% {
    q = sprintf(paste('%s from %s as a where not exists',
                      '(select 1 from %s as b',
                      'where a.pmid = b.pmid and a.version = b.version)'),
                qStart, tableName, tableKeep)
    n = runStatement(con, q)
    dNow = data.table(table_name = tableName, nrow_delete = n)}

  if (isTRUE(dryRun)) {
    DBI::dbRemoveTable(con, tableKeep)
  } else {
    DBI::dbRemoveTable(con, tableNow)
    q = sprintf('alter table %s rename to %s', tableKeep, tableNow)
    n = DBI::dbExecute(con, q)}

  setattr(d, 'dryRun', dryRun)
  return(d)}


dropPmidVersionColumn = function(tableSuffix, con) {
  emptyTables = getEmptyTables(tableSuffix)
  idx = !grepl('^(pmid_status|xml_processed)', names(emptyTables))

  if (attr(class(con), 'package') == 'RSQLite') { # thanks, sqlite
    for (tableName in names(emptyTables)[idx]) {
      cols = setdiff(DBI::dbListFields(con, tableName), 'version')
      tableTmp = paste_(tableName, 'tmp')

      q = sprintf('create table %s as select %s from %s',
                  tableTmp, paste(cols, collapse = ', '), tableName)
      x = DBI::dbExecute(con, q)
      DBI::dbRemoveTable(con, tableName)

      q = sprintf('alter table %s rename to %s', tableTmp, tableName)
      x = DBI::dbExecute(con, q)}

  } else {
    for (tableName in names(emptyTables)[idx]) {
      q = sprintf('alter table %s drop column version', tableName)
      x = DBI::dbExecute(con, q)}}

  invisible()}
