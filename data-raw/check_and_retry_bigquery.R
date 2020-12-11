library(data.table)

checkAndRetryBigquery = function(pgDbName = 'pmdb', project = 'pmparser-test', dataset = 'pmdb', chunkSize = 10000L, fCompName = 'bigquery_comp_results.csv', fResName = 'retry_bigquery_result.csv'){

  notEqTables = comparePostgresBigquery(pgDbName = pgDbName, project = project, dataset = dataset)

  fwrite(notEqTables, fCompName)

  retryTables = notEqTables[which(notEqTables$reason == 'Missing IDs'), table, missingIds]

  dtFail = data.table(table = as.character(), offset = as.integer(), chunk_size = as.integer(), missingIds = as.character(), fail_reason = as.character(), timestamp = as.POSIXct(as.character()))

  for (retryTable in retryTables) {
    table = retryTable$table
    mIds = retryTable$missingIds

    conP =  pmparser:::connect('postgres', pgDbName)
    dCount = data.table::as.data.table(DBI::dbGetQuery(conP, glue('SELECT count(*) as count FROM {`table`} WHERE id IN ({mIds})')))
    totalRows = as.integer(dCount$count)

    for (rowOff in 0:(totalRows %/% chunkSize)) {
      off = rowOff * tChunkSize
      dTable = data.table::as.data.table(DBI::dbGetQuery(conP, glue('SELECT * FROM {`table`} WHERE id IN ({`mIds`}) ORDER BY id OFFSET {off} LIMIT {`chunkSize`}')))

      tryCatch({bq_table_upload(bq_table(project, dataset, tableName), values = dTable, write_disposition = 'WRITE_APPEND')},
               error = function(e){
                 dtFail = rbind(dtFail, data.table(table = tableName, offset = off, chunk_size =chunkSize, missingIds = mIds, fail_reason = trimws(as.character(e)), timestamp = Sys.time()))
               })
    }


  }


  notEqTablesAfter = comparePostgresBigquery(tables = retryTables$table, pgDbName = pgDbName, project = project, dataset = dataset)

  if(nrow(notEqTablesAfter) > 0){
    fwrite(notEqTablesAfter, fResName)
  }

}
