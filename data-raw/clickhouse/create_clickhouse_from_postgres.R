createClickhouseFromPostgres = function(pgDbName = 'pmdbclick', clDbName = 'pmdbclick', chunkSize = 15000L) {

  # Drop ClickHouse Database if exists and create database for fresh start.
  system(sprintf('clickhouse-client --query="drop database if exists %s"', clDbName))
  system(sprintf('clickhouse-client --query="create database %s"', clDbName))

  # Create tables on DB and remove version column
  createParsingTables(dbtype = 'clickhouse', dbname = clDbName)
  clCon = connect('clickhouse', clDbName)
  dropPmidVersionColumn('', clCon)
  disconnect(clCon)

  # Get tables for processing and add citation tables
  parseNames = getParsingTables('')
  parseNames = c(
    parseNames,
    list(citation = data.table::data.table(citing_pmid = as.integer(), cited_pmid = as.integer()),
         citation_version = data.table::data.table(md5_computed = as.character(),
                                                   pmparser_version = as.character(),
                                                   datetime_processed = as.POSIXct(as.character()))))
  tableNames = names(parseNames)

  verExclude = c('pmid_status', 'xml_processed', 'citation', 'citation_version')
  foreach(tableName = tableNames) %dopar% {
    # Get count of rows in table
    pCon = connect('postgres', pgDbName)
    dCount = DBI::dbGetQuery(pCon, glue('SELECT count(*) as count FROM {`tableName`}'))
    totalRows = as.integer(dCount$count)

    # Exclude version column and add sort order by all columns
    clCon = connect('clickhouse', clDbName)
    colNamesDT = parseNames[[tableName]]
    if (!(tableName %in% verExclude)) colNamesDT[, version := NULL]
    colOrder = paste(colnames(colNamesDT), collapse = ', ')

    # For offset multiplier, integer divide the totalRows by chunkSize
    for (rowOff in 0:(totalRows %/% chunkSize)) {
      # Calculate offset to use with limit, then query based off that
      off = rowOff * chunkSize
      dTable = data.table::as.data.table(DBI::dbGetQuery(
        pCon,
        glue('SELECT * FROM {`tableName`} ORDER BY {`colOrder`} LIMIT {`chunkSize`} OFFSET {`off`}')))

      # Update to special values
      dTable = setNAToSpecial(dTable)

      # Append to ClickHouse DB
      DBI::dbWriteTable(clCon, tableName, dTable, overwrite = FALSE, append = TRUE)
    }
    disconnect(pCon)
    disconnect(clCon)}
}
