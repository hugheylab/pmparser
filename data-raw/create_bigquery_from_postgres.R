library(pmparser)
library(data.table)
library(glue)
library(bigrquery)

createBigQueryFromPostgres = function(bqDbName = 'pmdbclick', project = 'pmparser-test', dataset = 'pmparser', tableName, nRowsPerChunk = 15000L){
  bqCon = pmparser:::connect('bigquery', bqDbName, project = 'pmparser-test', dataset = 'pmparser')
  pmparser:::writeTableInChunks(glue('{tableName}.csv'), bqCon, nRowsPerChunk, overwrite=FALSE, tableName, append=TRUE)
  pmparser:::disconnect(bqCon)
}

createBigQueryFromPostgres = function(pgDbName = 'pmdbclick', bqDbName = 'pmdbclick', project = 'pmparser-test', dataset = 'pmparser', tableName, chunkSize = 15000L){

  # Create tables on DB and remove version column
  pmparser:::createParsingTables(dbtype = 'bigquery', dbname = 'pmparser-test', project = 'pmparser-test', dataset = 'pmparser')
  bqCon = pmparser:::connect('bigquery', dbname = 'pmparser-test', project = 'pmparser-test', dataset = 'pmparser')
  pmparser:::dropPmidVersionColumn('', bqCon)
  pmparser:::disconnect(bqCon)

  # Get tables for processing and add citation tables
  parseNames = pmparser:::getParsingTables('')
  parseNames = c(parseNames, list(citation = data.table::data.table(citing_pmid = as.integer(), cited_pmid = as.integer()),
                                  citation_version = data.table::data.table(md5_computed = as.character(), pmparser_version = as.character(), datetime_processed = as.POSIXct(as.character()))))
  tableNames = names(parseNames)

  verExclude = c('pmid_status', 'xml_processed', 'citation', 'citation_version')
  foreach(tableName = tableNames) %dopar% {
    # Get count of rows in table
    pCon = pmparser:::connect('postgres', pgDbName)
    dCount = DBI::dbGetQuery(pCon, glue('SELECT count(*) as count FROM {`tableName`}'))
    totalRows = as.integer(dCount$count)

    # Exclude version column and add sort order by all columns
    colNamesDT = parseNames[[tableName]]
    if(!(tableName %in% verExclude)) colNamesDT[,version := NULL]
    colOrder = paste(colnames(colNamesDT), collapse=', ')

    # For offset multiplier, integer divide the totalRows by chunkSize
    for(rowOff in 0:(totalRows %/% chunkSize)) {
      # Calculate offset to use with limit, then query based off that
      off = rowOff * chunkSize
      dTable = data.table::as.data.table(DBI::dbGetQuery(pCon, glue('SELECT * FROM {`tableName`} ORDER BY {`colOrder`} LIMIT {`chunkSize`} OFFSET {`off`}')))

      # Append to BigQuery DB
      bq_table_upload(bq_table(project, dataset, tableName), values = dTable)
    }
    pmparser:::disconnect(pCon)}
}

