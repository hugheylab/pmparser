library(pmparser)
library(data.table)
library(glue)
library(bigrquery)

createBigQueryFromPostgres = function(pgDbName = 'pmdb', project = 'pmparser-test', dataset = 'pmdb', chunkSize = 15000L, overwriteTable = FALSE){

  # chunkSize variable is either an integer or a named list where the name is the table name and the value is the chunk size you want to process.
  # Integer values will be the chunkSize for all tables, and a named list will only process the tables in the list in the size provided.

  # Get tables for processing and add citation tables
  parseNames = pmparser:::getParsingTables('')
  parseNames = c(parseNames, list(citation = data.table::data.table(citing_pmid = as.integer(), cited_pmid = as.integer()),
                                  citation_version = data.table::data.table(md5_computed = as.character(), pmparser_version = as.character(), datetime_processed = as.POSIXct(as.character()))))


  tables = list()

  if(inherits(chunkSize, 'list')){
    tables = chunkSize[which(chunkSize > 0)]
    parseNames = parseNames[names(parseNames) %in% names(tables)]
  } else {
    tables = as.list(rep.int(chunkSize, length(parseNames)))
    names(tables) = names(parseNames)
  }

  tableNames = names(parseNames)

  bqCon = pmparser:::connect('bigquery', dbname = project, project = project, dataset = dataset)
  tableExists = sapply(
    tableNames, function(x) {
      exists = DBI::dbExistsTable(bqCon, x)
      if(exists && overwriteTable){
        DBI::dbRemoveTable(bqCon, x)
        exists = FALSE}
      return(exists)})
  stopifnot(!any(tableExists))
  pmparser:::disconnect(bqCon)

  # Create tables on DB and remove version column
  pmparser:::createParsingTables(dbtype = 'bigquery', dbname = project, project = project, dataset = dataset, tableNames = tableNames)
  bqCon = pmparser:::connect('bigquery', dbname = project, project = project, dataset = dataset)
  pmparser:::dropPmidVersionColumn('', bqCon)
  pmparser:::disconnect(bqCon)


  dtFail = data.table(table = as.character(), offset = as.integer(), chunk_size = as.integer(), fail_reason = as.character(), timestamp = as.POSIXct(as.character()))

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

    # Use chunkSize from tables list
    tChunkSize = tables[[tableName]]

    # For offset multiplier, integer divide the totalRows by chunkSize
    for(rowOff in 0:(totalRows %/% tChunkSize)) {
      # Calculate offset to use with limit, then query based off that
      off = rowOff * tChunkSize
      dTable = data.table::as.data.table(DBI::dbGetQuery(pCon, glue('SELECT * FROM {`tableName`} ORDER BY {`colOrder`} LIMIT {`tChunkSize`} OFFSET {`off`}')))

      # Append to BigQuery DB
      tryCatch({bq_table_upload(bq_table(project, dataset, tableName), values = dTable, write_disposition = 'WRITE_APPEND')},
               error = function(e){
                 dtFail = rbind(dtFail, data.table(table = tableName, offset = off, chunk_size =chunkSize, fail_reason = trimws(as.character(e)), timestamp = Sys.time()))
               })

    }
    pmparser:::disconnect(pCon)}
  return(dtFail)
}

