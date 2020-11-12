library(pmparser)
library(data.table)
library(glue)

createBigQueryFromPostgres = function(bqDbName = 'pmdbclick', project = 'pmparser-test', dataset = 'pmparser', tableName, nRowsPerChunk = 15000L){
  bqCon = pmparser:::connect('bigquery', bqDbName, project = 'pmparser-test', dataset = 'pmparser')
  writeTableInChunks(glue('{tableName}.csv'), bqCon, nRowsPerChunk, overwrite=TRUE, tableName)
}
