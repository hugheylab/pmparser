library('doFuture')

registerDoFuture()
nCores = round(availableCores() * 0.75)
plan(multisession, workers = nCores)

dbname = 'pmdb'
overwrite = TRUE
project = 'pmdb-bq'
dataset = 'pmdb'
auth = '~/credentials/pmdb-bq-0f1c794a7e39.json'
pg = pmparser::getPgParams()

pmparser:::copyPostgresToBigquery(
  dbname = dbname, overwrite = overwrite, project = project, dataset = dataset,
  auth = auth, host = pg$hostname, user = pg$username)
