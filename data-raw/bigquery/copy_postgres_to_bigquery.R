bigrquery::bq_auth(path = '~/credentials/pmdb-bq-0f1c794a7e39.json')
pg = pmparser::getPgParams()

pmparser:::copyPostgresToBigquery(
  dbname = 'pmdb', overwrite = TRUE, project = 'pmdb-bq', dataset = 'pmdb',
  host = pg$hostname, user = pg$username)
