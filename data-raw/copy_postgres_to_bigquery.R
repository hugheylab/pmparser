# library('doFuture')
#
# registerDoFuture()
# nCores = round(availableCores() * 0.75)
# plan(multisession, workers = nCores)

# row numbers in the bigquery console are incorrect and may not match those in
# postgres, but downloading the tables into R shows that they are identical

# select table_name, n_rows as to_char(n_live_tup, 'FM9,999,999,999')
# from pg_stat_user_tables
# order by table_name;

# nCores = round(parallel::detectCores() * 0.75)
# doParallel::registerDoParallel(cores = nCores)
foreach::registerDoSEQ() # slower but avoids irreproducible errors

dbname = 'pmdb'
overwrite = TRUE
project = 'pmdb-bq'
dataset = 'pmdb'
auth = '~/credentials/pmdb-bq-0f1c794a7e39.json'
pg = pmparser::getPgParams()

pmparser:::copyPostgresToBigquery(
  dbname = dbname, overwrite = overwrite, project = project, dataset = dataset,
  auth = auth, host = pg$hostname, user = pg$username)
