copyPostgresToBigquery = function(
  dbname, overwrite, project, dataset, auth, tableNames = NULL, ...) {
  tableName = NULL
  # get table names from postgres
  conPg = connect('postgres', dbname, ...)

  if (is.null(tableNames)) tableNames = sort(DBI::dbListTables(conPg))

  # check for tables on bigquery and possibly remove them
  bigrquery::bq_auth(path = auth)
  conBq = connect(
    'bigquery', dbname = '', project = project, dataset = dataset)
  tableExists = sapply(tableNames, function(x) DBI::dbExistsTable(conBq, x))
  stopifnot(!any(tableExists) || isTRUE(overwrite))

  for (i in seq_len(length(tableNames))) {
    if (tableExists[i]) DBI::dbRemoveTable(conBq, tableNames[i])}

  # are you ready for the fun part?
  feo = foreach(
    tableName = tableNames, .options.future = list(scheduling = Inf))

  optVal = getOption('future.rng.onMisuse')
  options(future.rng.onMisuse = 'ignore')

  r = feo %dopar% {
    # fresh connections for each parallel worker
    conPg = connect('postgres', dbname, ...)
    bigrquery::bq_auth(path = auth)
    conBq = connect(
      'bigquery', dbname = '', project = project, dataset = dataset)

    # create empty table on bigquery
    d = DBI::dbGetQuery(conPg, glue('select * from {tableName} limit 1'))
    createTable(conBq, tableName, d)

    # export table on postgres to csv
    withr::local_file(glue('{tableName}.csv'))
    cmd = glue(
      'psql -c "copy public.{tableName}
      to stdout with (format csv, header, encoding \'UTF-8\')"
      {dbname} > {tableName}.csv')
    system(gsub('\n', ' ', cmd))

    # send table to bigquery
    cmd = glue(
      'bq load --source_format=CSV --allow_quoted_newlines --skip_leading_rows=1
      "{project}:{dataset}.{tableName}" {tableName}.csv')
    system(gsub('\n', ' ', cmd))}

  options(future.rng.onMisuse = optVal)
  disconnect(conPg)
  disconnect(conBq)
  invisible()}
