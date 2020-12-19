copyPostgresToBigquery = function(dbname, overwrite, project, dataset, ...) {
  # create empty tables on bigquery
  createParsingTables(
    overwrite = overwrite, dbtype = 'bigquery', project = project,
    dataset = dataset)

  # drop version column from empty tables on bigquery
  con = connect('bigquery', project = project, dataset = dataset)
  dropPmidVersionColumn('', con)
  disconnect(con)

  # are you ready for the fun part?
  con = connect('postgres', dbname, ...)
  tableNames = DBI::dbListTables(con)
  disconnect(con)

  for (tableName in tableNames) {
    # export tables on postgres to csv
    withr::local_file(glue('{tableName}.csv'))
    cmd = glue(
      'psql -c "copy public.{tableName}
      to stdout with (format csv, header, encoding \'UTF-8\')"
      {dbname} > {tableName}.csv')
    cmd = gsub('\n', ' ', cmd)
    system(cmd)

    # send to bigquery
    cmd = glue(
      'bq load --source_format=CSV --allow_quoted_newlines --skip_leading_rows=1
      "{project}:{dataset}.{tableName}" {tableName}.csv')
    cmd = gsub('\n', ' ', cmd)
    system(cmd)}

  invisible()}
