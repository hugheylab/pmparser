SCHEMA="public"
DB="pmdb"

Rscript \
  -e "pmparser:::createParsingTables(dbtype = 'bigquery', dbname = 'pmparser-test', project = 'pmparser-test', dataset = 'pmdb')" \
  -e "bqCon = pmparser:::connect('bigquery', dbname = 'pmparser-test', project = 'pmparser-test', dataset = 'pmdb')" \
  -e "pmparser:::dropPmidVersionColumn('', bqCon)" \
  -e "pmparser:::disconnect(bqCon)"


psql -Atc "select tablename from pg_tables where schemaname='$SCHEMA'" $DB |\
  while read TBL; do
    psql -c "COPY $SCHEMA.$TBL TO STDOUT WITH (FORMAT CSV, HEADER, ENCODING 'UTF-8')" $DB > $TBL.csv
  done
for f in *.csv; do
  tName=$(echo $f | sed 's/\.csv//g')
  bq load \
  --source_format=CSV \
  --allow_quoted_newlines \
  --skip_leading_rows=1 \
  "pmparser-test:pmdb.$tName" \
  $f
done
