SCHEMA="public"
DB="pmdbclick"

Rscript \
  -e "pmparser:::createParsingTables(dbtype = 'bigquery', dbname = 'pmparser-test', project = 'pmparser-test', dataset = 'pmparser')" \
  -e "bqCon = pmparser:::connect('bigquery', dbname = 'pmparser-test', project = 'pmparser-test', dataset = 'pmparser')" \
  -e "pmparser:::dropPmidVersionColumn('', bqCon)" \
  -e "pmparser:::disconnect(bqCon)"


psql -Atc "select tablename from pg_tables where schemaname='$SCHEMA'" $DB |\
  while read TBL; do
    psql -c "COPY $SCHEMA.$TBL TO STDOUT WITH (FORMAT CSV, HEADER, ENCODING 'UTF-8')" $DB > $TBL.csv
  done
for f in *.csv; do
  tName=$(echo $f | sed 's/\.csv//g')

  bq load \
  --skip_leading_rows=1
  --allow_quoted_newlines=true \
  --source_format=CSV \
  "pmparser-test:pmparser.$tName" \
  $f

done
