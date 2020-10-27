SCHEMA="public"
DB="pmdbclick"

psql -Atc "select tablename from pg_tables where schemaname='$SCHEMA'" $DB |\
  while read TBL; do
    psql -c "COPY $SCHEMA.$TBL TO STDOUT WITH (FORMAT CSV, HEADER)" $DB > $TBL.csv
  done
for f in *.csv; do
  tName=$(echo $f | sed 's/\.csv//g')
  bq load \
  --autodetect \
  --source_format=CSV \
  "pmparser-test:pmparser.$tName" \
  $f
done
