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
  if [$tName == 'journal']
    then
      bq load \
        --allow_quoted_newlines \
        --source_format=CSV \
        "pmparser-test:pmparser.$tName" \
        $f \
        pmid:INT64,journal_name:STRING,journal_iso:STRING,pub_date=DATE,pub_year:STRING,pub_month:STRING,pub_day:STRING,medline_date = ac,volume:STRING,issue:STRING,cited_medium:STRING
    else
      bq load \
        --autodetect
        --allow_quoted_newlines \
        --source_format=CSV \
        "pmparser-test:pmparser.$tName" \
        $f
  fi

done
