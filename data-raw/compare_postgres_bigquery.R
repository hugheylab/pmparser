# TODO: Either call packages by loading explicitly or by using the :: syntax
library('pmparser')
library('data.table')
library('glue')
library('DBI')
parseNames = pmparser:::getParsingTables('')
parseNames = c(parseNames, list(citation = data.table::data.table(citing_pmid = as.integer(), cited_pmid = as.integer()),
                                citation_version = data.table::data.table(md5_computed = as.character(), pmparser_version = as.character(), datetime_processed = as.POSIXct(as.character()))))
tables = names(parseNames)
conP =  pmparser:::connect('postgres', 'pmdb')
project = 'pmparser-test'
dataset = 'pmdb'
conB =  pmparser:::connect('bigquery', 'pmparser-test', project = project, dataset = dataset)
chunkSize = 1000000L
complete = FALSE
verExclude = c('pmid_status', 'xml_processed', 'citation', 'citation_version')
notEqTables = data.table(table = as.character(), reason = as.character(), offset = as.integer(), chunkSize = as.integer())
for (table in tables) {
  if (is.null(chunkSize)) {
    dtP = as.data.table(dbReadTable(conP, table))
    setorder(dtP)

    dtB = as.data.table(dbReadTable(conB, table))
    setorder(dtB)

    allEq = all.equal(dtP, dtB, check.attributes = FALSE)

    if (isTRUE(allEq)) {
      warning(glue('{table} is equal.\n'))
    } else {
      notEqTables = rbind(notEqTables, data.table(table = table, reason = allEq, offset = as.integer(NA), chunkSize = as.integer(NA)))
      warning(glue('{table} is not equal: \n{allEq}\n'))
    }
  } else {
    colNamesDT = parseNames[[table]]
    if (!(table %in% verExclude)) colNamesDT[,version := NULL]
    colOrder = paste(colnames(colNamesDT), collapse=', ')
    colOrderDesc = paste(colnames(colNamesDT), collapse=' DESC, ')

    dCount = DBI::dbGetQuery(conP, glue('SELECT count(*) as count FROM {`table`}'))
    totalRows = as.integer(dCount$count)

    allEqTot = TRUE

    if (isTRUE(complete)) {
      for (rowOff in 0:(totalRows %/% chunkSize)) {
        # Calculate offset to use with limit, then query based off that
        off = rowOff * chunkSize
        dtP = data.table::as.data.table(DBI::dbGetQuery(conP, glue('SELECT * FROM {`table`} ORDER BY {`colOrder`} LIMIT {`chunkSize`} OFFSET {`off`}')))
        setorder(dtP)

        dtB = data.table::as.data.table(DBI::dbGetQuery(conB, glue('SELECT * FROM {`table`} ORDER BY {`colOrder`} LIMIT {`chunkSize`} OFFSET {`off`}')))
        setorder(dtB)

        allEq = all.equal(dtP, dtB, check.attributes = FALSE)

        if (isTRUE(allEq)) {
          # warning(glue('{table} is equal.\n'))
        } else{
          allEqTot = FALSE
          notEqTables = rbind(notEqTables, data.table(table = table, reason = allEq, offset = off, chunkSize = chunkSize))
          # warning(glue('{table} is not equal: \n{allEq}\n'))
        }
      }
    } else {

      # Query count from BigQuery to see if row totals match
      dCountB = DBI::dbGetQuery(conB, glue('SELECT count(*) as count FROM {`table`}'))
      totalRowsB = as.integer(dCountB$count)

      # Compare row totals
      if(totalRows != totalRowsB){
        allEqTot = FALSE
        notEqTables = rbind(notEqTables, data.table(table = table, reason = glue('Different number of rows. PSQL: {totalRows} vs. BigQuery: {totalRowsB}'), offset = as.integer(NA), chunkSize = chunkSize))
      }

      # Calculate first chunk and compare the first chunk
      dtP = data.table::as.data.table(DBI::dbGetQuery(conP, glue('SELECT * FROM {`table`} ORDER BY {`colOrder`} LIMIT {`chunkSize`}')))
      setorder(dtP)

      dtB = data.table::as.data.table(DBI::dbGetQuery(conB, glue('SELECT * FROM {`table`} ORDER BY {`colOrder`} LIMIT {`chunkSize`}')))
      setorder(dtB)

      allEq = all.equal(dtP, dtB, check.attributes = FALSE)

      if (isTRUE(allEq)) {
        # warning(glue('{table} is equal.\n'))
      } else{
        allEqTot = FALSE
        notEqTables = rbind(notEqTables, data.table(table = table, reason = allEq, offset = 0L, chunkSize = chunkSize))
        # warning(glue('{table} is not equal: \n{allEq}\n'))
      }

      # Query last chunk from the end and compare
      dtP = data.table::as.data.table(DBI::dbGetQuery(conP, glue('SELECT * FROM {`table`} ORDER BY {`colOrderDesc`} LIMIT {`chunkSize`}')))
      setorder(dtP)

      dtB = data.table::as.data.table(DBI::dbGetQuery(conB, glue('SELECT * FROM {`table`} ORDER BY {`colOrderDesc`} LIMIT {`chunkSize`}')))
      setorder(dtB)

      allEq = all.equal(dtP, dtB, check.attributes = FALSE)

      if (isTRUE(allEq)) {
        # warning(glue('{table} is equal.\n'))
      } else{
        allEqTot = FALSE
        notEqTables = rbind(notEqTables, data.table(table = table, reason = allEq, offset = totalRows, chunkSize = chunkSize))
        # warning(glue('{table} is not equal: \n{allEq}\n'))
      }


    }

    if (isTRUE(allEqTot)) {
      warning(glue('{table} is equal.\n'))
    } else {
      # notEqTables = rbind(notEqTables, data.table(table = table, reason = allEq, offset = off, chunkSize = chunkSize))
      warning(glue('{table} is not equal: \n{allEq}\n'))
    }
  }
}
warnings()
pmparser:::disconnect(conP)
pmparser:::disconnect(conB)

