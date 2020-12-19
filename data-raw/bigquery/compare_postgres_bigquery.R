# TODO: Either call packages by loading explicitly or by using the :: syntax
library('pmparser')
library('data.table')
library('glue')
library('DBI')

comparePostgresBigquery = function(tables = NULL, pgDbName = 'pmdb', project = 'pmparser-test', dataset = 'pmdb'){

  parseNames = pmparser:::getParsingTables('')
  parseNames = c(parseNames, list(citation = data.table::data.table(citing_pmid = as.integer(), cited_pmid = as.integer()),
                                  citation_version = data.table::data.table(md5_computed = as.character(), pmparser_version = as.character(), datetime_processed = as.POSIXct(as.character()))))
  if(is.null(tables)){
    tables = names(parseNames)
  }
  conP =  pmparser:::connect('postgres', pgDbName)
  conB =  pmparser:::connect('bigquery', project, project = project, dataset = dataset)
  chunkSize = 1000000L
  complete = FALSE
  verExclude = c('pmid_status', 'xml_processed', 'citation', 'citation_version')
  notEqTables = data.table(table = as.character(), reason = as.character(), offset = as.integer(), chunkSize = as.integer(), missingIds = as.character())
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
        notEqTables = rbind(notEqTables, data.table(table = table, reason = allEq, offset = as.integer(NA), chunkSize = as.integer(NA), missingIds = as.character(NA)))
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
            notEqTables = rbind(notEqTables, data.table(table = table, reason = allEq, offset = off, chunkSize = chunkSize, missingIds = as.character(NA)))
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
          notEqTables = rbind(notEqTables, data.table(table = table, reason = glue('Different number of rows. PSQL: {totalRows} vs. BigQuery: {totalRowsB}'), offset = as.integer(NA), chunkSize = chunkSize, missingIds = as.character(NA)))
        }

        # Calculate first chunk and compare the first chunk
        dtP = data.table::as.data.table(DBI::dbGetQuery(conP, glue('SELECT * FROM {`table`} ORDER BY {`colOrder`} LIMIT {`chunkSize`}')))

        dtB = data.table::as.data.table(DBI::dbGetQuery(conB, glue('SELECT * FROM {`table`} ORDER BY {`colOrder`} LIMIT {`chunkSize`}')))

        for (colname in colnames(parseNames[[table]])) {
          if (inherits(dtP[[colname]], 'character')) {
            dtP[[colname]] = gsub('\r', '', dtP[[colname]])
            dtP[[colname]] = gsub('\n', '', dtP[[colname]])
            dtP[[colname]] = gsub('\feff', '', dtP[[colname]])

            dtB[[colname]] = gsub('\r', '', dtB[[colname]])
            dtB[[colname]] = gsub('\n', '', dtB[[colname]])
            dtB[[colname]] = gsub('\feff', '', dtB[[colname]])
          }
        }
        setorder(dtP)
        setorder(dtB)

        allEq = all.equal(dtP, dtB, check.attributes = FALSE)

        if (isTRUE(allEq)) {
          # warning(glue('{table} is equal.\n'))
        } else{
          allEqTot = FALSE
          notEqTables = rbind(notEqTables, data.table(table = table, reason = allEq, offset = 0L, chunkSize = chunkSize, missingIds = as.character(NA)))
          # warning(glue('{table} is not equal: \n{allEq}\n'))
        }

        # Query last chunk from the end and compare
        dtP = data.table::as.data.table(DBI::dbGetQuery(conP, glue('SELECT * FROM {`table`} ORDER BY {`colOrderDesc`} DESC LIMIT {`chunkSize`}')))

        dtB = data.table::as.data.table(DBI::dbGetQuery(conB, glue('SELECT * FROM {`table`} ORDER BY {`colOrderDesc`} DESC LIMIT {`chunkSize`}')))

        for (colname in colnames(parseNames[[table]])) {
          if (inherits(dtP[[colname]], 'character')) {
            dtP[[colname]] = gsub('\r', '', dtP[[colname]])
            dtP[[colname]] = gsub('\n', '', dtP[[colname]])
            dtP[[colname]] = gsub('\feff', '', dtP[[colname]])

            dtB[[colname]] = gsub('\r', '', dtB[[colname]])
            dtB[[colname]] = gsub('\n', '', dtB[[colname]])
            dtB[[colname]] = gsub('\feff', '', dtB[[colname]])
          }
        }
        setorder(dtP)
        setorder(dtB)

        allEq = all.equal(dtP, dtB, check.attributes = FALSE)

        if (isTRUE(allEq)) {
          # warning(glue('{table} is equal.\n'))
        } else{
          allEqTot = FALSE
          notEqTables = rbind(notEqTables, data.table(table = table, reason = allEq, offset = totalRows, chunkSize = chunkSize, missingIds = as.character(NA)))
          # warning(glue('{table} is not equal: \n{allEq}\n'))
        }

        # Query for ids in the bigquery DB
        conB@page_size = 100
        dtB = data.table::as.data.table(DBI::dbGetQuery(conB, glue('SELECT id FROM {`table`}')))
        setorder(dtB)
        conB@page_size = 10000

        pIds = 1L:totalRows
        bIds = as.vector(dtB$id)

        missingIds = which(!(pIds %in% bIds))

        if (length(missingIds) > 0) {
          allEqTot = FALSE
          notEqTables = rbind(notEqTables, data.table(table = table, reason = 'Missing IDs', offset = 0, chunkSize = chunkSize, missingIds = paste(missingIds, collapse = ', ')))
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
  return(notEqTables)
}

