library('pmparser')
library('data.table')
tables = c(names(pmparser:::getParsingTables('')), 'citation', 'citation_version')
conP =  pmparser:::connect('postgres', 'pmdbclick')
conB =  pmparser:::connect('bigquery', 'pmparser-test', project = 'pmparser-test', dataset = 'pmparser')
for(table in tables){
  q = glue('select * from {table}')
  dtP = as.data.table(DBI::dbReadTable(conP, table))
  for(col in colnames(dtP)){
    if(inherits(dtP[[col]], 'character')){
      dtP[[col]] = gsub('\uFEFF', '', dtP[[col]])
    }
  }
  data.table::setorder(dtP)

  dtB = as.data.table(DBI::dbReadTable(conB, table))
  data.table::setorder(dtB)

  allEq = all.equal(dtP, dtB, check.attributes = FALSE)

  if(isTRUE(allEq)){
    warning(glue('{table} is equal.\n'))
  } else{
    warning(glue('{table} is not equal: \n{allEq}\n'))
  }
}
warnings()
pmparser:::disconnect(conP)
pmparser:::disconnect(conB)

