# TODO: Either call packages by loading explicitly or by using the :: syntax
library('pmparser')
library('data.table')
library('glue')
library('DBI')
tables = c(names(pmparser:::getParsingTables('')), 'citation', 'citation_version')
conP =  pmparser:::connect('postgres', 'pmdbclick')
conB =  pmparser:::connect('bigquery', 'pmparser-test', project = 'pmparser-test', dataset = 'pmparser')
for(table in tables){
  dtP = as.data.table(dbReadTable(conP, table))
  setorder(dtP)

  dtB = as.data.table(dbReadTable(conB, table))
  setorder(dtB)

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

