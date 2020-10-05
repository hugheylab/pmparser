library('pmparser')
library('data.table')
tables = c(names(pmparser:::getParsingTables('')), 'citation')
conP =  pmparser:::connect('postgres', 'pmdbclick')
conC =  pmparser:::connect('clickhouse', 'clicktest')
for(table in tables){
  q = glue('select * from {table}')
  dtP = as.data.table(DBI::dbReadTable(conP, table))
  for(colname in colnames(dtP)){
    if(inherits(dtP[[colname]], 'Date')){
      dtP[[colname]] = NULL
    }
  }
  data.table::setorder(dtP)

  dtC = as.data.table(DBI::dbReadTable(conC, table))
  for(colname in colnames(dtC)){
    if(inherits(dtC[[colname]], 'Date') || colname == 'version'){
      dtC[[colname]] = NULL
    }
  }
  data.table::setorder(dtC)

  allEq = all.equal(dtP, dtC, check.attributes = FALSE)

  if(isTRUE(allEq)){
    warning(glue('{table} is equal.\n'))
  } else{
    warning(glue('{table} is not equal: \n{allEq}\n'))
  }
}
warnings()
pmparser:::disconnect(conP)
pmparser:::disconnect(conC)
