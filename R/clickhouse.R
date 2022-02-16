# getClickhouseDataTypes = function(d, nullable = TRUE) {
#   r = c('logical', 'integer', 'numeric', 'character', 'Date', 'POSIXct')
#   ch = c('UInt8', 'Int32', 'Float64', 'String', 'Date', 'DateTime')
#   rIdx = lapply(d, function(x) inherits(x, r, which = TRUE))
#   dataTypesTmp = sapply(rIdx, function(i) ch[i == 1L])
#
#   if (!isTRUE(nullable)) {
#     return(dataTypesTmp)
#   } else {
#     dataTypes = glue('Nullable({dataTypesTmp})')
#     dataTypes[dataTypesTmp == 'DateTime'] = 'DateTime'
#     dataTypes[dataTypesTmp == 'Date'] = 'Date'
#     return(dataTypes)}}
#
#
# createTableClickhouse = function(con, tableName, d, nullable = TRUE) {
#   dataTypes = getClickhouseDataTypes(d, nullable = nullable)
#   q = glue(
#     'create table {tableName} ({z}) ENGINE = MergeTree() order by tuple()',
#     z = paste(colnames(d), dataTypes, collapse = ', '))
#   DBI::dbExecute(con, q)}
#
#
# setNAToSpecial = function(d) {
#   naDateVal = as.Date('2100-01-01')
#   priorDateVal = as.Date('2075-01-01')
#   if (nrow(d) == 1L && any(is.na(d))){
#     columns = colnames(d)
#     for(column in columns){
#       if (is.na(d[[column]])){
#         if (is.logical(d[[column]])) val = 0
#         else if (is.integer(d[[column]])) val = -1L
#         else if (is.numeric(d[[column]])) val = -1
#         else if (inherits(d[[column]], 'POSIXct')) val = naDateVal
#         else if (inherits(d[[column]], 'Date')) val = naDateVal
#         else val = as.character(NA)
#         d[, (column) := val]}}
#   } else {
#     for (j in 1:ncol(d)) {
#       if (inherits(d[[j]], 'Date')) {
#         data.table::set(
#           d, i = which(is.na(d[[j]])), j = j, value = naDateVal)
#         data.table::set(
#           d, i = which(d[[j]] < as.Date('1970-01-01')), j = j,
#           value = priorDateVal)}}}
#   return(invisible(d))}
