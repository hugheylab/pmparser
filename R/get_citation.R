getCitationInfo = function(filename = 'open_citation_collection.zip',
                           collectionId = 4586573,
                           baseUrl = 'https://api.figshare.com/v2') {

  #getting content from icite collection
  iciteUrl = sprintf('%s/collections/%d/articles', baseUrl, collectionId)
  iciteJson = data.table(jsonlite::fromJSON(iciteUrl))

  #extracting content from most recent icite snapshot
  latestId = iciteJson[published_date == max(published_date)]$id
  latestUrl = sprintf('%s/articles/%d/files', baseUrl, latestId)
  latestJson = data.table(jsonlite::fromJSON(latestUrl))

  citationInfo = latestJson[name == filename]
  return(citationInfo)}


#' @export
getCitation = function(path, tableSuffix = '', tableName = 'citation',
                       overwrite = FALSE, dbname = NULL, ...) {

  # figure out where to get the file
  defaultFilename = 'open_citation_collection.zip'

  if (dir.exists(path)) {
    filepath = file.path(path, defaultFilename)

    if (!file.exists(filepath)) {
      citationInfo = getCitationInfo()
      utils::download.file(citationInfo$download_url, filepath, mode = 'wb')
      md5Computed = tools::md5sum(filepath)

      if (md5Computed != citationInfo$supplied_md5) {
        stop('Supplied and computed MD5 checksums do not match.')}}

  } else if (file.exists(path)) {
    filepath = path

  } else {
    stop('File does not exist.')}

  # read the file
  if (tools::file_ext(filepath) == 'zip') {
    dCitation = data.table::fread(cmd = paste('unzip -p', filepath))
  } else {
    dCitation = data.table::fread(filepath)}

  # check the file
  stopifnot(all.equal(colnames(dCitation), c('citing', 'referenced')),
            all(sapply(dCitation, class) == 'integer'))
  setnames(dCitation, c('citing_pmid', 'cited_pmid'))

  # send to db
  if (!is.null(dbname)) {
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)
    DBI::dbWriteTable(con, paste0(tableName, tableSuffix), dCitation,
                      overwrite = overwrite)

    if (!exists('md5Computed')) {
      md5Computed = tools::md5sum(filepath)}

    dCitationVersion = data.table(
      md5_computed = md5Computed, datetime_processed = Sys.time())

    # if we make it to this point, set overwrite to TRUE
    DBI::dbWriteTable(con, paste0(tableName, '_version', tableSuffix),
                      dCitationVersion, overwrite = TRUE)}

  return(dCitation)}
