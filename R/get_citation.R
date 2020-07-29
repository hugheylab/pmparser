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
getCitation = function(localDir, filename = 'open_citation_collection.zip',
                       nrows = Inf, tableSuffix = NULL, overwrite = FALSE,
                       dbname = NULL, ...) {

  filepath = file.path(localDir, filename)
  tableBase = 'citation'

  if (!file.exists(filepath) || isTRUE(overwrite)) {
    citationInfo = getCitationInfo()

    if (!is.null(dbname)) {
      con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)
      tableName = paste_(tableBase, 'version', tableSuffix)

      if (DBI::dbExistsTable(con, tableName)) {
        dVersion = DBI::dbReadTable(con, tableName)

        if (dVersion$md5_computed == citationInfo$supplied_md5) {
          cat('Citation table is already up-to-date.\n')
          return()}}}

    utils::download.file(citationInfo$download_url, filepath, mode = 'wb')
    md5Computed = tools::md5sum(filepath)

    if (md5Computed != citationInfo$supplied_md5) {
      stop('Supplied and computed MD5 checksums do not match.')}}

  # read the file
  if (tools::file_ext(filepath) == 'zip') {
    dCitation = data.table::fread(cmd = paste('unzip -p', filepath),
                                  nrows = nrows)
  } else {
    dCitation = data.table::fread(filepath, nrows = nrows)}

  # check the file
  stopifnot(all.equal(colnames(dCitation), c('citing', 'referenced')),
            all(sapply(dCitation, class) == 'integer'))
  setnames(dCitation, c('citing_pmid', 'cited_pmid'))

  # send to db
  if (!is.null(dbname)) {
    con = DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)
    DBI::dbWriteTable(con, paste_(tableBase, tableSuffix), dCitation,
                      overwrite = overwrite)

    if (!exists('md5Computed')) {
      md5Computed = tools::md5sum(filepath)}

    dVersion = data.table(
      md5_computed = md5Computed, datetime_processed = Sys.time())

    # if we make it to this point, set overwrite to TRUE
    DBI::dbWriteTable(con, paste_(tableBase, 'version', tableSuffix),
                      dVersion, overwrite = TRUE)}

  return(dCitation)}
