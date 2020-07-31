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


#' Get public-domain citation data
#'
#' Get the latest version of the NIH Open Citation Collection from figshare
#' [here](https://nih.figshare.com/collections/iCite_Database_Snapshots_NIH_Open_Citation_Collection_/4586573),
#' and optionally write it to the database. This function requires the shell
#' command `unzip`, available by default on most Unix systems.
#'
#' @param localDir String indicating path to directory containing the citation
#'   file or to which the citation file should be downloaded.
#' @param filename String indicating name of the citation file. This should not
#'   normally be changed from the default.
#' @param nrows Number indicating how many rows of the citation file to read.
#'   This should not normally be changed from the default.
#' @param tableSuffix String indicating suffix, if any, to append to the table
#'   name.
#' @param overwrite Logical indicating whether to overwrite an existing table.
#' @param con Connection to the database, created using [DBI::dbConnect()].
#'
#' @return Normally, a data.table with columns `citing_pmid` and `cited_pmid`.
#'   Beware this is a large table and could swamp some machines' memories. If
#'   `dbname` is not `NULL` and the existing citation table and the citation
#'   file have identical MD5 sums, then the function returns `NULL` invisibly.
#'
#' @export
getCitation = function(localDir, filename = 'open_citation_collection.zip',
                       nrows = Inf, tableSuffix = NULL, overwrite = FALSE,
                       con = NULL) {

  filepath = file.path(localDir, filename)
  tableBase = 'citation'
  citationName = paste_(tableBase, tableSuffix)
  versionName = paste_(tableBase, 'version', tableSuffix)

  # get md5 from database
  if (is.null(con)) {
    md5Database = ''
  } else {

    if (DBI::dbExistsTable(con, versionName)) {
      dVersion = DBI::dbReadTable(con, versionName)
      md5Database = dVersion$md5_computed
    } else {
      md5Database = ''}}

  # get md5 of local file, download it if necessary
  if (file.exists(filepath) && isFALSE(overwrite)) {
    md5Computed = tools::md5sum(filepath)

    if (md5Computed == md5Database) {
      message('Citation table in database is already up-to-date.')
      return(invisible())}

  } else {
    citationInfo = getCitationInfo()

    if (citationInfo$supplied_md5 == md5Database) {
      message('Citation table in database is already up-to-date.')
      return(invisible())}

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

  # check the table
  stopifnot(all.equal(colnames(dCitation), c('citing', 'referenced')),
            all(sapply(dCitation, class) == 'integer'))
  setnames(dCitation, c('citing_pmid', 'cited_pmid'))

  # send to db
  if (!is.null(con)) {
    DBI::dbWriteTable(con, citationName, dCitation, overwrite = overwrite)

    dVersion = data.table(
      md5_computed = md5Computed,
      pmparser_version = getPkgVersion(),
      datetime_processed = Sys.time())

    # if we make it to this point, set overwrite to TRUE
    DBI::dbWriteTable(con, versionName, dVersion, overwrite = TRUE)}

  return(dCitation)}
