getCitationInfo = function(
  filename = 'open_citation_collection.zip', collectionId = 4586573,
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
#' command `unzip`, available by default on most Unix systems. This function
#' should not normally be called directly, as it is called by
#' [modifyPubmedDb()].
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
#' @param checkMd5 Logical indicating whether to download the citation file if
#'   the MD5 sums of the local and remote versions do not match. This should not
#'   normally be changed from the default.
#'
#' @return If `con` is `NULL`, the function returns a data.table with columns
#'   `citing_pmid` and `cited_pmid`. Beware this is a large table and could
#'   swamp the machine's memory. If `con` is not `NULL`, the function returns
#'   `NULL` invisibly.
#'
#' @examples
#' \dontrun{
#' dCitation = getCitation('.')
#' }
#'
#' @seealso [parsePmidStatus()], [modifyPubmedDb()]
#'
#' @export
getCitation = function(
  localDir, filename = 'open_citation_collection.zip', nrows = Inf,
  tableSuffix = NULL, overwrite = FALSE, con = NULL, checkMd5 = TRUE) {

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

  citationInfo = getCitationInfo()
  md5Remote = citationInfo$supplied_md5

  if (md5Database == md5Remote) {
    message('Citation table in database is already up-to-date.')
    return(invisible())}

  if (file.exists(filepath)) {
    md5Local = tools::md5sum(filepath)
  } else {
    md5Local = ''}

  if (md5Local != md5Remote && isTRUE(checkMd5)) {
    utils::download.file(citationInfo$download_url, filepath, mode = 'wb')
    md5Local = tools::md5sum(filepath)
    if (md5Local != md5Remote) {
      stop('Supplied and computed MD5 checksums do not match.')}}

  cmdHead = if (nrows < Inf) sprintf('| head -n %d', nrows + 1L) else ''
  dCols = data.table(old = c('citing', 'referenced'),
                     new = c('citing_pmid', 'cited_pmid'))

  if (is.null(con)) {
    if (tools::file_ext(filepath) == 'zip') {
      dCitation = data.table::fread(
        cmd = sprintf('unzip -p %s %s', filepath, cmdHead))
    } else {
      dCitation = data.table::fread(filepath, nrows = nrows)}
    setnames(dCitation, dCols$old, dCols$new)
    return(dCitation)}

  # unzip file if necessary, since unark chokes on zip files
  # use system unzip, since internal method truncates files >= 4GB pre-comp
  # use head, since unark only knows how to unarchive an entire file

  if (tools::file_ext(filepath) == 'zip') {
    filepathTmp = tempfile()
    withr::local_file(filepathTmp)
    cmd = sprintf('unzip -p %s %s > %s', filepath, cmdHead, filepathTmp)
    system(cmd)
  } else {
    if (nrows < Inf) {
      filepathTmp = tempfile()
      withr::local_file(filepathTmp)
      system(sprintf('head -n %d %s > %s', nrows + 1L, filepath, filepathTmp))
    } else {
      filepathTmp = filepath}}

  # unark the file into the db, make sure columns are integers
  arkdb::unark(
    filepathTmp, db_con = con, streamable_table = arkdb::streamable_readr_csv(),
    lines = 1e7, overwrite = overwrite, tablenames = citationName,
    col_types = readr::cols(citing = 'i', referenced = 'i'))

  # change column names in citation table
  for (i in 1:nrow(dCols)) {
    q = sprintf('alter table %s rename column %s to %s', citationName,
                dCols[i]$old, dCols[i]$new)
    x = DBI::dbExecute(con, q)}

  # add citation_version table
  dVersion = data.table(
    md5_computed = md5Remote,
    pmparser_version = getPkgVersion(),
    datetime_processed = Sys.time())

  # if we make it to this point, set overwrite to TRUE
  DBI::dbWriteTable(con, versionName, dVersion, overwrite = TRUE)
  invisible()}
