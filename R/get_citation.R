getCitationInfo = function(
  filename = 'open_citation_collection.zip', collectionId = 4586573,
  baseUrl = 'https://api.figshare.com/v2') {

  published_date = name = NULL

  #getting content from icite collection
  iciteUrl = glue('{baseUrl}/collections/{collectionId}/articles')
  iciteJson = data.table(jsonlite::fromJSON(iciteUrl))

  #extracting content from most recent icite snapshot
  latestId = iciteJson[published_date == max(published_date)]$id
  latestUrl = glue('{baseUrl}/articles/{latestId}/files')
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

  os = getOS()
  filenameNoExt = substr(filename, 1, nchar(filename) - 4)
  path = file.path(localDir, filename)
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

  if (file.exists(path)) {
    md5Local = tools::md5sum(path)
  } else {
    md5Local = ''}

  if (md5Local != md5Remote && isTRUE(checkMd5)) {
    timmy = getOption('timeout')
    options(timeout = 60 * 60)
    utils::download.file(citationInfo$download_url, path, mode = 'wb')
    options(timeout = timmy)

    md5Local = tools::md5sum(path)
    if (md5Local != md5Remote) {
      stop('Supplied and computed MD5 checksums do not match.')}}
  pathTmp = pathTmp = tempfile()
  withr::local_file(pathTmp)
  pathTmpCsv = file.path(
    pathTmp, paste0(filenameNoExt, '.csv'),
    fsep = if (os == 'Windows') '\\' else .Platform$file.sep)
  pathTmpCsv2 = file.path(
    pathTmp, paste0(filenameNoExt, '_tmp.csv'),
    fsep = if (os == 'Windows') '\\' else .Platform$file.sep)
  cmdHead = if (nrows < Inf) {
    if (os != 'Windows') {
      glue('| head -n {nrows + 1L}')
    } else {
      glue(
        ' \r\n powershell -command "Get-Content -Path {pathTmpCsv} -TotalCount',
        ' {nrows + 1L} | Set-Content {pathTmpCsv2}" ',
        ' \r\n powershell -command "Remove-Item {pathTmpCsv}"',
        ' \r\n powershell -command "Rename-Item {pathTmpCsv2} {pathTmpCsv}"')}
  } else {
    ''}
  dCols = data.table(
    old = c('citing', 'referenced'), new = c('citing_pmid', 'cited_pmid'))

  if (is.null(con)) {
    if (tools::file_ext(path) == 'zip') {
      cmd = if (os != 'Windows') {
        glue('unzip -p {path} {cmdHead}')
        } else {
          system(glue('powershell -command "Expand-Archive -Force {path} {pathTmp}"'))
          glue('powershell -command "Get-Content -Path {pathTmpCsv}',
               if (nrows < Inf) ' -TotalCount {nrows + 1L}"' else '"')}
      dCitation = data.table::fread(
        cmd = cmd)
    } else {
      dCitation = data.table::fread(path, nrows = nrows)}
    setnames(dCitation, dCols$old, dCols$new)
    return(dCitation)}

  # unzip file, since unark chokes on zip files and chunking is more efficient
  # use system unzip, since internal method truncates files >= 4GB pre-comp
  # use head, since unark only knows how to unarchive an entire file

  if (tools::file_ext(path) == 'zip') {
    withr::local_file(pathTmp)
    cmd = if (os != 'Windows') {
        glue('unzip -p {path} {cmdHead} > {pathTmp}')
      } else {
        glue('powershell -command "Expand-Archive -Force {path} {pathTmp}" {cmdHead}')}
    system(cmd)
    if (os == 'Windows') pathTmp = pathTmpCsv
  } else {
    if (nrows < Inf) {
      pathTmp = tempfile()
      withr::local_file(pathTmp)
      cmd = if (os != 'Windows') glue('head -n {nrows + 1L} {path} > {pathTmp}') else cmdHead
      system(cmd)
      if (os == 'Windows') pathTmp = pathTmpCsv
    } else {
      pathTmp = path}}

  writeTableInChunks(
    path = pathTmp, con = con, nRowsPerChunk = 1e7, overwrite = overwrite,
    tableName = citationName)

  # unark the file into the db, make sure columns are integers
  # arkdb::unark(
  #   pathTmp, db_con = con, streamable_table = arkdb::streamable_vroom(),
  #   lines = 1e7, overwrite = overwrite, tablenames = citationName,
  #   col_types = vroom::cols(citing = 'i', referenced = 'i'))

  # change column names in citation table
  for (i in seq_len(nrow(dCols))) {
    q = glue_sql('alter table {`citationName`} rename column \\
                 {`dCols[i]$old`} to {`dCols[i]$new`}', .con = con)
    x = DBI::dbExecute(con, q)}

  # add citation_version table
  dVersion = data.table(
    md5_computed = md5Remote,
    pmparser_version = getPkgVersion(),
    datetime_processed = Sys.time())

  # if we make it to this point, set overwrite to TRUE
  DBI::dbWriteTable(con, versionName, dVersion, overwrite = TRUE)
  invisible()}
