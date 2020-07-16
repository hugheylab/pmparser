#function to read newest open citations snapshot to a data.table

getCitations = function(filePath = NULL){
  if (is.null(filePath)) {
    #api base
    baseUrl = 'https://api.figshare.com/v2'

    #getting content from iCite api
    iCiteUrl = paste(baseUrl, '/collections/4586573/articles', sep = '')
    iCiteJson = data.table::data.table(jsonlite::fromJSON(iCiteUrl))

    #extracting content from most recent iCite snapshot
    latestId = as.character(
      iCiteJson[published_date == max(published_date), id])
    latestUrl = paste(c(baseUrl, '/articles/', latestId, '/files'),
                       collapse = '')
    latestJson = data.table::data.table(jsonlite::fromJSON(latestUrl))

    #extracting download url for open_citation_collection.zip
    downloadUrl = latestJson[name == 'open_citation_collection.zip',
                               download_url]
    #downloading to temporary file
    downloadFile = tempfile(fileext = '.zip')
    download.file(downloadUrl, downloadFile, mode = 'wb')

    #preprocessing zip file with shell and freading to table
    citationsTbl = data.table::fread(cmd = paste(c('unzip -p', downloadFile),
                                      collapse = ' '))

    #deleting temp file
    unlink(downloadFile)

  } else { #in case we want to use a local file
    citationsTbl = ifelse(tools::file_ext(filePath) == 'zip',
                           data.table::fread(cmd = paste(c('unzip -p', filePath),
                                       collapse = ' ')),
                           data.table::fread(filePath))}

  setnames(citationsTbl, c('citing_pmid', 'cited_pmid'))

  return(citationsTbl)}

