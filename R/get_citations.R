library(data.table)
library(jsonlite)
library(tools)

getCitations <- function(filePath = NULL){
  if (is.null(filePath)) {
    #api base
    baseUrl <- "https://api.figshare.com/v2"

    #getting content from iCite api
    iCiteUrl <- paste(baseUrl, "/collections/4586573/articles", sep = "")
    iCiteJson <- data.table(fromJSON(iCiteUrl))

    #extracting content from most recent iCite snapshot
    latestId <- as.character(
      iCiteJson[published_date == max(published_date), id])
    latestUrl <- paste(c(baseUrl, "/articles/", latestId, "/files"),
                       collapse = "")
    latestJson <- data.table(fromJSON(latestUrl))

    #extracting download url for open_citation_collection.zip
    downloadUrl <- latestJson[name == "open_citation_collection.zip",
                              download_url]
    #downloading to temporary file
    temp <- tempfile(fileext = ".zip")
    download.file(downloadUrl, temp, mode = "wb")

    #preprocessing zip file with shell and freading to table
    citationsTbl <- fread(cmd = paste(c("unzip -p", temp), collapse = " "))

  } else { #in case we want to use a local file
    citationsTbl <- ifelse(file_ext(filePath) == "zip",
                           fread(cmd = paste(c("unzip -p", filePath),
                                             collapse = " ")),
                           fread(filePath))}

  setnames(citationsTbl, c('citing_pmid', 'cited_pmid'))

  return(citationsTbl)
}
