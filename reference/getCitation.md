# Get public-domain citation data

Get the latest version of the NIH Open Citation Collection from figshare
[here](https://nih.figshare.com/collections/iCite_Database_Snapshots_NIH_Open_Citation_Collection_/4586573),
and optionally write it to the database. This function requires the
shell command `unzip`, available by default on most Unix systems. This
function should not normally be called directly, as it is called by
[`modifyPubmedDb()`](https://pmparser.hugheylab.org/reference/modifyPubmedDb.md).

## Usage

``` r
getCitation(
  localDir,
  filename = "open_citation_collection.zip",
  nrows = Inf,
  tableSuffix = NULL,
  overwrite = FALSE,
  con = NULL,
  checkMd5 = TRUE
)
```

## Arguments

- localDir:

  String indicating path to directory containing the citation file or to
  which the citation file should be downloaded.

- filename:

  String indicating name of the citation file. This should not normally
  be changed from the default.

- nrows:

  Number indicating how many rows of the citation file to read. This
  should not normally be changed from the default.

- tableSuffix:

  String indicating suffix, if any, to append to the table name.

- overwrite:

  Logical indicating whether to overwrite an existing table.

- con:

  Connection to the database, created using
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

- checkMd5:

  Logical indicating whether to download the citation file if the MD5
  sums of the local and remote versions do not match. This should not
  normally be changed from the default.

## Value

If `con` is `NULL`, the function returns a data.table with columns
`citing_pmid` and `cited_pmid`. Beware this is a large table and could
swamp the machine's memory. If `con` is not `NULL`, the function returns
`NULL` invisibly.

## See also

[`parsePmidStatus()`](https://pmparser.hugheylab.org/reference/parseElement.md),
[`modifyPubmedDb()`](https://pmparser.hugheylab.org/reference/modifyPubmedDb.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dCitation = getCitation('.')
} # }
```
