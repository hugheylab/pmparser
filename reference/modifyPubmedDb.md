# Create or update a PubMed database

This function downloads PubMed/MEDLINE XML files, parses them, and adds
the information to the database, then downloads the NIH Open Citation
Collection and adds it to the database. Only the most recent version of
each PMID is retained. Parsing of XML files will use a parallel backend
if one is registered, such as with
[`doParallel::registerDoParallel()`](https://rdrr.io/pkg/doParallel/man/registerDoParallel.html).

## Usage

``` r
modifyPubmedDb(
  localDir,
  dbname,
  dbtype = c("postgres", "mariadb", "mysql", "sqlite"),
  nFiles = Inf,
  retry = TRUE,
  nCitations = Inf,
  mode = c("create", "update"),
  ...
)
```

## Arguments

- localDir:

  Directory in which to download the files from PubMed.

- dbname:

  Name of database.

- dbtype:

  Type of database, either 'postgres', 'mariadb', 'mysql', or 'sqlite'.
  Make sure to install the corresponding DBI driver package first:
  RPostgres, RMariaDB (for both 'mariadb' and 'mysql'), or RSQLite. Due
  to the large size of the database, SQLite is recommended only for
  small-scale testing.

- nFiles:

  Maximum number of xml files to parse that are not already in the
  database. This should not normally be changed from the default.

- retry:

  Logical indicating whether to retry parsing steps that fail.

- nCitations:

  Maximum number of rows of the citation file to read. This should not
  normally be changed from the default.

- mode:

  String indicating whether to create the database using the baseline
  files or to update the database using the update files.

- ...:

  Other arguments passed to
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

## Value

`NULL`, invisibly. Tab-delimited log files will be created in a logs
folder in `localDir`.

## See also

[`parsePmidStatus()`](https://pmparser.hugheylab.org/reference/parseElement.md),
[`getCitation()`](https://pmparser.hugheylab.org/reference/getCitation.md),
[`getPgParams()`](https://pmparser.hugheylab.org/reference/getPgParams.md)

## Examples

``` r
if (FALSE) { # \dontrun{
modifyPubmedDb('.', 'pmdb', mode = 'create')
} # }
```
