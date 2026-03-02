# Changelog

## pmparser 1.0.25

- Updated file format again for NIH Open Citation Collection.

## pmparser 1.0.24

CRAN release: 2026-02-05

- Updated test standards based on Jan 2026 update to PubMed XML files.

## pmparser 1.0.23

CRAN release: 2025-12-15

- Updated file format for NIH Open Citation Collection.

## pmparser 1.0.22

- Fixed quoting of indexing method for mesh_list.
- Deduplicated pub_type, at least within an XML file.

## pmparser 1.0.21

CRAN release: 2025-01-14

- Updated test standards based on Jan 2025 update to PubMed XML files.

## pmparser 1.0.20

CRAN release: 2024-01-13

- Updated readme table in test standards.

## pmparser 1.0.19

CRAN release: 2023-12-18

- Updated test standards based on the annual update to PubMed XML files.

## pmparser 1.0.18

CRAN release: 2023-12-01

- Fixed parsing of CitedMedium within Journal.

## pmparser 1.0.17

CRAN release: 2023-05-23

- Updated parsing of empty investigator fields.

## pmparser 1.0.16

CRAN release: 2023-02-12

- Updated readme table in test standards.

## pmparser 1.0.15

CRAN release: 2022-12-13

- Changed ftp URLs to https.
- Updated test standards based on new PubMed XML files.

## pmparser 1.0.14

- Added columns for `language` and `vernacular_title` to the article
  table.

## pmparser 1.0.13

CRAN release: 2022-11-24

- Added tables from the OtherAbstract and OtherID sections.

## pmparser 1.0.12

- Fixed issues found by lintr.

## pmparser 1.0.11

CRAN release: 2022-11-08

- Updated test standards based on PubMed’s updated README.txt.

## pmparser 1.0.10

CRAN release: 2022-04-27

- Added import for
  [`curl::curl()`](https://jeroen.r-universe.dev/curl/reference/curl.html)
  to circumvent check NOTE.

## pmparser 1.0.9

- Clarified error message when downloading PubMed files.
- Skip testing of file downloads on CRAN.

## pmparser 1.0.8

- Increased timeout for downloading citation file.
- Reordered steps to check citation table even if other tables are
  up-to-date.
- Update to match lab style standard.

## pmparser 1.0.7

- Explicitly added `curl` to Imports, since it’s used by
  [`utils::download.file()`](https://rdrr.io/r/utils/download.file.html).
- Set “HUGE” option for
  [`xml2::read_xml()`](http://xml2.r-lib.org/reference/read_xml.md) to
  avoid occasional error.

## pmparser 1.0.6

- Updated styling based on linter.

## pmparser 1.0.5

- Added `abstract_pos` column to abstract table.
- If `pub_year` in journal table is empty, now extracts `pub_year` from
  `medline_date`.

## pmparser 1.0.4

CRAN release: 2022-02-17

- Updated test standards for latest PubMed XML files.
- Returned to `doParallel` in scripts for simplicity.

## pmparser 1.0.3

- Simplified `xml2` dependency, since new version is now on CRAN.

## pmparser 1.0.2

- Fixed windows compatibility.

## pmparser 1.0.1

- Revised code to not need
  [`globalVariables()`](https://rdrr.io/r/utils/globalVariables.html) in
  order to pass R CMD check without notes.

## pmparser 1.0.0

- Updated tests.

## pmparser 0.0.0.9036

- Added data dictionary as a vignette and a table in the database.

## pmparser 0.0.0.9035

- Remove rare, obnoxious unicode character from keyword name.

## pmparser 0.0.0.9034

- Explicitly set `doFuture` chunking for all dopar loops.

## pmparser 0.0.0.9033

- Suppress irrelevant warnings from `future`.

## pmparser 0.0.0.9032

- `%dopar%` loops in other places now work with doFuture.

## pmparser 0.0.0.9031

- `%dopar%` loops in `getPubmedFiles()` now work with doFuture.
- Use updated version of `withr` on CRAN.

## pmparser 0.0.0.9030

- Renamed internal functions for consistency and clarity.

## pmparser 0.0.0.9029

- [`getCitation()`](https://pmparser.hugheylab.org/reference/getCitation.md)
  now uses
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
  to write table in chunks, ~3.3x faster.
- Switched from `dbAppendTable()` to `dbWriteTable(..., append = TRUE)`
  for inexplicable speed increase.

## pmparser 0.0.0.9028

- [`getCitation()`](https://pmparser.hugheylab.org/reference/getCitation.md)
  now uses `vroom` for `arkdb::unark()`.
- Moved DBI driver packages to Suggests to reduce dependencies.

## pmparser 0.0.0.9027

- Removed obsolete copyright column from the abstract table.

## pmparser 0.0.0.9026

- [`parseMesh()`](https://pmparser.hugheylab.org/reference/parseElement.md)
  now returns an additional table containing the IndexingMethod
  attribute.

## pmparser 0.0.0.9025

- Fixed rare case of a parsing function returning a data.table with `NA`
  in columns instead of a data.table with no rows.

## pmparser 0.0.0.9024

- Switched to using `glue` under the hood.
