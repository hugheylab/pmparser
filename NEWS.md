# pmparser 1.0.10
* Added import for `curl::curl()` to circumvent check NOTE.

# pmparser 1.0.9
* Clarified error message when downloading PubMed files.
* Skip testing of file downloads on CRAN.

# pmparser 1.0.8
* Increased timeout for downloading citation file.
* Reordered steps to check citation table even if other tables are up-to-date.
* Update to match lab style standard.

# pmparser 1.0.7
* Explicitly added `curl` to Imports, since it's used by `utils::download.file()`.
* Set "HUGE" option for `xml2::read_xml()` to avoid occasional error.

# pmparser 1.0.6
* Updated styling based on linter.

# pmparser 1.0.5
* Added `abstract_pos` column to abstract table.
* If `pub_year` in journal table is empty, now extracts `pub_year` from `medline_date`.

# pmparser 1.0.4
* Updated test standards for latest PubMed XML files.
* Returned to `doParallel` in scripts for simplicity.

# pmparser 1.0.3
* Simplified `xml2` dependency, since new version is now on CRAN.

# pmparser 1.0.2
* Fixed windows compatibility.

# pmparser 1.0.1
* Revised code to not need `globalVariables()` in order to pass R CMD check without notes. 

# pmparser 1.0.0
* Updated tests.

# pmparser 0.0.0.9036
* Added data dictionary as a vignette and a table in the database.

# pmparser 0.0.0.9035
* Remove rare, obnoxious unicode character from keyword name.

# pmparser 0.0.0.9034
* Explicitly set `doFuture` chunking for all dopar loops.

# pmparser 0.0.0.9033
* Suppress irrelevant warnings from `future`.

# pmparser 0.0.0.9032
* `%dopar%` loops in other places now work with doFuture.

# pmparser 0.0.0.9031
* `%dopar%` loops in `getPubmedFiles()` now work with doFuture.
* Use updated version of `withr` on CRAN.

# pmparser 0.0.0.9030
* Renamed internal functions for consistency and clarity.

# pmparser 0.0.0.9029
* `getCitation()` now uses `data.table::fread()` to write table in chunks, ~3.3x faster.
* Switched from `dbAppendTable()` to `dbWriteTable(..., append = TRUE)` for inexplicable speed increase.

# pmparser 0.0.0.9028
* `getCitation()` now uses `vroom` for `arkdb::unark()`.
* Moved DBI driver packages to Suggests to reduce dependencies.

# pmparser 0.0.0.9027
* Removed obsolete copyright column from the abstract table. 

# pmparser 0.0.0.9026
* `parseMesh()` now returns an additional table containing the IndexingMethod attribute.

# pmparser 0.0.0.9025
* Fixed rare case of a parsing function returning a data.table with `NA` in columns instead of a data.table with no rows.

# pmparser 0.0.0.9024
* Switched to using `glue` under the hood.
