# pmparser 0.0.0.9033
* Suppress irrelevant warnings from future.

# pmparser 0.0.0.9032
* `%dopar%` loops in other places now work with doFuture.

# pmparser 0.0.0.9031
* `%dopar%` loops in `getPubmedFiles` now work with doFuture.
* Use updated version of withr on CRAN.

# pmparser 0.0.0.9030
* Renamed internal functions for consistency and clarity.

# pmparser 0.0.0.9029
* `getCitation` now uses `fread` to write table in chunks, ~3.3x faster.
* Switched from `dbAppendTable` to `dbWriteTable(..., append = TRUE)` for inexplicable speed increase.

# pmparser 0.0.0.9028
* `getCitation` now uses `vroom` for `arkdb::unark`.
* Moved DBI driver packages to Suggests to reduce dependencies.

# pmparser 0.0.0.9027
* Removed obsolete copyright column from the abstract table. 

# pmparser 0.0.0.9026
* `parseMesh` now returns an additional table containing the IndexingMethod attribute.

# pmparser 0.0.0.9025
* Fixed rare case of a parsing function returning a data.table with `NA` in columns instead of a data.table with no rows.

# pmparser 0.0.0.9024
* Switched to using `glue` under the hood.
