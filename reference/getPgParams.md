# Get Postgres connection parameters

This is a helper function to get parameters from a .pgpass file. See
[here](https://www.postgresql.org/docs/9.6/libpq-pgpass.html) for
details.

## Usage

``` r
getPgParams(path = "~/.pgpass")
```

## Arguments

- path:

  Path to .pgpass file.

## Value

A data.table with one row for each set of parameters.

## See also

[`modifyPubmedDb()`](https://pmparser.hugheylab.org/reference/modifyPubmedDb.md)

## Examples

``` r
pg = getPgParams(system.file('extdata', 'pgpass', package = 'pmparser'))
```
