## R CMD check results

### Local

`devtools::check()`:

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

### R-hub

`devtools::check_rhub()`:

  0 errors ✔ | 0 warnings ✔ | 2 notes ✖
  
  ❯ checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''
    
  ❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

See results for [Windows](https://builder.r-hub.io/status/pmparser_1.0.19.tar.gz-da4498bfdbad4e479b5cefc58b318ed9), [Ubuntu](https://builder.r-hub.io/status/pmparser_1.0.19.tar.gz-e3dc23cc69e1479d807cf853cfe1b3af), and [Fedora](https://builder.r-hub.io/status/pmparser_1.0.19.tar.gz-69efe91cf9a94f12bdf7f608674ec198).

### GitHub Actions

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

See results for Mac, Windows, and Ubuntu [here]().

## Changes from current CRAN release

* Updated test standards based on the annual update to PubMed XML files.
