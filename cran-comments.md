## R CMD check results

### Local

`devtools::check()`:

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

### R-hub

`devtools::check_rhub()`:

  0 errors ✔ | 0 warnings ✔ | 1 note ✖
  
  ❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

See results for [Windows](https://builder.r-hub.io/status/pmparser_1.0.16.tar.gz-7c295fe76dd64afdac4541030567c42f), [Ubuntu](https://builder.r-hub.io/status/pmparser_1.0.16.tar.gz-aa5fdb1f25ae49aabe4188b7cffba9ab), and [Fedora](https://builder.r-hub.io/status/pmparser_1.0.16.tar.gz-bf42f4c7ae324c94bcd7a0fb8ca9b8cc).

### GitHub Actions

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

See results for Mac, Windows, and Ubuntu [here]().

## Changes from current CRAN release

The test standards have been updated to include the latest version of the readme file on PubMed, which was the source of the test failures.
