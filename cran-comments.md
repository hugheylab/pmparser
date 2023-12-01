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

See results for [Windows](https://builder.r-hub.io/status/pmparser_1.0.18.tar.gz-cbd13e2f1d344d8f96f553da3b359393), [Ubuntu](https://builder.r-hub.io/status/pmparser_1.0.18.tar.gz-76519f3b70664ce1ba180286b009e7dd), and [Fedora](https://builder.r-hub.io/status/pmparser_1.0.18.tar.gz-e946f303031a4590a94f56a3dfdeb086).

### GitHub Actions

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

See results for Mac, Windows, and Ubuntu [here]().

## Changes from current CRAN release

* Fixed parsing of CitedMedium within Journal.
