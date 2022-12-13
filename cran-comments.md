## R CMD check results

### Local

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

### R-hub

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

## Changes from current CRAN release

All ftp URLs have been changed to https. In addition, tests were failing because PubMed updated the XML files, so the generated database tables were different than expected. The internet resources were still available and this was not affecting the functionality of the package itself. The test standards have now been updated accordingly, so the tests are passing locally, on [R-hub](https://builder.r-hub.io/status/original/pmparser_1.0.15.tar.gz-f3c5f8f8258d40f090186665198c4366), and on [GitHub Actions](https://github.com/hugheylab/pmparser/actions/workflows/check-deploy.yaml).
