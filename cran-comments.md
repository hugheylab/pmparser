## R CMD check results

### Local

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

### R-hub

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

## Changes from current CRAN release

A test was failing because PubMed updated the README.txt file that pmparser downloads and inserts as a one-row table in the database it creates, which meant the resulting table (called readme) was different than the expected table based on the previous README.txt. The solution was to update the test standards based on the new README.txt.
