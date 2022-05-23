# pmparser

[![check-deploy](https://github.com/hugheylab/pmparser/workflows/check-deploy/badge.svg)](https://github.com/hugheylab/pmparser/actions)
[![codecov](https://codecov.io/gh/hugheylab/pmparser/branch/master/graph/badge.svg)](https://codecov.io/gh/hugheylab/pmparser)
[![Netlify Status](https://api.netlify.com/api/v1/badges/865d8ac1-54df-47ee-ae71-f3f0226f324d/deploy-status)](https://app.netlify.com/sites/zealous-raman-c627d3/deploys)
[![CRAN Status](https://www.r-pkg.org/badges/version/pmparser)](https://cran.r-project.org/package=pmparser)
[![drat version](https://raw.githubusercontent.com/hugheylab/drat/gh-pages/badges/pmparser_drat_badge.svg)](https://github.com/hugheylab/drat/tree/gh-pages/src/contrib)

`pmparser` enables one to easily create and maintain a relational database of data from PubMed/MEDLINE. `pmparser` can download the publicly available XML files, parse them, incorporate PubMed's regular updates, and combine the data with the NIH Open Citation Collection. PMDB, our implementation of the database, is available on [Zenodo](https://doi.org/10.5281/zenodo.4008109) (as a PostgreSQL dump) and on [Google BigQuery](https://console.cloud.google.com/bigquery?project=pmdb-bq&d=pmdb). For a detailed description of `pmparser` and PMDB, check out the [article](https://doi.org/10.7717/peerj.11071).

## Installation

### Option 1: CRAN

```r
install.packages('pmparser')
```

### Option 2: Hughey Lab Drat Repository

1. Install [`BiocManager`](https://cran.r-project.org/package=BiocManager).

    ```r
    if (!requireNamespace('BiocManager', quietly = TRUE))
      install.packages('BiocManager')
    ```

1. If you use RStudio, go to Tools → Global Options... → Packages → Add... (under Secondary repositories), then enter:

    - Name: hugheylab
    - Url: https://hugheylab.github.io/drat/

    You only have to do this once. Then you can install or update the package by entering:

    ```r
    BiocManager::install('pmparser')
    ```

    Alternatively, you can install or update the package by entering:

    ```r
    BiocManager::install('pmparser', site_repository = 'https://hugheylab.github.io/drat/')
    ```

## Usage

See the examples and detailed guidance in the [reference documentation](https://pmparser.hugheylab.org/reference/index.html).
