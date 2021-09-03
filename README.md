# pmparser

[![CircleCI](https://circleci.com/gh/hugheylab/pmparser.svg?style=shield)](https://circleci.com/gh/hugheylab/pmparser)
[![codecov](https://codecov.io/gh/hugheylab/pmparser/branch/master/graph/badge.svg)](https://codecov.io/gh/hugheylab/pmparser)

`pmparser` enables one to easily create and maintain a relational database of data from PubMed/MEDLINE. `pmparser` can download the publicly available XML files, parse them, incorporate PubMed's regular updates, and combine the data with the NIH Open Citation Collection. PMDB, our implementation of the database, is available to download on [Zenodo](https://doi.org/10.5281/zenodo.4008109). For a detailed description of `pmparser` and PMDB, check out the [article](https://doi.org/10.7717/peerj.11071).

## Installation

If you use RStudio, go to Tools -> Global Options... -> Packages -> Add... (under Secondary repositories), then enter:

- Name: hugheylab
- Url: https://hugheylab.github.io/drat/

You only have to do this once. Then you can install or update the package by entering:

```R
if (!requireNamespace('remotes', quietly = TRUE))
  install.packages('remotes')
remotes::install_github('r-lib/xml2')

if (!requireNamespace('BiocManager', quietly = TRUE))
  install.packages('BiocManager')
BiocManager::install('pmparser')
```

Alternatively, you can install or update the package by entering:

```R
if (!requireNamespace('remotes', quietly = TRUE))
  install.packages('remotes')
remotes::install_github('r-lib/xml2')

if (!requireNamespace('BiocManager', quietly = TRUE))
  install.packages('BiocManager')
BiocManager::install('pmparser', site_repository = 'https://hugheylab.github.io/drat/')
```

There's also a [docker image](https://hub.docker.com/r/hugheylab/hugheyverse), which has all dependencies installed.

```bash
docker pull hugheylab/hugheyverse
```

## Usage

See the examples and detailed guidance in the [reference documentation](https://pmparser.hugheylab.org/reference/index.html).
