# pmparser

[![hugheylab](https://circleci.com/gh/hugheylab/pmparser.svg?style=shield&circle-token=74de6617444786304273fe7efed2999a74b2e51a)](https://app.circleci.com/pipelines/github/hugheylab/pmparser)
[![codecov](https://codecov.io/gh/hugheylab/pmparser/branch/master/graph/badge.svg)](https://codecov.io/gh/hugheylab/pmparser)

`pmparser` enables one to easily create and maintain a relational database of data from PubMed/MEDLINE. `pmparser` can download the publicly available XML files, parse them, incorporate PubMed's regular updates, and combine the data with the NIH Open Citation Collection.

## Installation

If you use RStudio, go to Tools -> Global Options... -> Packages -> Add... (under Secondary repositories), then enter:

- Name: hugheylab
- Url: https://hugheylab.github.io/drat/

You only have to do this once. Then you can install or update the package by entering:

```R
if (!requireNamespace('BiocManager', quietly = TRUE))
  install.packages('BiocManager')

BiocManager::install('pmparser')
```

Alternatively, you can install or update the package by entering:

```R
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
