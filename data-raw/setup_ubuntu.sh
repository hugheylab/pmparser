#!/bin/bash

# bash -i pubmed/scripts/setup_ubuntu.sh # for conda deactivate and activate

# starting with hugheylab-ubuntu20
conda deactivate # to avoid errors when installing R packages
sudo apt update
sudo apt upgrade -y

# https://www.postgresql.org/download/linux/ubuntu/
sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
sudo apt update

# postgres versions on EC2 and RDS instances must match for pg_dump to work
sudo apt install -y postgresql-12 libpq-dev libmariadbclient-dev

# make sure xml2::xml_find_all has flatten argument
Rscript -e "install.packages('devtools')"
Rscript -e "devtools::install_github(c('r-lib/withr', 'r-lib/xml2'))"

# now that pmparser is on the lab's drat repo
Rscript -e "install.packages('BiocManager')"
Rscript -e "BiocManager::install('pmparser', site_repository = 'https://hugheylab.github.io/drat/', ask = FALSE)"

conda activate

# create ~/.pgpass file according to https://www.postgresql.org/docs/9.6/libpq-pgpass.html
