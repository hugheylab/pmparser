#!/bin/bash

# crontab:
# 0 0 10 * * ./pubmed/scripts/update_and_dump.sh

cd pubmed
Rscript scripts/wrap_modify_pubmed_db.R update &&
Rscript scripts/dump_pubmed_db.R
