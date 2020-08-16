#!/bin/bash

# crontab:
# 0 0 1 1 * ./pubmed/scripts/create_and_dump.sh

cd pubmed
Rscript scripts/wrap_modify_pubmed_db.R create &&
Rscript scripts/dump_pubmed_db.R
