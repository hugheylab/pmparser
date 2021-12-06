# library('doFuture')
#
# registerDoFuture()
# nCores = round(availableCores() * 0.75)
# plan(multisession, workers = nCores)
nCores = round(parallel::detectCores() * 0.75)
doParallel::registerDoParallel(cores = nCores)
data.table::setDTthreads(nCores)

mode = commandArgs(TRUE)[1L]

pm = yaml::read_yaml(file.path('scripts', 'wrap_modify_pubmed_db.yml'))
pg = pmparser::getPgParams()

args = c(pm, mode = mode, dbname = pg$database,
         host = pg$hostname, user = pg$username)

do.call(pmparser::modifyPubmedDb, args)
