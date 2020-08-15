nCores = parallel::detectCores() - 2L
doParallel::registerDoParallel(cores = nCores)
data.table::setDTthreads(nCores)

mode = commandArgs(TRUE)[1L]

pm = yaml::read_yaml(file.path('scripts', 'wrap_modify_pubmed_db.yml'))
pg = pmparser::getPgParams()

args = c(pm, mode = mode, dbname = pg$database,
         host = pg$hostname, user = pg$username)

do.call(pmparser::modifyPubmedDb, args)
