nCores = future::availableCores() - 2L
pg = pmparser::getPgParams()

dumpDir = 'dumps'
if (!dir.exists(dumpDir)) dir.create(dumpDir)

dumpName = sprintf('%s_%s', pg$database, format(Sys.time(), '%Y%m%d_%H%M%S'))

args = c('-Fd', pg$database, '-j', nCores, '-f', file.path(dumpDir, dumpName),
         '-h', pg$hostname, '-U', pg$username)

system2('pg_dump', args)
