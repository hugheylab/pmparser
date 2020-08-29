dumpPubmedDb = function(pg, nCores = 2L, dumpDir = 'dumps', tar = TRUE) {
  if (!dir.exists(dumpDir)) dir.create(dumpDir)
  dumpName = sprintf('%s_%s', pg$database, format(Sys.time(), '%Y%m%d_%H%M%S'))

  args = c('-Fd', pg$database, '-j', nCores, '-f', file.path(dumpDir, dumpName),
           '-h', pg$hostname, '-U', pg$username)
  system2('pg_dump', args)

  if (isTRUE(tar)) {
    # tar file for zenodo, must be untarred prior to pg_restore
    tarArgs = c('-cf', file.path(dumpDir, paste0(dumpName, '.tar')),
                '-C', dumpDir, dumpName)
    system2('tar', tarArgs)
    unlink(file.path(dumpDir, dumpName), recursive = TRUE)}}

pg = pmparser::getPgParams()
nCores = future::availableCores() - 2L

dumpPubmedDb(pg, nCores)
