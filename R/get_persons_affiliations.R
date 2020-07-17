#' @export
getPersonsAffiliations = function(pmXml, pmids, con, tableSuffix = '',
                                  personType = c('author', 'investigator')) {
  personType = match.arg(personType)
  personPre = paste0(toupper(substring(personType, 1, 1)),
                     substring(personType, 2))
  personPos = sprintf('%s_pos', personType)

  tableNames = c(sprintf('%ss', personType),
                 sprintf('%s_affiliations', personType),
                 sprintf('%s_identifiers', personType),
                 sprintf('%s_affiliation_identifiers', personType))

  # get persons
  x2 = xml_find_all(pmXml, sprintf('.//%s', personPre))
  nPersons = xml_length(xml_find_first(pmXml, sprintf('.//%sList', personPre)))
  # above line may break if InvestigatorList gets an identifier

  dPersons = data.table(
    pmid = rep.int(pmids, nPersons),
    last_name = xml_text(xml_find_first(x2, './/LastName')),
    fore_name = xml_text(xml_find_first(x2, './/ForeName')),
    initials = xml_text(xml_find_first(x2, './/Initials')),
    suffix = xml_text(xml_find_first(x2, './/Suffix')))

  if (personType == 'author') {
    dPersons[, collective_name := xml_text(xml_find_first(x2, './/CollectiveName'))]}

  dPersons[, person_pos := 1:.N, by = pmid]
  setcolorder(dPersons, c('pmid', 'person_pos'))

  # get affiliations
  x4 = xml_find_all(x2, './/Affiliation', flatten = FALSE)
  nAffiliations = sapply(x4, length)

  dAffils = dPersons[rep.int(1:nrow(dPersons), nAffiliations),
                     .(pmid, person_pos)]
  dAffils[, affiliation_pos := 1:.N, by = .(pmid, person_pos)]
  dAffils[, affiliation := unlist(lapply(x4, xml_text))]

  # get affiliation identifiers
  # have to know which identifier belongs to which affiliation
  x5 = xml_find_all(x2, './/AffiliationInfo')
  x6 = xml_find_all(x5, './/Identifier', flatten = FALSE)
  nAffilIds = sapply(x6, length)

  dAffilIds = data.table(
    affil_idx = rep.int(1:length(x6), nAffilIds),
    source = unlist(lapply(x6, function(x) xml_attr(x, 'Source'))),
    identifier = unlist(lapply(x6, xml_text)))

  dAffils[, affil_idx := 1:.N]
  dAffilIds = merge(dAffilIds,
                    dAffils[, .(affil_idx, pmid, person_pos, affiliation_pos)],
                    by = 'affil_idx')
  dAffils[, affil_idx := NULL]
  dAffilIds[, affil_idx := NULL]
  setcolorder(dAffilIds, c('pmid', 'person_pos', 'affiliation_pos',
                           'source', 'identifier'))

  # get person identifiers
  # have to exclude affiliation identifiers
  # currently just orcid, so could be much simpler, but this should be robust
  x7 = xml_find_all(x2, './/Identifier', flatten = FALSE)
  nTotalIds = sapply(x7, length)

  dAllIds = data.table(
    person_idx = rep.int(1:length(x7), nTotalIds),
    source = unlist(lapply(x7, function(x) xml_attr(x, 'Source'))),
    identifier = unlist(lapply(x7, xml_text)))

  dPersons[, person_idx := 1:.N]
  dAllIds = merge(dAllIds, dPersons[, .(person_idx, pmid, person_pos)],
                  by = 'person_idx')

  dPersons[, person_idx := NULL]
  dAllIds[, person_idx := NULL]
  dEmpty = data.table(pmid = as.integer(), person_pos = as.integer(),
                      source = as.character(), identifier = as.character())

  if (nrow(dAllIds) > 0) {
    x8 = dAffilIds[, .(n_affil_ids = .N), by = .(pmid, person_pos)]
    x9 = dAllIds[, .(n_total_ids = .N), by = .(pmid, person_pos)]
    x10 = merge(x9, x8, by = c('pmid', 'person_pos'), all.x = TRUE)
    x10[is.na(n_affil_ids), n_affil_ids := 0]
    x10[, n_person_ids := n_total_ids - n_affil_ids]

    if (nrow(x10) > 0) {
      x11 = x10[n_person_ids > 0,
                .(id_pos = 1:n_person_ids),
                by = .(pmid, person_pos)]

      dAllIds[, id_pos := 1:.N, by = .(pmid, person_pos)]
      dPersonIds = merge(dAllIds, x11, by = c('pmid', 'person_pos', 'id_pos'))
      dPersonIds[, id_pos := NULL]

    } else {
      dPersonIds = dEmpty}

  } else {
    dPersonIds = dEmpty}

  # change colnames based on personType
  setnames(dPersons, 'person_pos', personPos, skip_absent = TRUE)
  setnames(dPersonIds, 'person_pos', personPos, skip_absent = TRUE)
  setnames(dAffils, 'person_pos', personPos, skip_absent = TRUE)
  setnames(dAffilIds, 'person_pos', personPos, skip_absent = TRUE)

  # append to db
  appendTable(con, paste0(tableNames[1L], tableSuffix), dPersons)
  appendTable(con, paste0(tableNames[2L], tableSuffix), dAffils)
  appendTable(con, paste0(tableNames[3L], tableSuffix), dPersonIds)
  appendTable(con, paste0(tableNames[4L], tableSuffix), dAffilIds)
  r = list(dPersons, dAffils, dPersonIds, dAffilIds)
  names(r) = tableNames
  return(r)}


#' @export
getAuthorsAffiliations = function(pmXml, pmids, con, tableSuffix = '') {
  getPersonsAffiliations(pmXml, pmids, con, tableSuffix, personType = 'author')}


#' @export
getInvestigatorsAffiliations = function(pmXml, pmids, con, tableSuffix = '') {
  getPersonsAffiliations(pmXml, pmids, con, tableSuffix,
                         personType = 'investigator')}

