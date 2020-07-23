#' @export
getPersonAffiliation = function(pmXml, pmids, filename = NULL, con = NULL,
                                tableSuffix = NULL,
                                personType = c('author', 'investigator')) {
  personType = match.arg(personType)
  personPre = paste0(toupper(substring(personType, 1, 1)),
                     substring(personType, 2))
  personPos = sprintf('%s_pos', personType)

  # get persons
  x2 = xml_find_all(pmXml, sprintf('.//%s', personPre))
  nPersons = xml_length(xml_find_first(pmXml, sprintf('.//%sList', personPre)))
  # above line may break if InvestigatorList gets an identifier

  dPerson = data.table(
    pmid = rep.int(pmids, nPersons),
    last_name = xml_text(xml_find_first(x2, './/LastName')),
    fore_name = xml_text(xml_find_first(x2, './/ForeName')),
    initials = xml_text(xml_find_first(x2, './/Initials')),
    suffix = xml_text(xml_find_first(x2, './/Suffix')))

  if (personType == 'author') {
    dPerson[, collective_name :=
              xml_text(xml_find_first(x2, './/CollectiveName'))]}

  dPerson[, person_pos := 1:.N, by = pmid]
  setcolorder(dPerson, c('pmid', 'person_pos'))

  # get affiliations
  x4 = xml_find_all(x2, './/Affiliation', flatten = FALSE)
  nAffiliations = sapply(x4, length)

  dAffil = dPerson[rep.int(1:nrow(dPerson), nAffiliations),
                   .(pmid, person_pos)]
  dAffil[, affiliation_pos := 1:.N, by = .(pmid, person_pos)]
  dAffil[, affiliation := unlist(lapply(x4, xml_text))]

  # get affiliation identifiers
  # have to know which identifier belongs to which affiliation
  x5 = xml_find_all(x2, './/AffiliationInfo')
  x6 = xml_find_all(x5, './/Identifier', flatten = FALSE)
  nAffilIds = sapply(x6, length)

  dAffilId = data.table(
    affil_idx = rep.int(1:length(x6), nAffilIds),
    source = unlist(lapply(x6, function(x) xml_attr(x, 'Source'))),
    identifier = unlist(lapply(x6, xml_text)))

  dAffil[, affil_idx := 1:.N]
  dAffilId = merge(dAffilId,
                   dAffil[, .(affil_idx, pmid, person_pos, affiliation_pos)],
                   by = 'affil_idx')
  dAffil[, affil_idx := NULL]
  dAffilId[, affil_idx := NULL]
  setcolorder(dAffilId, c('pmid', 'person_pos', 'affiliation_pos',
                          'source', 'identifier'))

  # get person identifiers
  # have to exclude affiliation identifiers
  # currently just orcid, so could be much simpler, but this should be robust
  x7 = xml_find_all(x2, './/Identifier', flatten = FALSE)
  nTotalIds = sapply(x7, length)

  dAllId = data.table(
    person_idx = rep.int(1:length(x7), nTotalIds),
    source = unlist(lapply(x7, function(x) xml_attr(x, 'Source'))),
    identifier = unlist(lapply(x7, xml_text)))

  dPerson[, person_idx := 1:.N]
  dAllId = merge(dAllId, dPerson[, .(person_idx, pmid, person_pos)],
                 by = 'person_idx')

  dPerson[, person_idx := NULL]
  dAllId[, person_idx := NULL]
  dEmpty = data.table(pmid = as.integer(), person_pos = as.integer(),
                      source = as.character(), identifier = as.character())

  if (nrow(dAllId) > 0) {
    x8 = dAffilId[, .(n_affil_ids = .N), by = .(pmid, person_pos)]
    x9 = dAllId[, .(n_total_ids = .N), by = .(pmid, person_pos)]
    x10 = merge(x9, x8, by = c('pmid', 'person_pos'), all.x = TRUE)
    x10[is.na(n_affil_ids), n_affil_ids := 0]
    x10[, n_person_ids := n_total_ids - n_affil_ids]

    if (nrow(x10) > 0) {
      x11 = x10[n_person_ids > 0,
                .(id_pos = 1:n_person_ids),
                by = .(pmid, person_pos)]

      dAllId[, id_pos := 1:.N, by = .(pmid, person_pos)]
      dPersonId = merge(dAllId, x11, by = c('pmid', 'person_pos', 'id_pos'))
      dPersonId[, id_pos := NULL]

    } else {
      dPersonId = dEmpty}

  } else {
    dPersonId = dEmpty}

  # change colnames based on personType
  setnames(dPerson, 'person_pos', personPos, skip_absent = TRUE)
  setnames(dPersonId, 'person_pos', personPos, skip_absent = TRUE)
  setnames(dAffil, 'person_pos', personPos, skip_absent = TRUE)
  setnames(dAffilId, 'person_pos', personPos, skip_absent = TRUE)

  # possibly add column for xml_filename
  setXmlFilename(dPerson, filename)
  setXmlFilename(dPersonId, filename)
  setXmlFilename(dAffil, filename)
  setXmlFilename(dAffilId, filename)

  # append to db
  tableNames = c(paste_(personType, tableSuffix),
                 paste_(personType, 'affiliation', tableSuffix),
                 paste_(personType, 'identifier', tableSuffix),
                 paste_(personType, 'affiliation_identifier', tableSuffix))

  appendTable(con, tableNames[1L], dPerson)
  appendTable(con, tableNames[2L], dAffil)
  appendTable(con, tableNames[3L], dPersonId)
  appendTable(con, tableNames[4L], dAffilId)
  r = list(dPerson, dAffil, dPersonId, dAffilId)
  names(r) = tableNames
  return(r)}


#' @export
getAuthorAffiliation = function(pmXml, pmids, filename = NULL, con = NULL,
                                tableSuffix = NULL) {
  getPersonAffiliation(pmXml, pmids, filename, con, tableSuffix,
                       personType = 'author')}


#' @export
getInvestigatorAffiliation = function(pmXml, pmids, filename = NULL, con = NULL,
                                      tableSuffix = NULL) {
  getPersonAffiliation(pmXml, pmids, filename, con, tableSuffix,
                       personType = 'investigator')}
