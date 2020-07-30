parsePersonAffiliation = function(pmXml, pmids, filename = NULL, con = NULL,
                                  tableSuffix = NULL,
                                  personType = c('author', 'investigator')) {
  personType = match.arg(personType)
  personPre = paste0(toupper(substring(personType, 1, 1)),
                     substring(personType, 2))
  personPos = sprintf('%s_pos', personType)

  ai = as.integer()
  ac = as.character()

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

  if (nrow(dPerson) > 0) {
    dPerson[, person_pos := 1:.N, by = pmid]
  } else {
    dPerson[, person_pos := ai]}
  setcolorder(dPerson, c('pmid', 'person_pos'))

  # get affiliations
  x4 = xml_find_all(x2, './/Affiliation', flatten = FALSE)
  nAffiliations = sapply(x4, length)

  if (length(nAffiliations) > 0 && sum(nAffiliations) > 0) {
    dAffil = dPerson[rep.int(1:nrow(dPerson), nAffiliations),
                     .(pmid, person_pos)]
    dAffil[, affiliation_pos := 1:.N, by = .(pmid, person_pos)]
    dAffil[, affiliation := unlist(lapply(x4, xml_text))]
  } else {
    dAffil = data.table(
      pmid = ai, person_pos = ai, affiliation_pos = ai, affiliation = ac)}

  # get affiliation identifiers
  # have to know which identifier belongs to which affiliation
  x5 = xml_find_all(x2, './/AffiliationInfo')
  x6 = xml_find_all(x5, './/Identifier', flatten = FALSE)
  nAffilIds = sapply(x6, length)

  if (length(nAffilIds) > 0) {
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
  } else {
    dAffilId = data.table(pmid = ai, person_pos = ai, affiliation_pos = ai,
                          source = ac, identifier = ac)}

  # get person identifiers
  # have to exclude affiliation identifiers
  # currently just orcid, so could be much simpler, but this should be robust
  x7 = xml_find_all(x2, './/Identifier', flatten = FALSE)
  nTotalIds = sapply(x7, length)

  dEmpty = data.table(pmid = ai, person_pos = ai, source = ac, identifier = ac)

  if (length(nTotalIds) > 0 && sum(nTotalIds) > 0) {
    dAllId = data.table(
      person_idx = rep.int(1:length(x7), nTotalIds),
      source = unlist(lapply(x7, function(x) xml_attr(x, 'Source'))),
      identifier = unlist(lapply(x7, xml_text)))

    dPerson[, person_idx := 1:.N]
    dAllId = merge(dAllId, dPerson[, .(person_idx, pmid, person_pos)],
                   by = 'person_idx')

    dPerson[, person_idx := NULL]
    dAllId[, person_idx := NULL]

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

  # possibly add xml_filename and append to db
  r = list(dPerson, dAffil, dPersonId, dAffilId)
  names(r) = c(paste_(personType, tableSuffix),
               paste_(personType, 'affiliation', tableSuffix),
               paste_(personType, 'identifier', tableSuffix),
               paste_(personType, 'affiliation_identifier', tableSuffix))

  for (i in 1:length(r)) {
    # change colnames based on personType
    setnames(r[[i]], 'person_pos', personPos, skip_absent = TRUE)
    # possibly add xml_filename as column
    setXmlFilename(r[[i]], filename)
    # possibly append to db
    appendTable(con, names(r)[i], r[[i]])}

  return(r)}


#' @rdname parseElement
#' @export
parseAuthorAffiliation = function(pmXml, pmids, filename = NULL, con = NULL,
                                  tableSuffix = NULL) {
  parsePersonAffiliation(pmXml, pmids, filename, con, tableSuffix,
                         personType = 'author')}


#' @rdname parseElement
#' @export
parseInvestigatorAffiliation = function(pmXml, pmids, filename = NULL,
                                        con = NULL, tableSuffix = NULL) {
  parsePersonAffiliation(pmXml, pmids, filename, con, tableSuffix,
                         personType = 'investigator')}
