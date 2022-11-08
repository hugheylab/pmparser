parsePerson = function(pmXml, dPmid, con = NULL, tableSuffix = NULL,
                       personType = c('author', 'investigator')) {

  . = pmid = .N = equal_contrib = collective_name = person_pos  =
    affiliation_pos = affiliation = affil_idx = person_idx = n_affil_ids =
    n_person_ids = n_total_ids = id_pos = NULL

  stopifnot(length(pmXml) == nrow(dPmid))
  personType = match.arg(personType)
  personPre = paste0(toupper(substring(personType, 1, 1)),
                     substring(personType, 2))
  personPos = glue('{personType}_pos')

  dPmidNow = dPmid[, .(pmid, version)] # add filename back at the end
  cols = c(colnames(dPmidNow), 'person_pos')
  ai = as.integer()
  ac = as.character()

  # get persons
  x2 = xml_find_all(pmXml, glue('.//{personPre}'))
  x3 = xml_find_first(pmXml, glue('.//{personPre}List'))
  nPersons = xml_length(x3)
  # above line may break if InvestigatorList gets an identifier

  dPerson = data.table(
    dPmidNow[rep.int(seq_len(.N), nPersons)],
    last_name = xml_text(xml_find_first(x2, './/LastName')),
    fore_name = xml_text(xml_find_first(x2, './/ForeName')),
    initials = xml_text(xml_find_first(x2, './/Initials')),
    suffix = xml_text(xml_find_first(x2, './/Suffix')),
    valid = xml_attr(x2, 'ValidYN'))

  if (personType == 'author') {
    dPerson[, equal_contrib := xml_attr(x2, 'EqualContrib')]
    dPerson[, collective_name :=
              xml_text(xml_find_first(x2, './/CollectiveName'))]}

  if (nrow(dPerson) > 0) {
    dPerson[, person_pos := seq_len(.N), by = pmid]
  } else {
    dPerson[, person_pos := ai]}
  setcolorder(dPerson, cols)

  # get affiliations
  x4 = xml_find_all(x2, './/Affiliation', flatten = FALSE)
  nAffiliations = sapply(x4, length)

  if (length(nAffiliations) > 0 && sum(nAffiliations) > 0) {
    dAffil = dPerson[rep.int(seq_len(nrow(dPerson)), nAffiliations), cols, with = FALSE]
    dAffil[, affiliation_pos := seq_len(.N), by = cols]
    dAffil[, affiliation := unlist(lapply(x4, xml_text))]
  } else {
    dAffil = data.table(
      pmid = ai, version = ai, person_pos = ai, affiliation_pos = ac,
      affiliation = ac)}

  # get affiliation identifiers
  # have to know which identifier belongs to which affiliation
  x5 = xml_find_all(x2, './/AffiliationInfo')
  x6 = xml_find_all(x5, './/Identifier', flatten = FALSE)
  nAffilIds = sapply(x6, length)

  if (length(nAffilIds) > 0) {
    dAffilId = data.table(
      affil_idx = rep.int(seq_len(length(x6)), nAffilIds),
      source = unlist(lapply(x6, function(x) xml_attr(x, 'Source'))),
      identifier = unlist(lapply(x6, xml_text)))

    dAffil[, affil_idx := seq_len(.N)]
    dAffilId = merge(
      dAffilId, dAffil[, c('affil_idx', cols, 'affiliation_pos'), with = FALSE],
      by = 'affil_idx')

    dAffil[, affil_idx := NULL]
    dAffilId[, affil_idx := NULL]
    setcolorder(dAffilId, c(colnames(dPmidNow), 'person_pos', 'affiliation_pos',
                            'source', 'identifier'))
  } else {
    dAffilId = data.table(
      pmid = ai, version = ai, person_pos = ai, affiliation_pos = ai,
      source = ac, identifier = ac)}

  # get person identifiers
  # have to exclude affiliation identifiers
  # currently just orcid, so could be much simpler, but this should be robust
  x7 = xml_find_all(x2, './/Identifier', flatten = FALSE)
  nTotalIds = sapply(x7, length)

  dEmpty = data.table(
    pmid = ai, version = ai, person_pos = ai, source = ac, identifier = ac)

  if (length(nTotalIds) > 0 && sum(nTotalIds) > 0) {
    dAllId = data.table(
      person_idx = rep.int(seq_len(length(x7)), nTotalIds),
      source = unlist(lapply(x7, function(x) xml_attr(x, 'Source'))),
      identifier = unlist(lapply(x7, xml_text)))

    dPerson[, person_idx := seq_len(.N)]
    dAllId = merge(
      dAllId, dPerson[, c('person_idx', cols), with = FALSE], by = 'person_idx')

    dPerson[, person_idx := NULL]
    dAllId[, person_idx := NULL]

    x8 = dAffilId[, .(n_affil_ids = .N), by = cols]
    x9 = dAllId[, .(n_total_ids = .N), by = cols]
    x10 = merge(x9, x8, by = cols, all.x = TRUE)
    x10[is.na(n_affil_ids), n_affil_ids := 0]
    x10[, n_person_ids := n_total_ids - n_affil_ids]

    if (nrow(x10) > 0) {
      x11 = x10[n_person_ids > 0, .(id_pos = 1:n_person_ids), by = cols]

      dAllId[, id_pos := seq_len(.N), by = cols]
      dPersonId = merge(dAllId, x11, by = c(cols, 'id_pos'))
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

  if (personType == 'author') {
    tableName = paste_('author_list', tableSuffix)
    r[[tableName]] = data.table(
      dPmidNow[nPersons > 0],
      complete = xml_attr(x3[nPersons > 0], 'CompleteYN'))}

  for (i in seq_len(length(r))) {
    # change colnames based on personType
    setnames(r[[i]], 'person_pos', personPos, skip_absent = TRUE)
    # possibly add xml_filename as column
    setColumn(r[[i]], dPmid$xml_filename[1L])
    setcolorder(r[[i]], colnames(dPmid))
    # possibly append to db
    appendTable(con, names(r)[i], r[[i]])}

  return(r)}


#' @rdname parseElement
#' @export
parseAuthor = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  parsePerson(pmXml, dPmid, con, tableSuffix, personType = 'author')}


#' @rdname parseElement
#' @export
parseInvestigator = function(pmXml, dPmid, con = NULL, tableSuffix = NULL) {
  parsePerson(pmXml, dPmid, con, tableSuffix, personType = 'investigator')}
