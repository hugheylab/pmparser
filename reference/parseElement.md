# Parse elements from a PubMed XML file

Elements are parsed according to the MEDLINE®PubMed® XML Element
Descriptions and their Attributes
[here](https://www.nlm.nih.gov/bsd/licensee/elements_descriptions.html).
These functions should not normally be called directly, as they are
called by
[`modifyPubmedDb()`](https://pmparser.hugheylab.org/reference/modifyPubmedDb.md).

## Usage

``` r
parsePmidStatus(rawXml, filename, con = NULL, tableSuffix = NULL)

parseArticleId(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parseArticle(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parsePubHistory(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parseJournal(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parsePubType(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parseMesh(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parseKeyword(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parseGrant(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parseChemical(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parseDataBank(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parseComment(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parseAbstract(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parseOther(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parseAuthor(pmXml, dPmid, con = NULL, tableSuffix = NULL)

parseInvestigator(pmXml, dPmid, con = NULL, tableSuffix = NULL)
```

## Arguments

- rawXml:

  An xml document obtained by loading a PubMed XML file using
  [`xml2::read_xml()`](http://xml2.r-lib.org/reference/read_xml.md).

- filename:

  A string that will be added to a column `xml_filename`.

- con:

  Connection to the database, created using
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

- tableSuffix:

  String to append to the table names.

- pmXml:

  An xml nodeset derived from `rawXml`, such as that returned by
  `parsePmidStatus()`, where each node corresponds to a PMID.

- dPmid:

  A data.table with one row for each node of `pmXml`, should have
  columns `pmid`, `version`, and possibly `xml_filename`.

## Value

`parsePmidStatus()` returns a list of two objects. The first is an xml
nodeset in which each node corresponds to a PubmedArticle in the
`rawXml` object. The second is a data.table with columns `pmid`,
`version`, `xml_filename`, and `status`, in which each row corresponds
to a PubmedArticle in the `rawXml` object or a deleted pmid. The
`status` column is parsed from the DeleteCitation and MedlineCitation
sections.

The following functions return a data.table or list of data.tables with
columns from `dPmid` plus the columns specified.

`parseArticleId()`: a data.table with columns `id_type` and `id_value`,
parsed from the ArticleIdList section. Only `id_type`s "doi" and "pmc"
are retained.

`parseArticle()`: a data.table with columns `title`, `language`,
`vernacular_title`, `pub_model`, and `pub_date`, parsed from the Article
section.

`parsePubHistory()`: a data.table with columns `pub_status` and
`pub_date`, parsed from the History section.

`parseJournal()`: a data.table with columns `journal_name`,
`journal_iso`, `pub_date`, `pub_year`, `pub_month`, `pub_day`,
`medline_date`, `volume`, `issue`, and `cited_medium`, parsed from the
Journal section.

`parsePubType()`: a data.table with columns `type_name` and `type_id`,
parsed from the PublicationTypeList section.

`parseMesh()`: a list of three data.tables parsed mostly from the
MeshHeadingList section. The first has column `indexing_method` (parsed
from the MedlineCitation section), the second has columns
`descriptor_pos`, `descriptor_name`, `descriptor_ui`, and
`descriptor_major_topic`, the third has columns `descriptor_pos`,
`qualifier_name`, `qualifier_ui`, and `qualifier_major_topic`.

`parseKeyword()`: a list of two data.tables parsed from the KeywordList
section. The first has column `list_owner`, the second has columns
`keyword_name` and `major_topic`.

`parseGrant()`: a list of two data.tables parsed from the GrantList
section. The first has column `complete`, the second has columns
`grant_id`, `acronym`, `agency`, and `country`.

`parseChemical()`: a data.table with columns `registry_number`,
`substance_name`, and `substance_ui`, parsed from the ChemicalList
section.

`parseDataBank()`: a data.table with columns `data_bank_name` and
`accession_number`, parsed from the DataBankList section.

`parseComment()`: a data.table with columns `ref_type` and `ref_pmid`,
parsed from the CommentsCorrectionsList section.

`parseAbstract()`: a list of two data.tables parsed from the Abstract
section. The first has column `copyright`. The second has columns
`text`, `label`, and `nlm_category`.

`parseAuthor()`: a list of data.tables parsed from the AuthorList
section. The first is for authors and has columns `author_pos`,
`last_name`, `fore_name`, `initials`, `suffix`, `valid`,
`equal_contrib`, and `collective_name`. The second is for affiliations
and has columns `author_pos`, `affiliation_pos`, and `affiliation`. The
third is for author identifiers and has columns `author_pos`, `source`,
and `identifier`. The fourth is for author affiliation identifiers and
has columns `author_pos`, `affiliation_pos`, `source`, and `identifier`.
The fifth is for the author list itself and has a column `complete`.

`parseInvestigator()`: a list of data.tables similar to those returned
by `parseAuthor()`, except parsed from the InvestigatorList section,
with column names containing "investigator" instead of "author", and
where the first data.table lacks columns for `equal_contrib` and
`collective_name` and the fifth data.table does not exist.

`parseOther()`: a list of data.tables parsed from the OtherAbstract and
OtherID sections. The first has columns `text`, `type`, and `language`.
The second has columns `source` and `id_value`.

## See also

[`getCitation()`](https://pmparser.hugheylab.org/reference/getCitation.md),
[`modifyPubmedDb()`](https://pmparser.hugheylab.org/reference/modifyPubmedDb.md)

## Examples

``` r
library('data.table')
library('xml2')

filename = 'pubmed20n1016.xml.gz'
rawXml = read_xml(system.file('extdata', filename, package = 'pmparser'))

pmidStatusList = parsePmidStatus(rawXml, filename)
pmXml = pmidStatusList[[1L]]
dPmidRaw = pmidStatusList[[2L]]
dPmid = dPmidRaw[status != 'Deleted', !'status']

dArticleId = parseArticleId(pmXml, dPmid)
dArticle = parseArticle(pmXml, dPmid)
dJournal = parseJournal(pmXml, dPmid)
dPubType = parsePubType(pmXml, dPmid)
dPubHistory = parsePubHistory(pmXml, dPmid)
meshRes = parseMesh(pmXml, dPmid)
keywordRes = parseKeyword(pmXml, dPmid)
grantRes = parseGrant(pmXml, dPmid)
dChemical = parseChemical(pmXml, dPmid)
dDataBank = parseDataBank(pmXml, dPmid)
dComment = parseComment(pmXml, dPmid)
abstractRes = parseAbstract(pmXml, dPmid)
authorRes = parseAuthor(pmXml, dPmid)
investigatorRes = parseInvestigator(pmXml, dPmid)
otherRes = parseOther(pmXml, dPmid)
```
