## Submission

This is a new package.

## Test environments

* Local Windows 11, R 4.6.1
* win-builder, R-devel (2026-09-30 r90605 ucrt)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
* "Metaphone" is flagged as possibly misspelled. It is the name of the
  phonetic algorithm that the package implements, and is spelled
  correctly.

## Notes for the reviewer

* Lawrence Philips designed the algorithms. Maurice Aubrey and Kevin
  Atkinson wrote the C code whose rules are translated into R here, and
  are listed as contributors. The Metaphone rules and the test data
  come from Apache Commons Codec (Apache License 2.0), so The Apache
  Software Foundation is listed as a copyright holder. The us_surnames
  data come from the United States Census Bureau and are in the public
  domain. Details are in inst/COPYRIGHTS.
* Philips (2000) is cited with a link to an archived copy, because the
  original magazine website is no longer online. Philips (1990) is a
  magazine article with no DOI, ISBN or online copy, so it is cited by
  journal, volume, issue and pages.
