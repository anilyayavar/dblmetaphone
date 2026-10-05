#' fullmetaphone: Full-length Metaphone and Double Metaphone codes
#'
#' Phonetic codes give names that sound alike the same code, even when
#' they are spelled differently. This package provides
#'
#' * [metaphone()], the original Metaphone algorithm (Philips 1990),
#' * [double_metaphone()], the improved Double Metaphone algorithm
#'   (Philips 2000), with a primary and a secondary code,
#' * [sounds_like()], which compares two sets of names using either one,
#'   and
#' * [us_surnames], the 5000 most common surnames in the 2010 United
#'   States Census, for examples and testing.
#'
#' Codes keep up to 32 characters by default, which is the complete code
#' for practically every real name. Most other implementations cut them
#' to four characters, which makes many different long names look the
#' same. All functions take a `max_length` argument to change this. Use
#' `max_length = Inf` for no limit, or `max_length = 4` for the
#' traditional short codes.
#'
#' Start with `vignette("fullmetaphone")`, which explains the algorithms
#' and shows how to use the codes for de-duplication, record linkage and
#' name search.
#'
#' @section Credits:
#' Lawrence Philips designed both algorithms and published the original
#' code. The Double Metaphone rules here are translated from the C
#' implementation by Maurice Aubrey in the Perl module
#' \href{https://metacpan.org/pod/Text::DoubleMetaphone}{Text::DoubleMetaphone},
#' which includes fixes by Kevin Atkinson. The same C code is used in the
#' \href{https://CRAN.R-project.org/package=PGRdup}{'PGRdup'} package,
#' whose authors first made Double Metaphone available in R. The
#' Metaphone rules and the reference test data come from
#' \href{https://commons.apache.org/proper/commons-codec/}{Apache Commons Codec}.
#' See the file `COPYRIGHTS` in the installed package for details.
#'
#' @references
#' Philips, L. (1990). Hanging on the metaphone. *Computer Language*,
#' 7(12), 38-43.
#'
#' Philips, L. (2000). The double metaphone search algorithm. *C/C++
#' Users Journal*, 18(6), 38-43.
#' \url{https://web.archive.org/web/20250702064845/https://drdobbs.com/the-double-metaphone-search-algorithm/184401251}
#'
#' @keywords internal
"_PACKAGE"
