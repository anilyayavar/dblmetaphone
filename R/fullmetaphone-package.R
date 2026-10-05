#' fullmetaphone: Full-length Metaphone and Double Metaphone codes
#'
#' Phonetic codes give names that sound alike the same code, even when
#' they are spelt differently. This package provides
#'
#' * [metaphone()], the original Metaphone algorithm (Philips 1990),
#' * [double_metaphone()], the improved Double Metaphone algorithm
#'   (Philips 2000), with a primary and a secondary code, and
#' * [sounds_like()], which compares two sets of names using either one.
#'
#' Codes are returned in full. Most other implementations cut them to four
#' characters, which makes many different long names look the same. Use
#' `max_length = 4` when the traditional short codes are needed.
#'
#' @section Credits:
#' Lawrence Philips designed both algorithms and published the original
#' code. The Double Metaphone rules here are translated from the C
#' implementation by Maurice Aubrey, which includes fixes by Kevin
#' Atkinson. The same C code is used in the 'PGRdup' package, whose
#' authors made Double Metaphone available in R and inspired this
#' package. The Metaphone rules and the reference test data come from
#' Apache Commons Codec. See the file `COPYRIGHTS` in the installed
#' package for details.
#'
#' @keywords internal
"_PACKAGE"
