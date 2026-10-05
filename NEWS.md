# fullmetaphone 0.1.0

First CRAN release. The package grew out of `cagmetaphone`, written by
Atharv Tyagi.

* `double_metaphone()` gives full-length primary and secondary Double
  Metaphone codes. It now takes whole vectors and returns a data frame.
* New `metaphone()` for the original single-code Metaphone algorithm.
* New `sounds_like()` to compare two vectors of names.
* New data set `us_surnames`, the 5000 most common surnames in the 2010
  United States Census.
* New vignette with worked examples of de-duplication, record linkage
  and name search.
* New `max_length` argument to set the code length. The default stays
  at 32 characters, as in the old version. Use `Inf` for no limit or `4`
  for the traditional short codes.
* New `by_word` argument to code each word of a name separately.
* Missing values now give `NA` instead of empty strings.
* Accented Latin letters are read as plain letters instead of being
  dropped.
* Spaces between words are kept while coding, as in the original
  algorithm, so rules such as "San Jacinto" and "Van" now work.
* Fixed a bug that coded a double L twice in names such as "Cabrillo"
  and "Gallegos".
