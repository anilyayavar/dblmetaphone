# fullmetaphone 0.1.0

First CRAN release. The package grew out of `cagmetaphone`, written by
Atharv Tyagi.

* `double_metaphone()` gives full-length primary and secondary Double
  Metaphone codes. It now takes whole vectors and returns a data frame.
* New `metaphone()` for the original single-code Metaphone algorithm.
* New `sounds_like()` to compare two vectors of names.
* New `max_length` argument. The default keeps the full code. The old
  version cut codes at 32 characters.
* New `by_word` argument to code each word of a name separately.
* Missing values now give `NA` instead of empty strings.
* Accented Latin letters are read as plain letters instead of being
  dropped.
* Spaces between words are kept while coding, as in the original
  algorithm, so rules such as "San Jacinto" and "Van" now work.
* Fixed a bug that coded a double L twice in names such as "Cabrillo"
  and "Gallegos".
