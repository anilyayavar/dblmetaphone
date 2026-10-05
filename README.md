# fullmetaphone

Full-length Metaphone and Double Metaphone phonetic codes for R.

Phonetic codes give names that sound alike the same code, even when they
are spelled differently. They are useful for finding duplicate payees,
beneficiaries or vendors, and for linking records across files.

Most implementations, including `PGRdup::DoubleMetaphone()`, keep only
the first four characters of each code. That is too short for long
names. **fullmetaphone** returns the codes in full.

```r
library(fullmetaphone)

double_metaphone(c("Venkatesh", "Venkataraman", "Venkatraman"), max_length = 4)
#>   primary secondary
#> 1    FNKT      FNKT
#> 2    FNKT      FNKT
#> 3    FNKT      FNKT

double_metaphone(c("Venkatesh", "Venkataraman", "Venkatraman"))
#>   primary secondary
#> 1   FNKTX     FNKTX
#> 2 FNKTRMN   FNKTRMN
#> 3 FNKTRMN   FNKTRMN
```

With four characters all three names look alike. With full codes only
the two spellings of Venkataraman match.

## Installation

From GitHub, until the package is on CRAN.

```r
# install.packages("remotes")
remotes::install_github("anilyayavar/dblmetaphone")
```

## What it does

| Function | What it gives |
|---|---|
| `metaphone(x)` | One code per name, from the original Metaphone algorithm (Philips 1990). |
| `double_metaphone(x)` | A data frame with a primary and a secondary code per name, from Double Metaphone (Philips 2000). |
| `sounds_like(x, y)` | `TRUE` where two names share a code. |

All three take these arguments.

* `max_length` sets the longest code to keep. The default `Inf` keeps
  the full code. Use `4` for the traditional short codes.
* `by_word = TRUE` codes each word of a name separately, so
  "Ramesh Kumar Sharma" gives `"RMX KMR XRM"`.

```r
metaphone(c("Knight", "Night", "Agarwal", "Agrawal"))
#> [1] "NT"    "NT"    "AKRWL" "AKRWL"

sounds_like("Smith", c("Schmidt", "Smyth", "Jones"))
#> [1]  TRUE  TRUE FALSE
```

Missing values stay missing. Accented Latin letters are read as plain
letters, and punctuation is ignored. See the vignette,
`vignette("fullmetaphone")`, for a worked de-duplication example and the
limits of phonetic matching.

## How it was checked

* The four-character Double Metaphone codes match all 1221 reference
  names in the Apache Commons Codec test suite.
* On 5000 synthetic Indian names, the four-character codes match
  `PGRdup::DoubleMetaphone()` for 4995 names. The other 5 differ
  because of two small bugs in the C code that PGRdup uses, in the
  rules for "GN" (as in "Wagner") and "SC" (as in "Frascella").
  This package follows the original algorithm there.
* The Metaphone codes match the Apache Commons Codec test cases.

## Credits

* **Lawrence Philips** designed Metaphone (1990) and Double Metaphone
  (2000) and published the original code.
* The Double Metaphone rules follow the C implementation by **Maurice
  Aubrey**, with fixes by **Kevin Atkinson**.
* The **PGRdup** package by J. Aravind, J. Radhamani, Kalyani
  Srinivasan, B. Ananda Subhash and co-authors brought Double Metaphone
  to R and inspired this package. Run `citation("PGRdup")` to cite it.
* The Metaphone rules and the test data come from **Apache Commons
  Codec**.
* The first version of this package was written by **Atharv Tyagi**,
  under the guidance of **Anil Kumar Goyal**. Both maintain it.

See `inst/COPYRIGHTS` for details.

## Licence

GPL-3.

## References

Philips, L. (1990). Hanging on the metaphone. *Computer Language*,
7(12), 39-44.

Philips, L. (2000). The double metaphone search algorithm. *C/C++ Users
Journal*, 18(6), 38-43.
