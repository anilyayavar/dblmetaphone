# fullmetaphone

Full-length [Metaphone](https://en.wikipedia.org/wiki/Metaphone) and
Double Metaphone phonetic codes for R.

People spell the same name in different ways. Meyer, Meier, Mayer and
Maier are one surname. Catherine and Katherine are one first name. Exact
comparison misses these, which makes deduplication, record linkage and
name search unreliable. Phonetic algorithms turn each name into a code
for its sound, so that names which sound alike can be matched.

**fullmetaphone** implements the two algorithms of Lawrence Philips.

* **Metaphone** (1990) gives one code per name, following the rules of
  English spelling.
* **Double Metaphone** (2000) handles names of many language origins and
  gives a second code where a name has two common pronunciations, so
  that Smith matches Schmidt and Wasserman matches Vasserman.

Most implementations, including `PGRdup::DoubleMetaphone()`, cut the
codes to four characters. That is often too short. Here codes keep up to
32 characters by default, which is the complete code for practically
every real name, so long and multi-part names keep their identity. The
length is set with the `max_length` argument.

```r
library(fullmetaphone)

people <- c("Christopher Anderson", "Christina Andrews")
double_metaphone(people, max_length = 4)
#>   primary secondary
#> 1    KRST      KRST
#> 2    KRST      KRST

double_metaphone(people)
#>       primary   secondary
#> 1 KRSTFRNTRSN KRSTFRNTRSN
#> 2   KRSTNNTRS   KRSTNNTRS
```

In a test with almost 5000 synthetic full names, four-character codes
left fewer than 900 distinct codes. Full-length codes left more than
4200.

## Installation

From GitHub, until the package is on CRAN.

```r
# install.packages("remotes")
remotes::install_github("anilyayavar/dblmetaphone", build_vignettes = TRUE)
```

## Usage

| Function | What it gives |
|---|---|
| `metaphone(x)` | One Metaphone code per name. |
| `double_metaphone(x)` | A data frame with a primary and a secondary Double Metaphone code per name. |
| `sounds_like(x, y)` | `TRUE` where two names share a code. |
| `us_surnames` | The 5000 most common surnames in the 2010 US Census, for examples and testing. |

```r
sounds_like("Smith", c("Schmidt", "Smyth", "Jones"))
#> [1]  TRUE  TRUE FALSE

# Search a register by sound
us_surnames[sounds_like("Schneider", us_surnames$surname), ]
#>        surname rank  count
#> 165     Snyder  165 160262
#> 312  Schneider  312 101290
#> 1088    Snider 1088  32148
#> 3906    Sander 3906   9090
#> 4077   Santoro 4076   8713
```

All functions share these arguments.

| Argument | Default | Meaning |
|---|---|---|
| `max_length` | `32` | Largest number of characters kept in each code. `32` keeps the complete code for practically every real name. Use `Inf` for no limit, or `4` for the traditional short codes. |
| `by_word` | `FALSE` | If `TRUE`, encode each word of a name separately and join the codes with spaces. |
| `method` | `"double"` | `sounds_like()` only. Compare Double Metaphone codes (`"double"`) or Metaphone codes (`"metaphone"`). |

```r
double_metaphone("Christopher Anderson", max_length = 6)
#>   primary secondary
#> 1  KRSTFR    KRSTFR

double_metaphone("Christopher Anderson", by_word = TRUE)
#>       primary   secondary
#> 1 KRSTFR ANTRSN KRSTFR ANTRSN
```

Missing values stay missing, accented Latin letters are read as plain
letters, and punctuation is ignored.

The vignette, `vignette("fullmetaphone")`, explains the algorithms and
works through deduplication, record linkage and name search, including
the limits of phonetic matching.

## Validation

* Cut to four characters, the Double Metaphone codes agree with all 1221
  reference names in the test suite of
  [Apache Commons Codec](https://commons.apache.org/proper/commons-codec/).
* The Metaphone codes agree with the Apache Commons Codec test cases.
* On 5000 synthetic names, the four-character codes agree with
  `PGRdup::DoubleMetaphone()` for 4995 names. The other five differ
  because of two small errors in the C code that 'PGRdup' uses, in the
  rules for "GN" (as in Wagner) and "SC" (as in Frascella). This package
  follows the published algorithm there.

## Credits

* **Lawrence Philips** designed Metaphone and Double Metaphone and
  published the original code.
* The Double Metaphone rules follow the C implementation by **Maurice
  Aubrey** in the Perl module
  [Text::DoubleMetaphone](https://metacpan.org/pod/Text::DoubleMetaphone),
  with fixes by **Kevin Atkinson**.
* The [PGRdup](https://CRAN.R-project.org/package=PGRdup) package by
  J. Aravind, J. Radhamani, Kalyani Srinivasan, B. Ananda Subhash and
  co-authors first brought Double Metaphone to R and inspired this
  package. Run `citation("PGRdup")` to cite it.
* The Metaphone rules and the test data come from **Apache Commons
  Codec**.
* The `us_surnames` data come from the
  [United States Census Bureau](https://www.census.gov/topics/population/genealogy/data/2010_surnames.html)
  and are in the public domain.
* The first version of this package was written by **Atharv Tyagi**
  under the guidance of **Anil Kumar Goyal**, who maintain it together.

See `inst/COPYRIGHTS` for details.

## Licence

GPL-3.

## References

Philips, L. (1990). Hanging on the metaphone. *Computer Language*,
7(12), 38-43.

Philips, L. (2000). The double metaphone search algorithm. *C/C++ Users
Journal*, 18(6), 38-43.
[Archived copy](https://web.archive.org/web/20250702064845/https://drdobbs.com/the-double-metaphone-search-algorithm/184401251).
