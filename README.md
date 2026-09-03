Welcome to trcpetc!

`table_one()` reports standardized mean differences (SMDs) by default when a grouping variable is supplied. Use `stat_test = "pval"` for p-values or `stat_test = "none"` to omit between-group statistics. The legacy arguments `pval = TRUE` and `pval = FALSE` remain supported but are deprecated.

SMDs follow the unweighted `tableone` implementation: numeric and logical variables use absolute pairwise mean differences divided by the square root of the average within-group variance, with Bernoulli variance for logical variables. Factor SMDs use generalized Mahalanobis distances between multinomial proportion vectors and the average multinomial covariance matrix. For more than two groups, pairwise SMDs are averaged.

## Running tests

Install the package and its test dependency from the package directory:

```r
install.packages(c("devtools", "testthat"))
devtools::install()
```

Run all tests with:

```r
devtools::test()
```

Run only the descriptive-table tests with:

```r
devtools::test(filter = "table-one")
```

The tests demonstrate formula checks using a small controlled data set and a smoke test using the bundled `cardio_data` data set.



To install onto R use 



remotes::install\_github('TRCHR-CP/trcpetc',build\_vignettes = TRUE)





To read the vignette, use



vignette('trcpetc-intro',package='trcpetc')





Please clone (or pull) repository to your local drive, make changes, and push, if needed. If you update package, use

devtools::check()

and also

devtools::build\_vignette()



If this takes a few minutes, then the vignette is too long. It could be shortened for CRAN, and the full version transferred to pdgdown site.





(vignette as of May 2026 is here [trcpetc\_introduction.html](https://github.com/user-attachments/files/27513290/trcpetc_introduction.html) but is not updated.)

