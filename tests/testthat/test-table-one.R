testthat::test_that("table_one reports tableone-style SMDs", {
  dat <- data.frame(
    group = factor(c("A", "A", "B", "B")),
    continuous = c(1, 2, 3, 4),
    logical = c(FALSE, TRUE, TRUE, TRUE),
    factor = factor(c("x", "y", "x", "y"))
  )

  result <- trcpetc::table_one(dat, group = group, stat_test = "smd")$tab

  testthat::expect_true("smd" %in% names(result))
  testthat::expect_false("pval" %in% names(result))
  testthat::expect_equal(
    result$smd[result$variable == "continuous" & result$row_id == "continuous_mediqr"],
    "2.828"
  )
  testthat::expect_equal(
    result$smd[result$variable == "logical"],
    "1.414"
  )
  testthat::expect_equal(
    result$smd[result$variable == "factor" & result$row_id == "factor"],
    "0.000"
  )
})

testthat::test_that("factor SMD uses group by level proportions", {
  dat <- data.frame(
    group = factor(c("A", "A", "A", "B", "B", "B")),
    factor = factor(c("x", "x", "y", "x", "y", "y"))
  )

  result <- trcpetc::table_one(dat, group = group, stat_test = "smd")$tab

  testthat::expect_equal(
    result$smd[result$variable == "factor" & result$row_id == "factor"],
    "0.707"
  )
})

testthat::test_that("table_one selects p-values or no comparison statistic", {
  dat <- data.frame(
    group = factor(c("A", "A", "B", "B")),
    value = c(1, 2, 3, 4)
  )

  pval_result <- suppressWarnings(
    trcpetc::table_one(dat, group = group, stat_test = "pval", print_test = TRUE)
  )$tab
  none_result <- trcpetc::table_one(dat, group = group, stat_test = "none")$tab

  testthat::expect_true("pval" %in% names(pval_result))
  testthat::expect_true("test" %in% names(pval_result))
  testthat::expect_false("smd" %in% names(pval_result))
  testthat::expect_false(any(c("pval", "smd") %in% names(none_result)))
})

testthat::test_that("legacy pval arguments remain compatible", {
  dat <- data.frame(group = factor(c("A", "A", "B", "B")), value = 1:4)

  pval_result <- suppressWarnings(trcpetc::table_one(dat, group = group, pval = TRUE))$tab
  none_result <- suppressWarnings(trcpetc::table_one(dat, group = group, pval = FALSE))$tab

  testthat::expect_true("pval" %in% names(pval_result))
  testthat::expect_false("smd" %in% names(pval_result))
  testthat::expect_false(any(c("pval", "smd") %in% names(none_result)))
})

testthat::test_that("default SMD output works with bundled cardio_data", {
  result <- trcpetc::table_one(trcpetc::cardio_data, group = Sex)$tab
  rendered <- as.character(trcpetc::kable_table_one(
    trcpetc::table_one(trcpetc::cardio_data, group = Sex),
    caption = "SMD test"
  ))

  testthat::expect_true("smd" %in% names(result))
  testthat::expect_false("pval" %in% names(result))
  testthat::expect_gt(nrow(result), 1)
  testthat::expect_true(any(!is.na(result$smd)))
  testthat::expect_true(grepl("SMD", rendered, fixed = TRUE))
})