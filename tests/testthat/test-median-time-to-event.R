make_survfitms_for_median_test <- function(cif, lower, upper) {
  structure(
    list(
      states = c("0", "1"),
      pstate = cbind(1 - cif, cif),
      lower = cbind(1 - lower, lower),
      upper = cbind(1 - upper, upper),
      time = seq_along(cif) - 1,
      n.transition = matrix(0, nrow = length(cif), ncol = 1),
      n.risk = matrix(1, nrow = length(cif), ncol = 2)
    ),
    class = "survfitms"
  )
}

testthat::test_that("median confidence limits invert CIF bounds in both modes", {
  fit <- make_survfitms_for_median_test(
    cif = c(0, 0.3, 0.6, 0.8),
    lower = c(0, 0.1, 0.4, 0.6),
    upper = c(0, 0.5, 0.8, 0.9)
  )

  for (interpolate in c(TRUE, FALSE)) {
    result <- trcpetc::median_time_to_event(
      fit, target_prob = 0.5, interpolate = interpolate
    )

    if (interpolate) {
      testthat::expect_equal(result$lower_ci, 1)
      testthat::expect_equal(result$upper_ci, 2.5)
    } else {
      testthat::expect_equal(result$lower_ci, 1)
      testthat::expect_equal(result$upper_ci, 3)
    }

    testthat::expect_lte(result$lower_ci, result$upper_ci)
  }
})

testthat::test_that("unreached CIF bounds produce missing time limits in both modes", {
  fit <- make_survfitms_for_median_test(
    cif = c(0, 0.3, 0.6, 0.8),
    lower = c(0, 0.1, 0.2, 0.3),
    upper = c(0, 0.5, 0.6, 0.7)
  )

  for (interpolate in c(TRUE, FALSE)) {
    result <- trcpetc::median_time_to_event(
      fit, target_prob = 0.5, interpolate = interpolate
    )

    testthat::expect_equal(result$lower_ci, 1)
    testthat::expect_true(is.na(result$upper_ci))
  }
})
