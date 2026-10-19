testthat::test_that("gcp_theme is an exported ggplot2 theme", {
  testthat::expect_s3_class(trcpetc::gcp_theme, "theme")
  testthat::expect_equal(trcpetc::gcp_theme$axis.title$family, "Arial")
  testthat::expect_equal(trcpetc::gcp_theme$strip.background$fill, "black")
  testthat::expect_equal(trcpetc::gcp_theme$plot.margin, ggplot2::margin(
    t = 18, r = 18, b = 48, l = 48
  ))
})
