test_that("test suppress", {
  df <- data.frame(a = c(10, 20, 5, 2, 0),
                   n = c(10, 20, 5, 2, 0)) %>%
    suppressCounts(colNames = c("n"), minCellCount = 5)

  testthat::expect_equal(df$a, c(10, 20, 5, 2, 0))
  testthat::expect_equal(df$n, c("10", "20", "5", "<5", "0"))
})
