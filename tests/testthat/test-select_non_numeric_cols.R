

testthat::test_that("tests if column selection works appropriately", {

  # Should only pick up 2 of these columns here
  test_data <- data.frame(
    a = c("A", "B", "C"),
    b = c("1", "2", "3"),
    c = c(1,2,3),
    d = c("A", "1", "D")
  )

  # should pick up the one column here
  test_data2 <- data.frame(
    a = c("A", "B", "C")
  )

  # Should not pick up the one column here
  test_data3 <- data.frame(
    c = c(1,2,3)
  )

  # it treats dates, posixct, vectors of only NA, numeric and doubles, all as
  # TRUE for check.numeric. It should only pick up the BOOLEAN column here
  test_data4 <- data.frame(
    a = c(12.4, 99.1, 8.0),
    b = c(as.POSIXct("2022/12/20"), as.POSIXct("2021/10/05"), as.POSIXct("1998/01/24")),
    c = c(1,2,3),
    d = c(FALSE, TRUE, FALSE),
    e = c(NA, NA, NA)

  )

  expect_equal(length(names( select_non_numeric_cols(test_data))), 2)

  expect_equal(length(names( select_non_numeric_cols(test_data2))), 1)

  expect_equal(length(names( select_non_numeric_cols(test_data3))), 0)

  expect_equal(length(names( select_non_numeric_cols(test_data4))), 1)

})
#> Test passed 🎉
