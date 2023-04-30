test_that("column selection for non-dates is working properly", {

  test_data <- data.frame(
    a = c(12.4, 99.1, 8.0),
    b = c(as.POSIXct("2022/12/20"), as.POSIXct("2021/10/05"), as.POSIXct("1998/01/24")),
    c = c(1,2,3),
    d = c(FALSE, TRUE, FALSE),
    e = c(NA, NA, NA),
    f = c(as.character("20/12/2022"), as.character("05/10/2022"), as.character("24/01/1998"))

  )

  expect_equal(names(select_non_date_cols(test_data)), c("a", "c", "d", "e"))

})

#> Test passed 🎉
