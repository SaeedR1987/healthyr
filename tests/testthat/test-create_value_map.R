test_that("test if value mapping is functioning", {

  test_data <- data.frame(
    a = c("A", "B", "C"),
    b = c("1", "2", "3"),
    c = c(1,2,3),
    d = c("A", "1", "D")
  )

  test_data2 <- data.frame(
    variable = c("a", "a", "a", "d", "d", "d"),
    value = c("A", "B", "C", "1", "A", "D"),
    new_value = c("", "", "", "", "", "")
  )

  expect_equal(create_value_map(test_data), test_data2)

})
