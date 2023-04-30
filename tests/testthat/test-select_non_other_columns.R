test_that("multiplication works", {

  test_data <- data.frame(
    a = c("A", "B", "C"),
    b_other = c("1", "2", "3"),
    c = c(1,2,3),
    d_other = c("A", "1", "D"),
    e_autre = c("as", "a", "boss")
  )

  test_data2 <- data.frame(
    a = c("A", "B", "C"),
    c = c(1,2,3)
  )

  expect_equal(select_non_other_columns(test_data), test_data2)
})
