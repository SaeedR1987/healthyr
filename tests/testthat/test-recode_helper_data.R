test_that("test that the recoding is working appropriately", {

  test_data <- data.frame(
    a = c("A", "B", "C"),
    b = c("m", "m", "f"),
    c = c(1,2,3),
    d = c("A", "1", "D")
  )

  test_data2 <- data.frame(
    a = c("A", "B", "C"),
    b = c("m", "m", "Female"),
    c = c(1,2,3),
    d = c("A", "1", "D")
  )

  test_data3 <- data.frame(
    a = c("A", "B", "C"),
    b = c("m", "m", "f"),
    c = c("Female","2","3"),
    d = c("A", "1", "D")
  )

  # Expect it should change
  expect_equal(recode_helper_data(df = test_data, column = "b", old_val = "f", new_val = "Female"), test_data2)

  # Expect that nothing should change
  expect_equal(recode_helper_data(df = test_data, column = "b", old_val = "x", new_val = "Female"), test_data)

  # What happens if it targets a numeric column? Column target should be changed to character.
  expect_equal(recode_helper_data(df = test_data, column = "c", old_val = "1", new_val = "Female"), test_data3)

})
