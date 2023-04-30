test_that("test that the data map is correctly updated with the given inputs", {

  test_data_map <- data.frame(
    variable = c("V1", "V2", "V3"),
    value = c("m", "m", "f"),
    new_variable = c("var1", "var2", "var2"),
    new_value = c("", "", "")
  )

  test_data_map2 <- data.frame(
    variable = c("V1", "V2", "V3"),
    value = c("m", "m", "f"),
    new_variable = c("var1", "var2", "var2"),
    new_value = c("", "Male", "")
  )

  # Test that recode works
  expect_equal(recode_helper_map(df = test_data_map, column = "var2", old_val = "m", new_val = "Male"), test_data_map2)

  # in this situation, nothing should change
  expect_equal(recode_helper_map(df = test_data_map, column = "var1", old_val = "f", new_val = "Female"), test_data_map)

})
