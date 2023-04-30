test_that("check that the function to assess if the data map can be used is functioning properly", {

  test_data <- data.frame(
    a = c("A", "B", "C"),
    gender = c("mAle", "FEMALE", "female"),
    xrazy = c(1,2,3),
    d = c("A", "1", "D")
  )

  test_data2 <- data.frame(
    a = c("A", "B", "C"),
    gender = c("mAle", "FEMALE", "female"),
    crazy = c(1,2,3),
    d = c("A", "1", "D")
  )

  map <- data.frame(
    variable = c("gender", "gender", "gender", "crazy", "crazy", "crazy"),
    value = c("mAle", "FEMALE", "female", "1", "2", "3"),
    new_variable = c("sex", "sex", "sex", "silly", "silly", "silly"),
    new_value = c("")
  )

  map2 <- data.frame(
    variable = c("gender", "gender", "gender", "crazy", "crazy", "crazy"),
    value = c("mAle", "FEfaeMALE", "female", "1", "4", "3"),
    new_variable = c("sex", "sex", "sex", "silly", "silly", "silly"),
    new_value = c("")
  )

  map_wrong <- data.frame(
    variable = c("gender", "gender", "gender", "crazy", "crazy", "crazy"),
    value = c("mAle", "FEfaeMALE", "female", "1", "4", "3"),
    new_variable = c("sex", "sex", "sex", "silly", "silly", "silly")
  )

  map_not_unique <- data.frame(
    variable = c("gender", "gender", "gender", "crazy", "crazy", "crazy"),
    value = c("mAle", "FEfaeMALE", "female", "1", "1", "1"),
    new_variable = c("sex", "sex", "sex", "silly", "silly", "silly"),
    new_value = c("")
  )

  # Expect error, format of map is not ok.
  expect_error(check_map_applicable(test_data, map_wrong))

  # Expect error, not all map rows are unique
  expect_error(check_map_applicable(test_data, map_not_unique))

  # Expect error, not all mapped column names are in the data
  expect_error(check_map_applicable(test_data, map))

  # Expect error, not all dataset values are mapped.
  expect_error(check_map_applicable(test_data2, map2))

  # Check it works as expected.
  expect_equal(check_map_applicable(test_data2, map), TRUE)


})
