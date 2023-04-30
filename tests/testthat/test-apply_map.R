test_that("multiplication works", {


  test_data <- data.frame(
    a = c("A", "B", "C"),
    gender = c("mAle", "FEMALE", "female"),
    crazy = c(1,2,3),
    d = c("A", "1", "D")
  )

  test_data2 <- data.frame(
    quay = c("X", "Y", "Z"),
    sex = c("m", "f", "f"),
    silly = c("what","why","how"),
    d = c("A", "1", "D")
  )

  map <- data.frame(
    variable = c("gender", "gender", "gender", "crazy", "crazy", "crazy", "a", "a", "a"),
    value = c("mAle", "FEMALE", "female", "1", "2", "3", "A", "B", "C"),
    new_variable = c("sex", "sex", "sex", "silly", "silly", "silly", "quay", "quay", "quay"),
    new_value = c("m", "f", "f", "what", "why", "how", "X", "Y", "Z")
  )

  expect_equal(apply_map(test_data, map), test_data2)

})
