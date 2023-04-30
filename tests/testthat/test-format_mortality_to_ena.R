
test_that("testing if formatting to ena mortality is working", {

  test_data2 <- proc_mortality1

  proc_mortality1_ena2 <- format_mortality_to_ena(test_data2, keep_cluster_ids = T, keep_hh_ids = F)

  # the function output should be equivalent to the pre-provided proc_mortality1_ena
  expect_equal(format_mortality_to_ena(test_data2, keep_cluster_ids = F, keep_hh_ids = F), proc_mortality1_ena)

  # the function output should be equivalent ot the pre-provided proc_mortality_ena
  expect_equal(format_mortality_to_ena(test_data2, keep_cluster_ids = T, keep_hh_ids = F), proc_mortality1_ena2)

  # Should throw an error, because this dataset does not have acceptable default values for hh_id.
  expect_error(format_mortality_to_ena(test_data2, keep_cluster_ids = F, keep_hh_ids = T))

  # Should throw an error, because this dataset does not have acceptable default values for hh_id.
  expect_error(format_mortality_to_ena(test_data2, keep_cluster_ids = T, keep_hh_ids = T))


})
