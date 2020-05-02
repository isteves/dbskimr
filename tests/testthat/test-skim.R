test_that("skimr works on iris", {
  conn <- local_conn()
  table_name <- local_table(iris,  conn)
  dbskim(table_name, conn)
})

test_that("skimr works on test_df", {
  conn <- local_conn()
  table_name <- local_table(test_df,  conn)
  test_df_skim <- dbskim(table_name, conn)
  
  expect_true(check_col_equal(test_df_skim, test_df_summary, "missing"))
  expect_true(check_col_equal(test_df_skim, test_df_summary, "complete"))
  # expect_true(check_col_equal(test_df_skim, test_df_summary, "min")) #doesn't  work for  SQLite
  # expect_true(check_col_equal(test_df_skim, test_df_summary, "max"))
  expect_true(check_col_equal(test_df_skim, test_df_summary, "n_unique"))
})

