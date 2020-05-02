set.seed(5)
test_df <- data.frame(some_numeric = runif(10),
                      some_integer = 1L:10L,
                      some_character = c(letters[1:5], rep(NA, 5)),
                      some_logical = sample(c(TRUE, FALSE, NA), 10, replace = TRUE),
                      some_date = as.Date("2020-01-02") + 1:10,
                      some_datetime = as.POSIXct("2020-02-03 12:00") + 1:10,
                      stringsAsFactors = FALSE)

test_df_summary_wide <- test_df %>% 
  dplyr::summarize_all(list(`_missing` = ~sum(is.na(.)),
                            `_complete` = ~sum(!is.na(.)),
                            `_n` = ~length(.),
                            `_min` = ~min(., na.rm = TRUE), 
                            `_max` = ~max(., na.rm = TRUE),
                            `_n_unique` = ~dplyr::n_distinct(., na.rm = TRUE)))

test_df_summary <- test_df_summary_wide %>% 
  dplyr::mutate_all(as.character) %>% 
  tidyr::gather(key, value) %>% 
  tidyr::separate(key, c("name", "type"),  sep = "__") %>% 
  tidyr::spread(type, value) %>% 
  dplyr::select(name, missing, complete, min, max, n_unique) 
