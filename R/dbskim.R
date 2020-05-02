#' Skim a database table, getting usefulsummary statistics
#' 
#' `dbskim()` is inspired by \link[skimr]{skim}, quickly providing an overview of all the column names, types, and summary stats of a database table.
#'
#' @param table_name Database table name #TODO: support schemas
#' @param conn Database connection
#'
#' @return tibble of table summary statistics
#' @export
#'
#' @examples
#' conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' DBI::dbWriteTable(conn, "storms", dplyr::storms)
#' dbskim("storms", conn)
#' DBI::dbDisconnect(conn)
#' 
dbskim <- function(table_name, conn) {
  table_metadata <- suppressWarnings(get_table_metadata(table_name, conn))
  col_summaries <- create_col_summary_sql(table_metadata$name)
  
  all_summaries <- suppressWarnings(get_col_summaries(table_name, 
                                                      col_summaries, 
                                                      conn))
  
  col_summary <- all_summaries %>% 
    tidyr::gather(key, value, -n) %>% 
    tidyr::separate(key, c("name", "type"),  sep = "__") %>% 
    tidyr::spread(type, value) %>% 
    dplyr::select(name, missing, complete, n, min, max, n_unique)
  
  dplyr::as_tibble(dplyr::left_join(table_metadata, col_summary, by = "name"))
}

get_table_metadata <- function(table_name, conn) {
  res <- DBI::dbSendQuery(conn, sprintf("SELECT * FROM %s LIMIT 0", table_name))
  withr::defer(DBI::dbClearResult(res), envir = parent.frame())
  col_info <- DBI::dbColumnInfo(res, table_name)
  col_info[c("name", "type")]
}

create_col_summary_sql <- function(column_name, collapse = ", ") {
  glue::glue_collapse(glue::glue(
    "sum(CASE WHEN {column_name} ISNULL THEN 1 ELSE 0 END) AS {column_name}__missing,
sum(CASE WHEN {column_name} ISNULL THEN 0 ELSE 1 END) AS {column_name}__complete,
count(DISTINCT {column_name})                         AS {column_name}__n_unique,
min({column_name})                                    AS {column_name}__min,
max({column_name})                                    AS {column_name}__max"),
    sep = collapse)
}

get_col_summaries <- function(table_name, col_summaries, conn) {
  q <- glue::glue("SELECT count(*) AS n,
  {col_summaries}
  FROM {table_name}")
  DBI::dbGetQuery(conn, q)
}

