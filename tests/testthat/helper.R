local_table <- function(df, conn) {
  table_name <- deparse(substitute(df))
  colnames(df) <- gsub("[[:punct:]]", "_", colnames(df))
  DBI::dbWriteTable(conn, table_name, df)
  invisible(table_name)
}

local_conn <- function(env = parent.frame()) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(conn), envir = env)
  invisible(conn)
}

check_col_equal <- function(df1, df2, col_name, id = "name") {
  dplyr::all_equal(df1[c(id, col_name)],
                   df2[c(id, col_name)])
}
