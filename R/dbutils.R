#' @export
#' @title Create a tbl that references a db table
#'
#' @param conn A database connection.
#' @param schema A string with schema name.
#' @param table A string with table name.
#'
#' @return
#'  A reference to a database table.
#'
#' @importFrom dbplyr in_schema
#' @importFrom dplyr tbl
db_table <- function(conn, schema, table) {
  tbl(conn, in_schema(schema, table))
}

#' @export
#' @title Fetch data from db
#'
#' @param conn A database connection.
#' @param path A sql file path.
#' @param ... Named values to interpolate into the query string.
#'
#' @return
#'  A data frame with as many rows as records and as many
#'  columns as fields in the result set.
#'
#' @importFrom readr read_file
#' @importFrom DBI sqlInterpolate dbGetQuery
#' @importFrom dplyr "%>%" as_tibble
fetch_db <- function(conn, path, ...) {
  query_string <- read_file(path) %>%
    sqlInterpolate(conn, ., ...)

  dbGetQuery(conn, query_string) %>%
    as_tibble()
}
