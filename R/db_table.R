#' DB Table pointer
#'
#' @description
#' Create a tbl that references a schema.table in the `conn` database.
#'
#' @param conn A database connection.
#' @param schema A string with schema name.
#' @param table A string with table name.
#'
#' @return
#'  A reference to a database table.
#'
#' @export
db_table <- function(conn, schema, table) {
  dplyr::tbl(conn, dbplyr::in_schema(schema, table))
}
