#' @export
#' @title Create a tbl that references a schema.table in the `conn` database.
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
