#' @export
#' @title Transform a vector into a SQL-interpreted array.
#'
#' @param vector character vector
#' @return
#'   List with a single string that's interpreted as a SQL array to interpolate in a query.
as_sql_array <- function(vector) {
  array <- paste0("(", paste(paste0("'", vector, "'"), collapse = ", "), ")")
  list(array)
}
