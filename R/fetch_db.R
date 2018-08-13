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
#'  In case of error, a print of the full SQL error is
#'  returned before execution of error action.
#'
#' @importFrom readr read_file
#' @importFrom DBI sqlInterpolate dbGetQuery
#' @importFrom dplyr "%>%" as_tibble
fetch_db <- function(conn, path, ...) {
  query_string <- read_file(path) %>%
    sqlInterpolate(conn, ., ...)

  tryCatch({
    dbGetQuery(conn, query_string) %>%
      as_tibble()
  },
  error = function(e) {
    print(e$message)
    stop(e)
  })
}
