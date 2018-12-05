#' Fetch data from db
#'
#' @param conn A database connection. Only necessary if `async = FALSE`.
#' @param path A SQL file path. Optionally, use `query`.
#' @param query A SQL string. Optionally, use `path`. If both are supplied, `path`` is used.
#' @param async A logical indicating whether to execute the query asynchronously or (default) not.
#' @param dsn The Data Source Name.
#' @param ... Named values to interpolate into the query string.
#'
#' @return
#'  If processed in a synchronous manner (the default), a data frame with as many rows as records and as many
#'  columns as fields in the result set.
#'  If processed in an asynchronous manner, a Future. To get the value of a future, use future::value.
#'  In case of error, a print of the full SQL error is
#'  returned before execution of error action.
#' @export
fetch_db <- function(conn = NULL, path = NULL, query = NULL, async = FALSE, dsn = NULL, ...) {
  if (async) {
    check_pkg("future")
    if (!missing(conn)) {
      message("Asynchronous execution flag set to TRUE but a connection was provided. Connection will not be used.")
    }
    fetch_async(path, query, dsn, ...)
  } else {
    fetch_default(conn, path, query, ...)
  }
}

#'
fetch_init <- function(conn, path, query, ...) {
  query_str <- if (!is.null(path)) {
    readr::read_file(path)
  } else if (!is.null(query)) {
    query
  } else {
    rlang::abort("Both path and query are empty.")
  }
  if (missing(...)) query_str else DBI::sqlInterpolate(conn, query_str, ...)
}

#'
fetch_default <- function(conn, path, query, ...) {
  query_str <- fetch_init(conn, path, query, ...)

  tryCatch({
    dplyr::as_tibble(DBI::dbGetQuery(conn, query_str))
  },
  error = function(e) {
    print(e$message)
    rlang::abort(e)
  })
}

#'
fetch_async <- function(path, query, dsn, ...) {
  if (is.null(dsn)) rlang::abort("Missing dsn string")
  future::plan(future::multiprocess)

  future::future({
    con <- odbc::dbConnect(odbc::odbc(), dsn)
    out <- fetch_default(con, path, query, ...)
    odbc::dbDisconnect(con)
    out
  })
}
