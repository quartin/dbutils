#' @export
#' @title Fetch data from db
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
fetch_db <- function(conn = NULL, path = NULL, query = NULL, async = FALSE, dsn = NULL, ...) {
  if (async) {
    if(!missing(conn)) {
      message("Asynchronous execution flag set to TRUE but a connection was provided. Connection will not be used.")
    }
    fetch_async(path, query, dsn, ...)
  } else {
    fetch_default(conn, path, query, ...)
  }
}

#' @title Fetch init
#'
#' @importFrom readr read_file
#' @importFrom DBI sqlInterpolate
fetch_init <- function(conn, path, query, ...) {
  query_str <- if (!is.null(path)) {
    readr::read_file(path)
  } else if (!is.null(query)) {
    query
  } else {
    stop("Both path and query are empty.")
  }
  if (missing(...)) query_str else sqlInterpolate(conn, query_str, ...)
}

#' @title Fetch default
#'
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr as_tibble
fetch_default <- function(conn, path, query, ...) {
  query_str <- fetch_init(conn, path, query, ...)

  tryCatch({
    as_tibble(dbGetQuery(conn, query_str))
  },
  error = function(e) {
    print(e$message)
    stop(e)
  })
}

#' @importFrom future future plan multiprocess
#' @importFrom odbc dbDisconnect dbConnect odbc
fetch_async <- function(path, query, dsn, ...) {
  if(is.null(dsn)) stop("Missing dsn string")
  future::plan(future::multiprocess)

  future({
    con <- odbc::dbConnect(odbc::odbc(), dsn)
    out <- fetch_default(con, path, query, ...)
    odbc::dbDisconnect(con)
    out
  })
}
