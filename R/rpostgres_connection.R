#' DB Connection
#'
#' @description
#' Use RPostgres::Posgres() driver
#'
#' @param host
#' @param dbname
#' @param user
#' @param password
#' @param port
#' @export
rpostgres_connection <- function(host,
                                 dbname,
                                 user,
                                 password,
                                 port) {
  check_pkg("RPostgres")

  dbConnect(RPostgres::Postgres(),
            host = host,
            dbname = dbname,
            user = user,
            password = password,
            port = port,
            bigint = "numeric")
}
