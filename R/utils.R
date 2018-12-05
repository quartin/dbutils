check_pkg <- function(name) {
  if (!rlang::is_installed(name)) {
    rlang::abort(sprintf("The `%s` package must be installed", name))
  }
}
