.nbs_abort <- function(class, message, ..., call = NULL) {
  condition <- structure(
    c(
      list(message = as.character(message), call = call),
      list(...)
    ),
    class = c(class, "readNBS_error", "error", "condition")
  )
  stop(condition)
}

.nbs_warn <- function(class, message, ..., call = NULL) {
  condition <- structure(
    c(
      list(message = as.character(message), call = call),
      list(...)
    ),
    class = c(class, "readNBS_warning", "warning", "condition")
  )
  warning(condition)
}

