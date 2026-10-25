.nbs_cache_get <- function(client, key) {
  if (!exists(key, envir = client$cache, inherits = FALSE)) {
    return(NULL)
  }
  entry <- get(key, envir = client$cache, inherits = FALSE)
  age <- as.numeric(difftime(client$now(), entry$stored_at, units = "secs"))
  if (!is.finite(age) || age > client$cache_ttl) {
    rm(list = key, envir = client$cache)
    client$cache_order <- setdiff(client$cache_order, key)
    return(NULL)
  }
  client$cache_order <- c(setdiff(client$cache_order, key), key)
  entry$value
}

.nbs_cache_set <- function(client, key, value) {
  assign(
    key,
    list(value = value, stored_at = client$now()),
    envir = client$cache
  )
  client$cache_order <- c(setdiff(client$cache_order, key), key)
  while (length(client$cache_order) > client$cache_max_entries) {
    oldest <- client$cache_order[[1L]]
    client$cache_order <- client$cache_order[-1L]
    if (exists(oldest, envir = client$cache, inherits = FALSE)) {
      rm(list = oldest, envir = client$cache)
    }
  }
  invisible(value)
}

#' Clear a client's bounded response cache
#'
#' Removes every cached observation response held by `client`. Configuration,
#' including the portal bases, retry bounds, and cache limits, is unchanged.
#' The cache is per-client and in memory, so this operation has no filesystem,
#' network, or process-global side effect. Clearing an already empty cache is
#' a successful no-op.
#'
#' @param client An object returned by [nbs_client()].
#' @return The same `nbs_client` environment as `client`, invisibly.
#' @section Errors:
#' An invalid or modified client signals an `nbs_argument_error` condition.
#' @family client configuration
#' @seealso [nbs_client()], [nbs_fetch_series()]
#' @examples
#' client <- nbs_client(cache_max_entries = 8L)
#' invisible(nbs_clear_cache(client))
#' @export
nbs_clear_cache <- function(client) {
  .nbs_validate_client(client)
  keys <- ls(envir = client$cache, all.names = TRUE)
  if (length(keys)) {
    rm(list = keys, envir = client$cache)
  }
  client$cache_order <- character()
  invisible(client)
}
