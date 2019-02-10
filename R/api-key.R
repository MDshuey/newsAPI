#Only change at this point has replaced v1 with v2. No bugs for simple tests so far.
.NEWSAPI_KEY <- function() {
  Sys.getenv("NEWSAPI_KEY")
}

.makeurl <- function(query = "everything", keyword = NULL, ..., version = "v2") {
  stopifnot(is.atomic(version), is.atomic(query))
  rurl <- paste0("https://newsapi.org/", version, "/", query, "?")
  if(!is.null(keyword)) {
    rurl <- paste0(rurl, "q=",keyword)
  }
  params <- c(...)
  if (all(
    length(params) > 0L,
    identical(length(names(params)), length(params)))
  ) {
    params <- paste(names(params), params, sep = "=")
    params <- paste(params, collapse = "&")
    rurl <- paste(rurl, params, sep="&")
  }
  rurl
}



