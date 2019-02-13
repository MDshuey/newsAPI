#Source parameters should be functional now. -MD
.NEWSAPI_KEY <- function() {
  Sys.getenv("NEWSAPI_KEY")
}

.makeurl <- function(query = "everything",
                     keyword = NULL, 
                     ...,
                     version = "v2") {
  stopifnot(is.atomic(version), is.atomic(query))
  rurl <- paste0("https://newsapi.org/", version, "/", query, "?")
 
  params <- c(...)
  if (all(
    length(params) > 0L,
    identical(length(names(params)), length(params)))
  ) {
    params <- paste(names(params), params, sep = "=")
    params <- paste0(params, collapse = "&")
    rurl <- paste(rurl, params, sep = "")
 if(!is.null(keyword)) {
    rurl <- paste0(rurl, "&q=", keyword)
  }  }
  rurl
}



