
#' get_sources
#'
#' Returns news sources with meta data.
#'
#' @param sources Names of news sources.
#' @param sortBy Name of sorting mechanism must be one of relevancy, time (publishedAt), or popularity. Certain methods
#'   only work for certain news sources.
#' @param apiKey Character string API token. Default is to grab it from user R environ.
#' @param parse Logical indicating whether to parse response object to data frame.
#' @examples
#' \dontrun{
#' df <- get_articles("espn")
#' }
#' @importFrom httr content GET warn_for_status
#' @return Data frame or nested list.
#' @export
get_articles <- function(sources=NULL, 
                         keyword=NULL,
                         sortBy = "",
                         from = NULL, 
                         to = NULL,
                         pageSize = NULL,
                         apiKey = NULL,
                         parse = TRUE) {
  if (!sortBy %in% c("relevancy", "popularity", "publishedAt", "")) {
    stop("sortBy must be relevancy, popularity, or publishedAt.", call. = FALSE)
  }
  if(from < Sys.Date() - 31){
    warning("Warning: Free API subsciptions cannot search further back than one month.")
  }
  if(length(sources)>20){
    stop("sources cap in the api is currently 20. Try pulling multiple times for all sources and rbind for now. 
         Or lapply for each source, which was how it was done in v1 of this wrapper. -MD")
  }
  if (is.null(apiKey)) {
    apiKey <- .NEWSAPI_KEY()
  }
  params <- list(sources = paste(sources, collapse = ","), 
                 from = from,
                 to = to,
                 pageSize = pageSize,
                 sortBy = sortBy, 
                 apiKey = apiKey)
  rurl <- .makeurl(query = "everything", keyword,  params)
  rurl
  r <- httr::GET(rurl)
  warn_for_status(r)
  r <- httr::content(r, "parsed")
  if (parse) {
    parse_articles(r)
  } else {
    r
  }
}
#' I had to adjust this function to account for source being a list of "id" and "name".
#' Solution isn't the most elegant, but it works. -MD
#' parse_articles
#'
#' Converts response object from get articles to data frame
#'
#' @param x Response object returned by get_articles.
#' @return Data frame
#' @importFrom tibble as_tibble
#' @export
parse_articles <- function(x) {
  if ("articles" %in% names(x)) {
    source <- lapply(x[[3]], `[[`, "source") %>%
              lapply(`[[`, "name") %>%
              lapply(as.character)
    sortBy <- x[["sortBy"]]
    x <- x[["articles"]]
  } else {
    source <- NA_character_
    sortBy <- NA_character_
  }

  vars <- unique(unlist(lapply(x, names)))
  data <- vector("list", length(x))
  for (i in seq_along(x)) {
    for (j in vars) {
      if (!j %in% names(x[[i]]) | length(x[[i]][[j]]) == 0L) {
        data[[i]][[j]] <- NA
      } else {
        data[[i]][[j]] <- x[[i]][[j]]
      }
    }
  }
  data <- tibble::as_tibble(do.call("rbind", data))
  data$publishedAt <- as.POSIXct(
    strptime(data$publishedAt, format = "%Y-%m-%dT%TZ", tz = "UTC")
  )
  data$source <- source
  data$sortBy <- sortBy
  if(length(data) == 8){
  data <- data[, c(7, 6, 1, 2:5, 8)]
  }
  else print("Error in function -MD")
  data
}
