#' delete tweets
#'
#' @description delete tweets
#'
#' @param id tweet id
#' @param token Twitter token created in rtweet
#'
#' @examples
#' \dontrun{
#' # delete a tweet:
#' destroy_tweet(id="97690247271840677", token)
#' }
#'
#' @export


destroy_tweet <- function(id,
                          token = token,
                          ...) {
  # ad hoc function

  make_url <- function(restapi = TRUE, query, param = NULL) {
    if (restapi) {
      hostname <- "api.twitter.com"
    } else {
      hostname <- "stream.twitter.com"
    }
    structure(
      list(
        scheme = "https",
        hostname = hostname,
        port = NULL,
        path = paste0("1.1/", query, ".json"),
        query = param,
        params = NULL,
        fragment = NULL,
        username = NULL,
        password = NULL),
      class = "url")
  }

  query <- "statuses/destroy"

  params <- list(
    id = id,
    ...)
  url <- make_url(
    query = query,
    param = params)
  tm <- httr::POST(url, token)
  if(tm$status_code == 200) {
    message("sucessfully delted id ", id)
  } else (
    stop(tm$status_code)
  )
}
