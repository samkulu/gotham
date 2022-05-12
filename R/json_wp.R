#' Get WordPress Json (wp-json)
#'
#' @param typ
#'
#' @return
#' @export
#'
#' @examples
#' json_wp()
json_wp<- function(typ = "posts",  dest = "../gothamcity/json"){
  require(jsonlite)

  # Request Typ
  url <- switch(typ,
                posts = "https://gothamcity.ch/wp-json/wp/v2/posts",
                links = "https://gothamcity.ch/wp-json/wp/v2/posts?_fields=link",
                ids = "https://gothamcity.ch/wp-json/wp/v2/posts?_fields=id",
                pages = "https://gothamcity.ch/wp-json/wp/v2/pages",
                categories = "https://gothamcity.ch/wp-json/wp/v2/categories",
                v2 = "https://gothamcity.ch/wp-json/wp/v2",
                json = "https://gothamcity.ch/wp-json/"
  )

  tmp <- readLines(url, warn = FALSE)

  # Export
  filename <- paste(Sys.Date(),"_",typ,".json",sep="")
  fullname <- file.path(dest,filename)

  if(!dir.exists(dest)) dir.create(dest, recursive = TRUE)

  if(!file.exists(fullname)) writeLines(tmp,fullname)

  page <- fromJSON(tmp, flatten=TRUE)

  # View(page)
  page
}


#' Get Latest Gotham Posts
#'
#' @return
#' @export
#'
#' @examples
get_posts <- function(){
  json_wp("posts")
}
