#' Get WordPress Json File (wp-json)
#'
#' @param typ
#'
#' @return
#' @export
#'
#' @examples
#' posts <- json_wp()
json_wp<- function(typ = "posts",  dest = NA){
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

  # 2022-11-29 Changed
  # tmp <- readLines(url, warn = FALSE)
  tmp <- read_get(url)

  # 2024-03-04 Changed
  tmp <- gsub(".*(<script>.*</script>)(.*)", "\\2", tmp)


  # Export
  if(is.na(dest) && exists("user")) dest <- user$DESTINATION
  if (is.na(dest)) stop("Missing Destination!")

  filename <- paste(Sys.Date(), "_", typ, ".json", sep="")
  fullname <- file.path(dest, "json", filename)

  # Create folders
  if(!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  # Do not overwrite
  if(!file.exists(fullname)) writeLines(tmp, fullname)

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
#' posts <- get_posts()
get_posts <- function(){
  json_wp("posts")
}
