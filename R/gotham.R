
#' Set Gotham User Credentials
#'
#' @param usr
#' @param pwd
#'
#' @return
#' @export
#'
#' @examples
set_user <- function(usr = NA, pwd = NA){
  if (is.na(usr) & is.na(pwd))
    user <<- readLines(file.path(Sys.getenv("R_USER"),".gotham"),
                                      warn = FALSE)
  else
    user <<- c(usr, pwd)
}


#' Browse to Website and Login
#'
#' @return
#' @export
#'
#' @examples
login <- function(){
  cat("Please Login!")
  cat("Retrieve cookie with development tools: ")
  message("Ctrl+Shift+I")
  browseURL("https://gothamcity.ch/mon-compte/")
}


#' Title
#'
#' @param cookie
#'
#' @return
#' @export
#'
#' @examples
#' cookie <- r"(...)" # use R's raw string to store value
#'
set_cookie <- function(cookie){
  cookie <<- cookie
}
