
#' Set Gotham User Credentials
#'
#' @param usr
#' @param pwd
#'
#' @return
#' @export
#'
#' @examples
set_user <- function(usr = NA, pwd = NA, dest = NA){
  if (is.na(usr) & is.na(pwd)){
    user <- readLines(file.path(Sys.getenv("R_USER"),".gotham"),
                       warn = FALSE)
    names(user) <- c("USER", "PASSWORD", "DESTINATION")

    user <<- as.list(user)
  }  else {
    user <<- list(USER = usr, PASSWORD = pwd, DESTINATION = dest)
  }
}

#' Cover User in stored files
#'
#' @param txt
#'
#' @return
#' @export
#'
#' @examples
cover_user <- function(txt){
  # User must exist
  stopifnot(exists("user"))
  # Get User info
  s <- strsplit(user$USER, split="@")[[1]]
  # Cover tracks
  txt <- gsub(gsub("\\.", " ", s[1]), "John Doe", txt, ignore.case = TRUE)
  txt <- gsub(user$USER, "john.doe@acme.admin.ch", txt, ignore.case = TRUE)
  txt <- gsub(s[1], "john.doe", txt, ignore.case = TRUE)
  txt
}


#' Login to Website
#'
#' @return
#' @export
#'
#' @examples
#' logtin()
login <- function(){
  cat("Please Login!")
  cat("Retrieve cookie with development tools: ")
  message("Ctrl+Shift+I")
  browseURL("https://gothamcity.ch/mon-compte/")
}


#' Set Cookie
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


#' Logout from Website
#'
#' @return
#' @export
#'
#' @examples
#' logout()
logout <- function(){
  # SET Proxy and authenticate
  set_config(
    use_proxy(get_proxy(),
              username=pwdWindows[1],password = pwdWindows , auth="any"),
    override = TRUE
  )
  tmp <- httr::GET("https://gothamcity.ch/wp-login.php?action=logout",
                   set_cookies(.cookies = cookie))
}
