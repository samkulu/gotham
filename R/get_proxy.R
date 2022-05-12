#' Get Proxy-Server from Browser
#'
#' @return
#' @export
#'
#' @examples
#' get_proxy()
get_proxy <- function(verbose = FALSE){
  require(curl)
  if (verbose){
    cat("Curl version: ", curl::curl_version()$version, "\n")
    cat("Has internet: ", curl::has_internet(),  "\n")
    cat("Proxy info: \n")
    print(str(curl::ie_proxy_info()))
  }

  # Return Proxy
  curl::ie_get_proxy_for_url('https://www.google.com')
}

#' Get Proxy User from stored file
#'
#' Proxy credentials can be stored in a file. Make sure that
#' this information is secured. Hide it and place it in a
#' encrypted folder.
#'
#'
#' @return
#' @export
#'
#' @examples
#' get_proxy_user()
get_proxy_user <- function(path = Sys.getenv("R_USER"),
                           file = ".usr"){
  # Stored credentials in a hidden file in a encrypted folder
  filename <- file.path(path, file)
  # Return
  if(file.exists(filename))
    return(readLines(filename, warn = FALSE))
  else
    return(NA)
}

#' Test Proxy-Server
#'
#' @return
#' @export
#'
#' @examples
#' proxy_user <- c("username","password")
#' test_proxy(proxy_user)
#' test_proxy()
test_proxy <- function(proxy_user = NA){
  require(httr)

  if (is.na(proxy_user))
    proxy_user <- get_proxy_user()


  # GET
  tmp <- httr::GET("http://had.co.nz",
                   httr::use_proxy(get_proxy(),
                                   username = proxy_user[1],
                                   password = proxy_user[2],
                                   auth = "any"),
                   verbose = TRUE)
  # HTTP-Statuscode 200 is a success
  if(httr::status_code(tmp) == 200)
    return(TRUE)
  else
    return(FALSE)
}
