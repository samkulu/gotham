#' Dowload Client with Cookie
#'
#' This is a cookie aware download client.
#'
#' In this task, we need to create a cookie-aware download client in R.
#' The client should be able to download files from a given URI
#' (Uniform Resource Identifier) and save them to a specified destination.
#' Additionally, the client should support the use of cookies for authentication
#' or session management.
#'
#' This code defines a function `download_with_cookies` that takes three
#' arguments: uri (the URI of the file to download), destination (the path
#' where the downloaded file should be saved), and cookie (the cookie string
#' to be used for authentication or session management).
#'
#' Inside the function, we create a new HTTP client using the
#' `curl::new_handle()` function. We then set the cookie option of the client
#' using the `curl::handle_setopt()` function. This ensures that the client
#' sends the specified cookie with the HTTP request.
#'
#' Next, we use the `curl::curl_download()` function to download the file from
#' the given uri and save it to the specified destination. We pass the client
#' handle to the function to ensure that the cookie is included in the request.
#' Finally, we clean up the client using the `curl::handle_free()` function to
#' release any resources associated with the client.
#'
#' You can use this `download_with_cookies` function to download files from a
#' URI while providing the necessary cookie for authentication or session
#' management.
#'
#'
#' @param uri
#' @param destination
#' @param cookie
#'
#' @return
#' @export
#'
#' @examples
#' cookie <<- "(...)"
#' u <- "https://gothamcity.ch/2023/07/05/wirecard-un-armateur-grec-prepare-une-plainte-contre-credit-suisse/"
#' download_with_cookies(u, "test.html", cookie)
#'
download_with_cookies <- function(uri, destination, cookie) {
  # Create a new HTTP client
  client <- curl::new_handle()

  # Set the cookie option
  curl::handle_setopt(client, cookie = cookie)

  # Download the file
  curl::curl_download(uri, destfile = destination, handle = client)

  # Clean up the client
  # ChatGPT suggests to run curl::handle_free(client)
  # But there is an ERROR
  # Error: 'handle_free' is not an exported object from 'namespace:curl'
  client <- NULL
  rm(client)
  gc()
}
