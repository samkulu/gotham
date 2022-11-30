read_get <- function(url){
  require(httr)


  # Check Proxy
  if(!exists("proxy_user")) proxy_user <<- get_proxy_user()
  if(is.na(proxy_user[1])) stop("Proxy user for authentication missing!")

  # Set Proxy
  httr::set_config(
    httr::use_proxy(url = get_proxy(), username=proxy_user[1],password = proxy_user[2] , auth="any"),
    override = TRUE
  )

  # Retrieve data
  result <- httr::GET(url)
  json <- content(result, as="text")

  # Return
  return(json)
}

