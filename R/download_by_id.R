#' Download by ID
#'
#' @param id
#' @param dest
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
#' set_user()
#' posts <- get_posts()
#' download_by_id(posts$id)
#' download_by_id(34828:34999) # brute force
download_by_id <- function(id, dest = NA, overwrite = FALSE, browse = 3L){
  require(httr)

  stopifnot(!is.null(id))

  if(is.na(dest) && exists("user")) dest <- user$DESTINATION
  if(is.na(dest)) stop("Destination missing!")

  # Check Proxy
  if(!exists("proxy_user")) proxy_user <<- get_proxy_user()
  if(is.na(proxy_user[1])) stop("Proxy user for authentication missing!")


  # Check Cookie
  if(!exists("cookie")){
    # if not yet stored
    login()
    # copy-from developer tools
    # store cookie into global variable "cookie"
    # cookie <<- r"(...)"
    browser()

    # Log cookie
    log_cookie(cookie)
  }

  # Look for authentication cookie
  # if(!grepl("wfwaf-authcookie",cookie))  browser()

  stopifnot(exists("cookie"))


  # Process Multiple Articles ####
  # id <- c(28316, 28314, 28248, 28246, 27910, 28182, 28147, 27994, 27939, 27921 )

  if(length(id) > 1){
    result <- lapply(id, download_by_id)
    return(result)
  }

  # Process Single Page / Article ####
  # https://gothamcity.ch/?p=28314
  # https://gothamcity.ch/wp-json/wp/v2/posts/

  cat(id, "\n")

  # JSON (lightweight)
  urlJSON <- gsub("%ID%", id, "https://gothamcity.ch/wp-json/wp/v2/posts/%ID%")


  # Proxy-Server
  httr::set_config(
    use_proxy(url = get_proxy(), username=proxy_user[1],password = proxy_user[2] , auth="any"),
    override = TRUE
  )

  # GET Json Response

  h <- httr::add_headers(
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
    `Accept-Encoding` = "gzip, deflate, br",
    `Accept-Language` = "de,de-DE;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
    `Cache-Control` = "max-age=0",
    `Connection` = "keep-alive",
    Cookie =  cookie,
    Host = "gothamcity.ch",
    # Referer = "https://gothamcity.ch/mon-compte/",
    `sec-ch-ua` =  "\"Microsoft Edge\";v=\"105\", \"Not)A;Brand\";v=\"8\", \"Chromium\";v=\"105\"",
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = "Windows",
    `Sec-Fetch-Dest` = "document",
    `Sec-Fetch-Mode` = "navigate",
    `Sec-Fetch-Site` = "none",
    `Sec-Fetch-User` = "?1",
    `Upgrade-Insecure-Requests` = "1",
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36 Edg/105.0.1343.27"
  )

  h <- httr::add_headers(
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
    `Accept-Encoding` = "gzip, deflate, br",
    `Accept-Language` = "de,de-DE;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
    `Cache-Control` = "max-age=0",
    `Connection` = "keep-alive",
    Cookie =  cookie,
    Host = "gothamcity.ch",
    # Referer = "https://gothamcity.ch/mon-compte/",
    `sec-ch-ua` =  r"("Not_A Brand";v="8", "Chromium";v="120", "Microsoft Edge";v="120")",
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = "Windows",
    `Sec-Fetch-Dest` = "document",
    `Sec-Fetch-Mode` = "navigate",
    `Sec-Fetch-Site` = "none",
    `Sec-Fetch-User` = "?1",
    `Upgrade-Insecure-Requests` = "1",
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36 Edg/120.0.0.0"
  )



  responseJSON <- httr::GET(urlJSON, h)

  # HTTP Error Codes
  # https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
  if(responseJSON$status_code >= 400) {
    cat("HTTP Code ")
    message(responseJSON$status_code)
    tmp <- paste0(" HTTPCODE=", responseJSON$status_code,
                  " REQUEST=", urlJSON )
    log_error(tmp)
    return(NULL)
  }

    # GET PAGE Response
  urlPAGE <- gsub("%ID%", id, "https://gothamcity.ch/?p=%ID%")
  responsePAGE <- httr::GET(urlPAGE, h)

  # Browse pageURL
  if(is.integer(as.integer(browse))) {
    if(which(id == posts$id) <= browse)
      browseURL(urlPAGE)
  }

  # Show progress
  cat(id, " ")
  message(responsePAGE$url)

  # BODY
  page <- content(responsePAGE, as="text")
  json <- content(responseJSON, as="text")

  # Covering tracks
  page <- cover_user(page)


  # Filename MEDADATA
  filename <- get_filename(responsePAGE$url, id, dest)

  # Create directory
  if(!dir.exists(dirname(filename)))
    dir.create(dirname(filename), recursive = TRUE)

  # Export
  if(overwrite | !file.exists(filename)){
    writeLines(page, filename, useBytes = TRUE)
    filename <- gsub("\\.html","\\.json",filename)
    writeLines(json, filename, useBytes = TRUE)
  } else {
    cat(filename, " already exists")
  }

}

# Test from 2023-12-18 13:30

# C:\Tools>curl -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: de,de-DE;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6' -H 'Cache-Control: max-age=0' -H 'Connection: keep-alive' -H 'Cookie: FTNT-EP-FG3K6ETB19900142=pqWlpQ6P6NqkpaWluKWlpfCdlZ2Wk5CSlOXg7_Xhi-zr8ffki-Th6Ozri-btpaWl; wordpress_logged_in_5181f9f770df578d6152d01ed525a3d1=samuel.meichtry%7C1703073225%7CYWEuiJQx6KT5eDBGFtKvGEVAT81TBjEoe6Xxsuhjo01%7C9c86c9b3497be1a037bec4f1489ebfeb8571d58ff996474c1410a1c7e7ef4748' -H 'Host: gothamcity.ch' -H 'Sec-Fetch-Dest: document' -H 'Sec-Fetch-Mode: navigate' -H 'Sec-Fetch-Site: none' -H 'Sec-Fetch-User: ?1' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36 Edg/120.0.0.0' -H 'sec-ch-ua: "Not_A Brand";v="8", "Chromium";v="120", "Microsoft Edge";v="120"' -H 'sec-ch-ua-mobile: ?0' -H 'sec-ch-ua-platform: "Windows"' https://gothamcity.ch/wp-json/wp/v2/posts/42424

# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: No host part in the URL
# curl: (3) URL rejected: Bad hostname
# curl: (6) Could not resolve host: Mozilla
# curl: (3) URL rejected: Bad hostname
# curl: (6) Could not resolve host: NT
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Bad hostname
# curl: (6) Could not resolve host: AppleWebKit
# curl: (3) URL rejected: Bad hostname
# curl: (6) Could not resolve host: like
# curl: (3) URL rejected: Bad hostname
# curl: (6) Could not resolve host: Chrome
# curl: (6) Could not resolve host: Safari
# curl: (6) Could not resolve host: Edg
# curl: (3) URL rejected: Malformed input to a URL function
# curl: (3) URL rejected: Bad hostname
# curl: (3) URL rejected: Malformed input to a URL function
# curl: (3) URL rejected: No host part in the URL
# curl: (3) URL rejected: Bad hostname
