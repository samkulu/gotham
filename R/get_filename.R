#' Get Filename
#'
#' @param x URL for page response
#' @param id Post id
#' @param dest Destination where to store file
#'
#' @return
#' @export
#'
#' @examples
#'
#' get_filename(url, id)
get_filename <- function(x, id, dest){
  m <- gregexpr("20[0-9]{2}\\/[0-9]{2}\\/[0-9]{2}",x)
  dt <- as.Date(gsub("\\/","-",regmatches(x, m)))
  filename <- paste(format(dt, "%Y%m%d"),"_", basename(x),"_(",id,").html",sep="")
  if(length(grep("gothamcity\\.fr", x)) == 1)
    fullname <- file.path(dest, format(dt, "%Y"), "AboFR",filename)
  else
    fullname <- file.path(dest, format(dt, "%Y"), filename)
  fullname
}
