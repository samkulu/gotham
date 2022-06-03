#' Backup JSON into Zip
#'
#' @param year
#' @param src
#'
#' @return
#' @export
#'
#' @examples
#' set_user()
#' backup_json()
#' backup_json("2021")
#' backup_json("2020")
backup_json <- function(year = format(Sys.Date(),"%Y"), src = NA){

  # Source files
  if(is.na(src) && exists("user")) src <- user$DESTINATION
  if(is.na(src)) stop("Destination missing!")

  # Zipping needs to go into source path
  # Otherwise you will have the path information in the zip archive
  oldPath <- getwd()

  # Define zip
  if(dir.exists("./data"))
    zipName <- file.path(oldPath,"data", paste0(year,".zip"))
  else
    zipName <- file.path(oldPath, paste0(year,".zip"))

  # Make shure you get back to original dir
  on.exit(setwd(oldPath))

  # Goto location
  path <- file.path(src,"json")
  stopifnot(dir.exists(path))
  setwd(path)


  # Get Files to archive
  fls <- list.files(path)
  fls <- fls[grep(year, fls)]

  # Archive into zip
  if(file.exists(zipName)){
    # Read archive
    z <- unzip(zipName, list = TRUE)
    # Find missing files
    tmp <- fls[which(!fls %in% z$Name)]
    # Just add the missing files
    if(length(tmp) > 0)
      zip(zipfile = zipName, tmp)
    else
      cat("nothing to add")

  } else {
    # Zip all files
    zip(zipfile = zipName, fls)
  }


}
