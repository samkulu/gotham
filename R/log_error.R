log_error <- function(error, dest = Sys.getenv("R_USER")){
  logfile = file.path(dest, ".gotham_error.log")
  tmp <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M"), error)
  write(tmp, file = logfile, append=TRUE)
}
