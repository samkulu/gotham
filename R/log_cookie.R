log_cookie <- function(cookie, dest = Sys.getenv("R_USER")){
  logfile = file.path(dest, ".gotham_cookie.log")
  tmp <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M"), " Cookie: \n", cookie, "\n")
  write(tmp, file = logfile, append=TRUE)
}
