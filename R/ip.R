ip <- function(dns = "gothamcity.ch"){
  curl::nslookup(dns)
}

test_ip <- function(dns = "gothamcity.ch"){
  cmd <- paste0("ping -cS ", dns)
  system(cmd)
}
