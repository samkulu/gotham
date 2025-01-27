run_download <- function(){
  set_user()

  url <- "https://gothamcity.ch/"
  page <- rvest::read_html(url)

  # Extrahiere alle Links (a-Tags) von der Seite
  links <- page %>%
           html_nodes("div.post-intro a") %>%
           html_attr("href") %>%
           sort()

  yrs <- substr(links, 23, 26) %>% unique()
  stopofnot(Sys.Date() %>% format("%Y") %in% yrs)

  # Destination
  if(exists("user")) dest <- user$DESTINATION

  # Existing downloads
  fls <- list()
  for(y in yrs){
    fls[[y]] <- list.files(file.path(dest, y),
                         pattern = "\\.html",
                         full.names = TRUE) %>%
                # removing id-appendix from old download
                gsub("(.*)(_\\([0-9]+\\))(\\.html)", "\\1\\3", .)
  }
  fls <- unlist(fls)

  # Export
  for(u in links){
    filename <- get_filename2(u, dest)
    cat(u)
    # Download and Write
    if(!filename %in% fls){
      p <- rvest::read_html(u)
      html <- as.character(p)
      writeLines(html, filename, useBytes = TRUE)
      message( " done")
    } else {
      message( " exists")
    }
  }


}



# <div
# <div class="post-intro"> .. h3 title, date, tags, excerpt
# href="https://gothamcity.ch/2025/01/22/scandale-petrolier-nigerian-la-suisse-libere-110-millions-de-dollars/"
# href="https://gothamcity.ch/2025/01/22/affaire-al-rajaan-la-corruption-presumee-atteint-le-milliard-de-dollars/"
