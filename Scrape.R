library(tidyverse)
library(rvest)


# Scape names -------------------------------------------------------------

# Scraping wine names from magestic.co.uk
# They have about 650 wines - but making the page size bigger than ~500 makes the site time out
# We just use two separate calls to avoid this.

getWineNames <- function(page, size = 350){
  # page is pagenumber from the site
  # Size is how many wines per page
  # page numbers start at 0
  # could error handle page being too large in future
  
  page <- paste0('https://www.majestic.co.uk/wine?pageNum=', page, '&pageSize=', size) %>% read_html()
  
 names <-  page                           %>%
    html_node( css = '#searchResults')    %>%
    html_nodes(css = '.product-details')  %>%
    html_nodes(css = '.t-not-link')       %>%
    html_text()                           %>%
    gsub(pattern = "[^\x20-\x7F]", replacement = "") # removing unicode characters
   
  
 # The lapply looks to see if a div exists - if it doesnt it returns NA rather than nothing. 
 
 descr <- page                                                     %>% 
   html_nodes(".product-details")                                  %>%
   lapply(. %>% html_nodes(".product-content__description") %>% 
            html_text()                                 %>% 
            ifelse(identical(., character(0)), NA, .))  %>% 
   unlist                                                          %>%
   gsub(pattern = '\n            ', replacement = "", fixed = T)   %>%
   gsub(pattern = '    ', replacement = "", fixed = T)             %>%
   gsub(pattern = "[^\x20-\x7F]", replacement = "")
 
 
 return(tibble(names, descr))
}


do.call(bind_rows, lapply(0:1, getWineNames)) %>% write.csv("WineDf.csv", row.names = F)



