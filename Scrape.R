library(tidyverse)

Search1 <- read_html('https://www.majestic.co.uk/wine?pageNum=0&pageSize=350') %>%
  html_node( css = '#searchResults')    %>%
  html_nodes(css = '.product-details')  %>%
  html_nodes(css =  '.t-not-link')     %>%
  html_text()

Search2 <- read_html('https://www.majestic.co.uk/wine?pageNum=1&pageSize=350') %>%
  html_node( css = '#searchResults')    %>%
  html_nodes(css = '.product-details')  %>%
  html_nodes(css =  '.t-not-link')      %>%
  html_text()

c(Search1, Search2)