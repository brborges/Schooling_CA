# Pacotes -----------------------------------------------------------------
library(rvest)
library(tidyverse)


# Importando --------------------------------------------------------------
link <- "https://www.city-data.com/schools-dirs/schools-CA.html"
page <- rvest::read_html(link)

name <- page %>% 
          rvest::html_nodes("td:nth-child(1)")%>%
          rvest::html_text() 

address <- page %>% 
          rvest::html_nodes("td:nth-child(2)")%>%
          rvest::html_text() 

rating <- page %>% 
          rvest::html_nodes("td:nth-child(3)")%>%
          rvest::html_text() 

enrollment <- page %>% 
          rvest::html_nodes("td:nth-child(4)")%>%
          rvest::html_text() 

grade_span <- page %>% 
          rvest::html_nodes("td:nth-child(5)")%>%
          rvest::html_text() 

df <- tibble::tibble(name,address,rating,enrollment,grade_span)
df


# Manipulando -------------------------------------------------------------

df <- df %>% filter(stringr::str_length(df$name) > 2)
df %>% arrange(desc(rating))

# remove_trash <- function(){
# 
#   for (x in length(df$name)) {
#     if (stringr::str_length(df$name[X])) >= 2 remove
#     
#   }
#   
# }
  

# Visualizando ------------------------------------------------------------


# Exportando --------------------------------------------------------------


  