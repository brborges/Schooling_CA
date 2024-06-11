# Packages -----------------------------------------------------------------
library(rvest)
library(tidyverse)


# Importing --------------------------------------------------------------


## Importing table os schools ##

## main link
link <- "https://www.city-data.com/schools-dirs/schools-CA.html"
# download.file(link, destfile = "scrapedpage.html", quiet=TRUE)
# content <- read_html("scrapedpage.html")

## creating a dataframe
schools <- data.frame()

## looping to import the schools in all the 129 pages

for (page_result in seq(from = 0, to = 129, by = 1)) {

  link <- paste0("https://www.city-data.com/schools-dirs/schools-CA", page_result,".html")
  page <- rvest::read_html(link)
  page_table <- page %>% html_nodes("table") %>% html_table() %>% .[[8]]
  schools <- rbind(schools, page_table)
  print(paste("Page:", page_result))
  
}

## creating list of schools links
link_school <- schools %>%
                  select(Name) %>% 
                  mutate(link = stringr::str_to_lower(Name)) %>% 
                  mutate(link = stringr::str_replace_all(link," ","-")) %>% 
                  mutate(link = stringr::str_replace_all(link,"/","-")) %>% 
                  mutate(link = stringr::str_replace_all(link,"[[:punct:]]", "-")) %>% 
                  mutate(link = stringr::str_replace_all(link,"---","-")) %>%
                  mutate(link = stringr::str_replace_all(link,"--","-")) %>%
                  mutate(link_school = paste0("https://www.city-data.com/school/",link,"-ca.html")) %>% 
                  select(link_school)

## creating list of schools with rating above 80

schools_80 <- schools %>% filter(Rating >= 80)

link_school_80 <- schools_80 %>%
  select(Name) %>% 
  mutate(link = stringr::str_to_lower(Name)) %>% 
  mutate(link = stringr::str_replace_all(link," ","-")) %>% 
  mutate(link = stringr::str_replace_all(link,"/","-")) %>% 
  mutate(link = stringr::str_replace_all(link,"[[:punct:]]", "-")) %>% 
  mutate(link = stringr::str_replace_all(link,"---","-")) %>%
  mutate(link = stringr::str_replace_all(link,"--","-")) %>%
  mutate(link_school = paste0("https://www.city-data.com/school/",link,"-ca.html")) %>% 
  select(link_school)


## Importing details info from each school

details_list <- data.frame()
df_details <- data.frame()

get_details <- function(link_school){
  
  page_school <- rvest::read_html(link_school)
  length_details <- length(page_school %>% rvest::html_nodes("dd"))
  
  if (length_details == 0){
    
  } else
    {
  
      address <- page_school %>% rvest::html_nodes(".addr") %>% rvest::html_text() 
      city <- page_school %>% rvest::html_nodes("a") %>% rvest::html_text() %>% .[[322]]
      
      ## testing the length first
      
      if (length_details >=2) {
        telephone <- page_school %>% rvest::html_nodes("dd") %>%  rvest::html_text() %>% .[[2]]
      }else {
        telephone <- ""
      }
      
      if (length_details >=4) {
        students <- page_school %>% rvest::html_nodes("dd") %>% rvest::html_text() %>% .[[4]]
      }else {
        students <- ""
      }
      
      if (length_details >=5) {
        classroom_teachers <- page_school %>% rvest::html_nodes("dd") %>%  rvest::html_text() %>% .[[5]]
      }else {
        classroom_teachers <- ""
      }
     
      if (length_details >=7) {
        free_lunch_students <- page_school %>% rvest::html_nodes("dd") %>% rvest::html_text() %>% .[[7]]
      }else {
        free_lunch_students <- ""
      }
      
      if (length_details >=8) {
        reduce_price_lunch <- page_school %>% rvest::html_nodes("dd") %>%  rvest::html_text() %>% .[[8]]
      }else {
        reduce_price_lunch <- ""
      }
      
      school_district <- page_school %>% rvest::html_nodes("#sdName") %>% rvest::html_text()
        if (length(school_district != 0)) {
          school_district <- page_school %>% rvest::html_nodes("#sdName") %>% rvest::html_text()
        }else {
          school_district <- ""
        }
      
      school_name <- page_school %>% rvest::html_nodes("b") %>% rvest::html_text() %>% .[[10]]
    
      df_details <- tibble::tibble(school_name,address,city,school_district,telephone,students,classroom_teachers,free_lunch_students,reduce_price_lunch)
      
      details_list <- rbind(details_list, df_details)
    }

    return (details_list)
}

## initiating variables

x <- 1
df_schools_details <- data.frame()
temp <- data.frame()

## looping to create the school details dataframe

for (x in 1:length(unlist(link_school_80))) {
  
  temp <- get_details(link_school_80[[x,1]])
  df_schools_details <- rbind(df_schools_details, temp)
  print(paste("Capturing details:", x))

}
df_schools_details


# Tidy -------------------------------------------------------------

schools_80 <- schools_80 %>% unique()

df_consolidated <- schools_80 %>%
                      rename(school_name = Name) %>% 
                      left_join(df_schools_details_unique, by="school_name")
  
df_consolidated %>% glimpse()  

df_LA <- df_consolidated %>% 
  filter(!is.na(school_district)) %>% 
  filter(stringr::str_detect(city, "^Los Angeles"))

## rating average by Los Angeles districts
df_LA %>%
  mutate(Rating = as.numeric(Rating)) %>% 
  filter(Rating >= 80) %>% 
  group_by(school_district) %>% 
  summarise(Rating = median(Rating, na.rm =T)) %>% 
  arrange(desc(Rating)) %>% View

## rating average for the whole CA
df_consolidated %>%
  mutate(Rating = as.numeric(Rating)) %>% 
  filter(Rating >= 80) %>% 
  group_by(school_district) %>% 
  summarise(Rating = median(Rating, na.rm =T)) %>% 
  arrange(desc(Rating)) %>% View

df_LA$school_district %>%
  unique() 


df_LA %>% 
  filter(school_district == "Los Angeles Unified") %>% 
  arrange(desc(Rating)) %>% View

## 

# Visualization ------------------------------------------------------------


# Exporting --------------------------------------------------------------

readr::write_csv(link_school, "link_school.csv")
readr::write_csv(df_schools_details, "df_schools_details_80.csv")
readr::write_csv(schools, "schools.csv")
readr::write_csv(schools_80, "schools_80.csv")
readr::write_csv(df_schools_details_unique, "df_schools_details_80.csv")
readr::write_csv(df_consolidated, "df_consolidated.csv")
