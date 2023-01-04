x <- 1
# df_schools_details <- data.frame()
# temp <- data.frame()

link_school_80_new <- slice(link_school_80,1504:1789)

## looping to create the school details dataframe

for (x in 1:length(unlist(link_school_80_new))) {
  
  temp <- get_details(link_school_80_new[[x,1]])
  df_schools_details <- rbind(df_schools_details, temp)
  print(paste("Capturing details:", x))
  
}
df_schools_details



df_schools_details %>% dim
df_schools_details_unique <- df_schools_details %>% unique
df_schools_details_unique %>% dim
