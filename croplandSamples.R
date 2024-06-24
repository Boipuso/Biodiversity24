setwd("C:/Users/henni/Documents/Uni/Eagle/Semester2/Biodiversity/data/")
getwd()

library(sf)
library(dplyr)
library(ggplot2)

sample23 <- st_read("sample23corrected.shp") 
sample20 <- st_read("sample20corrected.shp") 

# assigning all classes except cropland to "other"
assign_landcover <- function(data) {
  data <- data %>%
    mutate(Landcover = case_when(
      LC_Nr == 1 ~ "other",
      LC_Nr == 2 ~ "other",
      LC_Nr == 3 ~ "other",
      LC_Nr == 4 ~ "cropland",
      LC_Nr == 5 ~ "other",
      LC_Nr == 6 ~ "other",
      LC_Nr == 7 ~ "other", 
      LC_Nr == 8 ~ "other"
    ))
  return(data)
}

# assigning all LC_Nr to be 1 for "cropland" and 0 for "other"
assign_LC_Nr <- function(data) {
  data <- data %>%
    mutate(LC_Nr = case_when(
      Landcover == "other" ~ 0,
      Landcover == "cropland" ~ 1
    ))
  return(data)
}

agri_sample20 <- assign_landcover(sample20)
agri_sample23 <- assign_landcover(sample23)

agri_sample20 <- assign_LC_Nr(agri_sample20)
agri_sample23 <- assign_LC_Nr(agri_sample23)

# Export to shapefile
st_write(agri_sample23, "C:/Users/henni/Documents/Uni/Eagle/Semester2/Biodiversity/data/agri_sample23.shp")
st_write(agri_sample20, "C:/Users/henni/Documents/Uni/Eagle/Semester2/Biodiversity/data/agri_sample20.shp")

