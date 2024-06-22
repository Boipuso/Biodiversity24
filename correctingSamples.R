setwd("C:/Users/henni/Documents/Uni/Eagle/Semester2/Biodiversity/data/Sichifulo-sample-data-POL-2020/")
getwd()

library(sf)
library(dplyr)
library(ggplot2)

sample23 <- st_read("Sichifulo-sample-data-POL-2023.shp") 
sample20 <- st_read("Sichifulo-sample-data-POL-2020.shp") 


# Convert 'canopy_den' to a factor with the specified levels
sample20$LC_Nr <- factor(sample20$LC_Nr, levels = c("1", "2", "3", "4", "5", "6", "7", "8"))
# make a barplot for the column canopy density and group by study site
ggplot() +
  geom_bar(data = sample20, aes(x = LC_Nr, fill = LC_Nr), position = "dodge", width = 0.5) +
  theme_minimal() +
  labs(x = "LC NR", y = "n samples") +
  theme(legend.position = "top")

# Convert 'canopy_den' to a factor with the specified levels
sample23$LC_Nr <- factor(sample23$LC_Nr, levels = c("1", "2", "3", "4", "5", "6", "7", "8"))
# make a barplot for the column canopy density and group by study site
ggplot() +
  geom_bar(data = sample23, aes(x = LC_Nr, fill = LC_Nr), position = "dodge", width = 0.5) +
  theme_minimal() +
  labs(x = "LC NR", y = "n samples") +
  theme(legend.position = "top")


# make a barplot for the column tree type and group by study site
ggplot() +
  geom_bar(data = sample23, aes(x = Landcover, fill = Landcover), position = "dodge", width = 0.5) +
  theme_minimal() +
  labs(x = "Landcover", y = "n samples") +
  facet_wrap(~LC_Nr) +
  theme(legend.position = "top")

# make a barplot for the column tree type and group by study site
ggplot() +
  geom_bar(data = sample20, aes(x = Landcover, fill = Landcover), position = "dodge", width = 0.5) +
  theme_minimal() +
  labs(x = "Landcover", y = "n samples") +
  facet_wrap(~LC_Nr) +
  theme(legend.position = "top")








# Count values in LC_Nr using table()
count_20<- table(sample20$LC_Nr, useNA = "always")
count_20_LC<- table(sample20$Landcover, useNA = "always")
count_23<- table(sample23$LC_Nr, useNA = "always")
count_23_LC<- table(sample23$Landcover, useNA = "always")

print(count_20)
print(count_20_LC)
print(count_23)
print(count_23_LC)


# Replace NA with "Water" using ifelse()
sample20$LC_Nr <- ifelse(is.na(sample20$LC_Nr), 1, sample20$LC_Nr)

assign_landcover <- function(data) {
  data <- data %>%
    mutate(Landcover = case_when(
      LC_Nr == 1 ~ "water",
      LC_Nr == 2 ~ "bare",
      LC_Nr == 3 ~ "built up",
      LC_Nr == 4 ~ "cropland",
      LC_Nr == 5 ~ "grass",
      LC_Nr == 6 ~ "shrub",
      LC_Nr == 7 ~ "forest", 
      LC_Nr == 8 ~ "wetland"
    ))
  return(data)
}


sample23 <- assign_landcover(sample23)
sample20 <- assign_landcover(sample20)

# make a barplot for the column tree type and group by study site
ggplot() +
  geom_bar(data = sample20, aes(x = Landcover, fill = Landcover), position = "dodge", width = 0.5) +
  theme_minimal() +
  labs(x = "Landcover", y = "n samples") +
  facet_wrap(~LC_Nr) +
  theme(legend.position = "top")



# Export to shapefile
st_write(sample23, "C:/Users/henni/Documents/Uni/Eagle/Semester2/Biodiversity/data/sample23corrected.shp")
st_write(sample20, "C:/Users/henni/Documents/Uni/Eagle/Semester2/Biodiversity/data/sample20corrected.shp")









