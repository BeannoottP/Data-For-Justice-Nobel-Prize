library(tidyverse)
library(tidyr)
library(dplyr)

load("Laureattes 1901-1970/unfilteredLaureates.RData")
load("Laureattes 1901-1970/unfilteredPrizes.RData")

# Extract the "id" from each row's list
prizes$id <- sapply(prizes$laureates, function(x) x[["id"]])
prizes$portion <- sapply(prizes$laureates, function(x) x[["portion"]])

# Clean and expand rows based on ID values and portion
prizes <- prizes %>%
  mutate(id = gsub("c\\(|\\)|'", "", id)) %>%  # Remove c(...) formatting for id
  mutate(id = gsub('"', "", id)) %>%
  mutate(portion = gsub("c\\(|\\)|'", "", portion)) %>%  # Remove c(...) formatting for portion
  mutate(portion = gsub('"', "", portion)) %>%
  separate_rows(id, portion, sep = ", ")  # Expand into multiple rows

#Remove years with no prize
prizes <- prizes %>% filter(!is.na(id) & id != "NULL")

prizesWithLaureates <- merge(prizes, laureates, by='id')


##TEMP COME BACK AND JUST TAKE THE COLUMNS YOU NEED
save(prizesWithLaureates, file = "prizesAwardedUncleaned.RData")