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

#combine two dataframes
prizesWithLaureates <- merge(prizes, laureates, by='id')

#remove extraneous cols
colReducedPrizesWithLaureates <- prizesWithLaureates[c("id", "awardYear", "category", "portion", "knownName", "gender", "birth", "wikidata", "orgName","founded")]

category <- colReducedPrizesWithLaureates$category
category <- category[c("en")]
colReducedPrizesWithLaureates$category <- category

name <- colReducedPrizesWithLaureates$knownName
name <- name[c("en")]
colReducedPrizesWithLaureates$knownName <- name

birthPlace <- colReducedPrizesWithLaureates$birth$place
birthPlace$city <- birthPlace$city[c("en")]
birthPlace$cityNow <- birthPlace$cityNow[c("en", "sameAs")]
birthPlace$country <- birthPlace$country[c("en")]
birthPlace$countryNow <- birthPlace$countryNow[c("en", "sameAs")]
birthPlace <- birthPlace[c("city", "cityNow", "country", "countryNow")]
colReducedPrizesWithLaureates$birth <- birthPlace

colReducedPrizesWithLaureates$wikidata <- colReducedPrizesWithLaureates$wikidata[c("id")]

colReducedPrizesWithLaureates$orgName <- colReducedPrizesWithLaureates$orgName[c("en")]

foundedPlace <- colReducedPrizesWithLaureates$founded$place
foundedPlace$city <- foundedPlace$city[c("en")]
foundedPlace$cityNow <- foundedPlace$cityNow[c("en", "sameAs")]
foundedPlace$country <- foundedPlace$country[c("en")]
foundedPlace$countryNow <- foundedPlace$countryNow[c("en", "sameAs")]
foundedPlace <- foundedPlace[c("city", "cityNow", "country", "countryNow")]
colReducedPrizesWithLaureates$founded <- foundedPlace

save(prizesWithLaureates, file = "prizesAwarded.RData")
