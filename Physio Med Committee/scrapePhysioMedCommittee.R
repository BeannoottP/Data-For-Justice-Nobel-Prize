# Date: July 1, 2022
# Author: Ariana Mendible
# Description -------------------------------------------------------------
# This script scrapes the Wikipedia page for the Nobel Committee for Physiology/Medicine.
# It gets the names of former members, years of membership, and Wikidata IDs.
# A plot is created to verify the number of seats in each year. 

# Load packages -----------------------------------------------------------
library(tidyverse)
library(rvest)
library(WikidataR)
library(pbmcapply)
library(DEoptim)

# Load in list of names --------------------------------------------------
url <- "https://sv.wikipedia.org/wiki/Karolinska_Institutets_Nobelkommitt%C3%A9"

txt <- url %>% 
  read_html %>%
  html_elements(css='ul:nth-child(34) li') %>% 
  html_text

links <- url %>% 
  read_html %>% 
  html_elements(css='ul:nth-child(34) a') %>% 
  html_attr('href') %>%
  paste0('https://sv.wikipedia.org',.)

good.links <- links %>% str_detect('/wiki/')
bad.links <- links %>% str_detect('&redlink=1')

# Pull names and years from text
name <- txt %>% str_extract('^.+(?=,)(?<!\\d)')
startyear <- txt %>% str_extract('\\d{4}(?=–)') %>% as.numeric
endyear <- txt %>% str_extract('(?<=–)\\d{4}') %>% as.numeric

df <- data.frame(name, startyear, endyear) 

# Manually fix Gunnar Hedrén's years
df$name <- str_trim(df$name)  # Trim whitespace for matching
df$startyear[df$name == "Gunnar Hedrén"] <- 1926  # Start year known
df$endyear[df$name == "Gunnar Hedrén"] <- 1931  # End year known

# Convert good page links to QIDS -----------------------------------------
PediaURLToQID <- function(PediaURL){
  qidout <- PediaURL %>% 
    read_html %>% 
    html_elements(xpath='//*[(@id = "t-wikibase")]') %>%
    html_elements('a') %>%
    html_attr('href') %>%
    str_extract('(?<=https://www.wikidata.org/wiki/Special:EntityPage/).+')
}

qid <- pbmclapply(links[good.links], PediaURLToQID, mc.cores = 23) %>% unlist
df$qid[good.links] <- qid

# Get rid of weird ones
df <- df[!is.na(df$startyear),]

# Preserve Gunnar Hedrén even though his startyear is NA
df <- df[!is.na(df$startyear) | df$name == "Gunnar Hedrén", ]

# Add manual entries
#remove anna or edit, maybe merge on names?
additional_members <- data.frame(
  name = c("Anna Wedell", "Ole Kiehn", "Patrik Ernfors", "Nils-Göran Larsson"),
  startyear = c(2013, 2014, 2019, 2021),
  endyear = c(2018, 2018, 2020, 2022),
  stringsAsFactors = FALSE
)

# Add missing QIDs for now (can query them later or leave as NA)
additional_members$qid <- c("Q4990241", "Q42319021", "Q19975955", "Q5938385")  # Or use NA if unknown

# Bind to main dataframe
df <- bind_rows(df, additional_members)

df<-df[-53,]
# # Filter
# df <- df %>%
#   filter(startyear <= 2025)

items <- paste0('wd:',df$qid) %>% paste(collapse=' ')

query <- paste0(
  'SELECT DISTINCT
  ?qid ?birthDate ?deathDate
  WHERE {
    VALUES ?qid {', items,'}
    ?qid p:P31 [ ps:P31 ?instanceof ] .
    OPTIONAL{?qid p:P569 [ps:P569 ?birthDate ].}
    OPTIONAL{?qid p:P570 [ps:P570 ?deathDate ].}
    }'
)

raw_returned <- query %>%
  query_wikidata()

returned <- raw_returned %>%
  mutate(
    qid = as.character(qid),
    birthDate = as.Date(birthDate, format="%Y-%m-%d"),
    deathDate = as.Date(deathDate, format="%Y-%m-%d")
  )

med <- merge(df, returned, by = "qid", all.x = TRUE)

#filters out multiple birth/death dates
parseReturned <- med %>%
  group_by(qid) %>%
  filter(
    if (n() > 1) {
      substr(birthDate, 6, 10) != "01-01" & substr(deathDate, 6, 10) != "01-01"
    } else {
      TRUE
    }
  ) %>%
  ungroup()


med_orig <- parseReturned
med <- parseReturned

# Convert deathDate to year
med$deathYear <- as.numeric(format(as.Date(med$deathDate), "%Y"))
med$birthYear <- as.numeric(format(as.Date(med$birthDate), "%Y"))

unknown_endyears <- med[is.na(med$endyear), ]

# Define the cost function
calculate_cost <- function(endyears) {
  # Replace NA endYears with the proposed values
  med[is.na(med$endyear), "endyear"] <- round(endyears)
  
  # Calculate committee counts for each year
  years <- 1901:2025
  committee_counts <- sapply(years, function(y) {
    sum(med$startyear <= y & med$endyear >= y)
  })
  
  # Cost is the number of years with committee count neq 5
  cost <- sum((committee_counts - 5)^2)
  return(cost)
}

# Lower and Upper bounds for DEoptim
lower_bounds <- as.numeric(unknown_endyears$startyear)
upper_bounds <- ifelse(is.na(unknown_endyears$deathYear), 2025, as.numeric(unknown_endyears$deathYear))

# Run the DEoptim function
result <- DEoptim(fn = calculate_cost, lower = lower_bounds, upper = upper_bounds, control = list(trace = 50, itermax = 500, strategy = 3))

# The optimal end years are in result$bestmem
med[is.na(med$endyear), "endyear"] <- round(result$optim$bestmem)


# Save data ---------------------------------------------------------------
save(med, file='PhysMedCommittee.Rdata')



# Verify years ------------------------------------------------------------
year = c()
seats = c()
for (i in 1901:2025){
  year <- c(year,i)
  seats <- c(seats, sum((med$startyear <= i) * (i <= med$endyear), na.rm = TRUE))
}
plot(year,seats)

