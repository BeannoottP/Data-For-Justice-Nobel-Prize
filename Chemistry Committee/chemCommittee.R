# Load necessary packages
library(tidyverse)
library(rvest)
library(WikidataR)
library(pbmcapply)
library(DEoptim)

# Set the Wikipedia page URL for the Chemistry Nobel Committee
url <- "https://sv.wikipedia.org/wiki/Vetenskapsakademiens_Nobelkommitt%C3%A9_f%C3%B6r_kemi"

# ------------------------------------------------------------------------
# Extract Members' Information
# ------------------------------------------------------------------------

# Extract all members' list items using the confirmed CSS selector
members <- url %>%
  read_html() %>%
  html_elements(".mw-heading2+ ul li , ul:nth-child(6) li")

# Extract names from the list items
name <- members %>%
  map_chr(~ .x %>% html_element("a:not([href^='#'])") %>% html_text())

# Extract Wikipedia profile links for members
links <- members %>%
  map_chr(~ .x %>% html_element("a:not([href^='#'])") %>% html_attr("href"))

# Identify valid Wikipedia links (exclude red links & non-Wikipedia links)
good_links <- str_detect(links, "/wiki/") & !str_detect(links, "&redlink=1")

# Create the initial data frame
chem <- tibble(
  name = name,
  startyear = NA_real_,
  endyear = NA_real_,
  link = paste0("https://sv.wikipedia.org", links),
  qid = NA_character_  # Initialize QID column
)

# ------------------------------------------------------------------------
# Convert Wikipedia URLs to Wikidata QIDs
# ------------------------------------------------------------------------

PediaURLToQID <- function(PediaURL) {
  PediaURL %>%
    read_html() %>%
    html_elements(xpath = '//*[(@id = "t-wikibase")]') %>%
    html_elements('a') %>%
    html_attr('href') %>%
    str_extract('(?<=https://www.wikidata.org/wiki/Special:EntityPage/).+')
}

# Retrieve QIDs for members with valid Wikipedia links
chem$qid[good_links] <- pbmclapply(chem$link[good_links], PediaURLToQID, mc.cores = 10) %>% unlist()

# ------------------------------------------------------------------------
# Extract Start and End Years
# ------------------------------------------------------------------------

# Extract numeric years from member descriptions
years <- members %>%
  html_text() %>%
  str_extract_all("\\d{4}")

# Assign start and end years
chem$startyear <- map_dbl(years, ~ as.numeric(.x[1]), .default = NA_real_)
chem$endyear <- map_dbl(years, ~ as.numeric(.x[2]), .default = NA_real_)

# ------------------------------------------------------------------------
# Query Additional Data from Wikidata
# ------------------------------------------------------------------------

# Prepare SPARQL query
items <- paste0('wd:', chem$qid[!is.na(chem$qid)]) %>% paste(collapse = ' ')

query <- paste0(
  'SELECT DISTINCT
  ?qid ?birthDate ?deathDate ?startyear ?endyear
  WHERE {
    VALUES ?qid {', items, '}
    OPTIONAL { ?qid p:P569 [ps:P569 ?birthDate]. }
    OPTIONAL { ?qid p:P570 [ps:P570 ?deathDate]. }
    OPTIONAL { ?qid p:P39 [ps:P39 ?position; pq:P580 ?startyear]. }  
    OPTIONAL { ?qid p:P39 [ps:P39 ?position; pq:P582 ?endyear]. }
  }'
)

returned <- query %>%
  query_wikidata()


#filters out multiple birth/death dates
parseReturned <- returned %>%
  group_by(qid) %>%
  filter(
    if (n() > 1) {
      substr(birthDate, 6, 10) != "01-01" & substr(deathDate, 6, 10) != "01-01"
    } else {
      TRUE
    }
  ) %>%
  ungroup()

#Hardfix for Lars, who has two birthdays on Wikidata
duplicateIndex <- which(parseReturned$qid == "Q5718294")[1]
if (!is.na(duplicateIndex)) {
  parseReturned <- parseReturned[-duplicateIndex, ]
}

# Merge Wikidata results into `chem`
chem <- merge(chem, parseReturned, by = "qid", all.x = TRUE)

# Convert dates to numeric year
chem$deathYear <- as.numeric(format(as.Date(chem$deathDate), "%Y"))
chem$birthYear <- as.numeric(format(as.Date(chem$birthDate), "%Y"))

# Fill in missing start and end years
chem$startyear[is.na(chem$startyear)] <- chem$startyear.y[is.na(chem$startyear)]
chem$endyear[is.na(chem$endyear)] <- chem$endyear.y[is.na(chem$endyear)]

chem <- chem %>% select(-startyear.y, -endyear.y)

# ------------------------------------------------------------------------
# Estimate Missing End Years
# ------------------------------------------------------------------------

# Identify missing end years
unknown_endyears <- chem[is.na(chem$endyear), ]
valid_indices <- !is.na(unknown_endyears$startyear)

# Set lower and upper bounds for estimation
lower_bounds <- as.numeric(unknown_endyears$startyear[valid_indices])
upper_bounds <- ifelse(is.na(unknown_endyears$deathYear[valid_indices]),
                       2025, as.numeric(unknown_endyears$deathYear[valid_indices]))

# Define cost function to estimate missing end years
calculate_cost <- function(endyears) {
  chem$endyear[is.na(chem$endyear)][valid_indices] <- round(endyears)
  
  years <- 1901:2025
  committee_counts <- sapply(years, function(y) {
    sum(chem$startyear <= y & chem$endyear >= y, na.rm = TRUE)
  })
  
  cost <- sum((committee_counts - 5)^2)  # Assume ~5 members per year
  return(cost)
}

# Optimize missing end years
if (length(lower_bounds) > 0 & length(upper_bounds) > 0) {
  result <- DEoptim(fn = calculate_cost, lower = lower_bounds, upper = upper_bounds,
                    control = list(trace = 50, itermax = 500, strategy = 3))
  
  chem$endyear[is.na(chem$endyear)][valid_indices] <- round(result$optim$bestmem)
}

# ------------------------------------------------------------------------
# Save Processed Data
# ------------------------------------------------------------------------

save(chem, file = 'Nobel_Chem_Committee.Rdata')

# ------------------------------------------------------------------------
# Visualize Committee Size Over Time
# ------------------------------------------------------------------------

year <- c()
seats <- c()

for (i in 1901:2025) {
  year <- c(year, i)
  seats <- c(seats, sum((chem$startyear <= i) * (i <= chem$endyear), na.rm = TRUE))
}

plot(year, seats, type = "o", main = "Nobel Chemistry Committee Over Time",
     xlab = "Year", ylab = "Committee Size")
