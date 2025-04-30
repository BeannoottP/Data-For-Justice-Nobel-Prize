# Load packages -----------------------------------------------------------
library(tidyverse)
library(rvest)
library(WikidataR)
library(pbmcapply)
library(DEoptim)

# ------------------------------------------------------------------------
# Load Wikipedia page and extract former committee members' information
# ------------------------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/Nobel_Committee_for_Literature"

# Extract names and links from the "Former members" section
former_members <- url %>%
  read_html() %>%
  html_elements(".mw-parser-output > ul li")

# Extract first name and valid Wikipedia link from each list item
name <- former_members %>%
  map_chr(~ .x %>% html_element("a:not([href^='#'])") %>% html_text())

links <- former_members %>%
  map_chr(~ .x %>% html_element("a:not([href^='#'])") %>% html_attr("href"))

# Identify valid Wikipedia links (exclude red links & non-Wikipedia links)
good_links <- str_detect(links, "/wiki/") & !str_detect(links, "&redlink=1")

# Construct initial dataframe for former members
lit <- tibble(
  name = name,
  startyear = NA_real_,
  endyear = NA_real_,
  link = paste0("https://en.wikipedia.org", links),
  qid = NA_character_  # Explicitly initialize qid column
)

# ------------------------------------------------------------------------
# Function: Convert Wikipedia URLs to Wikidata QIDs
# ------------------------------------------------------------------------

PediaURLToQID <- function(PediaURL) {
  PediaURL %>%
    read_html() %>%
    html_elements(xpath = '//*[(@id = "t-wikibase")]') %>%
    html_elements('a') %>%
    html_attr('href') %>%
    str_extract('(?<=https://www.wikidata.org/wiki/Special:EntityPage/).+')
}

# Retrieve QIDs for valid Wikipedia links
lit$qid[good_links] <- pbmclapply(lit$link[good_links], PediaURLToQID, mc.cores = 10) %>% unlist()

# ------------------------------------------------------------------------
# Extract Start and End Years
# ------------------------------------------------------------------------

years <- former_members %>%
  html_text() %>%
  str_extract_all("\\d{4}")

lit$startyear <- map_dbl(years, ~ as.numeric(.x[1]), .default = NA_real_)
lit$endyear <- map_dbl(years, ~ as.numeric(.x[2]), .default = NA_real_)

# ------------------------------------------------------------------------
# Extract Current Members from Table
# ------------------------------------------------------------------------

current_raw <- url %>%
  read_html() %>%
  html_element(".wikitable") %>%
  html_table() %>%
  tail(-1) %>%  # Remove first row (header)
  { setNames(., unlist(.[1, ])) } %>%  # Set column names
  tail(-1)  # Remove first row again since it's now the header

# Extract names (cleaned) and elected years
current <- tibble(
  name = current_raw %>%
    pull(Name) %>%
    str_remove("\\s*\\(.*\\)") %>%  # Remove parentheses and extra spaces
    str_trim(),
  startyear = as.numeric(current_raw$Elected),
  endyear = NA_real_
)

# Extract Wikipedia links from the third column of the table
current_links <- url %>%
  read_html() %>%
  html_element(".wikitable") %>%
  html_elements("tr td:nth-child(3) a") %>%
  html_attr("href") %>%
  paste0("https://en.wikipedia.org", .)

current$link <- current_links

# Retrieve QIDs for current members
current$qid <- pbmclapply(current$link, PediaURLToQID, mc.cores = 10) %>% unlist()

# ------------------------------------------------------------------------
# Combine Former and Current Members
# ------------------------------------------------------------------------

lit <- bind_rows(lit, current)
litPreserve <- lit

# Query Additional Data from Wikidata -------------------------------------
items <- paste0('wd:', lit$qid[!is.na(lit$qid)]) %>% paste(collapse = ' ')

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
length(unique(returned$qid))
lit <- merge(lit, returned, by = "qid", all.x = TRUE)
length(unique(lit$qid))
lit <- lit %>%
  group_by(qid) %>%
  filter(
    if (n() > 1) {
      (substr(birthDate, 6, 10) != "01-01" & substr(deathDate, 6, 10) != "01-01")
    } else {
      TRUE
    }
  ) %>%
  ungroup()
length(unique(lit$qid))

lit <- lit[!duplicated(lit$qid), ]
length(unique(lit$qid))

# Convert Dates to Numeric Year ------------------------------------------
lit$deathYear <- as.numeric(format(as.Date(lit$deathDate), "%Y"))
lit$birthYear <- as.numeric(format(as.Date(lit$birthDate), "%Y"))

# Fill in Missing Start and End Years ------------------------------------
lit$startyear[is.na(lit$startyear)] <- lit$startyear.y[is.na(lit$startyear)]
lit$endyear[is.na(lit$endyear)] <- lit$endyear.y[is.na(lit$endyear)]

lit <- lit %>% select(-startyear.y, -endyear.y)

# Estimate Missing End Years ---------------------------------------------
unknown_endyears <- lit[is.na(lit$endyear), ]

valid_indices <- !is.na(unknown_endyears$startyear)

lower_bounds <- as.numeric(unknown_endyears$startyear[valid_indices])
upper_bounds <- ifelse(is.na(unknown_endyears$deathYear[valid_indices]),
                       2025, as.numeric(unknown_endyears$deathYear[valid_indices]))

calculate_cost <- function(endyears) {
  lit$endyear[is.na(lit$endyear)][valid_indices] <- round(endyears)
  
  years <- 1901:2025
  committee_counts <- sapply(years, function(y) {
    sum(lit$startyear <= y & lit$endyear >= y, na.rm = TRUE)
  })
  
  cost <- sum((committee_counts - 5)^2)
  return(cost)
}

if (length(lower_bounds) > 0 & length(upper_bounds) > 0) {
  result <- DEoptim(fn = calculate_cost, lower = lower_bounds, upper = upper_bounds,
                    control = list(trace = 50, itermax = 500, strategy = 3))
  
  lit$endyear[is.na(lit$endyear)][valid_indices] <- round(result$optim$bestmem)
}

litFindExtras <- merge(litPreserve, lit, by = "name")

# Save Processed Data ----------------------------------------------------
save(lit, file = 'Nobel_Lit_Committee.Rdata')

# Visualize the Committee Over Time --------------------------------------
year <- c()
seats <- c()
for (i in 1901:2025) {
  year <- c(year, i)
  seats <- c(seats, sum((lit$startyear <= i) * (i <= lit$endyear), na.rm = TRUE))
}

plot(year, seats, type = "o", main = "Nobel Literature Committee Over Time",
     xlab = "Year", ylab = "Committee Size")