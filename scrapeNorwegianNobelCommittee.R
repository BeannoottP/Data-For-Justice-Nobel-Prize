# Load libraries
library(tidyverse)
library(rvest)
library(parallel)
library(pbmcapply)
library(httr)
library(dplyr)
library(stringr)

baseUrl <- "https://en.wikipedia.org/wiki/List_of_members_of_the_Norwegian_Nobel_Committee"
#get table of committee
wikiTable <- baseUrl %>%
  read_html %>%
  html_table %>%
  .[[1]]

wikiTable[] <- lapply(wikiTable, function(x) { #remove all the brackets
  if (is.character(x)) {
    gsub("\\[.*?\\]", "", x)  # Remove anything inside square brackets
  } else {
    x  # Keep non-character columns unchanged
  }
})

wikiTable <- wikiTable[1:(length(wikiTable)-1)] #remove last column

#QID Helper function
getQIDFromTitle <- function(page_title, lang = "en") {
  base_url <- "https://www.wikidata.org/wiki/Special:ItemByTitle"
  encoded_title <- URLencode(page_title, reserved = TRUE)
  url <- paste0(base_url, "/", lang, "/", encoded_title)
  
  resp <- GET(url)
  final_url <- resp$url
  
  if (grepl("/wiki/Q", final_url)) {
    qid <- sub(".*/wiki/", "", final_url)  # e.g. "Q457149"
    return(qid)
  } else {
    return(NA_character_)
  }
}

#Gets just links of people in the table
scrapeFirstTableFilterParties <- function(list_page_url, lang = "en") {
  # 1) Read the page
  page <- read_html(list_page_url)
  
  # 2) Grab all tables on the page, pick the first
  all_tables <- html_nodes(page, "table")
  if (length(all_tables) == 0) {
    stop("No <table> elements found on this page.")
  }
  first_table <- all_tables[[1]]
  
  # 3) Extract ALL links from that table
  links <- html_nodes(first_table, css = 'a[href^="/wiki/"]')
  
  # Known Norwegian parties to skip
  # (Add or remove from this list as needed)
  party_names <- c(
    "Liberal", "Conservative", "Labour", "Socialist Left",
    "Christian Democratic", "Centre", "Agrarian", "Progress"
    # If you see more party names, add them here
  )
  
  results_list <- list()
  
  for (lk in links) {
    href_val <- html_attr(lk, "href")
    if (!is.na(href_val) &&
        !grepl(":", href_val) &&  # skip File:, Category:, etc.
        !grepl("#", href_val)) {  # skip anchors
      page_title <- sub("^/wiki/", "", href_val)
      display_name <- html_text(lk, trim = TRUE)
      
      # 4) If the display name is one of the known parties, skip it
      if (display_name %in% party_names) {
        next
      }
      
      # 5) Otherwise, it's likely a person's name: get QID
      qid <- getQIDFromTitle(page_title, lang = lang)
      
      results_list[[length(results_list) + 1]] <-
        data.frame(
          display_name = display_name,
          page_title   = page_title,
          qid          = qid,
          stringsAsFactors = FALSE
        )
    }
  }
  
  final_df <- bind_rows(results_list)
  return(final_df)
}

#run above code
committee_members_no_parties <- scrapeFirstTableFilterParties(baseUrl, lang = "en")

#merge both data frames
colnames(wikiTable)[1] <- "display_name"
wikiTableWithQID <- merge(wikiTable, committee_members_no_parties, by='display_name')
wikiTableWithQID <- wikiTableWithQID[-7]