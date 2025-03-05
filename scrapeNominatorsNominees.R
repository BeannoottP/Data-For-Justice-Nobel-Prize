# Load libraries
library(tidyverse)
library(rvest)
library(parallel)
library(pbmcapply)

# Set base URL for scraping nominees/nominators
baseURL <- "https://www.nobelprize.org/nomination/archive/search.php?prize=0&startyear=1901"

# Extract the total number of records
totalrecords <- baseURL %>%
  read_html() %>%
  html_text() %>%
  str_extract("out of total \\d+") %>%
  str_extract("\\d+") %>%
  as.numeric

# Set number of records per page
recordsperpage <- 50

# Calculate offsets
offset <- recordsperpage*(0:floor(totalrecords/recordsperpage))

# Define function for scraping each page
scrapePage <- function(offset) {
  
  # Generate URL for the results with this offset
  url <- paste0(baseURL, "&offset=", offset)
  
  # Get data
  thispage <- url %>%
    read_html %>%
    html_table %>%
    .[[1]] %>%  # Extract first element from the list
    setNames(.[1, ]) %>%  # Set column names from the first row
    tail(-1) %>%
    select(-last_col())
  
  # Get links
  links <- url %>%
    read_html %>%
    html_elements(".butt") %>%
    html_attr("href") %>%
    paste0("https://www.nobelprize.org/nomination/archive/", .)
  
  # Add to dataframe
  thispage <- thispage %>%
    mutate(link = links)
  
}

# Apply in parallel
data <- pbmclapply(offset , scrapePage, mc.cores = detectCores() - 1) %>%
  bind_rows

# Function to scrape nomination details
scrape_nomination <- function(url) {
  page <- read_html(url)
  
  # Extract text using the correct selector
  raw_text <- page %>%
    html_node(".inside") %>%
    html_text(trim = TRUE)
  print(raw_text)
  # Use section headers to split text
  nominee_text <- str_extract(raw_text, "(?s)Nominee.*?(?=Nominator:|$)")
  nominator_text <- str_extract(raw_text, "(?s)Nominator:.*")
  
  # Helper function to extract field values
  extract_field <- function(label, text) {
    pattern <- paste0(label, ":\\s*([^\\n]+)")
    match <- str_match(text, pattern)
    if (!is.na(match[2])) {
      return(trimws(match[2]))
    }
    return(NA)
  }
  extract_category <- function(text) {
    pattern <- "Nomination for Nobel Prize in\\s*([^\\n]+)"
    match <- str_match(text, pattern)
    if (!is.na(match[2])) {
      return(trimws(match[2]))
    }
    return("Peace")
  }
  

  
  # Extract general fields
  year <- extract_field("Year", raw_text)
  number <- extract_field("Number", raw_text)
  category <- extract_category(raw_text)
  
  # Extract Nominator fields
  nominator_name <- extract_field("Name", nominator_text)
  nominator_gender <- extract_field("Gender", nominator_text)
  nominator_birth <- extract_field("Year, Birth", nominator_text)
  nominator_death <- extract_field("Year, Death", nominator_text)
  nominator_profession <- extract_field("Profession", nominator_text)
  nominator_university <- extract_field("University", nominator_text)
  nominator_city <- extract_field("City", nominator_text)
  nominator_state <- extract_field("State", nominator_text)
  nominator_country <- extract_field("Country", nominator_text)
  
  
  #Logical split for multiple Nominees in one page
  if (grepl("Nominee \\d+", nominee_text)) { #There is more than one nominee
    returnData <- data.frame(
      Category = character(), Year = numeric(), Number = numeric(),
      Nominee_Name = character(), Nominee_Gender = character(),
      Nominee_Birth = numeric(), Nominee_Death = numeric(),
      Nominee_Profession = character(), Nominee_University = character(),
      Nominee_City = character(), Nominee_State = character(), Nominee_Country = character(),
      Nominee_Motivation = character(),
      Nominator_Name = character(), Nominator_Gender = character(),
      Nominator_Birth = numeric(), Nominator_Death = numeric(),
      Nominator_Profession = character(), Nominator_University = character(),
      Nominator_City = character(), Nominator_State = character(), Nominator_Country = character(),
      stringsAsFactors = FALSE)
    nominee_digit <- 1
    while(grepl("Nominee \\d+", nominee_text)) { #While there is still info in the text, run this loop 
      # Extracts first instance of each Nominee fields
      nominee_name <- extract_field("Name", nominee_text)
      nominee_gender <- extract_field("Gender", nominee_text)
      nominee_birth <- extract_field("Year, Birth", nominee_text)
      nominee_death <- extract_field("Year, Death", nominee_text)
      nominee_profession <- extract_field("Profession", nominee_text)
      nominee_university <- extract_field("University", nominee_text)
      nominee_city <- extract_field("City", nominee_text)
      nominee_state <- extract_field("State", nominee_text)
      nominee_country <- extract_field("Country", nominee_text)
      nominee_motivation <- extract_field("Motivation", nominee_text)
    
      #Create a new row with each individual nomination
      new_row <- data.frame(
        Category = category, Year = year, Number = number,
        Nominee_Name = nominee_name, Nominee_Gender = nominee_gender,
        Nominee_Birth = nominee_birth, Nominee_Death = nominee_death,
        Nominee_Profession = nominee_profession, Nominee_University = nominee_university,
        Nominee_City = nominee_city, Nominee_State = nominee_state, Nominee_Country = nominee_country,
        Nominee_Motivation = nominee_motivation,
        Nominator_Name = nominator_name, Nominator_Gender = nominator_gender,
        Nominator_Birth = nominator_birth, Nominator_Death = nominator_death,
        Nominator_Profession = nominator_profession, Nominator_University = nominator_university,
        Nominator_City = nominator_city, Nominator_State = nominator_state, Nominator_Country = nominator_country,
        stringsAsFactors = FALSE
      )
      
      #add this row to 
      returnData <- rbind(returnData, new_row)
      nominee_digit <- nominee_digit + 1
      pattern <- paste0("(?s)Nominee ", nominee_digit, ".*")
      nominee_text <- str_extract(nominee_text, pattern)
      print(nominee_text)
    }
    
    return(returnData)

      } else {

  
  # Extract Nominee fields
  nominee_name <- extract_field("Name", nominee_text)
  nominee_gender <- extract_field("Gender", nominee_text)
  nominee_birth <- extract_field("Year, Birth", nominee_text)
  nominee_death <- extract_field("Year, Death", nominee_text)
  nominee_profession <- extract_field("Profession", nominee_text)
  nominee_university <- extract_field("University", nominee_text)
  nominee_city <- extract_field("City", nominee_text)
  nominee_state <- extract_field("State", nominee_text)
  nominee_country <- extract_field("Country", nominee_text)
  nominee_motivation <- extract_field("Motivation", nominee_text)
  

  
  # Return as a data frame
  return(data.frame(
    Category = category, Year = year, Number = number,
    Nominee_Name = nominee_name, Nominee_Gender = nominee_gender,
    Nominee_Birth = nominee_birth, Nominee_Death = nominee_death,
    Nominee_Profession = nominee_profession, Nominee_University = nominee_university,
    Nominee_City = nominee_city, Nominee_State = nominee_state, Nominee_Country = nominee_country,
    Nominee_Motivation = nominee_motivation,
    Nominator_Name = nominator_name, Nominator_Gender = nominator_gender,
    Nominator_Birth = nominator_birth, Nominator_Death = nominator_death,
    Nominator_Profession = nominator_profession, Nominator_University = nominator_university,
    Nominator_City = nominator_city, Nominator_State = nominator_state, Nominator_Country = nominator_country,
    stringsAsFactors = FALSE
  ))
  }
}

# Apply in parallel
detailedData <- pbmclapply(sample(data$link, 150), scrape_nomination, mc.cores = detectCores() - 1) %>%
  bind_rows
