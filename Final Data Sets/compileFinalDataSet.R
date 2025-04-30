library(purrr)
library(dplyr)


list.files(path = "Final Data Sets", pattern = "(?i)\\.RData$", recursive = TRUE, full.names = TRUE) %>%
  walk(~ load(.x, envir = .GlobalEnv))

finalSet <- data.frame(
    id = integer(),
    qid = character(),
    name = character(),
    role = character(), #nominee, nominator, governing, vetting, laureate
    department = character(), #phys, chem, lit, peace, med
    startYear = integer(),
    endYear = integer(),
    birthYear = integer(),
    birthPlace = character(),
    nationality = character(),
    gender = character(),
    additionalIds = list(),
    stringsAsFactors = FALSE
  )

#chem committee
chem <- chem %>%
  rename(
    startYear = startyear.x,
    endYear = endyear
  )

missing_cols <- setdiff(names(finalSet), names(chem))
chem[missing_cols] <- NA
chem$role <- "vetting"
chem$department <- "chem"
chem <- chem[, names(finalSet)]
finalSet <- rbind(finalSet, chem)

#Nominators/Nominees


#Karolinska Institute


#Lit committee
lit <- lit %>% 
  rename(
    endYear = endyear,
    startYear = startyear.x
  )

missing_cols <- setdiff(names(finalSet), names(lit))
lit[missing_cols] <- NA
lit$role <- "vetting"
lit$department <- "lit"
lit <- lit[, names(finalSet)]
finalSet <- rbind(finalSet, lit)
#physio med committee
med <- med %>% 
  rename(
    endYear = endyear,
    startYear = startyear
  )

missing_cols <- setdiff(names(finalSet), names(med))
med[missing_cols] <- NA
med$role <- "vetting"
med$department <- "med"
med <- med[, names(finalSet)]
finalSet <- rbind(finalSet, med)


#Norwegian Nobel Committee


#Physics committee


#Nobel prizes


#RSAS


#Swedish Academy


#Storting


#apply ID's find matching people


