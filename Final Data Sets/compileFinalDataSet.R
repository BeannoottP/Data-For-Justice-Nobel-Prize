library(purrr)
library(dplyr)


list.files(path = "Final Data Sets", pattern = "(?i)\\.RData$", recursive = TRUE, full.names = TRUE) %>%
  walk(~ load(.x, envir = .GlobalEnv))

finalSet <- data.frame(
    id = integer(),
    qid = character(),
    name = character(),
    role = character(), #nominee, nominator, governing, selection, vetting, laureate
    department = character(), #phys, chem, lit, peace, med
    startYear = integer(),
    endYear = integer(),
    birthYear = integer(),
    birthPlace = character(),
    nationality = character(),
    gender = character(),
    uniqueId = list(), #every person who is in multiple groups, ie a nobel winner is also a nominee and could also be a vetting body member, has a seperate entry + id. This ID is same among all entries of same person
    nomineeId = integer(), #only for nominators, aligns nominator with nominee
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

detailedData$Category <- gsub("Physics", "phys", detailedData$Category)
detailedData$Category <- gsub("Chemistry", "chem", detailedData$Category)
detailedData$Category <- gsub("Literature", "lit", detailedData$Category)
detailedData$Category <- gsub("Peace", "peace", detailedData$Category)
detailedData$Category <- gsub("Physiology or Medicine", "med", detailedData$Category)

nominees <- detailedData
nominators <- detailedData
nominees$id <- 1:nrow(nominees)
nominators$nomineeId <- 1:nrow(nominees)



#Karolinska Institute, med governing and selection
KIprofs <- KIprofs %>%
  rename(
    name=title,
    startYear = Min,
    endYear = Max
  )

missing_cols <- setdiff(names(finalSet), names(KIprofs))
KIprofs[missing_cols] <- NA
KIprofs$role <- "governing"
KIprofs$department <- "med"
KIprofs <- KIprofs[, names(finalSet)]
finalSet <- rbind(finalSet, KIprofs)

KIprofs$role <- "selection"
KIprofs$department <- "med"
KIprofs <- KIprofs[, names(finalSet)]
finalSet <- rbind(finalSet, KIprofs)


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


#Norwegian Nobel Committee peace vetting and selection
norwegianNobelCommittee <- norwegianNobelCommittee %>%
  rename(
    name=display_name,
    startYear = Start,
    endYear = End
  )

missing_cols <- setdiff(names(finalSet), names(norwegianNobelCommittee))
norwegianNobelCommittee[missing_cols] <- NA
norwegianNobelCommittee$role <- "vetting"
norwegianNobelCommittee$department <- "peace"
norwegianNobelCommittee <- norwegianNobelCommittee[, names(finalSet)]
finalSet <- rbind(finalSet, norwegianNobelCommittee)

norwegianNobelCommittee$role <- "selection"
norwegianNobelCommittee$department <- "peace"
norwegianNobelCommittee <- norwegianNobelCommittee[, names(finalSet)]
finalSet <- rbind(finalSet, norwegianNobelCommittee)


#Physics committee
phys <- phys %>% 
  rename(
    endYear = endyear,
    startYear = startyear,
    birthPlace = birthcountry,
    name = names
  )

missing_cols <- setdiff(names(finalSet), names(phys))
phys[missing_cols] <- NA
phys$role <- "vetting"
phys$department <- "phys"
phys <- phys[, names(finalSet)]
finalSet <- rbind(finalSet, phys)

#Nobel prizes, notes, removed all NON person entries, treating this as human prizes only
prizesWithLaureates <- prizesWithLaureates[prizesWithLaureates$category$en != "Economic Sciences", ]
prizesWithLaureates <- prizesWithLaureates[!is.na(prizesWithLaureates$gender), ] #removes organizations

prizesWithLaureates$category$en <- gsub("Physics", "phys", prizesWithLaureates$category$en)
prizesWithLaureates$category$en <- gsub("Chemistry", "chem", prizesWithLaureates$category$en)
prizesWithLaureates$category$en <- gsub("Literature", "lit", prizesWithLaureates$category$en)
prizesWithLaureates$category$en <- gsub("Peace", "peace", prizesWithLaureates$category$en)
prizesWithLaureates$category$en <- gsub("Physiology or Medicine", "med", prizesWithLaureates$category$en)

prizesWithLaureates$birthYear <- substr(prizesWithLaureates$birth$date, 1, 4)

prizesWithLaureates$department <- prizesWithLaureates$category$en
prizesWithLaureates$startYear <- prizesWithLaureates$awardYear
prizesWithLaureates$endYear <- prizesWithLaureates$startYear 
prizesWithLaureates$qid <- prizesWithLaureates$wikidata$id
prizesWithLaureates$name <- prizesWithLaureates$knownName$en
prizesWithLaureates$role <- "laureate"
prizesWithLaureates$birthPlace <- prizesWithLaureates$birth$place$country$en

missing_cols <- setdiff(names(finalSet), names(prizesWithLaureates))
prizesWithLaureates[missing_cols] <- NA
prizesWithLaureates <- prizesWithLaureates[, names(finalSet)]
finalSet <- rbind(finalSet, prizesWithLaureates)

#RSAS, Phys governing and selection, Chem governing and selection
RSAS <- RSAS %>%
  rename(
    startYear = startyear,
    endYear = endyear,
    birthPlace=birthcountry
  )

missing_cols <- setdiff(names(finalSet), names(RSAS))
RSAS[missing_cols] <- NA
RSAS$role <- "governing"
RSAS$department <- "phys"
RSAS <- RSAS[, names(finalSet)]
finalSet <- rbind(finalSet, RSAS)

RSAS$role <- "selection"
RSAS$department <- "phys"
RSAS <- RSAS[, names(finalSet)]
finalSet <- rbind(finalSet, RSAS)

RSAS$role <- "governing"
RSAS$department <- "chem"
RSAS <- RSAS[, names(finalSet)]
finalSet <- rbind(finalSet, RSAS)

RSAS$role <- "selection"
RSAS$department <- "chem"
RSAS <- RSAS[, names(finalSet)]
finalSet <- rbind(finalSet, RSAS)

#Swedish Academy, Literature Governing and Selection
SA <- SA %>% 
  rename(
    endYear = endyear,
    startYear = startyear,
    birthPlace = birthcountry,
  )

missing_cols <- setdiff(names(finalSet), names(SA))
SA[missing_cols] <- NA
SA$role <- "governing"
SA$department <- "lit"
SA <- SA[, names(finalSet)]
finalSet <- rbind(finalSet, SA)

SA$role <- "selection"
SA$department <- "lit"
SA <- SA[, names(finalSet)]
finalSet <- rbind(finalSet, SA)

#Storting Peace Governing 
Storting <- Storting %>% 
  rename(
    endYear = endyear,
    startYear = startyear,
    birthPlace = birthcountry,
  )

missing_cols <- setdiff(names(finalSet), names(Storting))
Storting[missing_cols] <- NA
Storting$role <- "governing"
Storting$department <- "peace"
Storting <- Storting[, names(finalSet)]
finalSet <- rbind(finalSet, Storting)


#wikidata query
install.packages(WikidataQueryServiceR)
install.packages(tidyverse)
install.packages(pbmcapply)
library(WikidataQueryServiceR)
library(tidyverse)
library(pbmcapply)

# Function to query Wikidata for gender, nationality, and birthplace using QID
query_wikidata_details <- function(qid) {
  sparql <- paste0('
    SELECT ?qid ?genderLabel ?birthCountryLabel ?nationalityLabel WHERE {
      BIND(wd:', qid, ' AS ?qid)
      OPTIONAL { ?qid wdt:P21 ?gender. }
      OPTIONAL { ?qid wdt:P19 ?birthPlace. ?birthPlace wdt:P17 ?birthCountry. }
      OPTIONAL { ?qid wdt:P27 ?nationality. }
      SERVICE wikibase:label { bd:serviceParam wikibase:language "en".
        ?gender rdfs:label ?genderLabel.
        ?birthCountry rdfs:label ?birthCountryLabel.
        ?nationality rdfs:label ?nationalityLabel.
      }
    }
  ')
  
  tryCatch({
    result <- query_wikidata(sparql)
    result$qid <- qid
    result
  }, error = function(e) {
    tibble(qid = qid, genderLabel = NA, birthCountryLabel = NA, nationalityLabel = NA)
  })
}

# Filter QIDs with missing info
missing_info_qids <- finalSet %>%
  filter(is.na(gender) | is.na(nationality) | is.na(birthPlace)) %>%
  distinct(qid) %>%
  pull(qid)

# Run query in parallel (change mc.cores if needed)
wikidata_fills <- pbmclapply(missing_info_qids, query_wikidata_details, mc.cores = 4) %>%
  bind_rows() %>%
  group_by(qid) %>%
  summarise(
    gender = first(genderLabel[!is.na(genderLabel)]),
    birthPlace = first(birthCountryLabel[!is.na(birthCountryLabel)]),
    nationality = paste(unique(na.omit(nationalityLabel)), collapse = "; ")
  )

# Join back to original dataset
finalSet <- finalSet %>%
  left_join(wikidata_fills, by = "qid") %>%
  mutate(
    gender = coalesce(gender.x, gender.y),
    birthPlace = coalesce(birthPlace.x, birthPlace.y),
    nationality = coalesce(nationality.x, nationality.y)
  ) %>%
  select(-gender.x, -gender.y, -birthPlace.x, -birthPlace.y, -nationality.x, -nationality.y)




