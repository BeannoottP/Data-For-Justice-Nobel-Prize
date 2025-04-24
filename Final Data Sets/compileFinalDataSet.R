library(purrr)

list.files(path = "Final Data Sets", pattern = "(?i)\\.RData$", recursive = TRUE, full.names = TRUE) %>%
  walk(~ load(.x, envir = .GlobalEnv))