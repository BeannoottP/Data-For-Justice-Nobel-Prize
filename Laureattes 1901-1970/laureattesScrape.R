library(httr)
library(jsonlite)
##must be run together
##_____________________
res = GET("https://api.nobelprize.org/2.1/laureates", query = list(offset = 0, limit = 1100))
laureates = fromJSON(rawToChar(res$content))
laureates <- laureates$laureates
##____________________
head(laureates)
save(laureates, file = "unfilteredLaureates.RData")
