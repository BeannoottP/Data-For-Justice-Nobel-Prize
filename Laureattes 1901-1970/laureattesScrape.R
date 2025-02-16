library(httr)
library(jsonlite)
res = GET("https://api.nobelprize.org/2.1/laureates")
rawToChar(res$content)
laureates = fromJSON(rawToChar(res$content))
laureates <- laureates$laureates
