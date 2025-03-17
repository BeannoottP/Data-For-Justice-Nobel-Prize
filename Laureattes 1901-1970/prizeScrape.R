library(httr)
library(jsonlite)
##must be run together
##_____________________
res = GET("https://api.nobelprize.org/2.1/nobelPrizes", query = list(offset = 0, limit = 1100))
prizes = fromJSON(rawToChar(res$content))
prizes <- prizes$nobelPrizes
save(prizes, file = "unfilteredPrizes.RData")

