#install.packages("rvest")
#install.packages("reshape2")

library("rvest")
library("reshape2")

BASE_URL <- "http://www.beurs.nl/koersen"
BASE_URL_EXT <- "fundamentals"
XPATH_TABLE <- "//div[contains(@class,'table1') and contains(@class,'fundamentals')]/table"

# read symbols from file
symbols <- read.csv(file = "symbols.csv", stringsAsFactors = FALSE)

#symbols <- symbols[1,] # test

# for each symbol, scrape fundamentals page
metrics <- do.call("rbind", apply(symbols, 1, function (f) {
  t <- as.list(f)
  url <- paste(BASE_URL, t$link.exch, as.numeric(t$link.id), t$link.page, BASE_URL_EXT, sep="/")
  print(url)
 
  # read table from url 
  d <- url %>%
    read_html() %>%
    html_nodes(xpath=XPATH_TABLE) %>%
    html_table()
  d <- d[[1]]
#  print(d)

  dl <- melt(d, id.vars = c(1), variable.name = "year")
  
  # check results
  if (dim(dl)[1] == 0) {
    print("no results")
    dl[1,] <- c("N/A","2009","0.00")
  }
  dl <- cbind(t$exchange, t$name, t$symbol, dl)
  
  # some renaming
  names(dl)[1:4] <- c("exchange", "name", "symbol", "metric")
  dl$metric[dl$metric == "Dividendrendement"] <- "dividend rendement"
  dl$metric <- sapply(dl$metric, function(x) { tolower(x) } )

  return(dl)
}))

# cast results
metrics <- dcast(metrics, exchange + symbol + name + year ~ metric)
#print(metrics)

# write results
write.csv(metrics, "fundamentals.csv", row.names = FALSE)

# TODO
#- char to num
#- internationalization
#- add AMX, AsX stock
