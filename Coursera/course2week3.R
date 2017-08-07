# coursera gettitng and cleaning data week 3 quiz

library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, stringr, data.table, jpeg)

idaho <- read.csv("idaho.csv")

agricultureLogical$idaho <- which(with(idaho, ACR == 3 & AGS == 6))

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
f <- file.path(getwd(), "jeff.jpg")
download.file(url, f, mode = "wb")
jpeg <- readJPEG(f, native = TRUE)
quantile(jpeg, probs = c(0.3, 0.8))

#download gdp data and name columns (first 4 rows are blank/descriptives)
url_2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
f_2 <- file.path(getwd(), "GDP.csv")
download.file(url_2, f_2)
dtGDP <- data.table(read.csv(f_2, skip = 4, nrows = 215))
dtGDP <- dtGDP[X != ""]
dtGDP <- dtGDP[, list(X, X.1, X.3, X.4)]
setnames(dtGDP, c("X", "X.1", "X.3", "X.4"), c("CountryCode", "rankingGDP", 
                                               "Long.Name", "gdp"))

# download ed stats data and merge by country code with gdp data
url_3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
f_3 <- file.path(getwd(), "EDSTATS_Country.csv")
download.file(url_3, f_3)
dtEd <- data.table(read.csv(f_3))
dt <- merge(dtGDP, dtEd, all = TRUE, by = c("CountryCode"))
sum(!is.na(unique(dt$rankingGDP)))

dt %>%
    arrange(desc(rankingGDP))

dt %>%
    group_by(Income.Group) %>%
    summarise(mean = mean(rankingGDP, na.rm=TRUE))

# cut GDP ranking by quantile, determine how many countries are Lower middle income but among 38 nations with highest GDP
breaks <- quantile(dt$rankingGDP, probs = seq(0, 1, 0.2), na.rm = TRUE)
dt$quantileGDP <- cut(dt$rankingGDP, breaks = breaks)
dt[dt$Income.Group == "Lower middle income", .N, by = c("Income.Group", "quantileGDP")]
