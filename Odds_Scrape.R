library(dplyr)
library(readxl)

# Cleaned data in excel
# Load in the data

ncaa_basketball_2007_08 <- read_excel("data/OddsLines/ncaa basketball 2007-08.xlsx")
ncaa_basketball_2008_09 <- read_excel("data/OddsLines/ncaa basketball 2008-09.xlsx")
ncaa_basketball_2009_10 <- read_excel("data/OddsLines/ncaa basketball 2009-10.xlsx")
ncaa_basketball_2010_11 <- read_excel("data/OddsLines/ncaa basketball 2010-11.xlsx")
ncaa_basketball_2011_12 <- read_excel("data/OddsLines/ncaa basketball 2011-12.xlsx")
ncaa_basketball_2012_13 <- read_excel("data/OddsLines/ncaa basketball 2012-13.xlsx")
ncaa_basketball_2013_14 <- read_excel("data/OddsLines/ncaa basketball 2013-14.xlsx")
ncaa_basketball_2014_15 <- read_excel("data/OddsLines/ncaa basketball 2014-15.xlsx")
ncaa_basketball_2015_16 <- read_excel("data/OddsLines/ncaa basketball 2015-16.xlsx")
ncaa_basketball_2016_17 <- read_excel("data/OddsLines/ncaa basketball 2016-17.xlsx")
ncaa_basketball_2017_18 <- read_excel("data/OddsLines/ncaa basketball 2017-18.xlsx")
ncaa_basketball_2018_19 <- read_excel("data/OddsLines/ncaa basketball 2018-19.xlsx")
ncaa_basketball_2019_20 <- read_excel("data/OddsLines/ncaa basketball 2019-20.xlsx")
ncaa_basketball_2020_21 <- read_excel("data/OddsLines/ncaa basketball 2020-21.xlsx")

# 2016 and 2020 showing error when using rbind Need to make Close column is character...

str(finalData$Close...10)
str(ncaa_basketball_2016_17$Close...10)
str(ncaa_basketball_2019_20$Close...10)

ncaa_basketball_2016_17$Close...10 <- as.character(ncaa_basketball_2016_17$Close...10)
ncaa_basketball_2019_20$Close...10 <- as.character(ncaa_basketball_2019_20$Close...10)

# Final dataset with all games since 2008 with odds and line

finalData <- bind_rows(ncaa_basketball_2007_08, ncaa_basketball_2008_09, ncaa_basketball_2009_10,
                       ncaa_basketball_2010_11, ncaa_basketball_2011_12, ncaa_basketball_2012_13,
                       ncaa_basketball_2013_14, ncaa_basketball_2014_15, ncaa_basketball_2015_16,
                       ncaa_basketball_2016_17, ncaa_basketball_2017_18, ncaa_basketball_2018_19,
                       ncaa_basketball_2019_20, ncaa_basketball_2020_21)

# More cleaning...

finalData$Team...4 <- gsub("([[:lower:]])([[:upper:]][[:lower:]])", "\\1 \\2", finalData$Team...4)
finalData$Team...16 <- gsub("([[:lower:]])([[:upper:]][[:lower:]])", "\\1 \\2", finalData$Team...16)
finalData$Team...4 <- gsub("([[:upper:]])([[:upper:]][[:lower:]])", "\\1 \\2", finalData$Team...4)
finalData$Team...16 <- gsub("([[:upper:]])([[:upper:]][[:lower:]])", "\\1 \\2", finalData$Team...16)
finalData$Team...4 <- gsub("\\.(?=[A-Za-z])", ". ", finalData$Team...4, perl = TRUE)
finalData$Team...16 <- gsub("\\.(?=[A-Za-z])", ". ", finalData$Team...16, perl = TRUE)

finalData <- finalData %>% 
  rename(
    Date = Date...1,
    Rot = Rot...2,
    VH = VH...3,
    Team = Team...4,
    FirstHalf = '1st...5',
    SecondHalf = '2nd...6',
    Final = Final...7,
    Result = Result...8,
    Open = Open...9,
    Close = Close...10,
    Moneyline = ML...11,
    '2H' = '2H...12',
    Date2 = Date...13,
    Rot2 = Rot...14,
    VH2 = VH...15,
    Team2 = Team...16,
    FirstHalf2 = '1st...17',
    SecondHalf2 = '2nd...18',
    Final2 = Final...19,
    Result2 = Result...20,
    Open2 = Open...21,
    Close2 = Close...22,
    Moneyline2 = ML...23,
    '2H2' = '2H...24',
    Season = Season
  )

collegeBasketball <- finalData[-c(2,5:6,10,12:14,17:18,22,24)]

collegeBasketball$Open <- ifelse(collegeBasketball$Open == 'NL', 100, collegeBasketball$Open)
collegeBasketball$Open2 <- ifelse(collegeBasketball$Open2 == 'NL', 100, collegeBasketball$Open2)

collegeBasketball$Open <- as.numeric(collegeBasketball$Open)
collegeBasketball$Open2 <- as.numeric(collegeBasketball$Open2)

collegeBasketball$Spread <- ifelse(collegeBasketball$Open < collegeBasketball$Open2, collegeBasketball$Open, collegeBasketball$Open2)
collegeBasketball$Spread <- ifelse(collegeBasketball$Open > collegeBasketball$Open2, collegeBasketball$Spread, collegeBasketball$Spread*(-1))

collegeBasketball <- collegeBasketball %>%
  select(Date, Team, Team2, Spread, Season)

collegeBasketball <- na.omit(collegeBasketball)

write.csv(collegeBasketball, "data/SpreadDataset.csv", row.names = FALSE)
