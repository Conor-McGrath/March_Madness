library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(lubridate)

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
  select(Date, Season, Team, Spread, Team2)

collegeBasketball <- na.omit(collegeBasketball)

collegeBasketball$Team <- gsub("(?<=[a-z])U", "", collegeBasketball$Team, perl = TRUE)
collegeBasketball$Team2 <- gsub("(?<=[a-z])U", "", collegeBasketball$Team2, perl = TRUE)
collegeBasketball$Team <- gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", collegeBasketball$Team)
collegeBasketball$Team2 <- gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", collegeBasketball$Team2)

collegeBasketball$Team <- tolower(collegeBasketball$Team)
collegeBasketball$Team2 <- tolower(collegeBasketball$Team2)

collegeBasketball$Team <- str_replace(collegeBasketball$Team, "tenn martin", "tenn-martin")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "tenn martin", "tenn-martin")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "fla atlantic", "florida atlantic")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "fla atlantic", "florida atlantic")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "ul monroe", "la-monroe")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "ul monroe", "la-monroe")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "no illinois", "northern illinois")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "no illinois", "northern illinois")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "northwestern st", "northwestern st.")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "northwestern st", "northwestern st.")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "e. washington", "eastern washington")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "e. washington", "eastern washington")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "st. josephs", "st. joseph's")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "st. josephs", "st. joseph's")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "william&mary", "st. joseph's")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "william&mary", "st. joseph's")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "coll charleston", "col charleston")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "coll charleston", "col charleston")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "cal santa barb", "california-santa barbara")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "cal santa barb", "california-santa barbara")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "arkansas lr", "arkansas-little rock")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "arkansas lr", "arkansas-little rock")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "wisc-green bay", "wi green bay")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "wisc-green bay", "wi green bay")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "fair dickinson", "fairleigh dickinson")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "fair dickinson", "fairleigh dickinson")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "tennessee chat", "tennessee-chattanooga")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "tennessee chat", "tennessee-chattanooga")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "cal irvine", "california-irvine")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "cal irvine", "california-irvine")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "central mich", "central mich.")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "central mich", "central mich.")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "miami ohio", "miami (oh)")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "miami ohio", "miami (oh)")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "wisc milwaukee", "wi milwaukee")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "wisc milwaukee", "wi milwaukee")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "so mississippi", "southern miss")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "so mississippi", "southern miss")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "miami florida", "miami (fl)")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "miami florida", "miami (fl)")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "ul lafayette", "la.-lafayette")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "ul lafayette", "la.-lafayette")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "illinois chicago", "illinois-chicago")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "illinois chicago", "illinois-chicago")
collegeBasketball$Team <- str_replace(collegeBasketball$Team, "illinois chicago", "illinois-chicago")
collegeBasketball$Team2 <- str_replace(collegeBasketball$Team2, "illinois chicago", "illinois-chicago")

write.csv(collegeBasketball, "data/SpreadDataset.csv", row.names = FALSE)

MTeamSpellings <- read.csv("data/MTeamSpellings.csv")

merged <- left_join(collegeBasketball, MTeamSpellings, by = c("Team" = "TeamNameSpelling"))
final <- left_join(merged, MTeamSpellings, by = c("Team2" = "TeamNameSpelling"))

final$TeamID <- final$TeamID.x
final$Team2ID <- final$TeamID.y

final <- final %>%
  select("Date", "Season", "Team", "TeamID", "Spread", "Team2", "Team2ID")

final <- final %>%
  drop_na()

## Merge KenPom with Vegas Lines

PastResults <- read.csv("data/all_past_results.csv")
MSeasons <- read.csv("data/Kaggle/MSeasons.csv")

PastResults2 <- PastResults %>%
  filter(Season >= 2008)

MSeasons2 <- MSeasons %>%
  filter(Season >= 2008) %>%
  select("Season", "DayZero")

DayZeroMerge <- left_join(PastResults2, MSeasons2, by = "Season")
DayZeroMerge$DayZero <- mdy(DayZeroMerge$DayZero)
DayZeroMerge$Date <- DayZeroMerge$DayZero + DayZeroMerge$DayNum

DayZeroMerge <- DayZeroMerge %>%
  select(-DayNum, -DayZero)

final$Day <- str_sub(final$Date, start = -2)
final$Month <- str_sub(final$Date, end = -3)

final$Month <- as.numeric(final$Month)
final$Season <- as.numeric(final$Season)

final$Year <- ifelse(final$Month > 9, final$Season - 1, final$Season)

final <- final %>%
  select(-Date)

final$Date <- paste(final$Month, final$Day, final$Year, sep = "-")

final <- final %>%
  select(-Season, -Day, -Month)

final$Date <- mdy(final$Date)

final <- final %>%
  select(-Year)

final$higher_team <- ifelse(final$TeamID > final$Team2ID, final$TeamID, final$Team2ID)
final$lower_team <- ifelse(final$TeamID < final$Team2ID, final$TeamID, final$Team2ID)

all_past_results <- left_join(DayZeroMerge, final, by = c("Date", "higher_team", "lower_team"))

all_past_results <- all_past_results %>%
  mutate(Spread = ifelse(TeamID > Team2ID, -Spread, Spread))

all_past_results <- all_past_results %>%
  select(-Date, -TeamID, -Team2ID, -Team, -Team2)
