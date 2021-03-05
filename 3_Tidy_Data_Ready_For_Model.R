library(readr)
library(tidyverse)
library(pacman)
library(janitor)
library(dplyr)
library(lubridate)
# Get Ken Pom Data from 2002 on joined with Kaggle results data for every game and tidy the data

kp_raw <- read_csv("data/PomeryRatings.csv")%>%
  dplyr::select(team_name = Team, TeamID, Season, EM = AdjEM, adj_off = AdjO, adj_def = AdjD, adj_tempo = AdjT)

regsznresults <-read_csv("data/Kaggle/MRegularSeasonCompactResults.csv")
tourneyresults <- read_csv("data/Kaggle/MNCAATourneyCompactResults.csv")%>%
  filter(Season > 2014) # For Submission 1, want to test model on tourny probs from 2015-2019. Not necessary for Submission 2.
sectourneyresults <- read_csv("data/Kaggle/MSecondaryTourneyCompactResults.csv")

all_past_results <- bind_rows(regsznresults, tourneyresults, sectourneyresults
)%>%
  dplyr::select(-SecondaryTourney, -NumOT)%>%
  mutate(lower_team = pmin(WTeamID, LTeamID), # lower refers to ID number
         higher_team = pmax(WTeamID, LTeamID),
         lower_team_wins = ifelse(lower_team == WTeamID, "YES", "NO")) %>% # the outcome we'll predict
  filter(Season >= 2008)%>%
  left_join(., kp_raw, by = c("Season", "lower_team" = "TeamID"))%>%
  left_join(., kp_raw, by = c("Season", "higher_team" = "TeamID"))%>%
  mutate(EM_diff = EM.x - EM.y,
         adj_off_diff = adj_off.x - adj_off.y,
         adj_def_diff = adj_def.x - adj_def.y,
         adj_tempo_diff = adj_tempo.x - adj_tempo.y,
         lower_team_court_adv = as.factor(ifelse(lower_team == WTeamID,
                                                   WLoc,
                                                   recode(WLoc, "A" = "H", "H" = "A", "N" = "N"))),
         lower_team_wins = as.factor(lower_team_wins))%>%
  dplyr::select(-contains(".x"), -contains(".y"))



collegeBasketball <- read.csv("data/SpreadDataset.csv")

MTeamSpellings <- read.csv("data/MTeamSpellings.csv")

merged <- left_join(collegeBasketball, MTeamSpellings, by = c("Team" = "TeamNameSpelling"))
final <- left_join(merged, MTeamSpellings, by = c("Team2" = "TeamNameSpelling"))

final$TeamID <- final$TeamID.x
final$Team2ID <- final$TeamID.y

final <- final %>%
  dplyr::select("Date", "Season", "Team", "TeamID", "Spread", "Team2", "Team2ID")

final <- final %>%
  drop_na()

## Merge KenPom with Vegas Lines

PastResults <- all_past_results
MSeasons <- read.csv("data/Kaggle/MSeasons.csv")

PastResults2 <- PastResults %>%
  filter(Season >= 2008)

MSeasons2 <- MSeasons %>%
  filter(Season >= 2008) %>%
  dplyr::select("Season", "DayZero")

DayZeroMerge <- left_join(PastResults2, MSeasons2, by = "Season")
DayZeroMerge$DayZero <- mdy(DayZeroMerge$DayZero)
DayZeroMerge$Date <- DayZeroMerge$DayZero + DayZeroMerge$DayNum


DayZeroMerge <- DayZeroMerge %>%
  dplyr::select(-DayNum, -DayZero)

final$Day <- str_sub(final$Date, start = -2)
final$Month <- str_sub(final$Date, end = -3)

final$Month <- as.numeric(final$Month)
final$Season <- as.numeric(final$Season)

final$Year <- ifelse(final$Month > 9, final$Season - 1, final$Season)

final <- final %>%
  dplyr::select(-Date)

final$Date <- paste(final$Month, final$Day, final$Year, sep = "-")

final <- final %>%
  dplyr::select(-Season, -Day, -Month)

final$Date <- mdy(final$Date)

final <- final %>%
  dplyr::select(-Year)

final$higher_team <- ifelse(final$TeamID > final$Team2ID, final$TeamID, final$Team2ID)
final$lower_team <- ifelse(final$TeamID < final$Team2ID, final$TeamID, final$Team2ID)

all_past_results <- left_join(DayZeroMerge, final, by = c("Date", "higher_team", "lower_team"))

all_past_results <- all_past_results %>%
  mutate(Spread = ifelse(TeamID > Team2ID, -Spread, Spread))

all_past_results <- all_past_results %>%
  dplyr::select(-TeamID, -Team2ID, -Team, -Team2)

write.csv(all_past_results, "data/all_past_results.csv", row.names = FALSE)
