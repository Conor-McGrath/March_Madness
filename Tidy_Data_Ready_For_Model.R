library(readr)
library(tidyverse)
library(pacman)
library(janitor)
library(dplyr)

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
  filter(Season > 2001)%>%
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


write.csv(all_past_results, "data/all_past_results.csv", row.names = FALSE)
