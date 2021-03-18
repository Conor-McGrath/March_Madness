# Make Stage 2 Predictions
library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(Matrix)
library(xgboost)

## Load Ken Pom Ratings
kp_raw <- read_csv("data/PomeryRatings.csv")%>%
  filter(Season == 2021)

# Get the Sample Submission df
submission_format <- read_csv("data/Kaggle/MSampleSubmissionStage2.csv") # Here we have Stage 1. Need to Change to Stage 2

# Get Season, lower_team and higher_team
blank_preds <- submission_format %>%
  clean_names() %>%
  separate(id, into = c("Season", "lower_team", "higher_team"), sep = "_", remove = FALSE, convert = TRUE) %>%
  dplyr::select(-pred)%>%
  left_join(., kp_raw, by = c("Season", "lower_team" = "TeamID"))%>%
  left_join(., kp_raw, by = c("Season", "higher_team" = "TeamID"))%>%
  mutate(EM_diff = AdjEM.x - AdjEM.y,
         adj_off_diff = AdjO.x - AdjO.y,
         adj_def_diff = AdjD.x - AdjD.y,
         adj_tempo_diff = AdjT.x - AdjT.y,
         lower_team_court_adv = 1)%>%
  dplyr::select(-contains(".x"), -contains(".y"))%>%
  filter(Season == 2021)

# Create matrix and run through model to make predictions

submission_matrix <- xgb.DMatrix(data = as.matrix(blank_preds[, -1]))

bst_final <- read_rds("data/bst_final_spreads.rds")

predicted_spreads <- predict(bst_final, submission_matrix)

blank_preds$predicted_spreads <- predicted_spreads


# Now let's get the actual spreads for first round games

# May need to scrape from here https://legacy.donbest.com/ncaab/scores/ for first round spreads
# if the excel files at https://www.sportsbookreviewsonline.com/scoresoddsarchives/ncaabasketball/ncaabasketballoddsarchives.htm
# aren't updated in time

library(dplyr)
library(rvest)
scrapepage <- "https://legacy.donbest.com/ncaab/scores/20210318.html"

testtable <- read_html(scrapepage) %>% 
  html_table(fill=T)
test1 <- testtable[[1]]%>%
  dplyr::select(1:10)%>%
  filter(!is.na(X2))%>%
  filter(X2 != "Team")%>%
  filter(!str_detect(X2,"COLLEGE BASKETBALL"))%>%
  dplyr::select(X2, X9)

scrapepage <- "https://legacy.donbest.com/ncaab/scores/20210319.html"

testtable <- read_html(scrapepage) %>% 
  html_table(fill=T)
test2 <- testtable[[1]]%>%
  dplyr::select(1:10)%>%
  filter(!is.na(X2))%>%
  filter(X2 != "Team")%>%
  filter(!str_detect(X2,"COLLEGE BASKETBALL"))%>%
  dplyr::select(X2, X9)

scrapepage <- "https://legacy.donbest.com/ncaab/scores/20210320.html"

testtable <- read_html(scrapepage) %>% 
  html_table(fill=T)
test3 <- testtable[[1]]%>%
  dplyr::select(1:10)%>%
  filter(!is.na(X2))%>%
  filter(X2 != "Team")%>%
  filter(!str_detect(X2,"COLLEGE BASKETBALL"))%>%
  dplyr::select(X2, X9)


test1 <- rbind(test1,test2, test3)

library(tidyr)

test1 <- tidyr::separate(test1, 
                       col = X2, 
                       into = c("Team1", "Team2"), 
                       sep = "(?<=[a-z])(?=[A-Z])")
test1 <- test1 %>%
  mutate(Team1 = if_else(Team1=="UCLAMichigan State", "UCLA", Team1))%>%
  mutate(Team2 = if_else(Team1=="UCLA", "Michigan State", Team2))



test1 <- tidyr::separate(test1, 
                       col = X9, 
                       into = c("Spread1", "Spread2"), 
                       sep = "-")

test1 <- test1 %>%
  mutate(Spread2 = if_else(is.na(Spread2),"0",Spread2))%>%
  mutate(Spread2 = if_else(Team1 == "Rutgers", "-1.0", Spread2))%>%
  dplyr::select(-Spread1)

test1$Team1 <- str_replace(test1$Team1, "cal santa barbara", "california-santa barbara")

TeamSpellings <- read.csv("data/MTeamSpellings.csv")

test1$Team1 <- tolower(test1$Team1)
test1$Team2 <- tolower(test1$Team2)

teams_w_spreads <- left_join(test1, TeamSpellings, by = c("Team1"="TeamNameSpelling"))

teams_w_spreads <- left_join(teams_w_spreads, TeamSpellings, by = c("Team2" = "TeamNameSpelling"))

names(teams_w_spreads)[4:5] <- c("TeamID1", "TeamID2") 

teams_w_spreads <- teams_w_spreads %>%
  mutate(lower_team = if_else(TeamID1 < TeamID2, TeamID1, TeamID2))%>%
  mutate(higher_team = if_else(TeamID1>TeamID2, TeamID1, TeamID2))

blank_preds <- left_join(blank_preds, teams_w_spreads, by=c("lower_team","higher_team"))

blank_preds$Spread2 <- as.numeric(blank_preds$Spread2)
blank_preds <- blank_preds %>%
  mutate(Spread2 = if_else(TeamID2 <TeamID1, -Spread2, Spread2))

blank_preds <- blank_preds %>%
  dplyr::select(-Team1, -Team2,-TeamID1, -TeamID2)

blank_preds <- blank_preds %>%
  mutate(predicted_spreads = if_else(!is.na(Spread2), Spread2, predicted_spreads))%>%
  dplyr::select(-Spread2)

names(blank_preds)[10] <- "Spread"

ind_vars2021 <- blank_preds 

write.csv(blank_preds, "data/ind_vars2021.csv")



