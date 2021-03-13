# Make Stage 1 Predictions
library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(Matrix)
library(xgboost)
## Load Ken Pom Ratings
kp_raw <- read_csv("data/PomeryRatings.csv")

## Load just the tournament results from 2015-2019
tourneyresults <- read_csv("data/Kaggle/MNCAATourneyCompactResults.csv")%>%
  filter(Season > 2014) # For Submission 1, want to test model on tourny probs from 2015-2019. Not necessary for Submission 2.

## Load in the data that helps convert DayNum variable to actual Date
MSeasons <- read.csv("data/Kaggle/MSeasons.csv")

# We only want the data from seasons 2008 and after since we are training the model using data starting in 2007-2008.
MSeasons2 <- MSeasons %>%
  filter(Season >= 2008) %>%
  dplyr::select("Season", "DayZero")

## Here we are merging the tournament results with the date converter so we can get the actual date of each tournament game.
DayZeroMerge <- left_join(tourneyresults, MSeasons2, by = "Season")
DayZeroMerge$DayZero <- mdy(DayZeroMerge$DayZero)
DayZeroMerge$Date <- DayZeroMerge$DayZero + DayZeroMerge$DayNum

## Load in all past results
all_past_results <- read.csv("data/all_past_results.csv")

## convert the date to a date type
library(lubridate)
all_past_results$Date <- ymd(all_past_results$Date)

## join the past results by date.
test <- left_join(DayZeroMerge, all_past_results, by=c("Date", "WTeamID", "LTeamID"))

## Create an id column in the submission file format
test <- test %>%
  mutate(id = paste(Season.x, lower_team, higher_team, sep = "_"))

## Select only the columns that are used in the model
test <- test %>%
  dplyr::select(id, Season.x, lower_team, higher_team, EM_diff, adj_off_diff, adj_def_diff, adj_tempo_diff, lower_team_court_adv, Spread)%>%
  rename("Season" = "Season.x")

# Get the Sample Submission df
submission_format <- read_csv("data/Kaggle/MSampleSubmissionStage1.csv")

# Link the spreads for the tournament games to the submission format df that has all hypothetical matchups. There will be many NAs since there are no spreads available for games that were never played.
# test2 <- left_join(submission_format, test, by=c("ID"="id"))

# Get Kaggle stats for all hypothetical matchups
blank_stage_1_preds <- read_csv("data/Kaggle/MSampleSubmissionStage1.csv") %>%
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
  dplyr::select(-contains(".x"), -contains(".y"))

# Cbind the spreads to the df with all other predictor variables. Again, the spread column will have NAs for matchups that did not actually take place.
# blank_stage_1_preds$Spread <- test2$Spread

########################################################
# This is where we have a question about how to proceed. To fill in these missing NAs, our idea
# create a model that can predict the Spread for all hypothetical matchups. This way each row will
# have a spread, either actual Vegas spread or model-generated.
########################################################

# Create matrix and run through model to make predictions

submission_matrix <- xgb.DMatrix(data = as.matrix(blank_stage_1_preds[, -1]))

bst_final <- read_rds("data/bst_final.rds")

stage_1_preds <- predict(bst_final, submission_matrix, type = "prob")

# Calculate log loss
log_loss(testSet$lower_team_wins %>% as.numeric - 1, xgb_test_preds)

# Get Predictions and format correctly for submission
preds_to_send <- blank_stage_1_preds %>%
  dplyr::select(id) %>%
  mutate(Pred = stage_1_preds)

colnames(preds_to_send) <- c("ID", "Pred")

preds_to_send <- preds_to_send %>%
  mutate(Pred = if_else(Pred>1,1,Pred))%>%
  mutate(Pred=if_else(Pred<0,0,Pred))

write.csv(preds_to_send, "data/preds_to_send.csv", row.names = FALSE)
