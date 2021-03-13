# Make Stage 1 Predictions
library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(Matrix)
library(xgboost)

## Load Ken Pom Ratings
kp_raw <- read_csv("data/PomeryRatings.csv")%>%
  filter(Season == 2019)

# Get the Sample Submission df
submission_format <- read_csv("data/Kaggle/MSampleSubmissionStage1.csv")

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
  filter(Season == 2019)

# Create matrix and run through model to make predictions

submission_matrix <- xgb.DMatrix(data = as.matrix(blank_preds[, -1]))

bst_final <- read_rds("data/bst_final_spreads.rds")

predicted_spreads <- predict(bst_final, submission_matrix)

blank_preds$predicted_spreads <- predicted_spreads


# Now let's get the actual spreads for some of these games

SpreadDataset <- read_csv("data/SpreadDataset.csv")




