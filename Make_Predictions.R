# Make Stage 1 Predictions
library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(Matrix)
library(xgboost)
## Load Ken Pom Ratings
kp_raw <- read_csv("data/PomeryRatings.csv")

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

submission_matrix <- xgb.DMatrix(data = as.matrix(blank_stage_1_preds[, -1]))

bst_final <- read_rds("data/bst_final.rds")

stage_1_preds <- predict(bst_final, submission_matrix, type = "prob")

log_loss(testSet$lower_team_wins %>% as.numeric - 1, xgb_test_preds)

preds_to_send <- blank_stage_1_preds %>%
  dplyr::select(id) %>%
  mutate(Pred = stage_1_preds)

colnames(preds_to_send) <- c("ID", "Pred")

write.csv(preds_to_send, "data/preds_to_send.csv", row.names = FALSE)
