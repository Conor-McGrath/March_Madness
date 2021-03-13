######### Part 1: Get Spreads for all Past Games

# Read in Spread info
SpreadDataset <- read.csv("data/SpreadDataset.csv")

# Get all past results
# Get Ken Pom Data from 2002 on joined with Kaggle results data for every game and tidy the data

kp_raw <- read_csv("data/PomeryRatings.csv")%>%
  dplyr::select(team_name = Team, TeamID, Season, EM = AdjEM, adj_off = AdjO, adj_def = AdjD, adj_tempo = AdjT)

regsznresults <-read_csv("data/Kaggle/MRegularSeasonCompactResults.csv")
tourneyresults <- read_csv("data/Kaggle/MNCAATourneyCompactResults.csv")%>%
  filter(Season <= 2014) # For Submission 1, want to test model on tourny probs from 2015-2019. Not necessary for Submission 2.
sectourneyresults <- read_csv("data/Kaggle/MSecondaryTourneyCompactResults.csv")

all_past_results <- bind_rows(regsznresults, tourneyresults, sectourneyresults) %>% 
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

all_past_results_with_spread <- all_past_results

write.csv(all_past_results_with_spread, "data/all_past_results_with_spread.csv", row.names = FALSE)


########## Part 2: Run model with Spread as Target Variable

library(caret)
library(MASS)
library(e1071)
library(xgboost)
library(randomForest)
library(tidyverse)
library(Matrix)

all_past_results <- read_csv("data/all_past_results_with_spread.csv")

### Convert to model ready matrix

model_rdy_results <- all_past_results %>%
  mutate(lower_team_wins = ifelse(lower_team_wins=="YES", 1, 0))%>%
  mutate(lower_team_court_adv = ifelse(lower_team_court_adv=="H", 2, ifelse(lower_team_court_adv=="A", 1, 0)))%>%
  dplyr::select(Season, lower_team, higher_team, EM_diff, adj_off_diff, adj_def_diff, adj_tempo_diff, lower_team_court_adv, Spread, lower_team_wins)

### Model

#### Split into train and test

trainSize <- round(nrow(model_rdy_results) * 0.8)
testSize <- nrow(model_rdy_results) - trainSize

set.seed(1)
training_indices <- sample(seq_len(nrow(model_rdy_results)),
                           size=trainSize)
trainSet <- model_rdy_results[training_indices, ]
testSet <- model_rdy_results[-training_indices, ]

# Create training matrix
dtrain <- xgb.DMatrix(data = as.matrix(trainSet[, 1:8]), label = trainSet$lower_team_wins) # Taking out Spread from model just for now. If you want it back in do 1:9
# Create test matrix
dtest <- xgb.DMatrix(data = as.matrix(testSet[, 1:8]), label = testSet$lower_team_wins) # same as above

# Cross validation for best iteration

set.seed(12345)
bst_1 <- xgb.cv(data = dtrain, 
                nfold=5,
                nrounds = 2000, 
                eta=.1,
                
                verbose = 1, 
                print_every_n = 20, 
                early_stopping_rounds = 20,
)

# Best iteration is 37

set.seed(12345)
bst_mod_1 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold=5,
                    
                    eta = 0.005, # Set learning rate
                    max.depth =  7, # Set max depth
                    min_child_weight = 10, # Set minimum number of samples in node to split
                    gamma = 0, # Set minimum loss reduction for split
                    subsample =  0.9, # Set proportion of training data to use in tree
                    colsample_bytree = 0.9, # Set number of variables to use in each tree
                    
                    nrounds = 100, # Set number of rounds
                    early_stopping_rounds = 20, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
)

set.seed(12345)
bst_mod_2 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold=5,
                    
                    eta = 0.01, # Set learning rate
                    max.depth =  7, # Set max depth
                    min_child_weight = 10, # Set minimum number of samples in node to split
                    gamma = 0, # Set minimum loss reduction for split
                    subsample =  0.9, # Set proportion of training data to use in tree
                    colsample_bytree = 0.9, # Set number of variables to use in each tree
                    
                    nrounds = 100, # Set number of rounds
                    early_stopping_rounds = 20, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
)

set.seed(12345)
bst_mod_3<- xgb.cv(data = dtrain, # Set training data
                   
                   nfold=5,
                   
                   eta = 0.05, # Set learning rate
                   max.depth =  7, # Set max depth
                   min_child_weight = 10, # Set minimum number of samples in node to split
                   gamma = 0, # Set minimum loss reduction for split
                   subsample =  0.9, # Set proportion of training data to use in tree
                   colsample_bytree = 0.9, # Set number of variables to use in each tree
                   
                   nrounds = 100, # Set number of rounds
                   early_stopping_rounds = 20, # Set number of rounds to stop at if there is no improvement
                   
                   verbose = 1, # 1 - Prints out fit
                   nthread = 1, # Set number of parallel threads
                   print_every_n = 20, # Prints out result every 20th iteration
)
set.seed(12345)
bst_mod_4 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold=5,
                    
                    eta = 0.1, # Set learning rate
                    max.depth =  7, # Set max depth
                    min_child_weight = 10, # Set minimum number of samples in node to split
                    gamma = 0, # Set minimum loss reduction for split
                    subsample =  0.9, # Set proportion of training data to use in tree
                    colsample_bytree = 0.9, # Set number of variables to use in each tree
                    
                    nrounds = 100, # Set number of rounds
                    early_stopping_rounds = 20, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
)
set.seed(12345)
bst_mod_5 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold=5,
                    
                    eta = 0.3, # Set learning rate
                    max.depth =  7, # Set max depth
                    min_child_weight = 10, # Set minimum number of samples in node to split
                    gamma = 0, # Set minimum loss reduction for split
                    subsample =  0.9, # Set proportion of training data to use in tree
                    colsample_bytree = 0.9, # Set number of variables to use in each tree
                    
                    nrounds = 100, # Set number of rounds
                    early_stopping_rounds = 20, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
)



# Visualize Results

# Extract results for model with eta = 0.005
pd1 <- cbind.data.frame(bst_mod_1$evaluation_log[,c("iter", "test_rmse_mean")], rep(0.005, nrow(bst_mod_1$evaluation_log)))
names(pd1)[3] <- "eta"
# Extract results for model with eta = 0.01
pd2 <- cbind.data.frame(bst_mod_2$evaluation_log[,c("iter", "test_rmse_mean")], rep(0.01, nrow(bst_mod_2$evaluation_log)))
names(pd2)[3] <- "eta"
# Extract results for model with eta = 0.05
pd3 <- cbind.data.frame(bst_mod_3$evaluation_log[,c("iter", "test_rmse_mean")], rep(0.05, nrow(bst_mod_3$evaluation_log)))
names(pd3)[3] <- "eta"
# Extract results for model with eta = 0.1
pd4 <- cbind.data.frame(bst_mod_4$evaluation_log[,c("iter", "test_rmse_mean")], rep(0.1, nrow(bst_mod_4$evaluation_log)))
names(pd4)[3] <- "eta"
# Extract results for model with eta = 0.3
pd5 <- cbind.data.frame(bst_mod_5$evaluation_log[,c("iter", "test_rmse_mean")], rep(0.3, nrow(bst_mod_5$evaluation_log)))
names(pd5)[3] <- "eta"
# Join datasets
plot_data <- rbind.data.frame(pd1, pd2, pd3, pd4, pd5)
# Converty ETA to factor
plot_data$eta <- as.factor(plot_data$eta)
# Plot points
g_6 <- ggplot(plot_data, aes(x = iter, y = test_rmse_mean, color = eta))+
  geom_point(alpha = 0.5) +
  theme_bw() + # Set theme
  theme(panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  labs(x = "Number of Trees", title = "Error Rate v Number of Trees",
       y = "Test RMSE Mean", color = "Learning \n Rate")  # Set labels
g_6

# Plot lines
g_7 <- ggplot(plot_data, aes(x = iter, y = test_rmse_mean, color = eta))+
  geom_smooth(alpha = 0.5) +
  theme_bw() + # Set theme
  theme(panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  labs(x = "Number of Trees", title = "Error Rate v Number of Trees",
       y = "Error Rate", color = "Learning \n Rate")  # Set labels
g_7

# Fit the XGBoost model

set.seed(12345)
bst_final <- xgboost(data = dtrain, # Set training data
                     
                     
                     
                     eta = 0.1, # Set learning rate
                     max.depth =  7, # Set max depth
                     min_child_weight = 10, # Set minimum number of samples in node to split
                     gamma = 0, # Set minimum loss reduction for split
                     subsample =  0.9, # Set proportion of training data to use in tree
                     colsample_bytree = 0.9, # Set number of variables to use in each tree
                     
                     nrounds = 35, # Set number of rounds
                     early_stopping_rounds = 20, # Set number of rounds to stop at if there is no improvement
                     
                     verbose = 1, # 1 - Prints out fit
                     nthread = 1, # Set number of parallel threads
                     print_every_n = 20, # Prints out result every 20th iteration
)


xgb_test_preds <- predict(bst_final, dtest, type = "prob")

imp_mat <- xgb.importance(model = bst_final)
# Plot importance (top 10 variables)
xgb.plot.importance(imp_mat, top_n = 10)

bst_final_spreads <- bst_final

saveRDS(bst_final, "data/bst_final_spreads.rds")

