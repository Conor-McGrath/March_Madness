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

trainSet <- trainSet %>%
  na.omit()

testSet <- testSet%>%
  na.omit()

# Create training matrix
dtrain <- xgb.DMatrix(data = as.matrix(trainSet[, 1:8]), label = trainSet$Spread) 
# Create test matrix
dtest <- xgb.DMatrix(data = as.matrix(testSet[, 1:8]), label = testSet$Spread)

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


xgb_test_preds <- predict(bst_final, dtest)

imp_mat <- xgb.importance(model = bst_final)
# Plot importance (top 10 variables)
xgb.plot.importance(imp_mat, top_n = 10)

bst_final_spreads <- bst_final

saveRDS(bst_final, "data/bst_final_spreads.rds")
