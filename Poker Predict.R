library(data.table) #data input
library(R.utils) #timing
library(Metrics) #auc
library(xgboost) #learning using Extreme Gradient Boosting
library(h2o) #learning using Random Forests

# Load data and set working directory

setwd("C:/Users/Laurae/Documents/Laurae Education/Machine Learning/Poker Data")
train <- as.data.frame(fread("train.csv", header = TRUE, sep = ","))
test <- as.data.frame(fread("test.csv", header = TRUE, sep = ","))
#submission <- as.data.frame(fread("sample_submission.csv", header = TRUE, sep = ","))$Win
submission <- as.data.frame(fread("solutions.csv", header = TRUE, sep = ","))$Win # This assumes you have the labeled test set

# Merge train/test
data <- rbind(train, cbind(test, Win = rep(-1, 1000000)))

# CreateFeatures <- function(data) {
#   
#   # Check for number occurrences
#   for (i in 1:13) {
#     cat("Number ", i, "\n", sep = "")
#     data[, paste("Number", i, sep = "")] <- apply(data[, 2:8], 1, function(x) {sum(x == i)})
#   }
#   # Add number 14 (Ace) for straight
#   cat("Number 14\n", sep = "")
#   data[, paste("Number", 14, sep = "")] <- apply(data[, 2:8], 1, function(x) {sum(x == 1)})
#   
#   # Check for suit occurrences
#   for (i in 1:4) {
#     cat("Suit ", i, "\n", sep = "")
#     data[, paste("Suit", i, sep = "")] <- apply(data[, 9:15], 1, function(x) {sum(x == i)})
#   }
#   
#   # Check for straight occurrences
#   for (i in 1:10) {
#     cat("Straight ", i, "-", i+4, "\n", sep = "")
#     data[, paste("Straight", i, sep = "")] <- apply(data[, i + (27:31) - 1], 1, function(x) {min(x[1], 1) + min(x[2], 1) + min(x[3], 1) + min(x[4], 1) + min(x[5], 1)})
#   }
#   
#   # Find pairs
#   cat("1 Pairs")
#   data[, "1Pairs"] <- apply(data[, 27:39], 1, function(x) {min(sum(x == 2), 1)})
#   cat(": ", sum(data[1:200000, "1Pairs"] == 1), "\n", sep = "")
#   
#   # Find double pairs
#   cat("2 Pairs")
#   data[, "2Pairs"] <- apply(data[, 27:39], 1, function(x) {floor(min(sum(x > 1), 2)/2)})
#   cat(": ", sum(data[1:200000, "2Pairs"] == 1), "\n", sep = "")
#   
#   # Find three of a kind
#   cat("3 Kind")
#   data[, "3Kind"] <- apply(data[, 27:39], 1, function(x) {min(sum(x == 3), 1)})
#   cat(": ", sum(data[1:200000, "3Kind"] == 1), "\n", sep = "")
#   
#   # Find straight
#   cat("Straight")
#   data[, "Straight"] <- apply(data[, 44:53], 1, function(x) {min(sum(x == 5), 1)})
#   cat(": ", sum(data[1:200000, "Straight"] == 1), "\n", sep = "")
#   
#   # Find flush
#   cat("Flush")
#   data[, "Flush"] <- apply(data[, 40:43], 1, function(x) {min(sum(x >= 5), 1)})
#   cat(": ", sum(data[1:200000, "Flush"] == 1), "\n", sep = "")
#   
#   # Find full house
#   cat("Full House")
#   data[, "FullHouse"] <- floor((data[, "1Pairs"] + data[, "3Kind"])/2)
#   cat(": ", sum(data[1:200000, "FullHouse"] == 1), "\n", sep = "")
#   
#   # Find four of a kind
#   cat("4 Kind")
#   data[, "4Kind"] <- apply(data[, 27:39], 1, function(x) {min(sum(x >= 4), 1)})
#   cat(": ", sum(data[1:200000, "4Kind"] == 1), "\n", sep = "")
#   
#   return(data)
#   
# }

reCat <- function(input, rewrite, reline, init, verbose) {
  
  # input => text to print
  # rewrite = erase the current line? (True/False)
  # reline => add a line break?
  # init = time to benchmark
  # verbose = to print or not to print?
  if (verbose == TRUE) {
    cat(ifelse(rewrite, "\r", ""), "[", sprintf("%7.2f", (System$currentTimeMillis() - init) / 1000), " seconds]: ", input, ifelse(reline, "\n", ""), sep = "")
  }
  
}

CreateFeatures <- function(data, cards, suits, merge = TRUE, verbose = TRUE, tag = "") {
  
  # data => initial data (R x C)
  # cards => number of the cards (R x n)
  # suits => suits of the cards (R x n)
  # merge => if TRUE, then merge all frames for debugging, else merge only available combinations
  # verbose => if TRUE, print debugging information
  
  # create temporary frames
  Card_Numbers <- data.frame(matrix(nrow = nrow(cards), ncol = 14))
  colnames(Card_Numbers) <- paste("Number", 1:14, sep = "")
  Card_Suits <- data.frame(matrix(nrow = nrow(cards), ncol = 4))
  colnames(Card_Suits) <- paste("Suit", 1:4, sep = "")
  Card_Straights <- data.frame(matrix(nrow = nrow(cards), ncol = 10))
  colnames(Card_Straights) <- paste("Straight", 1:10, sep = "")
  Card_Combos <- data.frame(matrix(nrow = nrow(cards), ncol = 7))
  colnames(Card_Combos) <- c("1Pairs", "2Pairs", "3Kind", "Straight", "Flush", "FullHouse", "4Kind")
  
  time <- System$currentTimeMillis()
  
  # Check for number occurrences
  for (i in 1:13) {
    toPrint <- paste("Number ", i, sep = "")
    reCat(input = toPrint, rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
    Card_Numbers[, paste("Number", i, sep = "")] <- apply(cards, 1, function(x) {sum(x == i)})
    toPrint <- paste(toPrint, " done.", sep = "")
    reCat(input = toPrint, rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)
  }
  # Add number 14 (Ace) for straight
  toPrint <- paste("Number ", 14, sep = "")
  reCat(input = toPrint, rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
  Card_Numbers[, paste("Number", 14, sep = "")] <- apply(cards, 1, function(x) {sum(x == 1)})
  toPrint <- paste(toPrint, " done.", sep = "")
  reCat(input = toPrint, rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)
  
  # Check for suit occurrences
  for (i in 1:4) {
    toPrint <- paste("Suit ", i, sep = "")
    reCat(input = toPrint, rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
    Card_Suits[, paste("Suit", i, sep = "")] <- apply(suits, 1, function(x) {sum(x == i)})
    toPrint <- paste(toPrint, " done.", sep = "")
    reCat(input = toPrint, rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)
  }
  
  # Check for straight occurrences
  for (i in 1:10) {
    toPrint <- paste("Straight ", i, "-", i+4, sep = "")
    reCat(input = toPrint, rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
    Card_Straights[, paste("Straight", i, sep = "")] <- apply(Card_Numbers[, i + (0:4)], 1, function(x) {min(x[1], 1) + min(x[2], 1) + min(x[3], 1) + min(x[4], 1) + min(x[5], 1)})
    toPrint <- paste(toPrint, " done.", sep = "")
    reCat(input = toPrint, rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)
  }
  
  # Find pairs
  toPrint <- paste("1-Pair", sep = "")
  reCat(input = toPrint, rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
  Card_Combos[, "1Pairs"] <- apply(Card_Numbers[, 1:13], 1, function(x) {min(sum(x == 2), 1)})
  toPrint <- paste(toPrint, ": ", sum(Card_Combos[, "1Pairs"] == 1), sep = "")
  reCat(input = toPrint, rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)

  
  # Find double pairs
  toPrint <- paste("2-Pair", sep = "")
  reCat(input = toPrint, rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
  Card_Combos[, "2Pairs"] <- apply(Card_Numbers[, 1:13], 1, function(x) {floor(min(sum(x == 2), 2)/2)})
  toPrint <- paste(toPrint, ": ", sum(Card_Combos[, "2Pairs"] == 1), sep = "")
  reCat(input = toPrint, rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)
  
  # Find three of a kind
  toPrint <- paste("3-Kind", sep = "")
  reCat(input = toPrint, rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
  Card_Combos[, "3Kind"] <- apply(Card_Numbers[, 1:13], 1, function(x) {min(sum(x == 3), 1)})
  toPrint <- paste(toPrint, ": ", sum(Card_Combos[, "3Kind"] == 1), sep = "")
  reCat(input = toPrint, rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)
  
  # Find straight
  toPrint <- paste("Straight", sep = "")
  reCat(input = toPrint, rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
  Card_Combos[, "Straight"] <- apply(Card_Straights, 1, function(x) {min(sum(x == 5), 1)})
  toPrint <- paste(toPrint, ": ", sum(Card_Combos[, "Straight"] == 1), sep = "")
  reCat(input = toPrint, rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)
  
  # Find flush
  toPrint <- paste("Flush", sep = "")
  reCat(input = toPrint, rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
  Card_Combos[, "Flush"] <- apply(Card_Suits, 1, function(x) {min(sum(x >= 5), 1)})
  toPrint <- paste(toPrint, ": ", sum(Card_Combos[, "Flush"] == 1), sep = "")
  reCat(input = toPrint, rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)
  
  # Find full house
  toPrint <- paste("Full House", sep = "")
  reCat(input = toPrint, rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
  Card_Combos[, "FullHouse"] <- floor((Card_Combos[, "1Pairs"] + Card_Combos[, "3Kind"])/2)
  toPrint <- paste(toPrint, ": ", sum(Card_Combos[, "FullHouse"] == 1), sep = "")
  reCat(input = toPrint, rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)
  
  # Find four of a kind
  toPrint <- paste("4-Kind", sep = "")
  reCat(input = toPrint, rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
  Card_Combos[, "4Kind"] <- apply(Card_Numbers[, 1:13], 1, function(x) {min(sum(x >= 4), 1)})
  toPrint <- paste(toPrint, ": ", sum(Card_Combos[, "4Kind"] == 1), sep = "")
  reCat(input = toPrint, rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)
  
  if (merge == TRUE) {
    if (!(tag == "")) {
      reCat(input = "Adding column tags", rewrite = FALSE, reline = TRUE, init = time, verbose = TRUE)
      colnames(Card_Numbers) <- paste(colnames(Card_Numbers), tag, sep = "")
      colnames(Card_Suits) <- paste(colnames(Card_Suits), tag, sep = "")
      colnames(Card_Straights) <- paste(colnames(Card_Straights), tag, sep = "")
      colnames(Card_Combos) <- paste(colnames(Card_Combos), tag, sep = "")
    }
    reCat(input = "Merging data frames", rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
    data <- cbind(data, Card_Numbers, Card_Suits, Card_Straights, Card_Combos)
    reCat(input = "Merged data frames", rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)
  } else {
    if (!(tag == "")) {
      reCat(input = "Adding column tags", rewrite = FALSE, reline = TRUE, init = time, verbose = TRUE)
      colnames(Card_Combos) <- paste(colnames(Card_Combos), tag, sep = "")
    }
    reCat(input = "Merging data frames", rewrite = FALSE, reline = FALSE, init = time, verbose = TRUE)
    data <- cbind(data, Card_Combos)
    reCat(input = "Merged data frames", rewrite = TRUE, reline = TRUE, init = time, verbose = TRUE)
  }
  
  return(data)
  
}

data <- CreateFeatures(data, cards = data[, 2:8], suits = data[9:15], merge = TRUE, verbose = TRUE, tag = "_Private")
data <- CreateFeatures(data, cards = data[, 4:8], suits = data[11:15], merge = TRUE, verbose = TRUE, tag = "_Public")

train_enhanced <- data[1:200000, ]
test_enhanced <- data[200001:1200000, -26]



# ~~~~ Setup fast AUC method
# Handles n1*n2 out of bounds (integer overflow)

FastAUC <- function(y, x) {
  
  # y = actual
  # x = predicted
  x1 = x[y==1]
  n1 = as.numeric(length(x1))
  x2 = x[y==0]
  n2 = as.numeric(length(x2))
  r = rank(c(x1,x2))
  return((sum(r[1:n1]) - n1*(n1+1)/2) / (n1*n2))
  
}


# ~~~~ Prepare for XGBoost

# Create datasets
NotLearn <- c("Win", "id", "Big_Blind", paste("Number", 1:14, "_Private", sep = ""), paste("Number", 1:14, "_Public", sep = ""), paste("Suit", 1:4, "_Private", sep = ""), paste("Suit", 1:4, "_Public", sep = ""), paste("Straight", 1:10, "_Private", sep = ""), paste("Straight", 1:10, "_Public", sep = ""))
train_temp <- xgb.DMatrix(data = data.matrix(train_enhanced[, !colnames(train_enhanced) %in% NotLearn]), label = data.matrix(train_enhanced$Win))
test_temp <- xgb.DMatrix(data = data.matrix(test_enhanced[, !colnames(test_enhanced) %in% NotLearn]))

# ~~ 5-fold Cross-Validation
# [104]	train-auc:0.860366+0.000434	test-auc:0.850154+0.002617
gc(verbose=FALSE)
set.seed(11111)
xgb.cv <- xgb.cv(data = train_temp,
                 nthread = 2,
                 nfold = 5,
                 max.depth = 5,
                 eta = 0.1,
                 subsample = 1.00,
                 colsample_bytree = 1.00,
                 nrounds = 1000000,
                 objective = "binary:logistic",
                 eval_metric = "auc",
                 verbose = TRUE,
                 early.stop.round = 40,
                 maximize = TRUE)

# ~~ Train XGBoost
# [124]	train-auc:0.860429
gc(verbose=FALSE)
set.seed(11111)
xgb.train <- xgb.train(data = train_temp,
                       nthread = 2,
                       max.depth = 5,
                       eta = 0.1,
                       subsample = 1.00,
                       colsample_bytree = 1.00,
                       nrounds = 124,
                       objective = "binary:logistic",
                       eval_metric = "auc",
                       verbose = TRUE,
                       early.stop.round = 40,
                       maximize = TRUE,
                       watchlist = list(train = train_temp))

# ~~ Debug information about XGBoost
xgb.features <- xgb.importance(feature_names = colnames(train_enhanced)[!colnames(train_enhanced) %in% NotLearn], model = xgb.train)
print(xgb.features, nrows = 999)

# ~~ Test XGBoost on the test set
# Public = 0.8506203
# Private = 0.8503818
xgb.predictions <- predict(xgb.train, test_temp)
cat("Public AUC: ", FastAUC(y = submission[1:100000], x = xgb.predictions[1:100000]), "\nPrivate AUC: ", FastAUC(y = submission[100001:1000000], x = xgb.predictions[100001:1000000]), "\n", sep = "")



# ~~~~ Prepare for h2o


h2o.init()
train_h2o <- as.h2o(cbind(train_enhanced[, !colnames(train_enhanced) %in% NotLearn], Win = as.factor(train_enhanced$Win)))
test_h2o <- as.h2o(test_enhanced[, !colnames(test_enhanced) %in% NotLearn])


# ~~~ Random Forest

# ~~ 5-fold Cross-Validation
# auc 0.8473551 + 0.0015013177
h2o.rf.cv <- h2o.randomForest(x = colnames(train_enhanced)[!colnames(train_enhanced) %in% NotLearn],
                              y = "Win",
                              training_frame = train_h2o,
                              ntrees = 50,
                              max_depth = 10,
                              seed = 11111,
                              nfolds = 5,
                              fold_assignment = "Stratified",
                              stopping_metric = "AUC")
summary(h2o.rf.cv)

# ~~ Test h2o Random Forest on the test set
# Public = 0.8473699
# Private = 0.8468554
h2o.rf.predictions <- predict(h2o.rf.cv, test_h2o)
h2o.rf.predictions <- as.data.frame(h2o.rf.predictions$p1)
h2o.rf.predictions <- as.numeric(h2o.rf.predictions$p1)
cat("Public AUC: ", FastAUC(y = submission[1:100000], x = h2o.rf.predictions[1:100000]), "\nPrivate AUC: ", FastAUC(y = submission[100001:1000000], x = h2o.rf.predictions[100001:1000000]), "\n", sep = "")


# ~~~ Gradient Boosted Machine

# ~~ 5-fold Cross-Validation
# auc 0.84802705 + 0.0014191598
h2o.gbm.cv <- h2o.gbm(x = colnames(train_enhanced)[!colnames(train_enhanced) %in% NotLearn],
                      y = "Win",
                      training_frame = train_h2o,
                      ntrees = 50,
                      max_depth = 5,
                      seed = 11111,
                      nfolds = 5,
                      score_each_iteration = TRUE,
                      fold_assignment = "Stratified",
                      stopping_metric = "AUC")
summary(h2o.gbm.cv)

# ~~ Test h2o Gradient Boosted Machine on the test set
# Public = 0.8485799
# Private = 0.8485444
h2o.gbm.predictions <- predict(h2o.gbm.cv, test_h2o)
h2o.gbm.predictions <- as.data.frame(h2o.gbm.predictions$p1)
h2o.gbm.predictions <- as.numeric(h2o.gbm.predictions$p1)
cat("Public AUC: ", FastAUC(y = submission[1:100000], x = h2o.gbm.predictions[1:100000]), "\nPrivate AUC: ", FastAUC(y = submission[100001:1000000], x = h2o.gbm.predictions[100001:1000000]), "\n", sep = "")


# ~~~ Deep Learning (Dense layers: 50x25x10x1 => 37x50 + 50x25 + 25x10 + 10 => 1850+1250+250+10 = 3360 parameters)

# ~~ 5-fold Cross-Validation
# auc 0.8437943 + 0.0014553933
h2o.dl.cv <- h2o.deeplearning(x = colnames(train_enhanced)[!colnames(train_enhanced) %in% NotLearn],
                              y = "Win",
                              training_frame = train_h2o,
                              standardize = TRUE,
                              activation = "Tanh",
                              hidden = c(50, 25, 10),
                              train_samples_per_iteration = -2,
                              adaptive_rate = TRUE,
                              loss = "CrossEntropy",
                              score_interval = 10,
                              score_training_samples = 0,
                              score_validation_samples = 0,
                              score_duty_cycle = 0.1,
                              stopping_metric = "AUC",
                              stopping_tolerance = 0.0000001,
                              fast_mode = TRUE,
                              seed = 11111,
                              nfolds = 5,
                              fold_assignment = "Stratified")
summary(h2o.dl.cv)

# ~~ Test h2o Gradient Boosted Machine on the test set
# Public = 0.8427155
# Private = 0.8432251
h2o.dl.predictions <- predict(h2o.dl.cv, test_h2o)
h2o.dl.predictions <- as.data.frame(h2o.dl.predictions$p1)
h2o.dl.predictions <- as.numeric(h2o.dl.predictions$p1)
cat("Public AUC: ", FastAUC(y = submission[1:100000], x = h2o.dl.predictions[1:100000]), "\nPrivate AUC: ", FastAUC(y = submission[100001:1000000], x = h2o.dl.predictions[100001:1000000]), "\n", sep = "")

# Shutdown h2o
h2o.shutdown()



# ~~~~ Ensemble using the Powering method (reference method: Laurae)

# ~~ Theoretical best (XGBoost)
# Public = 0.8506203
# Private = 0.8503818

# ~~ Ensemble best with weights: better public, similar private
# Public = 0.8506915
# Private = 0.850382

# ~~ Ensemble best with weights from magic numbers: lower public, higher private
# Public = 0.8504244
# Private = 0.8505212

PowerPrediction <- function(preds, power, weight = rep(1, ncol(preds))) {
  predicted <- rep(0, nrow(preds))
  for (i in 1:ncol(preds)) {
    predicted <- predicted + (weight[i] * (preds[[i]]^power) / sum(weight))
  }
  return(predicted)
}

df.predictions <- data.frame(XGBoost = xgb.predictions, h2o.rf = h2o.rf.predictions, h2o.gbm = h2o.gbm.predictions, h2o.dl = h2o.dl.predictions)
ens.predictions <- PowerPrediction(df.predictions, power = 12, weight = c(16, 1, 2, 0))
ens.predictions <- PowerPrediction(df.predictions, power = 4, weight = c(14, -2.5, 2.5, 0.5)) # Magic numbers (black magic)
cat("Public AUC: ", FastAUC(y = submission[1:100000], x = ens.predictions[1:100000]), "\nPrivate AUC: ", FastAUC(y = submission[100001:1000000], x = ens.predictions[100001:1000000]), "\n", sep = "")
