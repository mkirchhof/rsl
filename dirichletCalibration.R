# This is an implementation of Dirichlet Calibration that calibrates
# multi class classifiers
# Author: michael.kirchhof@tu-dortmund.de
# Date: 15.03.2021
# Version: 0.2.0 "Afraid of DAIN"


# Dependencies:
# library(glmnet)
# library(reldist)


# .trainDirCalibration - trains a dirichlet calibration by using multinomial
#                        regression with l2 loss from glmnet
.trainDirCalibration <- function(pred, actual, eps = 10e-6){
  # Preprocess data
  pred[pred < eps] <- eps
  pred <- log(pred)
  actual <- factor(actual, levels = colnames(pred))
  
  # Fit l2-penalized multinomial regression:
  cvfit <- glmnet::cv.glmnet(x = pred, y = actual, family = "multinomial",
                             alpha = 0)

  return(cvfit)
}


# .predictDirCalibration - given a trained calibrator, calibrates new given data
.predictDirCalibration <- function(cal, pred, eps = 10e-6){
  # Preprocess data
  pred[pred < eps] <- eps
  pred <- log(pred)
  
  # Calibrate
  calPred <- glmnet:::predict.cv.glmnet(cal, newx = pred, s = "lambda.min", 
                                        type = "response")
  
  return(calPred)
}


# dirCalibration - calibrates probabilities outputted by a multi-class classifier
#                  by using dirichlet calibration
# Input:
#  predTrain - matrix where each column contains predicted probabilities of one
#              class, used to train the calibrator
#  actualTrain - character vector containing the true class per observation, 
#                used to train the calibrator
#  newPred - matrix where each column contains predicted probabilities of one
#            class. Gets calibrated and is returned.
# Output:
#  a matrix of the same size as newPred, but with calibrated probabilities
dirCalibration <- function(predTrain, actualTrain, newPred){
  cal <- .trainDirCalibration(predTrain, actualTrain)
  calPred <- .predictDirCalibration(cal, newPred)
  
  return(calPred)
}


# .trainLogCalibration - trains a calibrator by fitting a logistic regression
# Input:
#  pred - numeric vector with probability estimates
#  actual - one-hot encoded vector of true classes
.trainLogCalibration <- function(pred, actual, eps = 10e-6){
  pred[pred < eps] <- eps
  
  # Fit logistic regression
  dat <- data.frame(x = pred, y = actual)
  mod <- glm(y ~ x, data = dat, family = binomial)
  
  return(mod)
}


# .predictLogCalibration - recalibrate probability estimates using logistic calibration
.predictLogCalibration <- function(cal, pred, eps = 10e-6){
  pred[pred < eps] <- eps
  
  recal <- predict(cal, data.frame(x = pred), type = "response")
  
  return(recal)
}


# evalCalibration - gives plot and measures of the calibration
evalCalibration <- function(pred, actual){
  # Compute number of true observations per class and per bin:
  binStats <- array(NA, dim = c(ncol(pred), 10, 3), 
                    dimnames = list("class" = colnames(pred),
                                    "bin" = 1:10,
                                    "measure" = c("n", "avgPred", "avgActual")))
  for(c in seq(ncol(pred))){
    for(b in 1:10){
      isInBin <- (b - 1) * 0.1 <= pred[, c] & pred[, c] < b * 0.1
      binStats[c, b, "n"] <- sum(isInBin)
      binStats[c, b, "avgPred"] <- mean(pred[isInBin, c])
      binStats[c, b, "avgActual"] <- mean(actual[isInBin] == colnames(pred)[c])
    }
  }
  
  # Overall metrics
  cwECE <- 1/(ncol(pred) * nrow(pred)) * sum(binStats[, , "n"] * 
           abs(binStats[, , "avgPred"] - binStats[, , "avgActual"]), na.rm = TRUE)
  cwuECE <- 1/(ncol(pred) * 10) * sum( 
            abs(binStats[, , "avgPred"] - binStats[, , "avgActual"]), na.rm = TRUE)
  
  # Binwise metrics
  bw <- matrix(NA, ncol = 4, nrow = 10)
  colnames(bw) <- c("weightedMeanPred", "weightedMeanActual", "lowerQ", "upperQ")
  for(b in 1:10){
    if(sum(binStats[, b, "n"]) > 0){
      bw[b, "weightedMeanActual"] <- weighted.mean(binStats[, b, "avgActual"], w = binStats[, b, "n"])
      bw[b, "weightedMeanPred"] <- weighted.mean(binStats[, b, "avgPred"], w = binStats[, b, "n"])
      bw[b, c("lowerQ", "upperQ")] <- reldist::wtd.quantile(binStats[, b, "avgActual"], q = c(0.25, 0.75), weight = binStats[, b, "n"])
    }
  }

  # Plot
  op <- par(mar = c(4, 4, 2, 0) + 0.1)
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), ylab = "Relative amount of true labels", xlab = "Prediction")
  lines(x = c(0, 1), y = c(0, 1), lty = 2)
  lines(x = bw[, "weightedMeanPred"], y = bw[, "lowerQ"], lty = 1, col = "grey")
  lines(x = bw[, "weightedMeanPred"], y = bw[, "upperQ"], lty = 1, col = "grey")
  lines(x = bw[, "weightedMeanPred"], y = bw[, "weightedMeanActual"], lty = 1)
  # for(c in seq(ncol(pred))){
  #   lines(x = binStats[c, , "avgPred"], y = binStats[c, , "avgActual"], lty = 1, col = "red")
  # }
  mtext(text = paste0("cw-ECE = ", round(cwECE, 4)), side = 3, line = 1)
  par(op)
  
  return(binStats)
}


balanceDataset <- function(pred){
  indexes <- c()
  for(c in seq(ncol(pred))){
    for(b in 1:10){
      isInBin <- (b - 1) * 0.1 <= pred[, c] & pred[, c] < b * 0.1
      try({indexes <- c(indexes, sample(which(isInBin), 20, replace = TRUE))})
    }
  }
  return(indexes)
}


balanceDataset2 <- function(pred){
  entropy <- apply(pred, 1, function(x) - sum(log(x) * x))
  entropy[is.na(entropy)] <- 0
  stdEntropy <- entropy / max(entropy)
  indexes <- sample(seq(nrow(pred)), 10000, replace = TRUE, prob = stdEntropy)
  
  return(indexes)
}
