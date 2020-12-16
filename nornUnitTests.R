# Unit tests for noisyornetwork
# Author: michael.kirchhof@udo.edu
# Created: 14.12.2020

# Dependencies:
# library(testthat)
source("rsl.R")
source("norn.R")

predictInternalOneRuleTest <- function(){
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "taste", c("tasty", "not_tasty"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "meat", c("meat", "noMeat"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "healthy", c("healthy", "junkFood", "toxic"), accuracy = 1)
  rsl <- .addNoisyOR(rsl, c(tasty = 0.2, not_tasty = 0.9, junkFood = 0.6, toxic = 1))
  
  norn <- as.norn.rsl(rsl)
  norn <- .addRule.norn(norn, list(L1 = c(0.2, 0.9), L3 = c(1, 0.6, 1)), "R1", "A1")
  
  input1 <- data.frame("tasty" = 0.7, "meat" = 0.2, "healthy" = 0.6, "junkFood" = 0.3)
  input2 <- list(L1 = c(0.7, 0.3), L2 = c(0.2, 0.8), L3 = c(0.6, 0.3, 0.1), R1 = c(0, 1))
  
  out1 <- predict(rsl, input1)
  out1 <- unlist(out1)
  out2 <- .predict.norn(norn, input2)
  out2$R1 <- NULL
  out2$A1 <- NULL
  out2 <- unlist(out2)
  names(out2) <- names(out1)
  
  testthat::expect_equal(out1, out2)
}

predictInternalOneRuleTest()

predictInternalMultipleRulesTest <- function(){
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "taste", c("tasty", "not_tasty"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "meat", c("meat", "noMeat"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "healthy", c("healthy", "junkFood", "toxic"), accuracy = 1)
  rsl <- .addNoisyOR(rsl, c(tasty = 0.2, not_tasty = 0.9, junkFood = 0.6, toxic = 1))
  rsl <- .addNoisyOR(rsl, c(tasty = 0.6, not_tasty = 0.1, meat = 0.2))
  
  norn <- as.norn.rsl(rsl)
  norn <- .addRule.norn(norn, list(L1 = c(0.2, 0.9), L3 = c(1, 0.6, 1)), "R1", "A1")
  norn <- .addRule.norn(norn, list(L1 = c(0.6, 0.1), L2 = c(0.2, 1)), "R2", "A2")
  
  input1 <- data.frame("tasty" = 0.7, "meat" = 0.2, "healthy" = 0.6, "junkFood" = 0.3)
  input2 <- list(L1 = c(0.7, 0.3), L2 = c(0.2, 0.8), L3 = c(0.6, 0.3, 0.1), R1 = c(0, 1), R2 = c(0, 1))
  
  out1 <- predict(rsl, input1)
  out1 <- unlist(out1)
  out2 <- .predict.norn(norn, input2)
  out2$R1 <- NULL
  out2$R2 <- NULL
  out2$A1 <- NULL
  out2$A2 <- NULL
  out2 <- unlist(out2)
  names(out2) <- names(out1)
  
  testthat::expect_equal(out1, out2)
}

predictInternalMultipleRulesTest()

predictInternalLoopTest <- function(){
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "taste", c("tasty", "not_tasty"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "meat", c("meat", "noMeat"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "healthy", c("healthy", "junkFood", "toxic"), accuracy = 1)
  rsl <- .addNoisyOR(rsl, c(tasty = 0.2, not_tasty = 0.9, junkFood = 0.6, toxic = 1))
  rsl <- .addNoisyOR(rsl, c(tasty = 0.6, not_tasty = 0.1, meat = 0.2))
  rsl <- .addNoisyOR(rsl, c(meat = 0.8, noMeat = 0.1, healthy = 0.1, toxic = 0.95))
  
  norn <- as.norn.rsl(rsl)
  norn <- .addRule.norn(norn, list(L1 = c(0.2, 0.9), L3 = c(1, 0.6, 1)), "R1", "A1")
  norn <- .addRule.norn(norn, list(L1 = c(0.6, 0.1), L2 = c(0.2, 1)), "R2", "A2")
  norn <- .addRule.norn(norn, list(L2 = c(0.8, 0.1), L3 = c(0.1, 1, 0.95)), "R3", "A3")
  
  input1 <- data.frame("tasty" = 0.7, "meat" = 0.2, "healthy" = 0.6, "junkFood" = 0.3)
  input2 <- list(L1 = c(0.7, 0.3), L2 = c(0.2, 0.8), L3 = c(0.6, 0.3, 0.1), 
                 R1 = c(0, 1), R2 = c(0, 1), R3 = c(0, 1))
  
  out1 <- predict(rsl, input1)
  out1 <- unlist(out1)
  out2 <- .predict.norn(norn, input2)
  out2$R1 <- NULL
  out2$R2 <- NULL
  out2$R3 <- NULL
  out2$A1 <- NULL
  out2$A2 <- NULL
  out2$A3 <- NULL
  out2 <- unlist(out2)
  names(out2) <- names(out1)
  
  testthat::expect_equal(out1, out2, tolerance = 0.01)
}

predictInternalLoopTest()

predictTest <- function(){
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "taste", c("tasty", "not_tasty"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "meat", c("meat", "noMeat"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "healthy", c("healthy", "junkFood", "toxic"), accuracy = 1)
  rsl <- .addNoisyOR(rsl, c(tasty = 0.2, not_tasty = 0.9, junkFood = 0.6, toxic = 1))
  rsl <- .addNoisyOR(rsl, c(tasty = 0.6, not_tasty = 0.1, meat = 0.2))
  
  norn <- as.norn.rsl(rsl)
  norn <- .addRule.norn(norn, list(L1 = c(0.2, 0.9), L3 = c(1, 0.6, 1)), "R1", "A1")
  norn <- .addRule.norn(norn, list(L1 = c(0.6, 0.1), L2 = c(0.2, 1)), "R2", "A2")
  
  input <- data.frame("tasty" = c(0.1, 0.7), "meat" = c(0.9, 0.2), "healthy" = c(0.5, 0.6), "junkFood" = c(0.2, 0.3))
  
  out1 <- predict(rsl, input)
  out2 <- predict.norn(norn, rsl, input)
  
  testthat::expect_equal(out1, out2)
}

predictTest()
