# End-to-end tests for the Rule Stacking Learner to see if the probabilistic
# inference works as expected. Expected results are from the experimente.ods
# conducted in an existing software. That's also where the experiment numbers
# are from. Not all of the experiments from experimente.ods have been conducted
# because they partly tested other approaches and are now redundant.
#
# Author: michael.kirchhof@udo.edu
# Created: 21.07.2020

# Dependencies:
# library(testthat)
source("rsl.R")

predictTest1to32 <- function(){
  # Test predictions on experiments 1-20, 33
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 1)
  input <- data.frame(A = c(1, 0.8, 0.5, 0.2, 0, 1, 0.8, 0.5, 0.2, 0, 0.3, 0.3, 0.3, 0.3, 0.3, 0.6, 0.6, 0.6, 0.6, 0.6, 0.3),
                      B = c(0.3, 0.3, 0.3, 0.3, 0.3, 0.6, 0.6, 0.6, 0.6, 0.6, 1, 0.8, 0.5, 0.2, 0, 1, 0.8, 0.5, 0.2, 0, 0.6))
  out <- predict(rsl, data = input)[, c("A", "B")]
  out <- round(out, 4)
  expected <- data.frame(A = c(1, 0.5455, 0.2308, 0.0698, 0, 1, 0.7059, 0.3750, 0.1304, 0, 0.3, 0.2553, 0.1765, 0.0789, 0, 0.6, 0.5455, 0.4286, 0.2308, 0, 0.2045),
                         B = c(1, 0.6818, 0.4615, 0.3488, 0.3, 1, 0.8824, 0.7500, 0.6522, 0.6, 1, 0.8511, 0.5882, 0.2632, 0, 1, 0.9091, 0.7143, 0.3846, 0, 0.6818))
  testthat::expect_equivalent(out, expected)
  
  # Test prediction on experiments 21-27
  rsl <- addRule(rsl, "A <- B", 1)
  input <- data.frame(A = c(0.2, 1, 0.2, 0.4, 0.1, 0.5, 1),
                      B = c(0.3, 0, 0.3, 0.4, 0.8, 0.5, 0))
  out <- predict(rsl, data = input)[, c("A", "B")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.0968, NaN, 0.0968, 0.3077, 0.3077, 0.5, NaN),
                         B = c(0.0968, NaN, 0.0968, 0.3077, 0.3077, 0.5, NaN))
  testthat::expect_equivalent(out, expected)
  
  # Test prediction on experiments 28-32
  rsl <- addClassifier(rsl, "C", "C", accuracy = 1)
  rsl <- addRule(rsl, "A <- C", 1)
  input <- data.frame(A = c(0.1, 0.1, 0.1, 0.1, 0.4),
                      B = c(0.3, 0.3, 0.3, 0.3, 0.4),
                      C = c(0.7, 0.1, 1, 0, 0.7))
  out <- predict(rsl, data = input)[, c("A", "B", "C")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.1370, 0.0503, 1, 0.0455, 0.5970),
                         B = c(0.1370, 0.0503, 1, 0.0455, 0.5970),
                         C = c(0.0959, 0.0050, 1, 0, 0.4179))
  testthat::expect_equivalent(out, expected)
}

# Experiments 34-36 cannot be conducted

predictTest37 <- function(){
  
  # Test prediction on experiment 37
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addClassifier(rsl, "C", "C", accuracy = 1)
  rsl <- addRule(rsl, "A, C <- ", 1)
  rsl <- addRule(rsl, "B, C <- ", 1)
  input <- data.frame(A = 0.1,
                      B = 0.3, 
                      C = 0.6)
  out <- predict(rsl, data = input)[, c("A", "B", "C")]
  out <- round(out, 4)
  expected <- data.frame(A = 0.1176,
                         B = 0.3137,
                         C = 0.9804)
  testthat::expect_equivalent(out, expected)
}

# Experiments 38-39 cannot be conducted

predictTest40 <- function(){  
  # Experiment 40:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 0.8)
  input <- data.frame(A = c(0.3),
                      B = c(0.6))
  out <- predict(rsl, data = input)[, c("A", "B")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.2308),
                         B = c(0.6593))
  testthat::expect_equivalent(out, expected)
}


# Experiments 41-47 cannot be conducted

# Experiment 48 is a duplicate of experiment 40

# Experiments 49 - 54 cannot be conducted

predictTest55 <- function(){
  # Experiment 55:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 0.2)
  input <- data.frame(A = c(0.1),
                      B = c(0.6))
  out <- predict(rsl, data = input)[, c("A", "B")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.1964),
                         B = c(0.5357))
  testthat::expect_equivalent(out, expected)
}

# Experiments 56 - 58 cannot be conducted

# Experiment 59: I think this was conducted wrong

# Experiments 60-61 cannot be conducted
predictTest62and64 <- function(){  
  # Experiments 62, 64
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), accuracy = 1)
  rsl <- addClassifier(rsl, "D", "D", accuracy = 1)
  rsl <- addRule(rsl, "A <- D", 1)
  input <- data.frame(A = c(0.1, 0.1),
                      B = c(0.6, 0.6),
                      C = c(0.3, 0.3),
                      D = c(0.9, 0.2))
  out <- predict(rsl, data = input)[, c("A", "B", "C", "D")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.5263, 0.1220),
                         B = c(0.3158, 0.5854),
                         C = c(0.1579, 0.2927),
                         D = c(0.4737, 0.0244))
  testthat::expect_equivalent(out, expected)
}

# Experiments 63 and 65 cannot be conducted

predictTest66 <- function(){
  # Experiments 66:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addClassifier(rsl, "C", "C", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 1)
  input <- data.frame(A = 0.4,
                      B = 0.2, 
                      C = 0.8)
  out <- predict(rsl, data = input)[, c("A", "B", "C")]
  out <- round(out, 4)
  expected <- data.frame(A = 0.1176,
                         B = 0.2941,
                         C = 0.8)
  testthat::expect_equivalent(out, expected)
}

predictTest67 <- function(){
  # Experiment 67:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addClassifier(rsl, "C", "C", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 1)
  rsl <- addRule(rsl, "B <- A, C", 1)
  input <- data.frame(A = 0.4,
                      B = 0.2, 
                      C = 0.8)
  out <- predict(rsl, data = input)[, c("A", "B", "C")]
  out <- round(out, 4)
  expected <- data.frame(A = 0.1176,
                         B = 0.2941,
                         C = 0.8)
  testthat::expect_equivalent(out, expected)
}

predictTest68 <- function(){
  # Experiment 68:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addClassifier(rsl, "C", "C", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 1)
  rsl <- addRule(rsl, "B <- C", 1)
  input <- data.frame(A = 0.4,
                      B = 0.2, 
                      C = 0.8)
  out <- predict(rsl, data = input)[, c("A", "B", "C")]
  out <- round(out, 4)
  expected <- data.frame(A = 0.2703,
                         B = 0.6757,
                         C = 0.5405)
  testthat::expect_equivalent(out, expected)
}

predictTest69 <- function(){
  # Experiment 69:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addClassifier(rsl, "C", "C", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 1)
  rsl <- addRule(rsl, "B <- C", 1)
  rsl <- addRule(rsl, "B <- A, C", 1)
  input <- data.frame(A = 0.4,
                      B = 0.2, 
                      C = 0.8)
  out <- predict(rsl, data = input)[, c("A", "B", "C")]
  out <- round(out, 4)
  expected <- data.frame(A = 0.2703,
                         B = 0.6757,
                         C = 0.5405)
  testthat::expect_equivalent(out, expected)
}

predictTest70to73 <- function(){
  # Experiments 70-73:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 1)
  rsl <- addRule(rsl, "!A, B <-", 0)
  input <- data.frame(A = c(0.3, 0, 1, 0.5),
                      B = c(0.2, 0, 1, 0.5))
  out <- predict(rsl, data = input)[, c("A", "B")]
  out <- round(out, 4)
  expected <- data.frame(A = c(NaN, NaN, NaN, NaN),
                         B = c(NaN, NaN, NaN, NaN))
  testthat::expect_equivalent(out, expected)
}

predictTest74 <- function(){
  # Experiment 74:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 1)
  input <- data.frame(A = c(0.1),
                      B = c(0.6))
  out <- predict(rsl, data = input)[, c("A", "B")]
  out <- round(out, 4)
  expected <- data.frame(A = 0.0625,
                         B = 0.625)
  testthat::expect_equivalent(out, expected)
}

predictTest84to86 <- function(){
  # Experiments 84 - 86:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 0)
  input <- data.frame(A = c(0.1, 0, 1),
                      B = c(0.6, 0.8, 0))
  out <- predict(rsl, data = input)[, c("A", "B")]
  out <- round(out, 4)
  expected <- data.frame(A = c(1, NaN, 1),
                         B = c(0, NaN, 0))
  testthat::expect_equivalent(out, expected)
}

predictTest107to113 <- function(){
  # Experiments 107 - 113:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 0.8)
  input <- data.frame(A = c(0.1, 1, 1, 0, 0.4, 0.4, 0.1),
                      B = c(0.3, 0, 0.5, 0.8, 0, 0.5, 1))
  out <- predict(rsl, data = input)[, c("A", "B")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.0501, 1, 1, 0, 0.1429, 0.2941, 0.1),
                         B = c(0.3166, 0, 0.8, 0.8, 0, 0.5882, 1))
  testthat::expect_equivalent(out, expected)
}

predictTest114to118 <- function(){
  # Experiments 114 - 118:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 0.5)
  input <- data.frame(A = c(0.1, 1, 0, 0.4, 0.1),
                      B = c(0.3, 0, 0.8, 0, 1))
  out <- predict(rsl, data = input)[, c("A", "B")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.1, 1, 0, 0.4, 0.1),
                         B = c(0.3, 0, 0.8, 0, 1))
  testthat::expect_equivalent(out, expected)
}

# Skipping Experiments 119-146

predictTest151to158 <- function(){ 
  # Experiments 151, 153, 155, 157
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addClassifier(rsl, "C", "C", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 0.9)
  rsl <- addRule(rsl, "<- A, C", 0.2)
  rsl <- addRule(rsl, "B <- C", 0.6)
  input <- data.frame(A = c(0.8, 0.2, 0.8, 0.2),
                      B = c(0.3, 0.6, 0.3, 0.6),
                      C = c(0.5, 1, 0.5, 1))
  out <- predict(rsl, data = input)[, c("A", "B", "C")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.8017, 0.4208, 0.8017, 0.4208),
                         B = c(0.7410, 0.8020, 0.7410, 0.8020),
                         C = c(0.7181, 1, 0.7181, 1))
  testthat::expect_equivalent(out, expected)
  
  # Experiments 152, 154, 156, 158
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addClassifier(rsl, "C", "C", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 0.4)
  rsl <- addRule(rsl, "<- A, C", 1)
  rsl <- addRule(rsl, "B <- C", 0.3)
  input <- data.frame(A = c(0.8, 0.2, 0.8, 0.2),
                      B = c(0.3, 0.6, 0.3, 0.6),
                      C = c(0.5, 1, 0.5, 1))
  out <- predict(rsl, data = input)[, c("A", "B", "C")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.6480, 0, 0.6480, 0),
                         B = c(0.2160, 0.3913, 0.2160, 0.3913),
                         C = c(0.2320, 1, 0.2320, 1))
  testthat::expect_equivalent(out, expected)
}

# Skipping Experiments 170-180

confA1 <- matrix(c(0.8, 0.1, 0.1, 0.1, 0.8, 0.1, 0.1, 0.1, 0.8), ncol = 3)
confA2 <- matrix(c(0.7, 0.3, 0.3, 0.7), ncol = 2)
confB1 <- matrix(c(0.9, 0, 0.1, 0.3, 0.6, 0.1, 0.2, 0, 0.8), ncol = 3)
confB2 <- matrix(c(0.8, 0.2, 0.01, 0.99), ncol = 2)
confC1 <- matrix(1/3, ncol = 3, nrow = 3)
confC2 <- matrix(0.5, ncol = 2, nrow = 2)
confD1 <- matrix(c(0.8, 0.2, 0.1, 0.9), ncol = 2)
confD2 <- matrix(c(0.7, 0.1, 0.2, 0.4, 0.4, 0.2, 0.1, 0.3, 0.6), ncol = 3)
confD3 <- matrix(c(0.9, 0.1, 0, 0, 0, 0.8, 0.2, 0, 0.01, 0.02, 0.96, 0.01, 0.01, 0.08, 0.06, 0.85), ncol = 4)
confE1 <- confA1
confE2 <- confA2
confE3 <- matrix(c(0.9, 0.1, 0.1, 0.9), ncol = 2)

predictTest181to183 <- function(){
  # Experiments 181-183:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = confA1)
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = confA2)
  rsl <- addRule(rsl, "D <- B", 1)
  rsl <- addRule(rsl, "C <- E", 1)
  input <- data.frame(A = c(0.1, 0.6, 1),
                      B = c(0.6, 0.2, 0),
                      C = c(0.3, 0.2, 0),
                      D = c(0.2, 0.5, 0.2),
                      E = c(0.8, 0.5, 0.8))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.1129, 0.4194, 0.6878),
                         B = c(0.3453, 0.1935, 0.086),
                         C = c(0.5418, 0.3871, 0.2262),
                         D = c(0.6641, 0.8065, 0.8597),
                         E = c(0.3359, 0.1935, 0.1403))
  testthat::expect_equivalent(out, expected)
}

predictTest184to186 <- function(){
  # Experiments 184-186:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = confB1)
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = confB2)
  rsl <- addRule(rsl, "D <- B", 1)
  rsl <- addRule(rsl, "C <- E", 1)
  input <- data.frame(A = c(0.1, 0.6, 1),
                      B = c(0.6, 0.2, 0),
                      C = c(0.3, 0.2, 0),
                      D = c(0.2, 0.5, 0.2),
                      E = c(0.8, 0.5, 0.8))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.1302, 0.4732, 0.6019),
                         B = c(0.1420, 0.0887, 0),
                         C = c(0.7278, 0.4381, 0.3981),
                         D = c(0.3944, 0.7393, 0.6688),
                         E = c(0.6056, 0.2607, 0.3312))
  testthat::expect_equivalent(out, expected)
}

predictTest187to189 <- function(){
  # Experiments 187-189:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = confC1, prior = rep(1/3, 3))
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = confC2)
  rsl <- addRule(rsl, "D <- B", 1)
  rsl <- addRule(rsl, "C <- E", 1)
  input <- data.frame(A = c(0.1, 0.6, 1),
                      B = c(0.6, 0.2, 0),
                      C = c(0.3, 0.2, 0),
                      D = c(0.2, 0.5, 0.2),
                      E = c(0.8, 0.5, 0.8))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.25, 0.25, 0.25),
                         B = c(0.25, 0.25, 0.25),
                         C = c(0.5, 0.5, 0.5),
                         D = c(0.75, 0.75, 0.75),
                         E = c(0.25, 0.25, 0.25))
  testthat::expect_equivalent(out, expected)
}

predictTest190to198 <- function(){
  # Experiments 190-198:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = diag(3))
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = diag(2))
  rsl <- addRule(rsl, "D <- B", 1)
  rsl <- addRule(rsl, "C <- E", 1)
  input <- data.frame(A = c(0.1, 0.6, 1, 0.1, 0.6, 1, 0.1, 0.6, 1),
                      B = c(0.6, 0.2, 0, 0.6, 0.2, 0, 0.6, 0.2, 0),
                      C = c(0.3, 0.2, 0, 0.3, 0.2, 0, 0.3, 0.2, 0),
                      D = c(0.2, 0.5, 0.2, 0.2, 0.5, 0.2, 0.2, 0.5, 0.2),
                      E = c(0.8, 0.5, 0.8, 0.8, 0.5, 0.8, 0.8, 0.5, 0.8))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.0455, 0.5, 1, 0.0455, 0.5, 1, 0.0455, 0.5, 1),
                         B = c(0.2727, 0.1667, 0, 0.2727, 0.1667, 0, 0.2727, 0.1667, 0),
                         C = c(0.6818, 0.3333, 0, 0.6818, 0.3333, 0, 0.6818, 0.3333, 0),
                         D = c(0.4545, 0.8333, 1, 0.4545, 0.8333, 1, 0.4545, 0.8333, 1),
                         E = c(0.5455, 0.1667, 0, 0.5455, 0.1667, 0, 0.5455, 0.1667, 0))
  testthat::expect_equivalent(out, expected)
}

predictTest205to207 <- function(){
  # Experiments 205-207:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = confB1)
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = confB2)
  rsl <- addRule(rsl, "D <- B", 1)
  input <- data.frame(A = c(0.1, 0.6, 1),
                      B = c(0.6, 0.2, 0),
                      C = c(0.3, 0.2, 0),
                      D = c(0.2, 0.5, 0.2),
                      E = c(0.8, 0.5, 0.8))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.4711, 0.6892, 0.9),
                         B = c(0.0863, 0.0523, 0),
                         C = c(0.4426, 0.2585, 0.1),
                         D = c(0.2398, 0.4361, 0.1680),
                         E = c(0.7602, 0.5639, 0.8320))
  testthat::expect_equivalent(out, expected)
}

predictTest211to213 <- function(){
  # Experiments 211-213:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = confB1)
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = confB2)
  rsl <- addRule(rsl, "D <- B", 1)
  rsl <- addRule(rsl, "C <- E", 1)
  input <- data.frame(A = c(0, 1, 1),
                      B = c(1, 0, 0),
                      C = c(0, 0, 0),
                      D = c(0, 1, 0),
                      E = c(1, 0, 1))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.0275, 0.8780, 0.0826),
                         B = c(0.0550, 0, 0),
                         C = c(0.9174, 0.1220, 0.9174),
                         D = c(0.0917, 0.9756, 0.0917),
                         E = c(0.9083, 0.0244, 0.9083))
  testthat::expect_equivalent(out, expected)
}

predictTest214 <- function(){
  # Experiment 214:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = diag(3))
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = diag(2))
  rsl <- addRule(rsl, "D <- B", 1)
  rsl <- addRule(rsl, "C <- E", 1)
  input <- data.frame(A = c(1/3),
                      B = c(1/3),
                      C = c(1/3),
                      D = c(0.5),
                      E = c(0.5))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.25),
                         B = c(0.25),
                         C = c(0.5),
                         D = c(0.75),
                         E = c(0.25))
  testthat::expect_equivalent(out, expected)
}

predictTest215to217 <- function(){
  # Experiments 215-217:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = diag(3))
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = diag(2))
  rsl <- addRule(rsl, "C <- E", 1)
  input <- data.frame(A = c(0.1, 0.6, 0.33),
                      B = c(0.6, 0.4, 0.33),
                      C = c(0.3, 0, 0.33),
                      D = c(0.5, 0.5, 0.5),
                      E = c(0.5, 0.5, 0.5))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.0769, 0.6, 0.25),
                         B = c(0.4615, 0.4, 0.25),
                         C = c(0.4615, 0, 0.5),
                         D = c(0.7692, 1, 0.75),
                         E = c(0.2308, 0, 0.25))
  testthat::expect_equivalent(out, expected)
}

predictTest218to220 <- function(){
  # Experiments 218-220
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = diag(3))
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "FG", c("F", "G"), confusionMatrix = diag(2))
  rsl <- addRule(rsl, "C <- E", 1)
  rsl <- addRule(rsl, "C <- F", 1)
  input <- data.frame(A = c(0.1, 0.6, 0.33),
                      B = c(0.6, 0.4, 0.33),
                      C = c(0.3, 0, 0.33),
                      D = c(0.5, 0.5, 0.5),
                      E = c(0.5, 0.5, 0.5),
                      F = c(0.5, 0.9, 0.5),
                      G = c(0.5, 0.1, 0.5))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E", "F", "G")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.0526, 0.6, 0.1667),
                         B = c(0.3158, 0.4, 0.1667),
                         C = c(0.6316, 0, 0.6667),
                         D = c(0.6842, 1, 0.6667),
                         E = c(0.3158, 0, 0.3333),
                         F = c(0.3158, 0, 0.3333),
                         G = c(0.6842, 1, 0.6667))
  testthat::expect_equivalent(out, expected)
}

predictTest221to223 <- function(){
  # Experiments 221-223
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = diag(3))
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "FG", c("F", "G"), confusionMatrix = diag(2))
  rsl <- addRule(rsl, "C <- E", 1)
  rsl <- addRule(rsl, "D <- F", 1)
  input <- data.frame(A = c(0.1, 0.6, 0.33),
                      B = c(0.6, 0.4, 0.33),
                      C = c(0.3, 0, 0.33),
                      D = c(0.5, 0.5, 0.5),
                      E = c(0.5, 0.5, 0.5),
                      F = c(0.9, 0.5, 0.9),
                      G = c(0.1, 0.5, 0.1))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E", "F", "G")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.0971, 0.6, 0.3226),
                         B = c(0.5825, 0.4, 0.3226),
                         C = c(0.3204, 0, 0.3548),
                         D = c(0.9709, 1, 0.9677),
                         E = c(0.0291, 0, 0.0323),
                         F = c(0.8738, 0.5, 0.8710),
                         G = c(0.1262, 0.5, 0.1290))
  testthat::expect_equivalent(out, expected)
}

predictTest224to226 <- function(){
  # Experiments 224-226
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = diag(3))
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = diag(2))
  rsl <- addRule(rsl, "D <- B", 1)
  input <- data.frame(A = c(0.33, 0.33, 0.33),
                      B = c(0.33, 0.33, 0.33),
                      C = c(0.33, 0.33, 0.33),
                      D = c(0.2, 0.5, 1),
                      E = c(0.8, 0.5, 0))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.4545, 0.4, 0.3333),
                         B = c(0.0909, 0.2, 0.3333),
                         C = c(0.4545, 0.4, 0.3333),
                         D = c(0.2727, 0.6, 1),
                         E = c(0.7273, 0.4, 0))
  testthat::expect_equivalent(out, expected)
}

predictTest227and229and230 <- function(){
  # Experiments 227, 229 and 230:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = confE1)
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = confE2)
  rsl <- addClassifier(rsl, "FG", c("F", "G"), confusionMatrix = confE3)
  rsl <- addRule(rsl, "C <- E", 1)
  rsl <- addRule(rsl, "C <- F", 1)
  input <- data.frame(A = c(0.1, 0.6, 0.33),
                      B = c(0.6, 0.4, 0.33),
                      C = c(0.3, 0, 0.33),
                      D = c(0.5, 0.5, 0.5),
                      E = c(0.5, 0.5, 0.5),
                      F = c(0.1, 0.5, 1),
                      G = c(0.9, 0.5, 0))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E", "F", "G")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.1176, 0.4, 0.0455),
                         B = c(0.3596, 0.2923, 0.0455),
                         C = c(0.5229, 0.3077, 0.9091),
                         D = c(0.7386, 0.8462, 0.5455),
                         E = c(0.2614, 0.1538, 0.4545),
                         F = c(0.0941, 0.1538, 0.8182),
                         G = c(0.9059, 0.8462, 0.1818)
  )
  testthat::expect_equivalent(out, expected)
}

predictTest228 <- function(){
  # Experiment 228:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = diag(3))
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "FG", c("F", "G"), confusionMatrix = diag(2))
  rsl <- addRule(rsl, "C <- E", 1)
  rsl <- addRule(rsl, "C <- F", 1)
  input <- data.frame(A = c(0.1),
                      B = c(0.6),
                      C = c(0.3),
                      D = c(0.5),
                      E = c(0.5),
                      F = c(0.9),
                      G = c(0.1))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E", "F", "G")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.0149),
                         B = c(0.0896),
                         C = c(0.8955),
                         D = c(0.5522),
                         E = c(0.4478),
                         F = c(0.8060),
                         G = c(0.1940))
  testthat::expect_equivalent(out, expected)
}

predictTest231to233 <- function(){
  # Experiments 231-233:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = confE1)
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = confE2)
  rsl <- addClassifier(rsl, "FG", c("F", "G"), confusionMatrix = confE3)
  rsl <- addRule(rsl, "C <- E", 1)
  rsl <- addRule(rsl, "D <- F", 1)
  input <- data.frame(A = c(0.1, 0.6, 0.33),
                      B = c(0.6, 0.4, 0.33),
                      C = c(0.3, 0, 0.33),
                      D = c(0.5, 0.5, 0.5),
                      E = c(0.5, 0.5, 0.5),
                      F = c(0.5, 1, 0.5),
                      G = c(0.5, 0, 0.5))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E", "F", "G")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.1472, 0.5149, 0.2857),
                         B = c(0.4502, 0.3762, 0.2857),
                         C = c(0.4026, 0.1089, 0.4286),
                         D = c(0.8658, 0.9901, 0.8571),
                         E = c(0.1342, 0.0099, 0.1429),
                         F = c(0.4329, 0.8911, 0.4286),
                         G = c(0.5671, 0.1089, 0.5714))
  testthat::expect_equivalent(out, expected)
}

# Experiments 234-239 skipped

predictTest255to259 <- function(){
  # Experiments 255 to 259:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", confusionMatrix = confD1)
  rsl <- addClassifier(rsl, "BCD", c("B", "C", "D"), confusionMatrix = confD2)
  rsl <- addClassifier(rsl, "EFGH", c("E", "F", "G", "H"), confusionMatrix = confD3)
  rsl <- addRule(rsl, "A <- B", 0.9)
  rsl <- addRule(rsl, "D <- F", 1)
  rsl <- addRule(rsl, "F <- D", 1)
  rsl <- addRule(rsl, "<- F, A", 0.9)
  rsl <- addRule(rsl, "B, H <- ", 0.8)
  input <- data.frame(A = c(0.1, 0, 0.92, 0.5, 0.95),
                      B = c(0.7, 1, 0.97, 0.9, 0.98),
                      C = c(0.2, 0, 0.02, 0.1, 0.01),
                      D = c(0.1, 0, 0.01, 0, 0.01),
                      E = c(0.2, 0, 0.85, 0, 0.8),
                      F = c(0.2, 1, 0.1, 0, 0.09),
                      G = c(0.2, 0, 0.04, 0.7, 0.02),
                      H = c(0.4, 0, 0.01, 0.3, 0.09))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E", "F", "G", "H")]
  out <- round(out, 4) 
  expected <- data.frame(A = c(0.4317, 0.2152, 0.9479, 0.815, 0.9517),
                         B = c(0.5633, 0.4032, 0.9443, 0.8504, 0.9374),
                         C = c(0.3848, 0.072, 0.0495, 0.1467, 0.0573),
                         D = c(0.052, 0.5248, 0.0061, 0.0029, 0.0053),
                         E = c(0.1732, 0, 0.9115, 0.0097, 0.8429),
                         F = c(0.052, 0.5248, 0.0061, 0.0029, 0.0053),
                         G = c(0.2383, 0.4752, 0.0703, 0.6678, 0.0498),
                         H = c(0.5365, 0, 0.0121, 0.3195, 0.1020))
  testthat::expect_equivalent(out, expected)
}

# conductAllPredictTests - runs all functions that start with "predictTest"
conductAllPredictTests <- function(){
  functions <- ls(envir = .GlobalEnv)
  functions <- functions[grepl("^predictTest", functions)]
  for(fct in functions){
    cat("Running", fct, "...")
    get(fct)()
    cat(" ok\n")
  }
}

conductAllPredictTests()


# For easier copy-pasting from the experimente.ods table, mark something and then:
# extractDF <- function(){
#   tab <- readClipboard()
#   tab <- gsub("[\\(\\),]", "", tab)
#   tab <- strsplit(tab, " ")
#   tab <- matrix(unlist(tab), nrow = length(tab), byrow = TRUE)
#   text <- apply(tab, 2, paste0, collapse = ", ")
#   text <- paste0(LETTERS[seq(length(text))], " = c(", text, ")", collapse = ",\n")
#   cat(text)
#   writeClipboard(text)
# }
# extractDF()
