# End-to-end tests for the Rule Stacking Learner to see if the probabilistic
# inference works as expected
# Author: michael.kirchhof@udo.edu
# Created: 21.07.2020

# Dependencies:
# library(testthat)
source("rsl.R")

predictTest <- function(){
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
  
  # Experiments 34-36 cannot be conducted
  
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
  
  # Experiments 38-39 cannot be conducted
  
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
  
  # Experiments 41-47 cannot be conducted
  
  # Experiment 48 is a duplicate of experiment 40
  
  # Experiments 49 - 54 cannot be conducted
  
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
  
  # Experiments 56 - 58 cannot be conducted
  
  # Experiment 59: I think this was conducted wrong
  
  # Experiments 60-61 cannot be conducted
  
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
  
  # Experiments 63 and 65 cannot be conducted
  
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
  
  # Skipping Experiments 119-146
  
  # Experiments 151, 153, 155, 157
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "A", "A", accuracy = 1)
  rsl <- addClassifier(rsl, "B", "B", accuracy = 1)
  rsl <- addClassifier(rsl, "C", "C", accuracy = 1)
  rsl <- addRule(rsl, "B <- A", 0.9)
  rsl <- addRule(rsl, "<- A, B", 0.2)
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
  rsl <- addRule(rsl, "<- A, B", 1)
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
  
  # Skipping Experiments 170-180
  
  confA1 <- matrix(c(0.8, 0.1, 0.1, 0.1, 0.8, 0.1, 0.1, 0.1, 0.8), ncol = 3)
  confA2 <- matrix(c(0.7, 0.3, 0.3, 0.7), ncol = 2)
  confB1 <- matrix(c(0.9, 0, 0.1, 0.3, 0.6, 0.1, 0.2, 0, 0.8), ncol = 3)
  confB2 <- matrix(c(0.8, 0.2, 0.01, 0.99), ncol = 2)
  confC1 <- matrix(1/3, ncol = 3, nrow = 3)
  confC2 <- matrix(0.5, ncol = 2, nrow = 2)
  confE1 <- confA1
  confE2 <- confA2
  confE3 <- matrix(c(0.9, 0.1, 0.1, 0.9), ncol = 2)
  
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
  
  # Experiments 190-198:
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
  
  # Experiments 227-230:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = confE1)
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = confE2)
  rsl <- addClassifier(rsl, "FG", c("F", "G"), confusionMatrix = confE3)
  rsl <- addRule(rsl, "C <- E", 1)
  rsl <- addRule(rsl, "C <- F", 1)
  input <- data.frame(A = c(0.1, 0.1, 0.6, 0.33),
                      B = c(0.6, 0.6, 0.4, 0.33),
                      C = c(0.3, 0.3, 0, 0.33),
                      D = c(0.5, 0.5, 0.5, 0.5),
                      E = c(0.5, 0.5, 0.5, 0.5),
                      F = c(0.1, 0.9, 0.5, 1),
                      G = c(0.9, 0.1, 0.5, 0))
  out <- predict(rsl, data = input)[, c("A", "B", "C","D", "E", "F", "G")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.1934, 0.0149, 0.52, 0.0238),
                         B = c(0.5916, 0.0896, 0.38, 0.0238),
                         C = c(0.2150, 0.8955, 0.1, 0.9524),
                         D = c(0.6075, 0.5522, 0.55, 0.9762),
                         E = c(0.3925, 0.4478, 0.45, 0.0238),
                         F = c(0.0387, 0.8060, 0.05, 0.8571),
                         G = c(0.9613, 0.1940, 0.95, 0.1429))
  testthat::expect_equivalent(out, expected)
  
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
  expected <- data.frame(A = c(0.1896, 0.5248, 0.3750),
                         B = c(0.5799, 0.3835, 0.3750),
                         C = c(0.2305, 0.0917, 0.25),
                         D = c(0.7435, 0.9174, 0.75),
                         E = c(0.2565, 0.0826, 0.25),
                         F = c(0.3717, 0.8257, 0.3750),
                         G = c(0.6283, 0.1743, 0.6250))
  testthat::expect_equivalent(out, expected)
  
  # Experiments 234-239 skipped
}

predictTest()


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
