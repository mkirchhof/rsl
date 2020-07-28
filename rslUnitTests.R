# Unit tests for rsl
# Author: michael.kirchhof@udo.edu
# Created: 16.07.2020

# Dependencies:
# library(testthat)
source("rsl.R")


.isValidRuleTest <- function(){
  testthat::expect_true(.isValidRule("A<-B"))
  testthat::expect_true(.isValidRule("A<-"))
  testthat::expect_true(.isValidRule(" A <-  B"))
  testthat::expect_true(.isValidRule("<-B"))
  testthat::expect_true(.isValidRule("!A, B<- C"))
  testthat::expect_true(.isValidRule("A<-!B"))
  testthat::expect_true(.isValidRule("A, !B, C, D<-B, F282.internal, !place"))
  testthat::expect_true(.isValidRule("<-!--A, A.B.C"))
  
  testthat::expect_false(.isValidRule("!!A<-B"))
  testthat::expect_false(.isValidRule("A->B"))
  testthat::expect_false(.isValidRule("A, B"))
  testthat::expect_false(.isValidRule("A, B!<-"))
  testthat::expect_false(.isValidRule("A<->B"))
  testthat::expect_false(.isValidRule("! <- B"))
  testthat::expect_false(.isValidRule("ItBl<wMyMind <- B"))
  testthat::expect_false(.isValidRule(""))
  testthat::expect_false(.isValidRule("      "))
  testthat::expect_false(.isValidRule(" <- "))
}

.isValidRuleTest()


.decomposeRuleTest <- function(){
  testthat::expect_identical(.decomposeRule("A<-B"), 
                             expected = c(A = TRUE, B = FALSE))
  testthat::expect_identical(.decomposeRule("!A, B, !puppy<-food"), 
                             expected = c(A = FALSE, B = TRUE, puppy = FALSE, food = FALSE))
  testthat::expect_identical(.decomposeRule("<-B"), 
                             expected = c(B = FALSE))
  testthat::expect_identical(.decomposeRule("A<-"), 
                             expected = c(A = TRUE))
  
  testthat::expect_error(.decomposeRule("this is invalid input"))
}

.decomposeRuleTest()


.getNewClassifierIDTest <- function(){
  rsl <- createRSL()
  testthat::expect_equal(.getNewClassifierID(rsl), "C1")
}

.getNewClassifierIDTest()


.getNewLabelIDTest <- function(){
  rsl <- createRSL()
  testthat::expect_equal(.getNewLabelID(rsl), "L1")
}

.getNewLabelIDTest()


.getNewAuxIDTest <- function(){
  rsl <- createRSL()
  testthat::expect_equal(.getNewAuxID(rsl), "A1")
}

.getNewAuxIDTest()


.getNewRuleIDTest <- function(){
  rsl <- createRSL()
  testthat::expect_equal(.getNewRuleID(rsl), "R1")
}

.getNewRuleIDTest()


addLabelsTest <- function(){
  rsl <- createRSL()
  rsl <- addLabels(rsl, c("cat", "dog", "mouse"), prior = c(0.2, 0.7, 0.1))
  testthat::expect_equal(length(rsl$labels), 1)
  testthat::expect_equal(bnlearn::nodes(rsl$bayesNet), c("placeholder", "L1"))
}

addLabelsTest()


addClassifierTest <- function(){
  # add a classificator with accuracy instead of confusion matrix
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "terminatorAI", labels = c("cat", "friend", "human"),
                       accuracy = 0.98)
  expectedCPT <- matrix(c(0.98, 0.01, 0.01,
                          0.01, 0.98, 0.01,
                          0.01, 0.01, 0.98), byrow = TRUE, ncol = 3)
  expectedCPT <- array(expectedCPT, dim = c(3, 3), 
                       dimnames = list(L1 = c("cat", "friend", "human"), 
                                       C1 = c("cat", "friend", "human")))
  testthat::expect_equal(rsl$bayesNet$L1$prob, as.table(expectedCPT))
  
  # add classificator with given confusion matrix and prior
  confMatr <- matrix(c(0.95, 0.01, 0.03,
                       0, 0.8, 0.04,
                       0.05, 0.19, 0.93), byrow = TRUE, ncol = 3)
  l2 <- c("grass", "tree", "bush")
  prior <- c(0.5, 0.3, 0.2)
  rsl <- addClassifier(rsl, "weakLearner", labels = l2, prior = prior,
                       confusionMatrix = confMatr)
  expectedCPT <- array(confMatr, dim = c(3, 3), dimnames = list(L2 = l2, C2 = l2))
  testthat::expect_equal(rsl$bayesNet$L2$prob, as.table(expectedCPT))
  rslPrior <- as.matrix(rsl$bayesNet$C2$prob)
  l2conf <- as.matrix(rsl$bayesNet$L2$prob)
  testthat::expect_equivalent(l2conf %*% rslPrior, prior)
}

addClassifierTest()


addRuleTest <- function(){
  rsl <- createRSL()
  rsl <- addLabels(rsl, c("cat", "dog", "mouse"))
  rsl <- addLabels(rsl, "clawsTrimmed")
  rsl <- addLabels(rsl, c("pet", "dontPet"))
  rsl <- addRule(rsl, rule = "pet <- cat, clawsTrimmed")
}

addRuleTest()

predictTest <- function(){
  # add a classificator with accuracy instead of confusion matrix
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "terminatorAI", labels = c("cat", "friend", "human"),
                       accuracy = 0.98)
  confMatr <- matrix(c(0.95, 0.01, 0.03,
                       0, 0.8, 0.04,
                       0.05, 0.19, 0.93), byrow = TRUE, ncol = 3)
  l2 <- c("grass", "tree", "bush")
  prior <- c(0.5, 0.3, 0.2)
  rsl <- addClassifier(rsl, "weakLearner", labels = l2, prior = prior,
                       confusionMatrix = confMatr)
  rsl <- addRule(rsl, rule = "friend <- tree")
  
  data <- data.frame(cat = c(0.8, 0.25),
                     friend = c(0.15, 0.5),
                     human = c(0.05, 0.25),
                     grass = c(0.3, 0.1),
                     tree = c(0.2, 0.7),
                     bush = c(0.5, 0.2))
  pred <- predict(rsl, data, method = "exact", type = "marginal")
  
}

predictTest()

predictTestNA <- function(){
  # do not give information on all labels for all observations:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = diag(3))
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "FG", c("F", "G"), confusionMatrix = diag(2))
  input <- data.frame(A = c(0.1, 0.6, 0.33),
                      B = c(NA, 0.4, 0.33),
                      C = c(0.3, NA, 0.33),
                      D = c(0.5, 0.5, NA),
                      F = c(0.9, NA, NA),
                      G = c(0.1, NA, 0.1))
  out <- predict(rsl, data = input)[, c("A", "B", "C", "D", "E", "F", "G")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.1, 0.6, 0.3333),
                         B = c(0.6, 0.4, 0.3333),
                         C = c(0.3, 0, 0.3333),
                         D = c(0.5, 0.5, 0.5),
                         E = c(0.5, 0.5, 0.5),
                         F = c(0.9, 0.5, 0.9),
                         G = c(0.1, 0.5, 0.1))
  testthat::expect_equivalent(out, expected)
}

predictTestNA()
