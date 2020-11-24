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
  pred <- predict(rsl, data, type = "marginal")
  
}

predictTest()

predictTestNA <- function(){
  # do not give information on all labels for all observations:
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "ABC", c("A", "B", "C"), confusionMatrix = diag(3),
                       prior = c(0.7, 0.2, 0.1))
  rsl <- addClassifier(rsl, "DE", c("D","E"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "FG", c("F", "G"), confusionMatrix = diag(2))
  input <- data.frame(A = c(0.1, 0.6, 0.33, NA, NA),
                      B = c(NA, 0.4, 0.33, NA, NA),
                      C = c(0.3, NA, 0.33, NA, NA),
                      D = c(0.5, 0.5, NA, 0.5, 0.5),
                      F = c(0.9, NA, NA, NA, 0.5),
                      G = c(0.1, NA, 0.1, 0.9, 0.5))
  out <- predict(rsl, data = input)[, c("A", "B", "C", "D", "E", "F", "G")]
  out <- round(out, 4)
  expected <- data.frame(A = c(0.1, 0.6, 0.3333, 0.7, 0.7),
                         B = c(0.6, 0.4, 0.3333, 0.2, 0.2),
                         C = c(0.3, 0, 0.3333, 0.1, 0.1),
                         D = c(0.5, 0.5, 0.5, 0.5, 0.5),
                         E = c(0.5, 0.5, 0.5, 0.5, 0.5),
                         F = c(0.9, 0.5, 0.9, 0.1, 0.5),
                         G = c(0.1, 0.5, 0.1, 0.9, 0.5))
  testthat::expect_equivalent(out, expected)
}

predictTestNA()

predictJointPosteriorTest <- function(){
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
  rsl <- addClassifier(rsl, "weather", labels = c("sunny", "cloudy"), accuracy = 1)
  
  data <- data.frame(cat = c(0.8, 0.25),
                     friend = c(0.15, 0.5),
                     human = c(0.05, 0.25),
                     grass = c(0.3, 0.1),
                     tree = c(0.2, 0.7),
                     bush = c(0.5, 0.2),
                     sunny = c(0.8, 0.3))
  pred <- predict(rsl, data, type = "joint")
  
  expected <- data.frame("cat" = c(1, 0),
                         "friend" = c(0, 1),
                         "human" = c(0, 0),
                         "grass" = c(0, 0),
                         "tree" = c(0, 1),
                         "bush" = c(1, 0),
                         "sunny" = c(1, 0),
                         "cloudy" = c(0, 1))
  
  testthat::expect_equivalent(pred, expected)
}

predictJointPosteriorTest()

learnRulesJointRuleTest <- function(){
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "animal", c("cat", "dog", "mouse"), confusionMatrix = diag(3))
  rsl <- addClassifier(rsl, "claws", "clawsTrimmed", confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "toPetOrNotToPet", c("pet", "dontPet"), confusionMatrix = diag(2))
  input <- data.frame(cat = c(0.8, 0.4, 0.2, 0.1, 0.5, 0.2, 0.9, 1/3, 1/3, 0.7),
                      dog = c(0.15, 0.4, 0.7, 0.2, 0.3, 0.8, 0.07, 1/3, 1/3, 0.2),
                      mouse = c(0.05, 0.2, 0.1, 0.7, 0.2, 0, 0.03, 1/3, 1/3, 0.1),
                      clawsTrimmed = c(0.99, 0.8, 0.5, 0.1, 0.5, 0.01, 0.05, 0.01, 0.5, 0.2),
                      pet = c(0.5, 0.5, 0.5, 0.5, 0.8, 0.6, 0.05, 0.96, 0.01, 0.8))
  actual <- data.frame(L1 = c("cat", "cat", "dog", "mouse", "cat", "dog", "cat", "dog", "mouse", "cat"),
                       L2 = c("clawsTrimmed", "clawsTrimmed", "not_clawsTrimmed", "not_clawsTrimmed", "not_clawsTrimmed", "not_clawsTrimmed", "not_clawsTrimmed", "not_clawsTrimmed", "clawsTrimmed", "not_clawsTrimmed"),
                       L3 = c("pet", "pet", "pet", "dontPet", "dontPet", "pet", "dontPet", "pet", "dontPet", "pet"),
                       stringsAsFactors = FALSE)
  rsl <- learnRules(rsl, prior = input, actual = actual, nRules = 4, method = "jointRule")
  pred <- predict(rsl, input)
  inputCrisp <- .probabilisticToCrispData(rsl, input, tieBreak = "first")
  predCrisp <- .probabilisticToCrispData(rsl, pred, tieBreak = "first")
  (lossPrior <- hammingLoss(inputCrisp, actual))
  (accPrior <- accuracy(inputCrisp, actual))
  (lossPosterior <- hammingLoss(predCrisp, actual))
  (accPosterior <- accuracy(predCrisp, actual))
}

learnRulesJointRuleTest()

learnRulesNoisyORTest <- function(){
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "animal", c("cat", "dog", "mouse"), confusionMatrix = diag(3))
  rsl <- addClassifier(rsl, "claws", "clawsTrimmed", confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "toPetOrNotToPet", c("pet", "dontPet"), confusionMatrix = diag(2))
  input <- data.frame(cat = c(0.8, 0.4, 0.2, 0.1, 0.5, 0.2, 0.9, 1/3, 1/3, 0.7),
                      dog = c(0.15, 0.4, 0.7, 0.2, 0.3, 0.8, 0.07, 1/3, 1/3, 0.2),
                      mouse = c(0.05, 0.2, 0.1, 0.7, 0.2, 0, 0.03, 1/3, 1/3, 0.1),
                      clawsTrimmed = c(0.99, 0.8, 0.5, 0.1, 0.5, 0.01, 0.05, 0.01, 0.5, 0.2),
                      pet = c(0.5, 0.5, 0.5, 0.5, 0.8, 0.6, 0.05, 0.96, 0.01, 0.8))
  actual <- data.frame(L1 = c("cat", "cat", "dog", "mouse", "cat", "dog", "cat", "dog", "mouse", "cat"),
                       L2 = c("clawsTrimmed", "clawsTrimmed", "not_clawsTrimmed", "not_clawsTrimmed", "not_clawsTrimmed", "not_clawsTrimmed", "not_clawsTrimmed", "not_clawsTrimmed", "clawsTrimmed", "not_clawsTrimmed"),
                       L3 = c("pet", "pet", "pet", "dontPet", "dontPet", "pet", "dontPet", "pet", "dontPet", "pet"),
                       stringsAsFactors = FALSE)
  rsl <- learnRules(rsl, prior = input, actual = actual, nRules = 4, method = "noisyor")
  pred <- predict(rsl, input)
  inputCrisp <- .probabilisticToCrispData(rsl, input, tieBreak = "first")
  predCrisp <- .probabilisticToCrispData(rsl, pred, tieBreak = "first")
  (lossPrior <- hammingLoss(inputCrisp, actual))
  (accPrior <- accuracy(inputCrisp, actual))
  (lossPosterior <- hammingLoss(predCrisp, actual))
  (accPosterior <- accuracy(predCrisp, actual))
}

learnRulesNoisyORTest()

addJointRuleTest <- function(){
  # See if the added joint rule works just like manually multiplying weights
  # to all possible variable states, and if it is equal  to the product of 
  # local rules
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "taste", c("tasty", "not_tasty"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "meat", c("meat", "noMeat"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "healthy", c("healthy", "junkFood"), confusionMatrix = diag(2))
  rsl <- addRule(rsl, "tasty <- junkFood", prob = 0.8)
  rsl <- addRule(rsl, "not_tasty <- meat", prob = 0.9)
  rsl <- addRule(rsl, "meat, tasty <- junkFood", prob = 0.6)
    
  input <- data.frame(tasty = c(0.1, 0.6, 1),
                      meat = c(0.7, 0.2, 0),
                      healthy = c(0.3, 0.7, 0))
  out <- predict(rsl, data = input)[, c("tasty", "meat", "healthy")]
  
  # Manual computation with the joint rule (in standard order):
  jointRule <- c(0.048, 0.432, 0.432, 0.432, 0.048, 0.108, 0.432, 0.072)
  p1 <- c(0.021, 0.189, 0.009, 0.081, 0.049, 0.441, 0.021, 0.189) # joint Prior of case 1
  p2 <- c(0.084, 0.056, 0.336, 0.224, 0.036, 0.024, 0.144, 0.096)
  p3 <- c(0, 0, 0, 0, 0, 0, 1, 0)
  post1 <- p1 * jointRule
  post2 <- p2 * jointRule
  post3 <- p3 * jointRule
  expected <- data.frame(tasty = c(sum(post1[c(1, 3, 5, 7)]) / sum(post1),
                                   sum(post2[c(1, 3, 5, 7)]) / sum(post2),
                                   sum(post3[c(1, 3, 5, 7)]) / sum(post3)),
                         meat = c(sum(post1[c(1, 2, 5, 6)]) / sum(post1), 
                                  sum(post2[c(1, 2, 5, 6)]) / sum(post2),
                                  sum(post3[c(1, 2, 5, 6)]) / sum(post3)),
                         healthy = c(sum(post1[c(1, 2, 3, 4)]) / sum(post1), 
                                     sum(post2[c(1, 2, 3, 4)]) / sum(post2),
                                     sum(post3[c(1, 2, 3, 4)]) / sum(post3)))
  
  # See if the rsl with local rules is the same as our manual joint rule
  testthat::expect_equivalent(out, expected)
  
  # Build a rsl with a joint rule
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "taste", c("tasty", "not_tasty"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "meat", c("meat", "noMeat"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "healthy", c("healthy", "junkFood"), confusionMatrix = diag(2))
  rsl <- .addJointRule(rsl, jointRule)
  out <- predict(rsl, data = input)[, c("tasty", "meat", "healthy")]
  
  # See if our manually computed jointRule is the same as the rsl's implementation
  testthat::expect_equivalent(out, expected)
}

addJointRuleTest()

simulateTest <- function(){
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "taste", c("tasty", "not_tasty"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "meat", c("meat", "noMeat"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "healthy", c("healthy", "junkFood"), confusionMatrix = diag(2))
  rsl <- addRule(rsl, "tasty <- junkFood", prob = 0.8)
  rsl <- addRule(rsl, "not_tasty <- meat", prob = 0.9)
  rsl <- addRule(rsl, "meat, tasty <- junkFood", prob = 0.6)
  
  data <- simulate(rsl, n = 20)
  
  # Test that we receive a character dataframe of the correct dimensions
  # (There is not really much more we can test due to randomness in simulation)
  testthat::expect_equal(class(data), "data.frame")
  testthat::expect_equal(nrow(data), 20)
  testthat::expect_equal(ncol(data), 9)
  testthat::expect_equal(unname(sapply(data, class)), rep("character", 9))
}

simulateTest()

removeRuleTest <- function(){
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "taste", c("tasty", "not_tasty"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "meat", c("meat", "noMeat"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "healthy", c("healthy", "junkFood"), confusionMatrix = diag(2))
  rsl <- addRule(rsl, "tasty <- junkFood", prob = 0.8)
  rsl <- addRule(rsl, "not_tasty <- meat", prob = 0.9)
  
  rsl2 <- addRule(rsl, "meat, tasty <- junkFood", prob = 0.6)
  rsl2 <- removeRule(rsl2, "R3")
  
  testthat::expect_equal(rsl, rsl2)
}

removeRuleTest()

removeRuleThatIsNotInRSLTest <- function(){
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "taste", c("tasty", "not_tasty"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "meat", c("meat", "noMeat"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "healthy", c("healthy", "junkFood"), confusionMatrix = diag(2))
  rsl <- addRule(rsl, "tasty <- junkFood", prob = 0.8)
  rsl <- addRule(rsl, "not_tasty <- meat", prob = 0.9)
  
  testthat::expect_warning(rsl2 <- removeRule(rsl, "R3"))
  testthat::expect_equal(rsl, rsl2)
}

removeRuleThatIsNotInRSLTest()

preprocessInhProbsTest <- function(){
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "taste", c("tasty", "not_tasty"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "meat", c("meat", "noMeat"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "healthy", c("healthy", "junkFood"), confusionMatrix = diag(2))
  inhProbs <- c(tasty = 0.8, junkFood = 0.1)
  
  inhProbs <- .preprocessInhProbs(rsl, inhProbs)
  
  expected <- list(L1 = c(tasty = 0.8, not_tasty = 1),
                   L3 = c(healthy = 1, junkFood = 0.1))
  
  testthat::expect_equal(inhProbs, expected)
}

preprocessInhProbsTest()

addNoisyORTest <- function(){
  rsl <- createRSL()
  rsl <- addClassifier(rsl, "taste", c("tasty", "not_tasty"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "meat", c("meat", "noMeat"), confusionMatrix = diag(2))
  rsl <- addClassifier(rsl, "healthy", c("healthy", "junkFood", "toxic"), accuracy = 1)
  shouldEatProbs <- c(tasty = 0.2, not_tasty = 0.9, junkFood = 0.6, toxic = 1)
  
  rsl <- .addNoisyOR(rsl, shouldEatProbs)
  
  expected <- array(c(0.8, 0.2, 0.1, 0.9, 0.88, 0.12, 0.46, 0.54, 0.8, 0.2, 0.1, 0.9),
                    dim = c(2, 2, 3), dimnames = list(R1 = c("fulfilled", "not_fulfilled"),
                                                      L1 = c("tasty", "not_tasty"),
                                                      L3 = c("healthy", "junkFood", "toxic")))
  expected <- as.table(expected)
  
  testthat::expect_equivalent(rsl$bayesNet$R1$prob, expected)
}

addNoisyORTest()
