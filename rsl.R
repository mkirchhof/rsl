# Bayesian Network based probabilistic Rule Stacking Learner
# Author: michael.kirchhof@udo.edu
# Created: 16.07.2020

# Dependencies: (not loaded into namespace due to style guide)
# library(bnlearn)
# library(gRain)


# createRSL - creates an empty Rule Stacking Learner
createRSL <- function(){
  # labels: list of lists. Each list represents one XOR-related group of labels,
  #         each entry contains the label names, the internal ID in the BN 
  #         and the priors
  # classifiers: list of lists. Each list contains a classifier's name, its
  #              internal ID in the BN and its confusion matrix
  # rules: dataframe containing a rules's name, probability, and the IDs of 
  #        the nodes representing the rule and its auxiliary node in the BN
  # bayesNet: the bnlearn object representing the network (always updated when
  #           the rsl is updated)
  # compiledNet: the grain object representing the network (only compiled when
  #              needed)
  # needsCompilation: logical, flags if the compiled net is in sync with rsl
  rsl <- list(labels = list(),
              classifiers = list(),
              rules = data.frame("name" = character(0), 
                                 "prob" = numeric(0), 
                                 "nodeID" = character(0),
                                 "auxID" = character(0),
                                 stringsAsFactors = FALSE),
              bayesNet = bnlearn::empty.graph(nodes = character(0)),
              compiledNet = NA,
              needsCompilation = TRUE)
  class(rsl) <- "rsl"
  
  return(rsl)
}


# addRule - adds a probabilistic rule to a rsl
# Input:
#  rsl - an rsl object
#  rule - a character string describing the rule in the form "A, B <- not C, D"
#         which corresponds to "((not C) and D) -> (A or B)", where A, B, C and 
#         D are names of classes in the network
#  prob - numeric in [0, 1], the marginal probability of the rule, or NA to
#         learn optimal prob given data
# Output:
#  the updated rsl object
addRule <- function(rsl, rule, prob = 0.9, data = NULL){
  
}


.optimizeRuleProb <- function(rsl, rule, data){
  
}


removeRule <- function(rsl, rule){
  
}


getRules <- function(rsl){
  
}


# addLabels - adds an XOR-related group of labels (or a binary label)
addLabels <- function(rsl, labels, prior = NA){
  
}


# removeLabels - removes a group of labels from the rsl
removeLabels <- function(rsl, labels){
  
}


getLabels <- function(rsl){
  
}


# addClassifier - adds a classifier that connects to one XOR related group of 
#                 labels
# Input:
#  rsl - an rsl object
#  name - character, name of the classifier
#  labels - character, labels the classifier gives an output for
#  confusionMatrix - numeric matrix of size labels X labels, if NULL, will be 
#                    constructed using the accuracy
#  accuracy - numeric in [0, 1], used to calculate confusionMatrix if it is not
#             given. If NULL, standard value of 0.9 is used
# Output:
#  updated rsl object
addClassifier <- function(rsl, name, labels, confusionMatrix = NULL, 
                          accuracy = NULL){
  
}


# removeClassifier - removes a classifier from an rsl
removeClassifier <- function(rsl, name){
  
}


getClassifiers <- function(rsl){
  
}


print.rsl <- function(rsl){
  
}


plot.rsl <- function(rsl){
  
}


# compile - creates a grain network for inference in the RSL
.compile <- function(rsl){
  if(rsl$needsCompilation){
    rsl$compiledNet <- bnlearn::as.grain(rsl$bayesNet)
  }
  
  return(rsl)
}


# .getAllAuxNodes - returns the names of all auxiliary rule nodes
.getAllAuxNodes <- function(rsl){
  
}


# .getNewAuxID - returns an unused ID for an Aux Node
.getNewAuxID <- function(rsl){
  
}


.getNewRuleID <- function(rsl){
  
}


.getNewLabelID <- function(rsl){
  
}


.getNewClassifierID <- function(rsl){
  
}


.classifierNameToID <- function(rsl, name){
  
}


.ruleNameToID <- function(rsl, name){
  
}


.labelNameToID <- function(rsl, name){
  
}


.classifierIDtoName <- function(rsl, id){
  
}


.ruleIDtoName <- function(rsl, id){
  
}


.labelIDtoName <- function(rsl, id){
  
}


# .setAuxEvidence - sets evidence of all auxiliary rule nodes to 1
.setAuxEvidence <- function(rsl){
  
}


# .setEvidence - sets (soft) evidence to all classifier inputs
.setEvidence <- function(rsl, data){
  
}


# .removeEvidence - resets all evidence in the grain classifier
.removeEvidence <- function(rsl){
  
}


# predict.rsl - computes a-posteriori estimates of all labels
predict.rsl <- function(rsl, data, method = "exact"){
  
}


# learnRules
learnRules <- function(rsl, data, nRules = NA, method = "hammingtion"){
  
}