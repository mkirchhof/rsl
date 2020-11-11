# Bayesian Network based probabilistic Rule Stacking Learner
# Author: michael.kirchhof@udo.edu
# Created: 16.07.2020

# Dependencies: (not loaded into namespace due to style guide)
# library(bnlearn) # for constructing bayesian networks
# library(gRain) # for exact inference on bayesian networks
# library(MASS) # for ginv()


# createRSL - creates an empty Rule Stacking Learner
createRSL <- function(){
  # labels: list of lists. Each list represents one XOR-related group of labels,
  #         each entry contains the label names, the internal ID in the BN 
  #         and the prior
  # classifiers: list of lists. Each list contains a classifier's name, its
  #              internal ID in the BN and its confusion matrix
  # rules: dataframe containing a rules's name, probability, and the IDs of 
  #        the nodes representing the rule and its auxiliary node in the BN
  # bayesNet: the bnlearn object representing the network (always updated when
  #           the rsl is updated)
  # compiledNet: the grain object representing the network (only compiled when
  #              needed)
  # needsCompilation: logical, flags if the compiled net is in sync with rsl
  bn <- bnlearn::empty.graph(nodes = "placeholder")
  cpt <- list(placeholder = array(c(1, 0), dim = 2, dimnames = list(placeholder = c("true", "false"))))
  bn <- bnlearn::custom.fit(bn, cpt)
  rsl <- list(labels = list(),
              classifiers = list(),
              rules = data.frame("name" = character(0), 
                                 "prob" = numeric(0), 
                                 "ruleID" = character(0),
                                 "auxID" = character(0),
                                 stringsAsFactors = FALSE),
              bayesNet = bn,
              compiledNet = NULL,
              needsCompilation = TRUE)
  class(rsl) <- "rsl"
  
  return(rsl)
}


# .normalizeRule - removes whitespaces from a rule
.normalizeRule <- function(rule){
  return(gsub("[[:blank:]]", "", rule))
}


# addRule - adds a probabilistic rule to a rsl
# Input:
#  rsl - an rsl object
#  rule - a character string describing the rule in the form "A, B <- !C, D"
#         which corresponds to "((not C) and D) -> (A or B)", where A, B, C and 
#         D are names of classes in the network
#  prob - numeric in [0, 1], the marginal probability of the rule, or NA to
#         learn optimal prob given data
# Output:
#  the updated rsl object
addRule <- function(rsl, rule, prob = 0.9){
  # TODO: Add argument data to automatically learn the prob
  
  if(.ruleAlreadyExists(rsl, rule)){
    warning(paste0("Rule ", rule, " already exists in the rsl. ",
                   "Please adjust its prob instead of adding it multiple times. Skipping this rule."))
    return(rsl)
  }
  # TODO: Add more type checks
  
  # Find label nodes associated to the rule
  rule <- .normalizeRule(rule)
  labels <- .decomposeRule(rule)
  rID <- .getNewRuleID(rsl)
  aID <- .getNewAuxID(rsl)
  
  # Add to rsl$rules
  rsl$rules <- rbind(rsl$rules, 
                     data.frame(name = rule, prob = prob, ruleID = rID, auxID = aID,
                                stringsAsFactors = FALSE))
  
  # save for each label node the labels that fulfill the rule
  # So we get a list like list(L1 = c("cat", "mouse"), L2 = c("hungry"), L3 = c())
  allowedStates <- list()
  for(i in seq(along = labels)){
    labelID <- .labelToID(rsl, names(labels)[i])
    if(!labelID %in% names(allowedStates)){
      # if we do not have this label node yet, add it
      allowedStates[[labelID]] <- rsl$labels[[labelID]]$names
    }
    if(labels[i] == TRUE){ # The rare occasion where "== TRUE" adds to understanding 
      allowedStates[[labelID]] <- intersect(allowedStates[[labelID]], names(labels)[i])
    } else {
      allowedStates[[labelID]] <- setdiff(allowedStates[[labelID]], names(labels)[i])
    }
  }
  
  # build rule CPT
  # for every combination of variables, check if they are %in% their allowedStates
  dimlist <- lapply(rsl$labels[names(allowedStates)], function(x) x$names)
  ruleStates <- list(c("fulfilled", "not_fulfilled"))
  names(ruleStates) <- rID
  allCombs <- expand.grid(dimlist)
  isFulfilled <- matrix(FALSE, ncol = ncol(allCombs), nrow = nrow(allCombs))
  for(labelIndex in seq(ncol(allCombs))){
    isFulfilled[, labelIndex] <- allCombs[, labelIndex] %in% allowedStates[[labelIndex]]
  }
  # We have a disjunctive form, so check row-wise if any label is ok
  ruleFulfilled <- rowSums(isFulfilled) > 0
  rProbTable <- array(as.numeric(c(t(matrix(c(ruleFulfilled, !ruleFulfilled), ncol = 2)))), 
                      dim = sapply(c(ruleStates, dimlist), length), 
                      dimnames = c(ruleStates, dimlist))
  
  # Build aux CPT
  auxStates <- list(c("fulfilled", "not_fulfilled"))
  names(auxStates) <- aID
  aProbTable <- array(c(prob, 1 - prob, 1 - prob, prob),
                      dim = c(2, 2), dimnames = c(auxStates, ruleStates))
  
  # add rule and aux to rsl$bayesNet
  tables <- lapply(rsl$bayesNet, "[[", "prob")
  tables[[rID]] <- rProbTable
  tables[[aID]] <- aProbTable
  nodes <- c(bnlearn::nodes(rsl$bayesNet), rID, aID)
  arcs <- rbind(bnlearn::arcs(rsl$bayesNet), 
                cbind(names(allowedStates), rID), 
                c(rID, aID))
  rsl$bayesNet <- bnlearn::empty.graph(nodes = nodes)
  bnlearn::arcs(rsl$bayesNet) <- arcs
  rsl$bayesNet <- bnlearn::custom.fit(rsl$bayesNet, tables)
  rsl$needsCompilation <- TRUE
  
  return(rsl)
}


# .ruleAlreadyExists - checks if a rule already exists in the rsl
.ruleAlreadyExists <- function(rsl, rule){
  rule <- .normalizeRule(rule)
  return(any(rsl$rules$name == rule))
}


# .labelToID - returns the label node a label or subset of labels is associated to
.labelToID <- function(rsl, labels){
  isLabel <- sapply(rsl$labels, function(x) all(labels %in% x$names))
  if(length(isLabel) > 0 && sum(isLabel) == 1){
    return(rsl$labels[[which(isLabel)]]$id)
  } else {
    return(NULL)
  }
}


# .isValidRule - returns if a character is in the form "A, B <- !C, D" etc.
.isValidRule <- function(rule){
  rule <- .normalizeRule(rule)
  # rule has correct scheme
  isValid <- grepl("^(!?[^!,<>]+,)*(!?[^!,<>]+)?<-(!?[^!,<>]+,)*(!?[^!,<>]+)?$", rule)
  # rule has at least one variable somewhere
  isValid <- isValid && grepl("^(!?[^!,<>]+){1}(,!?[^!,<>]+)*<-|<-(!?[^!,<>]+){1}(,!?[^!,<>]+)*$", rule)
  
  return(isValid)
}


# .decomposeRule - reads a rule and returns a named boolean vector representing
#                  that rule as a disjunction. The names are the variables and
#                  the boolean value is the true/false state of the variable.
.decomposeRule <- function(rule){
  if(!.isValidRule(rule)){
    stop("Rule ", rule, " is not in a valid format.")
  }
  
  rule <- .normalizeRule(rule)
  head <- gsub("^([^<]*)<-.*$", "\\1", rule)
  body <- gsub("^([^<]*)<-(.*)$", "\\2", rule)
  head <- .textToVariables(head)
  body <- .textToVariables(body)
  variables <- c(head, !body) # A <- B is eq. to "A or not B"
  
  return(variables)
}


# .textToVariables - turns a character in the form "A,!B,C" into a named boolean
#                    vector like c(A = TRUE, B = FALSE, C = TRUE)
.textToVariables <- function(text){
  entries <- strsplit(text, ",")[[1]]
  variables <- !grepl("^!", entries)
  names(variables) <- gsub("^!", "", entries)
  
  return(variables)
}


# .optimizeRuleProb - uses a line search to find a rule probability that
#                     optimizes the a-posteriori likelihood of the given data
.optimizeRuleProb <- function(rsl, rule, data){
  # TODO: Implement (medium priority)
}


# .addJointRule - adds a joint rule (learned in the learnRules intermediate step)
#                 CAUTION: Only for internal testing purposes!
# Input: 
#  rsl - an rsl object
#  weights - the weights of the joint rule in standard order 
#           (see .generateStandardOrder())
# Output:
#  the rsl object with the added joint rule
.addJointRule <- function(rsl, weights){
  # get IDs for the joint rule
  rID <- .getNewRuleID(rsl)
  aID <- .getNewAuxID(rsl)
  
  # Add to rsl$rules
  rsl$rules <- rbind(rsl$rules, 
                     data.frame(name = "jointRule", prob = 1, ruleID = rID, auxID = aID,
                                stringsAsFactors = FALSE))
  
  # Build weights into a CPT
  dimlist <- lapply(rsl$labels, function(x) x$names)
  ruleStates <- list(c("fulfilled", "not_fulfilled"))
  names(ruleStates) <- rID
  rProbTable <- array(as.numeric(c(t(matrix(c(weights, 1 - weights), ncol = 2)))), 
                      dim = sapply(c(ruleStates, dimlist), length), 
                      dimnames = c(ruleStates, dimlist))
  
  # Build aux CPT
  auxStates <- list(c("fulfilled", "not_fulfilled"))
  names(auxStates) <- aID
  aProbTable <- array(c(1, 0, 0, 1),
                      dim = c(2, 2), dimnames = c(auxStates, ruleStates))
  
  # add rule and aux to rsl$bayesNet
  tables <- lapply(rsl$bayesNet, "[[", "prob")
  tables[[rID]] <- rProbTable
  tables[[aID]] <- aProbTable
  nodes <- c(bnlearn::nodes(rsl$bayesNet), rID, aID)
  arcs <- rbind(bnlearn::arcs(rsl$bayesNet), 
                cbind(names(getLabels(rsl)), rID), 
                c(rID, aID))
  rsl$bayesNet <- bnlearn::empty.graph(nodes = nodes)
  bnlearn::arcs(rsl$bayesNet) <- arcs
  rsl$bayesNet <- bnlearn::custom.fit(rsl$bayesNet, tables)
  rsl$needsCompilation <- TRUE
  
  return(rsl)
}


removeRule <- function(rsl, rule){
  # TODO: Implement (low priority)
}


# getRules - returns a dataframe with information about all rules
getRules <- function(rsl){
  return(rsl$rules[, c("name", "prob")])
}


.coerceToMultilabel <- function(labels){
  if(length(labels) == 1){
    warning(paste0("label seems to be binary. Coerced it to multilabel by adding ",
                   "not_", labels, " as second label."))
    # Treat as a binary label and add a "not" case
    labels <- c(labels, paste0("not_", labels))
  }
  
  return(labels)
}


# addLabels - adds an XOR-related group of labels (or a binary label)
addLabels <- function(rsl, labels, prior = NA){
  if(!is.character(labels) || length(labels) < 1){
    stop("Labels vector is invalid.")
  }
  if(!is.na(prior) && (!is.numeric(prior) || length(prior) != length(labels) || sum(prior) != 1)){
    stop("Prior vector is invaid.")
  }
  if(.labelsAlreadyExist(rsl, labels)){
    stop("Some of the labels already exist in the rsl. Please choose unique names.")
    # Note: If you want to change the code to support non-unique labels, 
    # code for learnRules() has to be changed aswell.
  }
  if(any(duplicated(labels))){
    stop("Label names are not unique.")
  }
  if(any(grepl("[!<>,]", labels))){
    stop("Label names must not contain the following characters: <>!,")
  }
  
  labels <- .coerceToMultilabel(labels)
  
  if(length(prior) == 1 && !is.na(prior)){
    prior <- c(prior, 1 - prior)
  }
  if(length(prior) == 1 && is.na(prior)){
    prior <- rep(1/length(labels), length(labels))
  }
  
  # Add to rsl$labels
  id <- .getNewLabelID(rsl)
  rsl$labels[[id]] <- list(names = labels, 
                           id = id,
                           prior = prior)
  
  # build the probTable for the label node
  dimlist <- list(labels)
  names(dimlist)[1] <- id
  probTable <- array(prior, dim = length(labels), dimnames = dimlist)
  
  # rebuild the rsl$bayesNet
  tables <- lapply(rsl$bayesNet, "[[", "prob")
  tables[[id]] <- probTable
  nodes <- c(bnlearn::nodes(rsl$bayesNet), id)
  arcs <- bnlearn::arcs(rsl$bayesNet)
  rsl$bayesNet <- bnlearn::empty.graph(nodes = nodes)
  bnlearn::arcs(rsl$bayesNet) <- arcs
  rsl$bayesNet <- bnlearn::custom.fit(rsl$bayesNet, tables)
  rsl$needsCompilation <- TRUE
  
  return(rsl)
}


# .labelsAlreadyExist - checks if any of the labels in a given vector already
#                       exists in the rsl
.labelsAlreadyExist <- function(rsl, labels){
  isInLabelSet <- sapply(rsl$labels, function(x) any(labels %in% x$names))
  
  return(any(isInLabelSet))
}


# .classifierAlreadyExists - checks if there is an existing classifier called name
.classifierAlreadyExists <- function(rsl, name){
  hasEqualName <- sapply(rsl$classifiers, function(x) x$name == name)
  
  return(any(hasEqualName))
}


# removeLabels - removes a group of labels from the rsl
removeLabels <- function(rsl, labels){
  # TODO: Implement (low priority)
}


# getLabels - returns a list of all XOR-related label groups
getLabels <- function(rsl){
  return(lapply(rsl$labels, function(x) x$names))
}


# addClassifier - adds a classifier that connects to one XOR related group of 
#                 labels
# Input:
#  rsl - an rsl object
#  name - character, name of the classifier
#  labels - character, labels the classifier gives an output for
#  confusionMatrix - numeric matrix of size labels X labels, if NULL, will be 
#                    constructed using the accuracy (each row is an actual
#                    label, each column a prediction). Can either give the raw
#                    confusion matrix or a normalized version where columns
#                    sum to 1.
#  accuracy - numeric in [0, 1], used to calculate confusionMatrix if it is not
#             given. If NULL, standard value of 0.9 is used
#  prior - numeric, the priors of the actual labels
# Output:
#  updated rsl object
addClassifier <- function(rsl, name, labels, confusionMatrix = NULL, 
                          accuracy = NULL, prior = NA){
  if(.classifierAlreadyExists(rsl, name)){
    stop("name is already given to a classifier. Please select a new one.")
  }
  
  labels <- .coerceToMultilabel(labels)
  
  if(length(prior) == 1 && is.na(prior)){
    prior <- rep(1/length(labels), length(labels))
  }
  if(length(prior) == 1 && !is.na(prior)){
    # TODO: This could be dangerous if the labels are given by the user in a 
    # non-intuitive order. Maybe we should force the user to give all priors.
    prior <- c(prior, 1 - prior)
  }
  
  # Verify confusion matrix
  if(is.null(confusionMatrix)){
    # Construct confusion matrix from accuracy
    if(is.null(accuracy)){
      accuracy <- 0.95
    }
    confusionMatrix <- matrix((1 - accuracy) / (length(labels) - 1), 
                              ncol = length(labels), nrow = length(labels))
    diag(confusionMatrix) <- accuracy
  } else {
    if(!is.matrix(confusionMatrix)){
      stop("confusionMatrix is not a matrix.")
    }
    if(!is.numeric(confusionMatrix)){
      stop("confusionMatrix is not numeric.")
    }
    if(!all(dim(confusionMatrix) == c(length(labels), length(labels)))){
      stop("confusionMatrix has the wrong dimensions.")
    }
    if(any(colSums(confusionMatrix) == 0)){
      # We don't know the confusion structure of some label the classifier outputs.
      # Make them non-informative
      warning(paste0("confusionMatrix does not contain information on the case that ",
                     "the classifier outputs the label(s) ", 
                     colnames(confusionMatrix)[colSums(confusionMatrix) == 0],
                     ". Adding uniform distribution on those cases.", collapse = ", "))
      confusionMatrix[, colSums(confusionMatrix) == 0] <- 1 / nrow(confusionMatrix)
    }
    if(any(confusionMatrix > 1)){
      # Assume the confusionMatrix is raw
      confusionMatrix <- confusionMatrix / rep(colSums(confusionMatrix), each = nrow(confusionMatrix))
    }
    if(!all(colSums(confusionMatrix) == 1)){
      stop("Some cols of confusionMatrix do not sum to 1.")
    }
    if(!all(0 <= confusionMatrix & confusionMatrix <= 1)){
      stop("confusionMatrix contains values not in [0, 1]")
    }
    if(!is.null(rownames(confusionMatrix)) && !all(rownames(confusionMatrix) == labels)){
      stop("confusionMatrix has different rownames than labels (or different order)")
    }
    if(!is.null(colnames(confusionMatrix)) && !all(colnames(confusionMatrix) == labels)){
      stop("confusionMatrix has different colnames than labels (or different order)")
    }
  }
  colnames(confusionMatrix) <- labels
  rownames(confusionMatrix) <- labels
  
  # Find out if the labels already exist and have no classifier connected to
  # them yet:
  labelNode <- .labelSetToID(rsl, labels)
  if(!is.na(labelNode)){
    if(length(bnlearn::parents(rsl$bayesNet, labelNode)) != 0){
      stop("Labels already exist and have a classifier connected to them.")
    } else {
      # I think this is expected behaviour, so removed the warning
      # warning(paste0("labels ", labels, " already found in the rsl.", 
      #                "Connecting the classifier to those labels. Re-using old prior.", 
      #                collapse = ", "))
      prior <- rsl$labels[[labelNode]]$prior
    }
  } else {
    if(.labelsAlreadyExist(rsl, labels)){
      stop("Some of the labels already exist. Please choose new names.")
    } else {
      rsl <- addLabels(rsl, labels, prior)
      labelNode <- .labelSetToID(rsl, labels)
    }
  }
  
  # add to rsl$classifiers
  cID <- .getNewClassifierID(rsl)
  rsl$classifiers[[cID]] <- list(name = name,
                                 id = cID,
                                 confusionMatrix = confusionMatrix)
  
  # add to rsl$bayesNet
  # build cpt for the classificator node
  dimlist <- list(labels)
  names(dimlist)[1] <- cID
  # If the label prior is uniform, we can choose the classifier prior uniform, too
  # (which can avoid problems by the numerical inversion of ginv() )
  if(all(prior == 1 / length(prior))){
    cPrior <- prior
  } else {
    # Use ginv instead of solve to handle confusion matrices without full rank
    cPrior <- c(MASS::ginv(confusionMatrix) %*% prior)
    # TODO: When using ginv, make sure the result is projected into the [0,1]
    # space when the rank is not full
  }
  cProbTable <- array(cPrior, dim = length(labels), dimnames = dimlist)
  if(any(cPrior < 0 | cPrior > 1) | !all.equal(sum(cPrior), 1)){
    stop("The given confusion matrix and prior do not work together.")
  }
  
  # build cpt for the labels node
  dimlist <- list(labels, labels)
  names(dimlist) <- c(labelNode, cID)
  lProbTable <- array(confusionMatrix, dim = c(length(labels), length(labels)), dimnames = dimlist)
  
  # rebuild the rsl$bayesNet
  tables <- lapply(rsl$bayesNet, "[[", "prob")
  tables[[cID]] <- cProbTable
  tables[[labelNode]] <- lProbTable
  nodes <- c(bnlearn::nodes(rsl$bayesNet), cID)
  arcs <- rbind(bnlearn::arcs(rsl$bayesNet), cbind(cID, labelNode))
  rsl$bayesNet <- bnlearn::empty.graph(nodes = nodes)
  bnlearn::arcs(rsl$bayesNet) <- arcs
  rsl$bayesNet <- bnlearn::custom.fit(rsl$bayesNet, tables)
  rsl$needsCompilation <- TRUE
  
  return(rsl)
}


# removeClassifier - removes a classifier from an rsl
removeClassifier <- function(rsl, name){
  # TODO: Implement (low priority)
}


# getClassifiers - returns a list with information about all classifiers
getClassifiers <- function(rsl){
  res <- lapply(rsl$classifiers, function(x){
    return(list(confusionMatrix = x$confusionMatrix,
                labels = rownames(x$confusionMatrix)))
  })
  names(res) <- sapply(rsl$classifiers, "[[", "name")
  
  return(res)
}


print.rsl <- function(rsl){
  classifiers <- getClassifiers(rsl)
  labels <- getLabels(rsl)
  rules <- getRules(rsl)
  
  cat("Rule Stacking Learner comprises ", 
      length(classifiers), " classifiers, ",
      sum(sapply(labels, length)), " labels and ",
      nrow(rules), " rules.\n\n",
      "Classifiers:\n",
      paste0(names(classifiers), rep(": ", length(classifiers)), 
             sapply(classifiers, function(x) paste0(x$labels, collapse = ", ")), 
             collapse = "\n"), "\n\n",
      "Labels:\n", 
      paste(sapply(labels, paste, collapse = " | "), collapse = "\n"), "\n\n",
      "Rules:\n",
      paste(rules$name, " (prob = ", round(rules$prob, 4), ")", sep = "", collapse = "\n"), "\n",
      sep = "")
}


plot.rsl <- function(rsl){
  # TODO: Make the plot nicer (include node names instead of IDs)
  # -> maybe with igraph?
  rsl <- .compile(rsl)
  plot(rsl$compiledNet)
}


# compile - creates a grain network for inference in the RSL
.compile <- function(rsl){
  if(rsl$needsCompilation){
    rsl$compiledNet <- bnlearn::as.grain(rsl$bayesNet)
  }
  
  return(rsl)
}


# .getAllAuxNodes - returns the IDs of all auxiliary rule nodes
.getAllAuxNodes <- function(rsl){
  return(rsl$rules$auxID)
}


# .getNewAuxID - returns an unused ID for an Aux node (e.g. "A123")
.getNewAuxID <- function(rsl){
  auxs <- rsl$rules$auxID
  auxIDs <- gsub("^A([[:digit:]]+)$", "\\1", auxs)
  maxID <- max(c(0, as.integer(auxIDs)))
  return(paste0("A", maxID + 1))
}


# .getAllRules - returns a character vector containing all rules by their ID
.getAllRules <- function(rsl){
  return(rsl$rules$ruleID)
}


# .getNewRuleID - returns an unused ID for a rule node (e.g. "R123")
.getNewRuleID <- function(rsl){
  rules <- .getAllRules(rsl)
  ruleIDs <- gsub("^R([[:digit:]]+)$", "\\1", rules)
  maxID <- max(c(0, as.integer(ruleIDs)))
  return(paste0("R", maxID + 1))
}


# .getAllLabelNodes - returns a character vector including all label group nodes
.getAllLabelNodes <- function(rsl){
  return(unname(sapply(rsl$labels, "[[", "id")))
}


# .getNewLabelID - returns an unused ID for a label node (e.g. "L123")
.getNewLabelID <- function(rsl){
  labels <- .getAllLabelNodes(rsl)
  labelIDs <- gsub("^L([[:digit:]]+)$", "\\1", labels)
  maxID <- max(c(0, as.integer(labelIDs)))
  return(paste0("L", maxID + 1))
}


# .getAllClassifiers - returns a character vector including all classifier names
.getAllClassifiers <- function(rsl){
  return(unname(sapply(rsl$classifiers, "[[", "id")))
}


# .getNewClassifierID - returns an unused ID for a classifier node (e.g. "C123")
.getNewClassifierID <- function(rsl){
  classifiers <- .getAllClassifiers(rsl)
  classifierIDs <- gsub("^C([[:digit:]]+)$", "\\1", classifiers)
  maxID <- max(c(0, as.integer(classifierIDs)))
  return(paste0("C", maxID + 1))
}


.classifierToID <- function(rsl, name){
  isClassifier <- sapply(rsl$classifiers, function(x) x$name == name)
  if(length(isClassifier) > 0 && sum(isClassifier) == 1){
    return(rsl$classifiers[[which(isClassifier)]]$id)
  } else {
    return(NA)
  }
}


# .labelsToClassiferID - for a (subset of) label(s), returns the ID of a 
#                        classifier that includes all of them (and possibly more)
.labelsToClassifierID <- function(rsl, labels){
  isClassif <- sapply(rsl$classifiers, function(x){
    classifLabels <- colnames(x$confusionMatrix)
    return(all(labels %in% classifLabels))
  })
  if(length(isClassif) > 0 && sum(isClassif) == 1){
    return(names(rsl$classifiers)[isClassif])
  } else {
    return(NA)
  }
}

# .classifierIDtoLabelID - for a given classifier ID, returns the ID of the 
#                          label set it classifies
.classifierIDtoLabelID <- function(rsl, id){
  return(.labelSetToID(rsl, .IDtoClassifierLabels(rsl, id)))
}


.ruleToID <- function(rsl, name){
  # TODO: Implement (on demand)
}


# .labelSetToID - for a given vector of labels, gives back the label node id
.labelSetToID <- function(rsl, labels){
  isLabel <- sapply(rsl$labels, function(x) 
    length(x$names) == length(labels) && all(x$names == labels))
  if(length(isLabel) > 0 && sum(isLabel) == 1){
    return(rsl$labels[[which(isLabel)]]$id)
  } else {
    return(NA)
  }
}


.IDtoClassifier <- function(rsl, id){
  return(rsl$classifiers[[id]]$name)
}


.IDtoRule <- function(rsl, id){
  return(rsl$rules$name[rsl$rules$ruleID == id])
}


.IDtoLabelNode <- function(rsl, id){
  # TODO: Make the rsl actually save the label node names
  return(rsl$labels[[id]]$id)
}


.IDtoLabels <- function(rsl, id){
  return(rsl$labels[[id]]$names)
}


.IDtoClassifierLabels <- function(rsl, id){
  return(colnames(rsl$classifiers[[id]]$confusionMatrix))
}


# .setAuxEvidence - sets evidence of all auxiliary rule nodes to 1
.setAuxEvidence <- function(rsl){
  auxs <- .getAllAuxNodes(rsl)
  rsl$compiledNet <- gRain::setEvidence(rsl$compiledNet, nodes = auxs, 
                                        states = rep("fulfilled", length(auxs)))
  
  return(rsl)
}


# .setEvidence - sets (soft) evidence to all classifier inputs
# Input:
#  rsl - an rsl object
#  evidence - a list where each entry corresponds to a classifier node and has a 
#             1-row dataframe with probabilities of all labels of that node
.setEvidence <- function(rsl, evidence){
  ev <- lapply(evidence, function(x) unlist(x[1, ]))
  rsl$compiledNet <- gRain::setCPT(rsl$compiledNet, ev)
  
  return(rsl)
}


# .removeEvidence - resets all evidence in the grain classifier
.removeEvidence <- function(rsl){
  # TODO: Implement this in a faster way
  return(.compile(rsl))
}


# .preprocessData - adds missing labels and missing probabilities and reorders
#                   the columns of a dataframe into the standard scheme
# Output:
#  a list where each entry corresponds to a node and contains a dataframe with
#  the imputed prior weights of the labels in that node for each observation
.preprocessData <- function(rsl, data){
  # TODO: Make sure the output is in the correct order
  # TODO: Currently removes all labels that are not connected to a classifier (high priority)
  
  # match columns of data to classifiers (https://stackoverflow.com/a/51298361)
  labelIDs <- sapply(colnames(data), .labelsToClassifierID, rsl = rsl)
  dataList <- split.default(data, labelIDs)
  # check if the nodes contain all of their labels
  # Add labels and re-order if necessary
  for(i in seq(along = dataList)){
    allLabels <- .IDtoClassifierLabels(rsl, names(dataList)[i])
    order <- match(allLabels, colnames(dataList[[i]]))
    if(any(is.na(order))){
      # add missing labels
      missing <- setdiff(allLabels, colnames(dataList[[i]]))
      missingData <- matrix(NA, ncol = length(missing))
      colnames(missingData) <- missing
      dataList[[i]] <- cbind(dataList[[i]], as.data.frame(missingData))
      order <- match(allLabels, colnames(dataList[[i]]))
    }
    dataList[[i]] <- dataList[[i]][, order]
  }
  
  # Impute probabilities to missing labels and NAs
  # per observation: (1-sum(known probabilities)) / (number of missing probabilities)
  for(i in seq(along = dataList)){
    existingProb <- rowSums(dataList[[i]], na.rm = TRUE)
    nNA <- rowSums(is.na(dataList[[i]]))
    missingProb <- (1 - existingProb) / nNA
    missingData <- matrix(missingProb, nrow = nrow(dataList[[i]]), ncol = ncol(dataList[[i]]))
    dataList[[i]][is.na(dataList[[i]])] <- missingData[is.na(dataList[[i]])]
  }
  
  return(dataList)
}


# predict.rsl - computes a-posteriori estimates of all labels
# Input:
#  rsl - an rsl object
#  data - a dataframe where each column corresponds to a label and gives the 
#         probability  of that label (not all labels have to be given, NA are allowed)
#  method - "exact" or "approximate" to use exact calculation or likelihood
#           weighting (useful in big networks)
#  type - "marginal" or "joint", whether the a-posteriori estimates should be
#         marginal a-posteriori probabilities or joint MAP estimates
# Output:
#   a dataframe where each column gives the estimates of each label
predict.rsl <- function(rsl, data, showProgress = FALSE){
  # TODO: Add type checks
  # TODO: Implement method "approximate"
  # TODO: Implement type "joint"
  # TODO: Optionally also output the rule a-posteriori probabilities
  type <- "marginal"
  
  if(showProgress) cat("Compiling rsl...")
  rsl <- .compile(rsl)
  
  if(showProgress) cat("Preprocessing data...")
  dataList <- .preprocessData(rsl, data)
  
  # compute a-posteriori probabilities
  labels <- unlist(getLabels(rsl))
  post <- matrix(NA_real_, ncol = length(labels), nrow = nrow(data))
  colnames(post) <- labels
  rownames(post) <- rownames(data)
  post <- as.data.frame(post)
  if(showProgress) cat("Predicting...\n")
  for(i in seq(nrow(data))){
    if(showProgress) cat(i, "/", nrow(data), "\n")
    observation <- lapply(dataList, "[", i, , drop = FALSE) # argument left blank on purpose
    rsl <- .removeEvidence(rsl)
    rsl <- .setEvidence(rsl, observation)
    rsl <- .setAuxEvidence(rsl)
    
    relevantNodes <- names(rsl$labels)
    est <- gRain::querygrain(rsl$compiledNet, nodes = relevantNodes, type = type)
    # bring est to the order of labels
    names(est) <- NULL
    est <- unlist(est)
    # TODO: Do this matching only once in the end to save runtime
    est <- est[match(labels, names(est))]
    post[i, ] <- est
  }
  
  return(post)
}


# .generateStandardOrder - creates a matrix of all possible label combinations.
#                          Each column is a label node and iterates through its
#                          possible labels using expand.grid order
.generateStandardOrder <- function(rsl){
  return(expand.grid(getLabels(rsl), stringsAsFactors = FALSE))
}


# .computeGradientHamming - computes the gradient for hamming loss of a 
#                               given combination of priors and actuals
# Input:
#  weights - the current weights of the rules in standard order
#  jointPrior - the joint prior distribution of the observation in standard order
#  actual - character vector containing the actual values for each label node
#  standardOrder - the dataframe with the standard order of labels (this could
#                  also just be generated by .generateStandardOrder(), but re-
#                  creating it each time we compute a gradient is too costy)
# Output:
#  the gradient, a numeric vector of the same length as weights
# NOTE:
#  For a better intuition: standardOrder is a matrix. jointPrior, weights and
#  the gradient are in the same order as the COLUMNS of that matrix. actual is
#  in the order of the ROWS of that matrix.
.computeGradientHamming <- function(weights, jointPrior, actual, standardOrder){
  # TODO: re-check if this is implemented correctly (with Lena)
  grad <- rep(0, length(weights))
  logLik <- 0
  for(l in seq(ncol(standardOrder))){
    # For each label, add its gradient to the overall log loss gradient
    labelIsCorrect <- standardOrder[, l] == actual[l]
    probLabelCorrect <- sum((jointPrior * weights)[labelIsCorrect]) / sum(jointPrior * weights)
    prefactor <- probLabelCorrect^2 / sum((weights * jointPrior)[labelIsCorrect]) * jointPrior
    gradCorrect <- prefactor * (1 - probLabelCorrect) / probLabelCorrect
    gradIncorrect <- - prefactor
    labelGrad <- ifelse(labelIsCorrect, gradCorrect, gradIncorrect)
    
    # TODO: Check if this way of avoiding NaNs in the grad is ok (high priority)
    # TODO: Change logLik etc to other names corresponding to the new method
    gradAdd <- labelGrad
    if(!any(is.nan(gradAdd))){
      # Because d/dx log(f(x)) = 1 / x * d/dx f(x)
      grad <- grad + 1 / probLabelCorrect * gradAdd
    }
    
    logLik <- logLik + log(probLabelCorrect)
  }
  
  # Note that we have to use a "* (-1)" in order to have the gradient showing
  # towards the steepest ascent (not descent), because we want to maximize 
  # likelihood, not minimize it
  grad <- -grad
  
  return(list(grad = grad, logLik = logLik))
}


# .cosSimilarity - computes the cosine similarity between two numeric vectors
.cosSimilarity <- function(a, b){
  return(sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2))))
}


#.findOptJointRule - uses adam optimizer to find the weights of an optimal joint
#                    rule
# Input:
#  rsl - an rsl object
#  prior - dataset with priors of the labels (assumed to be preprocessed already)
#  actual - dataset of correct labels (assumed to be preprocessed already)
#  nRules - the desired number of rules to be learned
#  standardOrder - result of .generateStandardOrder(rsl) (handed over as 
#                  argument to avoid duplicate generation)
#  batchsize - batchsize for adam optimizer
#  alpha - hyperparameter for adam optimizer
#  beta1 - hyperparameter for adam optimizer
#  beta2 - hyperparameter for adam optimizer
#  maxIter - maximum iterations before adam optimizer is forcefully stopped
#  eps - term to avoid dividing by zero for adam optimizer
# Output:
#  a numeric vector giving the 
.findOptJointRule <- function(rsl, prior, actual, nRules, standardOrder,
                              batchsize, alpha, beta1, beta2, eps, maxIter){
  # Adam optimizer
  # The goal is to find the best "joint rule" that produces the best 
  # a-posteriori probabilities for the given inputs.
  cat("Finding best joint rule...")
  # TODO: Start with random weights or with 0.5^L?
  weights <- rep(0.5^nRules, nrow(standardOrder))
  # Adam parameters as in https://towardsdatascience.com/10-gradient-descent-optimisation-algorithms-86989510b5e9
  t <- 1 # iteration
  m <- 0 # momentum
  v <- 0 # exponential moving average of squared gradients
  repeat{
    # Compute the minibatch gradient:
    selectedObs <- sample(nrow(prior), batchsize)
    grad <- rep(0, length(weights))
    logLik <- 0
    for(obs in selectedObs){
      # It might make sense to compute the joint prior once for all observations
      # in the beginning, but it might run out of memory
      # The joint Prior gives the joint prior weight for each possible combination
      # of labels (= each row) in standardOrder
      jointPrior <- apply(standardOrder, 1, function(x)
        prod(prior[obs, colnames(prior) %in% x]))
      # This uses that labels are unique. If that is changed, we have to change
      # the computation of jointPrior here too
      ham <- .computeGradientHamming(weights = weights, 
                                     jointPrior = jointPrior, 
                                     actual = unlist(actual[obs, ]),
                                     standardOrder = standardOrder)
      logLik <- logLik + ham$logLik
      grad <- grad + ham$grad
    }
    
    m <- beta1 * m + (1 - beta1) * grad
    v <- beta2 * v + (1 - beta2) * grad^2
    mHat <- m / (1 - beta1^t)
    vHat <- v / (1 - beta2^t)
    # TODO: Is truncating to [0, 1] a good idea? We could also rescale to [0, 1].
    newWeights <- pmin(1, pmax(0, weights - alpha / (sqrt(vHat) + eps) * mHat))
    # TODO: Should we use another loss here because of the invariance of the BN
    #       to constant factors in the weight vector?
    diff <- sum((weights -  newWeights)^2)
    relDiff <- sqrt(diff) / sqrt(sum(weights^2))
    weights <- newWeights
    
    cat("Diff:", diff, ", relDiff:", relDiff, "logLik:", logLik, "\n")
    # temp <- list(weights = weights, diff = diff, relDiff = relDiff, logLik = logLik)
    # save(temp, file = paste0("gradDesc", t, ".RData"))
    
    t <- t + 1
    # if(diff < delta1){
    #   cat("Converged.\n")
    #   break
    # }
    if(t > maxIter){
      cat("Reached maxIter without converging.\n")
      break
    } 
  }
  
  return(weights)
}

# .splitJointRule - Splits the joint rule into local rules.
#                   Find rules such that the weights generated by that rule set 
#                   is as similar as possible to given joint rule weights
# Input:
#  rsl - an rsl object
#  weights - the weights vector of the joint rule
#  nRules - integer giving the desired number of local rules
#  standardOrder - as generated by .generateStandardOrder() (given as argument
#                  to avoid duplicated generation)
# onlyPositiveRules - logical whether all local rules should be forced to have a 
#                     probability of > 0.5
# Output:
#  a list where each entry is a list consisting of
#    ruleHead - the labels that are in the rule head
#    p - the probability of the rule
#    weights - the vector of rule weights in standard order
.splitJointRule <- function(rsl, weights, nRules, standardOrder, onlyPositiveRules){
  cat("Splitting joint rule into", nRules, "local rules...")
  # start with non-informative rules. 
  rules <- list()
  allLabels <- c(unlist(getLabels(rsl)), paste0("!", unlist(getLabels(rsl))))
  for(rule in seq(nRules)){
    # -> cos distance only uses direction, not length. So, it does not matter how
    #    big the values in the vector are as long as they are all the same. So we
    #    will just use p = 0.5
    rules[[rule]] <- list(ruleHead = c(),
                          p = 0.5,
                          weights = rep(0.5, nrow(standardOrder)))
  }
  
  # Greedy local rule extraction:
  pLower <- ifelse(onlyPositiveRules, 0.5, 0)
  rulesWeights <- rep(0.5^nRules, nrow(standardOrder))
  bestSimilarity <- .cosSimilarity(rulesWeights, weights)
  # -> why cos distance? Because it ignores the vector's length and only direction counts
  #    (and the BN always normalizes the weights - so to say - too because of 
  #    the normalization over the sum of all combinations)
  # For each rule (greedy)
  for(rule in seq(nRules)){
    cat("Rule", rule, "...")
    # Forward search through all labels for a rule head 
    best <- rules[[rule]]
    for(i in seq(length(allLabels))){
      bestChanged <- FALSE
      for(label in setdiff(allLabels, best$ruleHead)){
        proposal <- rules[[rule]]
        proposal$ruleHead <- c(proposal$ruleHead, label)
        startsWithNot <- grepl("^!", proposal$ruleHead)
        cleanedHead <- gsub("^!", "", proposal$ruleHead)
        trueLabels <- cleanedHead[!startsWithNot]
        falseLabels <- cleanedHead[startsWithNot]
        isFulfilled <- apply(standardOrder, 1, function(x){
          return(any(x %in% trueLabels) | any(!falseLabels %in% x))
        })
        # Select the p that optimizes the similarity
        p <- optimize(function(p){
          curWeights <- p * isFulfilled + (1 - p) * (!isFulfilled)
          # Replace current best proposal with our new one
          proposedWeights <- rulesWeights / rules[[rule]]$weights * curWeights
          return(.cosSimilarity(proposedWeights, weights))
        }, lower = pLower, upper = 1, maximum = TRUE)
        proposal$p <- p$maximum
        proposal$weights <- proposal$p * isFulfilled + (1 - proposal$p) * (!isFulfilled)
        
        # Check if the rule has increased the similarity
        if(p$objective > bestSimilarity){
          bestChanged <- TRUE
          best <- proposal
          bestSimilarity <- p$objective
        }
      }
      
      # Stop if similarity has not increased by adding a label
      if(bestChanged){
        rulesWeights <- rulesWeights / rules[[rule]]$weights * best$weights
        rules[[rule]] <- best
      } else {
        break
      }
    }
  }
  
  return(rules)
}


# learnRules - learns rules for an rsl object in order to maximize an 
#              a-posteriori loss given data
# Input:
#  rsl - an rsl object without any rules yet (existing rules will be deleted)
#  prior - a dataframe where each column corresponds to a label and gives the 
#          probability  of that label (not all labels have to be given, NA are allowed)
#  actual - a dataframe where each column corresponds to a label node (!) and 
#           gives its correct label (all nodes have to be given, NAs are not allowed)
#           So, it has to has as many columns as there are label groups.
#  nRules - the desired number of rules to be learned
#  method - "hamming" for optimizing the a-posteriori hamming loss
#  onlyPositiveRules - logical indicating whether only rules with p in [0.5, 1] 
#                      should be searched (TRUE) or with p in [0, 1] (FALSE).
#                      The former is easier to interpret, the latter will have
#                      better prediction accuracy.
#  batchsize - batchsize for adam optimizer
#  alpha - hyperparameter for adam optimizer
#  beta1 - hyperparameter for adam optimizer
#  beta2 - hyperparameter for adam optimizer
#  maxIter1 - maximum iterations before adam optimizer is forcefully stopped
#  eps - term to avoid dividing by zero for adam optimizer
#  delta1 - convergence threshold for adam optimizer
# Output:
#  rsl object, but with added rules
learnRules <- function(rsl, prior, actual, nRules = 20, method = "hamming",
                       onlyPositiveRules = FALSE, 
                       batchsize = 20, alpha = 0.002, beta1 = 0.9, beta2 = 0.999,
                       maxIter1 = 10000, delta1 = 1e-6, eps = 1e-8){
  # TODO: Allow rsl to have existing rules and use them as starting point
  # TODO: Allow to auto-tune the nRules by setting it to NA
  # TODO: Implement method "joint loss"
  # TODO: Make less restrictions on the actual input and impute if possible
  # TODO: check that actual is in a correct format
  # TODO: Make the gradient descent work with incomplete data
  
  if(nrow(getRules(rsl)) > 0){
    stop("The rsl object must not have any rules in it already.")
  }
  if(nrow(prior) != nrow(actual)){
    stop("Unequal number of observations in prior and actual.")
  }
  batchsize <- min(nrow(actual), batchsize)
  
  # Convert the classifier priors into the actual-label priors
  # (which corresponds to predicting without any rules)
  cat("Preparing data...")
  prior <- predict(rsl, prior)
  
  # Apply Adam optimizer to find best weights for joint rule
  standardOrder <- .generateStandardOrder(rsl)
  weights <- .findOptJointRule(rsl, prior, actual, nRules, standardOrder, 
                               batchsize, alpha, beta1, beta2, eps, maxIter1)
  
  # Split the joint rule into local rules:
  rules <- .splitJointRule(rsl, weights, nRules, standardOrder, onlyPositiveRules)
  
  # Build found rules into the rsl
  cat("\nAdding local rules to rsl...\n")
  for(rule in seq(nRules)){
    # TODO: Refactor most of this into .splitJointRule()
    if(length(rules[[rule]]$ruleHead) == 0 | isTRUE(all.equal(rules[[rule]]$p, 0.5))){
      warning(paste0("Local rule ", rule, " is noninformative. Not adding this rule to rsl."))
    } else {
      startsWithNot <- grepl("^!", rules[[rule]]$ruleHead)
      cleanedHead <- gsub("^!", "", rules[[rule]]$ruleHead)
      trueLabels <- cleanedHead[!startsWithNot]
      falseLabels <- cleanedHead[startsWithNot]
      rsl <- addRule(rsl, 
                     rule = paste(paste(trueLabels, collapse = ", "), 
                                  "<-", 
                                  paste(falseLabels, collapse = ", ")), 
                     prob = rules[[rule]]$p)
    }
  }
  
  return(rsl)
}


# .crispToProbabilisticData - turns a dataframe that columns are label nodes and
#                             is filled with the (crisp) labels of those nodes
#                             into a dataframe where each column is a label and
#                             is 1 or 0 depending on whether it is active or not
.crispToProabilisticData <- function(rsl, data){
  # TODO: Check for incomplete data and make this a public function
  # TODO: Implement Unit Tests
  datalist <- lapply(seq(ncol(data)), function(i){
    labels <- .IDtoLabels(rsl, colnames(data)[i])
    isLabel <- matrix(as.numeric(rep(data[, i], each = length(labels)) == labels), 
                      ncol = length(labels), byrow = TRUE)
    isLabel <- as.data.frame(isLabel)
    colnames(isLabel) <- labels
    return(isLabel)
  })
  
  return(do.call(cbind, datalist))
}


# .probabilisticToCrispData - turns a dataframe where each column is a label and
#                             gives its label probability into a dataframe where
#                             each column is a label node and has the label with
#                             the highest probability
# Input:
#  tieBreak - if two labels have the same probability, which should be chosen as
#             label. "random" to randomize it, "first" to always use the first
#             and "NA" to report an NA in that case.
.probabilisticToCrispData <- function(rsl, data, tieBreak = "NA"){
  # TODO: Check for incomplete data and make this a public function
  # TODO: Implement Unit Tests
  
  .whichMax <- function(probs, tieBreak){
    isMax <- which(probs == max(probs))
    if(length(isMax) == 0){
      return(NA)
    } else if(length(isMax) == 1){
      return(isMax)
    } else if(length(isMax) > 1){
      if(tieBreak == "random"){
        return(sample(isMax, 1))
      } else if(tieBreak == "first"){
        return(isMax[1])
      } else if(is.na(tieBreak) || tieBreak == "NA"){
        return(NA)
      }
    }
  }
  
  dataList <- .preprocessData(rsl, data)
  labelList <- lapply(dataList, function(x){
    colnames(x)[apply(x, 1, .whichMax, tieBreak)]
  })
  labelDataframe <- do.call(cbind, labelList)
  labelDataframe <- as.data.frame(labelDataframe, stringsAsFactors = FALSE)
  colnames(labelDataframe) <- sapply(colnames(labelDataframe), function(x) 
    .classifierIDtoLabelID(rsl, x))
  
  return(labelDataframe)
}


# hammingLoss - computes the relative amount of labels that are wrong
# Input:
#  pred - dataframe where each column is a labelset and includes its label
#         prediction per observation
#  actual - dataframe where each column is a labelset and includes its true label
#           per observation
#  na.rm - logical indicating whether NAs in pred or in actual should be ignored
hammingLoss <- function(pred, actual, na.rm = TRUE){
  if(any(sapply(pred, class) != "character") | any(sapply(actual, class) != "character")){
    stop("Wrong types. Please provide dataframes with the label names per observation.")
  }
  if(any(dim(pred) != dim(actual))){
    stop("pred and actual have different dimensions.")
  }
  if(!all(colnames(pred) %in% colnames(actual)) | !all(colnames(actual) %in% colnames(pred))){
    stop("pred and actual have different colnames.")
  }
  pred <- pred[, match(colnames(actual), colnames(pred))]
  
  return(mean(pred != actual, na.rm = na.rm))
}


# accuracy - computes the relative amount of observations in which all labels are
#            correct (keep in mind that here more is better!)
# Input:
#  pred - dataframe where each column is a labelset and includes its label
#         prediction per observation
#  actual - dataframe where each column is a labelset and includes its true label
#           per observation
#  na.rm - logical indicating whether NAs in pred or in actual should be ignored
accuracy <- function(pred, actual, na.rm = TRUE){
  if(any(sapply(pred, class) != "character") | any(sapply(actual, class) != "character")){
    stop("Wrong types. Please provide dataframes with the label names per observation.")
  }
  if(any(dim(pred) != dim(actual))){
    stop("pred and actual have different dimensions.")
  }
  if(!all(colnames(pred) %in% colnames(actual)) | !all(colnames(actual) %in% colnames(pred))){
    stop("pred and actual have different colnames.")
  }
  pred <- pred[, match(colnames(actual), colnames(pred))]
  
  return(mean(rowSums(pred == actual) == ncol(pred), na.rm = na.rm))
}


# .labelwiseLogLikelihood - given some predictions and actual labels, gives the
#                          summed labelwise log-likelihood of the true labels
# Input:
#  pred - dataframe where each column is a label and includes its predicted 
#         likelihood
#  actual - dataframe where each column is a labelset and includes its true label
#           per observation
.labelwiseLogLikelihood <- function(pred, actual){
  # TODO: Add type checks to make this a public function 
  
  logL <- numeric(nrow(pred))
  for(i in seq(along = logL)){
    logL[i] <- sum(log(pred[i, unlist(actual[i, ])]))
  }
  
  return(logL)
}


# .labelwiseLikelihood - given some predictions and actual labels, gives the
#                        summed labelwise likelihood of the true labels
# Input:
#  pred - dataframe where each column is a label and includes its predicted 
#         likelihood
#  actual - dataframe where each column is a labelset and includes its true label
#           per observation
.labelwiseLikelihood <- function(pred, actual){
  # TODO: Add type checks to make this a public function 
  
  lik <- numeric(nrow(pred))
  for(i in seq(along = lik)){
    lik[i] <- sum(pred[i, unlist(actual[i, ])])
  }
  
  return(lik)
}


# .colnamesIDtoNames - replaces the IDs that might be in the colnames of a 
#                      dataframe to their corresponding names
.colnamesIDtoNames <- function(rsl, data){
  if(ncol(data) < 1){
    return(data)
  }
  
  for(i in seq(ncol(data))){
    cur <- colnames(data)[i]
    name <- c(.IDtoClassifier(rsl, cur), 
              .IDtoLabelNode(rsl, cur),
              .IDtoRule(rsl, cur))
    if(sum(!is.null(name)) == 1){
      cur <- name[!is.null(name)]
    }
    colnames(data)[i] <- cur
  }
  
  return(data)
}



# simulate - samples observations from a given rsl
# Input:
#  rsl - an rsl object
#  n - number of observations to simulate
#  outputClassifiers - boolean indicating whether the classifier inputs should 
#                      be included in the output
#  outputLabels - boolean indicating whether the (true) labels should be included 
#                 in the output
#  outputRules - boolean indicating whether the active rules should be included 
#                in the output
# Output:
#  a dataframe containing the simulated observations
simulate <- function(rsl, n, outputClassifiers = TRUE, outputLabels = TRUE, 
                     outputRules = TRUE){
  # simulate data
  rsl <- .compile(rsl)
  rsl <- .setAuxEvidence(rsl)
  data <- gRain::simulate.grain(rsl$compiledNet, n)
  
  # throw out variables the user did not request
  outputVars <- character(0)
  if(outputClassifiers) outputVars <- c(outputVars, .getAllClassifiers(rsl))
  if(outputLabels) outputVars <- c(outputVars, .getAllLabelNodes(rsl))
  if(outputRules) outputVars <- c(outputVars, .getAllRules(rsl))
  data <- data[, outputVars]
  
  # make the dataframe a bit more user friendly
  data <- .colnamesIDtoNames(rsl, data)
  data[] <- lapply(data, as.character) # factors to character
  
  return(data)
}
