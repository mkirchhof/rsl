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
    stop(paste0("Rule ", rule, " already exists in the rsl. ",
                "Please adjust its prob instead of adding it multiple times."))
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


removeRule <- function(rsl, rule){
  # TODO: Implement (low priority)
}


# getRules - returns a dataframe with information about all rules
getRules <- function(rsl){
  return(rsl$rules[, c("name", "prob")])
}


.coerceToMultilabel <- function(labels){
  if(length(labels) == 1){
    warning(paste0("label seems to be binary. Coerced it to multinomial by adding ",
                   "not_", labels, " as second category."))
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
  if(any(confusionMatrix > 1)){
    # Assume the confusionMatrix is raw
    confusionMatrix <- confusionMatrix / rep(colSums(confusionMatrix), nrow(confusionMatrix))
  }
  
  labels <- .coerceToMultilabel(labels)
  
  if(length(prior) == 1 && is.na(prior)){
    prior <- rep(1/length(labels), length(labels))
  }
  if(length(prior) == 1 && !is.na(prior)){
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
    if(!is.null(bnlearn::parents(rsl$bayesNet, labelNode))){
      stop("Labels already exist and have a classifier connected to them.")
    } else {
      warning(paste0("labels ", labels, " already found in the rsl.", 
                     "Connecting the classifier to those labels. Re-using old prior."))
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
  # TODO: What to do if solving does not work (singular system)?
  cPrior <- solve(a = confusionMatrix, b = prior)
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
      paste(rules$name, " (prob = ", rules$prob, ")", sep = "", collapse = "\n"), "\n",
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


# .getNewRuleID - returns an unused ID for a rule node (e.g. "R123")
.getNewRuleID <- function(rsl){
  rules <- rsl$rules$ruleID
  ruleIDs <- gsub("^R([[:digit:]]+)$", "\\1", rules)
  maxID <- max(c(0, as.integer(ruleIDs)))
  return(paste0("R", maxID + 1))
}


# .getNewLabelID - returns an unused ID for a label node (e.g. "L123")
.getNewLabelID <- function(rsl){
  labels <- sapply(rsl$labels, "[[", "id")
  labelIDs <- gsub("^L([[:digit:]]+)$", "\\1", labels)
  maxID <- max(c(0, as.integer(labelIDs)))
  return(paste0("L", maxID + 1))
}


# .getNewClassifierID - returns an unused ID for a classifier node (e.g. "C123")
.getNewClassifierID <- function(rsl){
  classifiers <- sapply(rsl$classifiers, "[[", "id")
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
  rsl$compiledNet <- setCPT(rsl$compiledNet, ev)
  
  return(rsl)
}


# .removeEvidence - resets all evidence in the grain classifier
.removeEvidence <- function(rsl){
  # TODO: Implement this in a faster way
  return(.compile(rsl))
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
predict.rsl <- function(rsl, data){
  # TODO: Add type checks
  # TODO: Implement method "approximate"
  # TODO: Implement type "joint"
  type <- "marginal"
  # TODO: Optionally also output the rule a-posteriori probabilities
  
  rsl <- .compile(rsl)
  
  # match columns of data to classifiers (https://stackoverflow.com/a/51298361)
  labelIDs <- sapply(colnames(data), .labelsToClassifierID, rsl = rsl)
  dataList <- split.default(data, labelIDs)
  # check if the nodes contain all of their labels
  # Add labels and re-order if necessary
  for(i in seq(along = dataList)){
    allLabels <- .IDtoClassifierLabels(rsl, names(dataList)[i])
    order <- match(allLabels, colnames(dataList[[i]]))
    if(any(is.na(order))){
      # add missing labels with prob: (1-sum(prob)) / (number of missing labels)
      missing <- setdiff(allLabels, colnames(dataList[[i]]))
      existingProb <- rowSums(dataList[[i]])
      missingData <- matrix((1 - existingProb) / length(missing), ncol = length(missing))
      colnames(missingData) <- missing
      dataList[[i]] <- cbind(dataList[[i]], as.data.frame(missingData))
      order <- match(allLabels, colnames(dataList[[i]]))
    }
    dataList[[i]] <- dataList[[i]][, order]
  }
  
  # compute a-posteriori probabilities
  labels <- unlist(getLabels(rsl))
  post <- matrix(NA_real_, ncol = length(labels), nrow = nrow(data))
  colnames(post) <- labels
  rownames(post) <- rownames(data)
  post <- as.data.frame(post)
  for(i in seq(nrow(data))){
    observation <- lapply(dataList, "[", i, , drop = FALSE) # argument left blank on purpose
    rsl <- .removeEvidence(rsl)
    rsl <- .setEvidence(rsl, observation)
    rsl <- .setAuxEvidence(rsl)
    
    relevantNodes <- names(rsl$labels)
    est <- gRain::querygrain(rsl$compiledNet, nodes = relevantNodes, type = type)
    # bring est to the order of labels
    names(est) <- NULL
    est <- unlist(est)
    est <- est[match(labels, names(est))]
    post[i, ] <- est
  }
  
  return(post)
}


# learnRules
learnRules <- function(rsl, data, nRules = NA, method = "hammingtion"){
  # TODO: Implement (low priority)
}

