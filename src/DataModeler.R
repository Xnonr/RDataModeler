#By Maxence Brette

rm(list = ls()) #Clears the current work space & R Studio Global Environment
cat("\014") #Clears the R Studio Console

#Installs & imports the desired libraries
#install.packages('dplyr') #Remains commented out following instillation
#install.packages('plyr')
#install.packages('e1071')
#install.packages('caret')
#install.packages('rpart')
#install.packages('taRifx')
#install.packages('adabag')
#install.packages('stringr')
#install.packages('compare')
#install.packages('cluster')
#install.packages('rpart.plot')

library(dplyr)
#library(plyr)
library(e1071)
library(caret)
library(rpart)
library(taRifx)
#library(adabag)
library(stringr)
#library(compare)
#library(cluster)
library(rpart.plot)

#Adjusts the working directory as needed
#setwd("~/Documents/github/RDataModeler/src")

#CONSTANTS
TN <- 1
FN <- 2
FP <- 3
TP <- 4

SENSITIVITY <- 1
SPECIFICITY <- 2
ACCURACY <- 3
BALANCEDACCURACY <- 4

RESULTS <- 1
CONFUSIONMATRIX <- 2

#Loads the base data file into a data frame
createBaseDF <- function(baseCSVFile) {
   baseDF <- read.csv(file = baseCSVFile, stringsAsFactors = FALSE)
   return(baseDF)
}

#Obtains a list of a data frame's variable column names
obtainColumnNames <- function(baseDF) {
   columnNamesList <- colnames(baseDF)
   return(columnNamesList)
}

#Creates a list of undesired variable columns to be removed
filterColumns <- function(baseDF) {
   availableColumns <- obtainColumnNames(baseDF)
   
   print('Type in the name of a column you wish to exclude from the model.')
   
   done <- FALSE
   while (done == FALSE) {
      print('Removable Columns: ')
      print(availableColumns)
      
      columnToRemove <- readInputs("Type in 'D' if done: ")
      
      if (columnToRemove == 'D') {
         done <- TRUE
      }
      
      else if (columnToRemove %in% availableColumns == TRUE) {
         availableColumns <- availableColumns[availableColumns != columnToRemove]
         print(paste('The following column has been removed: ', columnToRemove))
      }
      
      else if (columnToRemove %in% availableColumns == FALSE) {
         print('This column is currently not available for removal.')
      }
   }
   return(availableColumns)
}

#Removes unwanted variable columns to create a new adjusted data frame
adjustBaseDF <- function(predefinedToRemoveColumnsList, baseDF) {
   if (is.null(predefinedToRemoveColumnsList) == FALSE) {
      columnsToKeep <- obtainColumnNames(baseDF)
      columnsToKeep <- setdiff(columnsToKeep, predefinedToRemoveColumnsList)
   }
   else {
      columnsToKeep <- filterColumns(baseDF)
   }

   adjustedDF <- baseDF[columnsToKeep]
   return(adjustedDF)
}

selectVariableToPredict <- function(adjustedDF) {
   availableColumns <- obtainColumnNames(adjustedDF)
   
   done <- FALSE
   while (done == FALSE) {
      print('Available Columns: ')
      print(availableColumns)
      
      columnToPredict <- readInputs('Type in the name of the column whose variable you wish predict: ')
      
      if (columnToPredict %in% availableColumns == TRUE) {
         print(paste('The following variable column has been selected for prediction: ', columnToPredict))
         done <- TRUE
      }
      
      else if (columnToPredict %in% availableColumns == FALSE) {
         print('This column is currently not available for prediction.')
      }
   }
   
   return(columnToPredict)
}

#Changes the following relevant variable columns into factors as directed
factorizeDF <- function(toFactorizeDF) {
   for (col in 1:ncol(toFactorizeDF)) {
      uniqueValues <- n_distinct(toFactorizeDF[, col])
      if (uniqueValues <= 5) {
         columnName <- colnames(toFactorizeDF)[col]
         #print(paste(columnName, uniqueValues))
         toFactorizeDF[, col] <- as.factor(toFactorizeDF[, col])
      }
         
   }
   return(toFactorizeDF)
}

#Splits the data frame into the 2 separate testing and training data frames
splitIntoTrainingTestingDFs <- function(toSplitDF) {
   #Sets a predetermined seed for reproducible results in future repeat tests
   set.seed(123)

   #Randomly selects 2/3 of the data frame's rows as a new sub-index
   trainDF <- sample(1:nrow(toSplitDF), nrow(toSplitDF) * (2/3))

   #Uses the 'train' index in order to split the 'toSplitDF' data set into 2 parts
   toSplitDF.train <- toSplitDF[trainDF,] #Builds the model
   toSplitDF.test <- toSplitDF[-trainDF,] #Trains the model

   trainTestDFs <- list('trainDF' = toSplitDF.train, 'testDF' = toSplitDF.test)

   return(trainTestDFs)
}

#Asks whether or not the user would like to print out the Model CM & its Results
printTFQuestionaire <- function() {
   i = 0
   while (i != 1) {
      decision <- readInputs("Would you like to print out the Model's Confusion Matrix & Results? (Y | N): ")

      if (is.character(decision) == FALSE) {
         print("This input is invalid. Please try again.")
      }
      if (is.character(decision) == TRUE) {
         decision <- toupper(decision)
         if (decision != 'Y' & decision != 'N') {
            print("This input is invalid. Please try again.")
         }
         if (decision == 'Y') {
            i = 1
            return(TRUE)
         }
         if (decision == 'N') {
            i = 1
            return(FALSE)
         }
      }
   }
}

#Creates a base from the actual test data frame's desired variable to predict
#for easy model comparison
createBaseResults <- function(trainTestDFsList, variableToPredict) {
   testingDF <- trainTestDFsList$testDF
   
   baseActual <- testingDF[, variableToPredict]
   baseResultsTable <- table(baseActual, baseActual)
   
   numUniqueValues <- nrow(baseResultsTable)
   uniqueValues <- unique(testingDF[, variableToPredict])
   
   baseCMList <- calcModelCM(baseResultsTable, 
                             numUniqueValues, 
                             'base')
   
   avgBaseOutput <- calcAvgModelCM(baseCMList, 
                                   uniqueValues, 
                                   numUniqueValues)
   
   avgBaseResults <- avgBaseOutput[[RESULTS]]
   avgBaseCM <- avgBaseOutput[[CONFUSIONMATRIX]]
   
   baseResults <- list('bSen' = avgBaseResults[SENSITIVITY],
                       'bSpe' = avgBaseResults[SPECIFICITY],
                       'bAcc' = avgBaseResults[ACCURACY],
                       'bBalAcc' = avgBaseResults[BALANCEDACCURACY],
                       'bTP' = avgBaseCM[2,2], 
                       'bTN' = avgBaseCM[1,1],
                       'bFP' = avgBaseCM[2,1], 
                       'bFN' = avgBaseCM[1,2])

   return(baseResults)
}

#Linear Regression Model
createLinearRegressionModel <- function(trainTestDFsList, 
                                        variableToPredict, 
                                        printTF) {
   testingDF <- trainTestDFsList$testDF
   trainingDF <- trainTestDFsList$trainDF
   
   linRegFormula <- paste(variableToPredict, '~ .')
   
   linRegFit1 <- lm(formula = linRegFormula, 
                   data = trainingDF)
   
   ss <- coef(summary(linRegFit1))
   ss_sig <- ss[ss[,"Pr(>|t|)"] < 0.05,]
   
   significantVariables <- c(variableToPredict)
   
   for (i in colnames(trainingDF)) {
      for (j in names(ss_sig[,"Pr(>|t|)"])) {
         if (grepl(i, j, fixed = TRUE)) {
            print(paste(i,j))
            significantVariables <- c(significantVariables, i)
         }
      }
   }
   
   insignificantVariables <- setdiff(colnames(trainingDF), 
                                     significantVariables)
   
   trainingDF <- trainingDF[ , ! names(trainingDF) %in% insignificantVariables]
   
   linRegFit2 <- lm(formula = linRegFormula, 
                   data = trainingDF)
   
   linRegPred <- round(predict(linRegFit2, testingDF))
   linRegActual <- testingDF[, variableToPredict]
   
   linRegResultsTable <- cbind(linRegPred, linRegActual)
   
   if (printTF == TRUE) {
      #print(head(linRegResultsTable, 50))
      print(significantVariables)
      print(insignificantVariables)
      print(summary(linRegFit1))
      print(paste('Linear Regression Model 1 Accuracy: ', summary(linRegFit1)$adj.r.squared))
      print(summary(linRegFit2))
      print(paste('Linear Regression Model 2 Accuracy: ', summary(linRegFit2)$adj.r.squared))
   }
}

#Decision Tree Model
createDecisionTreeModel <- function(trainTestDFsList, 
                                    variableToPredict, 
                                    printTF) {
   testingDF <- trainTestDFsList$testDF
   trainingDF <- trainTestDFsList$trainDF

   #Creates the formula using all other variables
   treeFormula <- paste(variableToPredict, '~ .')
   
   #Grows the Tree, attempting to model the desired variable column to predict
   treeFit <- rpart(formula = treeFormula,
                    data = trainingDF,
                    method = "class",
                    control = rpart.control(xval = 10, minsplit = 50),
                    parms = list(split = "gini"))

   #Plots & prints out the Decision Tree Model results
   #Plots the decision tree in a more aesthetically pleasing fashion
   rpart.plot(treeFit, type = 1,
              extra = 1,
              main = paste('Classification Tree Prediction for: ', 
                           variableToPredict))
   
   #Extracts the vector of predicted class for each observation in 'testingDF'
   treePred <- predict(treeFit, testingDF, type = "class")
   #Extracts the actual class of each observation in 'testingDF'
   treeActual <- testingDF[, variableToPredict]

   #Builds the results table
   treeResultsTable <- table(treePred, treeActual)

   #Determines how many and what are the unique variables in the chosen column
   numUniqueValues <- nrow(treeResultsTable)
   uniqueValues <- unique(testingDF[, variableToPredict])
   
   treeCMList <- calcModelCM(treeResultsTable, 
                             numUniqueValues, 
                             'decisionTree')
      
   avgTreeOutput <- calcAvgModelCM(treeCMList, 
                                   uniqueValues, 
                                   numUniqueValues)
   
   avgTreeResults <- avgTreeOutput[[RESULTS]]
   avgTreeCM <- avgTreeOutput[[CONFUSIONMATRIX]]
   
   treeTable <- concentrateModelResults(treeCMList, 
                                        avgTreeCM, 
                                        avgTreeResults, 
                                        uniqueValues, 
                                        numUniqueValues)
   
   if (printTF == TRUE) {
      printModelResults('Decision Tree Model', 
                        variableToPredict, 
                        treeTable)
      
      # printAverageModelResults('Decision Tree Model Average', 
      #                           variableToPredict, avgTreeCM, 
      #                           avgTreeResults[SENSITIVITY], 
      #                           avgTreeResults[SPECIFICITY], 
      #                           avgTreeResults[ACCURACY], 
      #                           avgTreeResults[BALANCEDACCURACY])
   }
   
   decisionTreeResults <- list('dtPred' = treePred,
                               'dtTable' = treeTable)
   
   return(decisionTreeResults)
}

#Logistic Regression Model (Only for use on binomial outcomes)
createLogisticRegressionModel <- function(trainTestDFsList, 
                                          variableToPredict, 
                                          printTF) {
   testingDF <- trainTestDFsList$testDF
   trainingDF <- trainTestDFsList$trainDF

   logitRegFormula <- paste(variableToPredict, '~ .')
   
   logitReg <- glm(formula = logitRegFormula,
                   data = trainingDF,
                   family = "binomial")

   #Computes the predicted probabilities
   logitRegPred <- predict(logitReg, testingDF, type = "response")
   testingDF$logitRegPred <- logitRegPred
   #Chooses 0.5 as the cutoff for 1 VS. 0 classes
   testingDF$logitRegVariablePred <- ifelse(testingDF$logitRegPred > 0.5, 1, 0)

   logitRegPred <- testingDF$logitRegVariablePred
   logitRegActual <- testingDF[, variableToPredict]
   
   logitRegResultsTable <- table(logitRegPred, logitRegActual)

   testingDF <- testingDF[, !(names(testingDF) %in% c("logitRegPred",
                                                      "logitRegVariablePred"))]
   
   numUniqueValues <- nrow(logitRegResultsTable)
   uniqueValues <- unique(testingDF[, variableToPredict])
   
   logitRegCMList <- calcModelCM(logitRegResultsTable, 
                                 numUniqueValues, 
                                 'logisticRegression')
   
   avgLogitRegOutput <- calcAvgModelCM(logitRegCMList, 
                                       uniqueValues, 
                                       numUniqueValues)
   
   avgLogitRegResults <- avgLogitRegOutput[[RESULTS]]
   avgLogitRegCM <- avgLogitRegOutput[[CONFUSIONMATRIX]]

   logitRegTable <- concentrateModelResults(logitRegCMList, 
                                            avgLogitRegCM, 
                                            avgLogitRegResults, 
                                            uniqueValues, 
                                            numUniqueValues)
   
   #Prints out the Logistic Regression Model results
   if (printTF == TRUE) {
      printModelResults('Logistic Regression Model', 
                        variableToPredict, 
                        logitRegTable)
      
      # printAverageModelResults('Logistic Regression Model Average', 
      #                           variableToPredict, avgLogitRegCM, 
      #                           avgLogitRegResults[SENSITIVITY], 
      #                           avgLogitRegResults[SPECIFICITY], 
      #                           avgLogitRegResults[ACCURACY], 
      #                           avgLogitRegResults[BALANCEDACCURACY])
   }

   logitRegResults <- list('lgPred' = logitRegPred,
                           'lgTable' = logitRegTable)
   
   return(logitRegResults)
}

#K-Nearest Neighbors Model
createKNearestNeighborsModel <- function(trainTestDFsList, 
                                         variableToPredict, 
                                         printTF) {
   testingDF <- trainTestDFsList$testDF
   trainingDF <- trainTestDFsList$trainDF

   knnCtrl <- trainControl(method = "cv", number = 10)

   knnFormula <- as.formula(paste(variableToPredict, ' ~ .', sep = ''))
   
   knnFit <- train(knnFormula, 
                   data = trainingDF, 
                   method = "knn", 
                   trControl = knnCtrl, 
                   preProcess = c("center", "scale"), 
                   tuneGrid = expand.grid(k = 1:10))
   plot(knnFit)

   knnPred <- predict(knnFit, testingDF)
   knnActual <- testingDF[, variableToPredict]
   
   knnResultsTable <- table(knnPred, knnActual)
   
   numUniqueValues <- nrow(knnResultsTable)
   uniqueValues <- unique(testingDF[, variableToPredict])
   
   knnCMList <- calcModelCM(knnResultsTable, 
                            numUniqueValues, 
                            'kNearestNeighbors')
   
   avgKNNOutput <- calcAvgModelCM(knnCMList, 
                                  uniqueValues, 
                                  numUniqueValues)
   
   avgKNNResults <- avgKNNOutput[[RESULTS]]
   avgKNNCM <- avgKNNOutput[[CONFUSIONMATRIX]]

   knnTable <- concentrateModelResults(knnCMList, 
                                       avgKNNCM, 
                                       avgKNNResults, 
                                       uniqueValues, 
                                       numUniqueValues)
   
   if (printTF == TRUE) {
      printModelResults('K-Nearest Neighbor Model', 
                        variableToPredict, 
                        knnTable)
      
      # printAverageModelResults('K-Nearest Neighbor Model Average', 
      #                           variableToPredict, avgKNNCM, 
      #                           avgKNNResults[SENSITIVITY], 
      #                           avgKNNResults[SPECIFICITY], 
      #                           avgKNNResults[ACCURACY], 
      #                           avgKNNResults[BALANCEDACCURACY])
   }

   knnResults <- list('knnPred' = knnPred,
                      'knnTable' = knnTable)

   return(knnResults)
}

#Naive Bayes Classifier Model
createNaiveBayesClassifierModel <- function(trainTestDFsList, 
                                            variableToPredict, 
                                            printTF) {
   testingDF <- trainTestDFsList$testDF
   trainingDF <- trainTestDFsList$trainDF

   nBayesFormula <- as.formula(paste(variableToPredict, ' ~ .', sep = ''))
   
   nBayesFit <- naiveBayes(formula = nBayesFormula,
                           data = trainingDF)

   nBayesPred <- predict(nBayesFit, testingDF, type = "raw")
   nBayesPredClass <- predict(nBayesFit, testingDF, type = "class")
   nBayesActual <- testingDF[, variableToPredict]
   
   nBayesResultsTable <- table(nBayesPredClass, nBayesActual)
   
   numUniqueValues <- nrow(nBayesResultsTable)
   uniqueValues <- unique(testingDF[, variableToPredict])
   
   nBayesCMList <- calcModelCM(nBayesResultsTable, 
                               numUniqueValues, 
                               'naiveBayes')
   
   avgNBayesOutput <- calcAvgModelCM(nBayesCMList, 
                                     uniqueValues, 
                                     numUniqueValues)
   
   avgNBayesResults <- avgNBayesOutput[[RESULTS]]
   avgNBayesCM <- avgNBayesOutput[[CONFUSIONMATRIX]]

   nBayesTable <- concentrateModelResults(nBayesCMList, 
                                          avgNBayesCM, 
                                          avgNBayesResults, 
                                          uniqueValues, 
                                          numUniqueValues)
   
   #Prints out the Naive Bayes Classifier Model results
   if (printTF == TRUE) {
      printModelResults('Naive Bayes Classifier Model', 
                        variableToPredict, 
                        nBayesTable)
      
      # printAverageModelResults('Naive Bayes Classifier Model Average', 
      #                           variableToPredict, avgNBayesCM, 
      #                           avgNBayesResults[SENSITIVITY], 
      #                           avgNBayesResults[SPECIFICITY], 
      #                           avgNBayesResults[ACCURACY], 
      #                           avgNBayesResults[BALANCEDACCURACY])
   }

   nBayesResults <- list('nbPred' = nBayesPredClass,
                         'nbTable' = nBayesTable)

   return(nBayesResults)
}

#Ensemble Methods Model
createEnsembleMethodsModel <- function(dtR, lgR, knnR, nbR, bR,
                                       trainTestDFsList, 
                                       variableToPredict, 
                                       printTF) {

   testingDF <- trainTestDFsList$testDF

   ensembleDF <- subset(testingDF, select = c(variableToPredict))

   uniqueValues <- unique(testingDF[, variableToPredict])
   numUniqueValues <- length(uniqueValues)
   
   #For Debugging Purposes
   #print(uniqueValues[1])
   #print(uniqueValues[2])
   
   #Creates the voting matrix
   votesMatrix <- data.frame(matrix(0, nrow = numUniqueValues, ncol = 4))
   rownames(votesMatrix) <- uniqueValues
   colnames(votesMatrix) <- c("treePred",
                             "logisticRegPred",
                             "knnPred",
                             "nBayesPred")
   
   #Assembles all the predicted results from every model previously ran
   ensembleDF$treePred <- dtR$dtPred
   ensembleDF$logitRegPred <- lgR$lgPred
   ensembleDF$knnPred <- knnR$knnPred
   ensembleDF$nBayesPred <- nbR$nbPred
   ensembleDF$ensemblePred <- 'NA'
   
   modelBalAccs <- c()
   
   #Factors can cause issues and so therefore must be removed
   ensembleDF <- remove.factors(ensembleDF)
   
   if(ensembleDF$logitRegPred == 0) {
      ensembleDF$logitRegPred <- uniqueValues[1]
   } 
   else {
      ensembleDF$logitRegPred <- uniqueValues[2]
   }

   for (i in 1:nrow(ensembleDF)) {
      for (j in 1:4) {
         if (j == 1) {
            if (numUniqueValues > 2) {
               modelBalAccs <- append(modelBalAccs, dtR$dtTable["Balanced Accuracy", ensembleDF[i,j+1]])
            }
            else {
               modelBalAccs <- append(modelBalAccs, dtR$dtTable["Balanced Accuracy", "Average"])
            }
         }
         else if (j == 2) {
            if (numUniqueValues > 2) {
               modelBalAccs <- append(modelBalAccs, lgR$lgTable["Balanced Accuracy", "Average"])
            }
            else {
               modelBalAccs <- append(modelBalAccs, lgR$lgTable["Balanced Accuracy", "Average"])
            }
         }
         else if (j == 3) {
            if (numUniqueValues > 2) {
               modelBalAccs <- append(modelBalAccs, knnR$knnTable["Balanced Accuracy", ensembleDF[i,j+1]])
            }
            else {
               modelBalAccs <- append(modelBalAccs, knnR$knnTable["Balanced Accuracy", "Average"])
            }
         }
         else if (j == 4) {
            if (numUniqueValues > 2) {
               modelBalAccs <- append(modelBalAccs, nbR$nbTable["Balanced Accuracy", ensembleDF[i,j+1]])
            }
            else {
               modelBalAccs <- append(modelBalAccs, nbR$nbTable["Balanced Accuracy", "Average"])
            }
         }
         
         for (k in 1:nrow(votesMatrix)) {
            if (ensembleDF[i,j+1] == uniqueValues[k]) {
               votesMatrix[k,j] <- 1 
            }
         }
      }
      #Removes the least accurate model's vote to avoid ties or deadlocks
      minBalAccModel <- which.min(modelBalAccs)
      
      votesMatrix[,minBalAccModel] <- 0
      
      #For Debugging Purposes
      #print(modelBalAccs)
      #print(paste('Index of Least Accurate Voting Model: ', minBalAccModel))
      
      #Determines the winning vote
      votesMatrix$WeightedTotal <- rowSums(votesMatrix[,c(1:4)])
      
      if (max(votesMatrix$WeightedTotal) < 2) {
         votesMatrix$treePred <- votesMatrix$treePred * modelBalAccs[1]
         votesMatrix$logisticRegPred <- votesMatrix$logisticRegPred * modelBalAccs[2]
         votesMatrix$knnPred <- votesMatrix$knnPred * modelBalAccs[3]
         votesMatrix$nBayesPred <- votesMatrix$nBayesPred * modelBalAccs[4]
         
         votesMatrix$WeightedTotal <- rowSums(votesMatrix[,c(1:4)])
      }
      maxVoteValueIndex <- which.max(votesMatrix$WeightedTotal)
      maxVoteValueName <- rownames(votesMatrix[maxVoteValueIndex,])
      
      #For Debugging Purposes
      #print(votesMatrix)
      #print(maxVoteValueName)
      #cat("\n")
      
      ensembleDF[i,6] <- maxVoteValueName
      
      #Reset the votes matrix for the next row
      modelBalAccs <- c()
      votesMatrix[votesMatrix > 0] <- 0
   }

   ensemblePred <- ensembleDF$ensemblePred
   ensembleActual <- ensembleDF[, variableToPredict]
   ensembleResultsTable <- table(ensemblePred, ensembleActual)
   
   ensembleCMList <- calcModelCM(ensembleResultsTable, 
                                 numUniqueValues, 
                                 'kNearestNeighbors')
   
   avgEnsembleOutput <- calcAvgModelCM(ensembleCMList, 
                                       uniqueValues, 
                                       numUniqueValues)

   avgEnsembleResults <- avgEnsembleOutput[[RESULTS]]
   avgEnsembleCM <- avgEnsembleOutput[[CONFUSIONMATRIX]]

   #Collects the various model results into easily comparable tables
   eResultsAccuracy <- c(dtR$dtTable["Accuracy", "Average"], 
                         lgR$lgTable["Accuracy", "Average"], 
                         knnR$knnTable["Accuracy", "Average"],
                         nbR$nbTable["Accuracy", "Average"], 
                         avgEnsembleResults[ACCURACY], 
                         bR$bAcc)
   
   eResultsBalancedAccuracy <- c(dtR$dtTable["Balanced Accuracy", "Average"],
                                 lgR$lgTable["Balanced Accuracy", "Average"],
                                 knnR$knnTable["Balanced Accuracy", "Average"],
                                 nbR$nbTable["Balanced Accuracy", "Average"],
                                 avgEnsembleResults[BALANCEDACCURACY],
                                 bR$bBalAcc)
   
   eResultsOverall <- rbind(eResultsAccuracy, eResultsBalancedAccuracy)

   rownames(eResultsOverall) <- c("Accuracy",
                                  "Balanced Accuracy")
   
   colnames(eResultsOverall) <- c("Decision Tree",
                                  "Logistic Regression",
                                  "K-Nearest Neighbors",
                                  "Naive Bayes Classifier",
                                  "Ensemble",
                                  "Actual Results")

   #Row binds all TP, TN, FP & FN counts into one combined Confusion Matrix
   eResultsTP <- c(dtR$dtTable["True Positives", "Average"], 
                   lgR$lgTable["True Positives", "Average"], 
                   knnR$knnTable["True Positives", "Average"], 
                   nbR$nbTable["True Positives", "Average"], 
                   avgEnsembleCM[2,2], 
                   bR$bTP)
   
   eResultsTN <- c(dtR$dtTable["True Negatives", "Average"], 
                   lgR$lgTable["True Negatives", "Average"], 
                   knnR$knnTable["True Negatives", "Average"], 
                   nbR$nbTable["True Negatives", "Average"], 
                   avgEnsembleCM[1,1], 
                   bR$bTN)
   
   eResultsFP <- c(dtR$dtTable["False Positives", "Average"], 
                   lgR$lgTable["False Positives", "Average"], 
                   knnR$knnTable["False Positives", "Average"], 
                   nbR$nbTable["False Positives", "Average"], 
                   avgEnsembleCM[2,1], 
                   bR$bFP)
   
   eResultsFN <- c(dtR$dtTable["False Negatives", "Average"], 
                   lgR$lgTable["False Negatives", "Average"], 
                   knnR$knnTable["False Negatives", "Average"], 
                   nbR$nbTable["False Negatives", "Average"], 
                   avgEnsembleCM[1,2], 
                   bR$bFN)
   
   eResultsCM <- rbind(eResultsTP, eResultsTN, eResultsFP, eResultsFN)

   #Renames the rows and columns of the 'eResultsCM' accordingly
   rownames(eResultsCM) <- c("True Positives",
                             "True Negatives",
                             "False Positives",
                             "False Negatives")
   
   colnames(eResultsCM) <- c("Decision Tree",
                             "Logistic Regression",
                             "K-Nearest Neighbors",
                             "Naive Bayes Classifier",
                             "Ensemble",
                             "Known Actual Results")

   eResultsComplete <- rbind(eResultsOverall, eResultsCM)
   eResultsComplete <- as.data.frame.matrix(eResultsComplete)

   #Prints out the Ensemble Method results
   if (printTF == TRUE) {
      print('Model Results Comparison Table')
      cat('\n')
      print(eResultsComplete)
   }

   assign('eResultsComplete', eResultsComplete, envir = .GlobalEnv)
}

#Exports the modeling results as a '.csv' file
writeModelingResultsCSVFile <- function() {
   write.csv(eResultsComplete, "~/Documents/github/RDataModeler/R_Data_Modeler_Results.csv")
}

compareDFs <- function(testDF, trueDF) {
   equal <- isTRUE(all.equal(testDF, trueDF, check.attributes = FALSE))
   print(paste0("The dataframes are equal: ", equal))
   return(equal)
}

calcModelCM <- function(resultsTable, numUniqueValues, modelType) {
   #Builds the confusion matrices, otherwise known as contingency matrices of 
   #predicted vs actual results, and an empty vector in which to place them
   cm1 <- matrix(0, nrow = 2, ncol = 2)
   cm2 <- cm1
   cmList <- c()
   
   #If the number of unique variables is binary, then the results table and 
   #confusion matrix are identical to one another
   if (numUniqueValues == 2 | modelType == 'logisticRegression') {
      cm2[2,2] <- resultsTable[2,2]
      cm2[1,1] <- resultsTable[1,1]
      cm2[2,1] <- resultsTable[2,1]
      cm2[1,2] <- resultsTable[1,2]
      
      cmList[[1]] <- cm2
      
      cm2 <- cm1
   }
   else {
      for (i in 1:numUniqueValues) {
         for (j in 1:numUniqueValues) {
            if (i == j) {
               tps <- resultsTable[i,j]
               tns <- (sum(resultsTable) - 
                          (sum(resultsTable[i,]) - resultsTable[i,j]) - 
                          (sum(resultsTable[,j]) - resultsTable[i,j]) - 
                          resultsTable[i,j])
               fps <- sum(resultsTable[i,]) - resultsTable[i,j]
               fns <- sum(resultsTable[,j]) - resultsTable[i,j]
               
               cm2[2,2] <- cm2[2,2] + tps #True Positives
               cm2[1,1] <- cm2[1,1] + tns #True Negatives
               cm2[2,1] <- cm2[2,1] + fps #False Positives
               cm2[1,2] <- cm2[1,2] + fns #False Negatives
               
               cmList[[i]] <- cm2
               
               cm2 <- cm1
            }
         }
      }
   }
   return(cmList)
}

calcAvgModelCM <- function(cmList, uniqueValues, numUniqueValues) {
   avgResults <- replicate(4, 0)
   avgCM <- matrix(0, nrow = 2, ncol = 2)
   
   for (cm in cmList) {
      #Calculates the model results for each confusion matrix in the given list
      modelResults <- calcModelResults(cm[2,2], cm[1,1], 
                                       cm[2,1], cm[1,2])
      
      avgResults <- avgResults + modelResults
      avgCM <- avgCM + cm
   }
   
   if (numUniqueValues > 2) {
      avgResults <- avgResults / numUniqueValues
      avgCM <- avgCM / numUniqueValues
   }
   
   results <- c()
   results[[1]] <- avgResults
   results[[2]] <- avgCM

   return(results)
}

#Calculates the model's sensitivity, specificity, accuracy and balanced accuracy
calcModelResults <- function(tP, tN, fP, fN) {
   sensitivity <- (tP / (tP + fN))
   specificity <- (tN / (tN + fP))
   accuracy <- (tP + tN) / (tP + tN + fP + fN)
   balancedAccuracy <- ((sensitivity + specificity) / 2)
   
   results <- c(sensitivity, specificity, accuracy, balancedAccuracy)
   return(results)
}

concentrateModelResults <- function(cmList, avgCM, avgResults, 
                                    uniqueValues, numUniqueValues) {
   
   if (numUniqueValues == 2) {
      modelResultsTable <- matrix(0, nrow = 6, ncol = 1)
      colnames(modelResultsTable) <- "Average"
   }
   else {
      modelResultsTable <- matrix(0, nrow = 6, ncol = numUniqueValues)
      colnames(modelResultsTable) <- uniqueValues
   }
   
   i <- 1
   for (cm in cmList) {
      modelResults <- calcModelResults(cm[2,2], cm[1,1], 
                                       cm[2,1], cm[1,2])
      
      modelResultsTable[1,i] <- modelResults[ACCURACY]
      modelResultsTable[2,i] <- modelResults[BALANCEDACCURACY]
      modelResultsTable[3,i] <- cm[2,2]
      modelResultsTable[4,i] <- cm[1,1]
      modelResultsTable[5,i] <- cm[2,1]
      modelResultsTable[6,i] <- cm[1,2]
      
      i <- i + 1
   }
   
   if (numUniqueValues > 2) {
      avgColumn <- c(avgResults[ACCURACY], 
                     avgResults[BALANCEDACCURACY], 
                     avgCM[2,2], 
                     avgCM[1,1], 
                     avgCM[2,2], 
                     avgCM[1,2])
      
      modelResultsTable <- cbind(modelResultsTable, avgColumn)
      colnames(modelResultsTable)[i] <- "Average"
   }
   
   rownames(modelResultsTable) <- c("Accuracy", 
                                    "Balanced Accuracy", 
                                    "True Positives", 
                                    "True Negatives", 
                                    "False Positives", 
                                    "False Negatives")
   
   return(modelResultsTable)
}

printModelResults <- function(modelName, variable, modelResultsTable) {
   print(modelName)
   print(paste('Modeled Variable: ', variable))
   cat('\n')
   print(modelResultsTable)
   cat('\n')
}

#Prints out the results of the appropriately selected model along with margins
printAverageModelResults <- function(modelName, variable, confusionMatrix, 
                                     sensitivity, 
                                     specificity, 
                                     accuracy, 
                                     balancedAccuracy) {
   print(modelName)
   print(paste('Modeled Variable: ', variable))
   print('Confusion Matrix: ')
   cat('\n')
   print(addmargins(confusionMatrix))
   cat('\n')
   print(paste('Sensitivity: ', sensitivity))
   print(paste('Specificity: ', specificity))
   print(paste('Accuracy: ', accuracy))
   print(paste('Balanced Accuracy: ', balancedAccuracy))
   cat('\n')
}

#Verifies for acceptable arguments passed in by the user to prevent errors
getArguments <- function() {
   done <- FALSE
   while (done == FALSE) {
      startMessage <- "Enter 2 Arguments: The value of autoTest (TRUE | FALSE) and the .csv file's name: "
      if (interactive() == FALSE) {
         print(startMessage)
         arguments <- commandArgs(trailingOnly = TRUE)
         print(arguments)
      }
      else {
         argumentsString <- readline(prompt = startMessage)
         arguments <- unlist(strsplit(argumentsString, split = ' '))
      }
      
      numArguments <- length(arguments)
      arg1 <- arguments[1]
      arg2 <- arguments[2]
      
      if (numArguments >= 3) {
         print('There are too many arguments. Please try again.')
      }
      
      else if (numArguments == 0) {
         print('No arguments detected.')
         print('Defaulting to autoTest = TRUE.')
         print('Defaulting to csvFileName = Telco_Customer_Churn.csv.')
         
         arg1 <- TRUE
         arg2 <- 'NA'
         done <- TRUE
      }
      
      else if (numArguments == 1) {
         if (grepl('.csv', arg1)) {
            arg2 <- arg1
            arg1 <- FALSE
            
            print(paste('Detected following csvFileName: ', arg2))
            print('Defaulting to autoTest = FALSE.')
            
            done <- TRUE
         }
         else if (toupper(arg1) == TRUE | toupper(arg1) == FALSE) {
            print(paste('Detected following autoTest: ', arg1))
            
            arg2 <- 'NA'
            
            done <- TRUE
         }
         else {
            print('Invalid arguments detected. Please try again.')
         }
      }
      
      else if (((toupper(arg1) == TRUE | toupper(arg1) == FALSE) & 
                (toupper(arg2) == TRUE | toupper(arg2) == FALSE)) | 
               (grepl('.csv', arg1) == TRUE & grepl('.csv', arg2) == TRUE)) {
         print('Arguments have been repeated too many times. Please try again.')
      }
      
      else if (numArguments == 2) {
         if (grepl('.csv', arg1)) {
            arg2 <- arg1
            arg1 <- arguments[2]
         }
         done <- TRUE
      }
   }
   arguments[1] <- as.logical(toupper(arg1))
   arguments[2] <- arg2
   return(arguments)
}

readInputs <- function(promptMessage) {
   if (interactive() == TRUE) {
      inputs <- readline(prompt = promptMessage)
   }
   else {
      cat(promptMessage)
      inputs <- readLines("stdin", n = 1)
   }
   return(inputs)
}

main <- function() {
   arguments <- getArguments()
   autoTest <- arguments[1]
   csvFileName <- arguments[2]
   
   #For Debugging Purposes
   print(paste('Auto Test Value: ', autoTest))
   print(paste('CSV File Value: ', csvFileName))
   cat('\n')
   
   if (autoTest == TRUE) {
      baseDF <- createBaseDF('Telco_Customer_Churn.csv')
      
      predefinedToRemoveColumnsList <- c('customerID', 'TotalCharges', 
                                         'PaperlessBilling', 'PaymentMethod')
      
      adjustedDF <- adjustBaseDF(predefinedToRemoveColumnsList, baseDF)
      
      variableToPredict <- 'tenure'
   }
   
   else {
      if (csvFileName == 'NA') {
         if (interactive() == TRUE) {
            csvFileName <- readline(prompt = 'Type in the name of .csv file you would like to have read: ')
         }
         else {
            cat('Type in the name of .csv file you would like to have read: ')
            csvFileName <- readLines("stdin", n = 1)
         }
      }
      baseDF <- createBaseDF(csvFileName)
      adjustedDF <- adjustBaseDF(NULL, baseDF)
      variableToPredict <- selectVariableToPredict(adjustedDF)
   }

   factorizedDF <- factorizeDF(adjustedDF)
   
   uniqueValuesType <- typeof(factorizedDF[, variableToPredict])
   numUniqueValues <- n_distinct(factorizedDF[, variableToPredict])
   
   trainTestDFsList <- splitIntoTrainingTestingDFs(factorizedDF)

   printResults <- TRUE
   if (autoTest == FALSE) {
      printResults <- printTFQuestionaire()
   }
   
   if (numUniqueValues > 5 & (uniqueValuesType == 'integer' | uniqueValuesType == 'double')) {
      linRMR <- createLinearRegressionModel(trainTestDFsList, 
                                            variableToPredict, 
                                            printResults)
   }
   else if (numUniqueValues <= 5) {
      bR <- createBaseResults(trainTestDFsList, variableToPredict)
      
      dtMR <- createDecisionTreeModel(trainTestDFsList, 
                                      variableToPredict, 
                                      printResults)
      
      lgMR <- createLogisticRegressionModel(trainTestDFsList, 
                                            variableToPredict, 
                                            printResults)
      
      knnMR <- createKNearestNeighborsModel(trainTestDFsList, 
                                            variableToPredict, 
                                            printResults)
      
      nbMR <- createNaiveBayesClassifierModel(trainTestDFsList, 
                                              variableToPredict, 
                                              printResults)
      
      eMR <- createEnsembleMethodsModel(dtMR, lgMR, knnMR, nbMR, bR,
                                        trainTestDFsList, 
                                        variableToPredict, 
                                        printResults)
   }
   else {
      print('This variable cannot currently be modeled, please try again.')
   }
}

main()

#For manual debugging & testing purposes

 # baseDF <- createBaseDF('Telco_Customer_Churn.csv')
 # predefinedToRemoveColumnsList <- c('customerID', 'TotalCharges', 'PaperlessBilling', 'PaymentMethod')
 # predefinedVariableToPredict <- 'Churn'
 # adjustedDF <- adjustBaseDF(predefinedToRemoveColumnsList, baseDF)
 # variableToPredict <- selectVariableToPredict(adjustedDF)
 # factorizedDF <- factorizeDF(adjustedDF)
 # numUniqueValues <- n_distinct(factorizedDF[, variableToPredict])
 # trainTestDFsList <- splitIntoTrainingTestingDFs(factorizedDF)
 # bR <- createBaseResults(trainTestDFsList, variableToPredict)
 # linMR <- createLinearRegressionModel(trainTestDFsList, variableToPredict, TRUE)
 # dtMR <- createDecisionTreeModel(trainTestDFsList, variableToPredict, TRUE)
 # lgMR <- createLogisticRegressionModel(trainTestDFsList, variableToPredict, TRUE)
 # knnMR <- createKNearestNeighborsModel(trainTestDFsList, variableToPredict, TRUE)
 # nbMR <- createNaiveBayesClassifierModel(trainTestDFsList, variableToPredict, TRUE)
