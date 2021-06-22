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
   
   baseCMList <- calcModelCM(baseResultsTable, numUniqueValues)
   
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
   
   treeCMList <- calcModelCM(treeResultsTable, numUniqueValues)
      
   avgTreeOutput <- calcAvgModelCM(treeCMList, 
                                   uniqueValues, 
                                   numUniqueValues)
   
   avgTreeResults <- avgTreeOutput[[RESULTS]]
   avgTreeCM <- avgTreeOutput[[CONFUSIONMATRIX]]
   
   if (printTF == TRUE) {
      printModelResults('Decision Tree Model Average', 
                        variableToPredict, avgTreeCM, 
                        avgTreeResults[SENSITIVITY], 
                        avgTreeResults[SPECIFICITY], 
                        avgTreeResults[ACCURACY], 
                        avgTreeResults[BALANCEDACCURACY])
   }
   
   decisionTreeResults <- list('dtPred' = treePred,
                               'dtSen' = avgTreeResults[SENSITIVITY],
                               'dtSpe' = avgTreeResults[SPECIFICITY],
                               'dtAcc' = avgTreeResults[ACCURACY],
                               'dtBalAcc' = avgTreeResults[BALANCEDACCURACY],
                               'dtTP' = avgTreeCM[2,2], 
                               'dtTN' = avgTreeCM[1,1],
                               'dtFP' = avgTreeCM[2,1], 
                               'dtFN' = avgTreeCM[1,2])
   
   return(decisionTreeResults)
}

#Logistic Regression Model
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
   
   logitRegCMList <- calcModelCM(logitRegResultsTable, numUniqueValues)
   
   avgLogitRegOutput <- calcAvgModelCM(logitRegCMList, 
                                       uniqueValues, 
                                       numUniqueValues)
   
   avgLogitRegResults <- avgLogitRegOutput[[RESULTS]]
   avgLogitRegCM <- avgLogitRegOutput[[CONFUSIONMATRIX]]

   #Prints out the Logistic Regression Model results
   if (printTF == TRUE) {
      printModelResults('Logistic Regression Model Average', 
                        variableToPredict, avgLogitRegCM, 
                        avgLogitRegResults[SENSITIVITY], 
                        avgLogitRegResults[SPECIFICITY], 
                        avgLogitRegResults[ACCURACY], 
                        avgLogitRegResults[BALANCEDACCURACY])
   }

   logitRegResults <- list('lgPred' = logitRegPred,
                           'lgSen' = avgLogitRegResults[SENSITIVITY],
                           'lgSpe' = avgLogitRegResults[SPECIFICITY],
                           'lgAcc' = avgLogitRegResults[ACCURACY],
                           'lgBalAcc' = avgLogitRegResults[BALANCEDACCURACY],
                           'lgTP' = avgLogitRegCM[2,2], 
                           'lgTN' = avgLogitRegCM[1,1],
                           'lgFP' = avgLogitRegCM[2,1], 
                           'lgFN' = avgLogitRegCM[1,2])
   
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
   
   knnCMList <- calcModelCM(knnResultsTable, numUniqueValues)
   
   avgKNNOutput <- calcAvgModelCM(knnCMList, 
                                  uniqueValues, 
                                  numUniqueValues)
   
   avgKNNResults <- avgKNNOutput[[RESULTS]]
   avgKNNCM <- avgKNNOutput[[CONFUSIONMATRIX]]

   if (printTF == TRUE) {
      printModelResults('K-Nearest Neighbor Model Average', 
                        variableToPredict, avgKNNCM, 
                        avgKNNResults[SENSITIVITY], 
                        avgKNNResults[SPECIFICITY], 
                        avgKNNResults[ACCURACY], 
                        avgKNNResults[BALANCEDACCURACY])
   }

   knnResults <- list('knnPred' = knnPred,
                      'knnSen' = avgKNNResults[SENSITIVITY],
                      'knnSpe' = avgKNNResults[SPECIFICITY],
                      'knnAcc' = avgKNNResults[ACCURACY],
                      'knnBalAcc' = avgKNNResults[BALANCEDACCURACY],
                      'knnTP' = avgKNNCM[2,2], 
                      'knnTN' = avgKNNCM[1,1],
                      'knnFP' = avgKNNCM[2,1], 
                      'knnFN' = avgKNNCM[1,2])

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
   
   nBayesCMList <- calcModelCM(nBayesResultsTable, numUniqueValues)
   
   avgNBayesOutput <- calcAvgModelCM(nBayesCMList, 
                                     uniqueValues, 
                                     numUniqueValues)
   
   avgNBayesResults <- avgNBayesOutput[[RESULTS]]
   avgNBayesCM <- avgNBayesOutput[[CONFUSIONMATRIX]]

   #Prints out the Naive Bayes Classifier Model results
   if (printTF == TRUE) {
      printModelResults('Naive Bayes Classifier Model Average', 
                        variableToPredict, avgNBayesCM, 
                        avgNBayesResults[SENSITIVITY], 
                        avgNBayesResults[SPECIFICITY], 
                        avgNBayesResults[ACCURACY], 
                        avgNBayesResults[BALANCEDACCURACY])
   }

   nBayesResults <- list('nbPred' = nBayesPredClass,
                         'nbSen' = avgNBayesResults[SENSITIVITY],
                         'nbSpe' = avgNBayesResults[SPECIFICITY],
                         'nbAcc' = avgNBayesResults[ACCURACY],
                         'nbBalAcc' = avgNBayesResults[BALANCEDACCURACY],
                         'nbTP' = avgNBayesCM[2,2], 
                         'nbTN' = avgNBayesCM[1,1],
                         'nbFP' = avgNBayesCM[2,1], 
                         'nbFN' = avgNBayesCM[1,2])

   return(nBayesResults)
}

#Ensemble Methods Model
createEnsembleMethodsModel <- function(dtR, lgR, knnR, nbR, bR,
                                       trainTestDFsList, printTF) {

   testingDF <- trainTestDFsList$testDF

   ensembleDF <- subset(testingDF, select = c("Churn"))

   #Assembles all the predicted Churn results from every model previously ran
   ensembleDF$treeChurnPred <- dtR$dtPred
   ensembleDF$logitChurnRegPred <- lgR$lgPred
   ensembleDF$knnChurnPred <- knnR$knnPred
   ensembleDF$nBayesChurnPred <- nbR$nbPred

   #Factors become 'NAN' values when setting them to binary values; must be removed
   ensembleDF <- remove.factors(ensembleDF)

   ensembleDF$Churn[ensembleDF$Churn == "No"] <- 0
   ensembleDF$Churn[ensembleDF$Churn == "Yes"] <- 1

   ensembleDF$treeChurnPred[ensembleDF$treeChurnPred == "No"] <- 0
   ensembleDF$treeChurnPred[ensembleDF$treeChurnPred == "Yes"] <- 1

   ensembleDF$knnChurnPred[ensembleDF$knnChurnPred == "No"] <- 0
   ensembleDF$knnChurnPred[ensembleDF$knnChurnPred == "Yes"] <- 1

   ensembleDF$nBayesChurnPred[ensembleDF$nBayesChurnPred == "No"] <- 0
   ensembleDF$nBayesChurnPred[ensembleDF$nBayesChurnPred == "Yes"] <- 1

   #Ensures no other problems by transforming all columns to numeric data type
   ensembleDF$Churn <- as.numeric(ensembleDF$Churn)
   ensembleDF$treeChurnPred <- as.numeric(ensembleDF$treeChurnPred)
   ensembleDF$knnChurnPred <- as.numeric(ensembleDF$knnChurnPred)
   ensembleDF$nBayesChurnPred <- as.numeric(ensembleDF$nBayesChurnPred)

   #Calculates 2 Vote Combinations' Summed Accuracy
   treeLogReg <- dtR$dtBalAcc + lgR$lgBalAcc #1100
   treeKnn <- dtR$dtBalAcc  + knnR$knnBalAcc #1010
   treeNBayes <- dtR$dtBalAcc  + nbR$nbBalAcc #1001
   logRegKnn <- lgR$lgBalAcc + knnR$knnBalAcc #0110
   logRegNBayes <- lgR$lgBalAcc + nbR$nbBalAcc #0101
   knnNBayes <- knnR$knnBalAcc + nbR$nbBalAcc #0011

   #Calculates which 2 vote combinations trump their opposites
   treeLogRegVSknnNBayes <- treeLogReg - knnNBayes #1100 vs 0011 #1100 Wins
   treeKnnVSlogRegNBayes <- treeKnn - logRegNBayes #1010 vs 0101 #1010 Wins
   treeNBayesVSlogRegKnn <- treeNBayes - logRegKnn #1001 vs 0110 #0110 Wins

   ensembleDF$ensembleChurnPred <- ifelse(ensembleDF$treeChurnPred
                                          + ensembleDF$logitChurnRegPred == 2,
                                          1, 0)

   ensembleDF$ensembleChurnPred <- ifelse(ensembleDF$treeChurnPred
                                          + ensembleDF$knnChurnPred == 2,
                                          1, 0)

   ensembleDF$ensembleChurnPred <- ifelse(ensembleDF$treeChurnPred
                                          + ensembleDF$nBayesChurnPred == 2,
                                          0, 1)

   ensembleDF$ensembleChurnPred <- ifelse(ensembleDF$treeChurnPred
                                          + ensembleDF$logitChurnRegPred
                                          + ensembleDF$knnChurnPred
                                          + ensembleDF$nBayesChurnPred >= 3,
                                          1, 0)


   ensemblePred <- ensembleDF$ensembleChurnPred
   ensembleActual <- ensembleDF$Churn
   ensembleCM <- table(ensemblePred, ensembleActual)

   ensembleTP <- ensembleCM[2,2]
   ensembleTN <- ensembleCM[1,1]
   ensembleFP <- ensembleCM[2,1]
   ensembleFN <- ensembleCM[1,2]

   ensembleSensitivity <- (ensembleTP / (ensembleTP + ensembleFN))
   ensembleSpecificity <- (ensembleTN / (ensembleTN + ensembleFP))
   ensembleAccuracy <- ((ensembleTP + ensembleTN) /
                        (ensembleTP + ensembleTN + ensembleFP + ensembleFN))
   ensembleBalancedAccuracy <- ((ensembleSensitivity + ensembleSpecificity) / 2)

   #Collects the various model results into easily comparable tables
   eResultsAccuracy <- c(dtR$dtAcc, lgR$lgAcc, knnR$knnAcc,
                         nbR$nbAcc, ensembleAccuracy, bR$bAcc)
   eResultsBalancedAccuracy <- c(dtR$dtBalAcc,
                                 lgR$lgBalAcc,
                                 knnR$knnBalAcc,
                                 nbR$nbBalAcc,
                                 ensembleBalancedAccuracy,
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
   eResultsTP <- c(dtR$dtTP, lgR$lgTP, knnR$knnTP, nbR$nbTP, ensembleTP, bR$bTP)
   eResultsTN <- c(dtR$dtTN, lgR$lgTN, knnR$knnTN, nbR$nbTN, ensembleTN, bR$bTN)
   eResultsFP <- c(dtR$dtFP, lgR$lgFP, knnR$knnFP, nbR$nbFP, ensembleFP, bR$bFP)
   eResultsFN <- c(dtR$dtFN, lgR$lgFN, knnR$knnFN, nbR$nbFN, ensembleFN, bR$bFN)
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

#Exports the churn modeling results as a '.csv' file
writeModelingResultsCSVFile <- function() {
   write.csv(eResultsComplete, "~/Documents/github/RDataModeler/R_Data_Modeler_Results.csv")
}

compareDFs <- function(testDF, trueDF) {
   equal <- isTRUE(all.equal(testDF, trueDF, check.attributes = FALSE))
   print(paste0("The dataframes are equal: ", equal))
   return(equal)
}

calcModelCM <- function(resultsTable, numUniqueValues) {
   #Builds the confusion matrices, otherwise known as contingency matrices of 
   #predicted vs actual results, and an empty vector in which to place them
   cm1 <- matrix(0, nrow = 2, ncol = 2)
   cm2 <- cm1
   cmList <- c()
   
   #If the number of unique variables is binary, then the results table and 
   #confusion matrix are identical to one another
   if (numUniqueValues == 2) {
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

#Prints out the results of the appropriately selected model along with margins
printModelResults <- function(modelName, variable, confusionMatrix, sensitivity, 
                              specificity, accuracy, balancedAccuracy) {
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
   print(autoTest)
   csvFileName <- arguments[2]
   print(csvFileName)
   
   if (autoTest == TRUE) {
      baseDF <- createBaseDF('Telco_Customer_Churn.csv')
      
      predefinedToRemoveColumnsList <- c('customerID', 'TotalCharges', 
                                         'PaperlessBilling', 'PaymentMethod')
      
      adjustedDF <- adjustBaseDF(predefinedToRemoveColumnsList, baseDF)
      
      variableToPredict <- 'Churn'
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
   trainTestDFsList <- splitIntoTrainingTestingDFs(factorizedDF)

   printResults <- TRUE
   if (autoTest == FALSE) {
      printResults <- printTFQuestionaire()
   }

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
                                     trainTestDFsList, printResults)
}

main()

#For manual debugging & testing purposes

# baseDF <- createBaseDF('Telco_Customer_Churn.csv')
# predefinedToRemoveColumnsList <- c('customerID', 'TotalCharges', 'PaperlessBilling', 'PaymentMethod')
# predefinedVariableToPredict <- 'Churn'
# adjustedDF <- adjustBaseDF(predefinedToRemoveColumnsList, baseDF)
# variableToPredict <- selectVariableToPredict(adjustedDF)
# factorizedDF <- factorizeDF(adjustedDF)
# trainTestDFsList <- splitIntoTrainingTestingDFs(factorizedDF)
# bR <- createBaseResults(trainTestDFsList, variableToPredict)
# dtMR <- createDecisionTreeModel(trainTestDFsList, variableToPredict, TRUE)
# lgMR <- createLogisticRegressionModel(trainTestDFsList, variableToPredict, TRUE)
# knnMR <- createKNearestNeighborsModel(trainTestDFsList, variableToPredict, TRUE)
# nbMR <- createNaiveBayesClassifierModel(trainTestDFsList, variableToPredict, TRUE)
