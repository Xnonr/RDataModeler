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
#setwd("~/Documents/github/RDataModeler")

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
      
      columnToRemove <- readline(prompt = "Type in 'D' if done: ")
      
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
      
      columnToPredict <- readline(prompt = 'Type in the name of the column whose variable you wish predict: ')
      
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
printCMResultsQuestionaire <- function() {
   i = 0
   while (i != 1) {
      decision <- readline(prompt = "Would you like to print out the Model's Confusion Matrix & Results? (Y | N): ")

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

   uniqueValues <- unique(testingDF[, variableToPredict])
   uniqueValue1 <- as.character(uniqueValues[1])
   uniqueValue2 <- as.character(uniqueValues[2])

   baseTP <- sum(str_count(testingDF[, variableToPredict], uniqueValue1))
   baseTN <- sum(str_count(testingDF[, variableToPredict], uniqueValue2))
   baseFP <- 0
   baseFN <- 0

   baseSensitivity <- (baseTP / (baseTP + baseFN))
   baseSpecificity <- (baseTN / (baseTN + baseFP))
   baseAccuracy <- (baseTP + baseTN) / (baseTP + baseTN + baseFP + baseFN)
   baseBalancedAccuracy <- ((baseSensitivity + baseSpecificity) / 2)

   baseResults <- list('bSen' = baseSensitivity,
                       'bSpe' = baseSpecificity,
                       'bAcc' = baseAccuracy,
                       'bBalAcc' = baseBalancedAccuracy,
                       'bTP' = baseTP, 'bTN' = baseTN,
                       'bFP' = baseFP, 'bFN' = baseFN)

   return(baseResults)
}

#Decision Tree Model
createDecisionTreeModel <- function(trainTestDFsList, 
                                    variableToPredict, 
                                    printCMResults) {
   testingDF <- trainTestDFsList$testDF
   trainingDF <- trainTestDFsList$trainDF

   #Creates the formula using all other variables
   treeFormula <- paste(variableToPredict, '~ .')
   #Grows the Tree, attempting to be model the desired variable to predict
   treeFit <- rpart(formula = treeFormula,
                    data = trainingDF,
                    method = "class",
                    control = rpart.control(xval = 10, minsplit = 50),
                    parms = list(split = "gini"))

   #Creates the confusion matrix called 'treeCM'
   #Extracts the vector of predicted class for each observation in 'testingDF'
   treePred <- predict(treeFit, testingDF, type = "class")
   #Extracts the actual class of each observation in 'testingDF'
   treeActual <- testingDF[, variableToPredict]

   #Builds the confusion matrix 'treeCM', otherwise known as the contingency
   #matrix of predicted vs actual
   treeCM <- table(treePred, treeActual)

   #Extracts the True Positive (TP), True Negative (TN), False Positive (FP)
   #& False Negative (FN) values from the confusion matrix 'treeCM'
   treeTP <- treeCM[2,2]
   treeTN <- treeCM[1,1]
   treeFP <- treeCM[2,1]
   treeFN <- treeCM[1,2]

   #Calculates the tree model recall / sensitivity
   #(TPR = TP / P = TP / (TP + FN)) of the model
   treeSensitivity <- (treeTP / (treeTP + treeFN))

   #Calculates the tree model specificity
   #(TNR = TN / N = TN / (TN + FP)) of the model
   treeSpecificity <- (treeTN / (treeTN + treeFP))

   #Calculates the tree model accuracy of the model (A = (TP + TN) / S)
   treeAccuracy <- (treeTP + treeTN) / (treeTP + treeTN + treeFP + treeFN)

   #Calculates the tree model balanced accuracy of the model
   #(BA = (((TP / P) + (TN / N)) / 2))
   treeBalancedAccuracy <- ((treeSensitivity + treeSpecificity) / 2)

   #Plots & prints out the Decision Tree Model results
   #Plots the decision tree in a more aesthetically pleasing fashion
   rpart.plot(treeFit, type = 1,
              extra = 1,
              main = paste('Classification Tree Prediction for: ', 
                           variableToPredict))

   #Prints out the confusion matrix 'treeCM', using the predicted
   #before actual results, along with margins
   if (printCMResults == TRUE) {
      print("Confusion Matrix of the Decision Tree Model: ")
      print(addmargins(treeCM))

      print(paste("Sensitivity of the Decision Tree Model: ", treeSensitivity))
      print(paste("Specificity of the Decision Tree Model: ", treeSpecificity))
      print(paste("Accuracy of the Decision Tree Model: ", treeAccuracy))
      print(paste("Balanced Accuracy of the Decision Tree Model: ",
                  treeBalancedAccuracy))
   }

   decisionTreeResults <- list('dtPred' = treePred,
                               'dtSen' = treeSensitivity,
                               'dtSpe' = treeSpecificity,
                               'dtAcc' = treeAccuracy,
                               'dtBalAcc' = treeBalancedAccuracy,
                               'dtTP' = treeTP, 'dtTN' = treeTN,
                               'dtFP' = treeFP, 'dtFN' = treeFN)

   return(decisionTreeResults)
}

#Logistic Regression Model
createLogisticRegressionModel <- function(trainTestDFsList, 
                                          variableToPredict, 
                                          printCMResults) {
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
   logitRegCM <- table(logitRegPred, logitRegActual)

   testingDF <- testingDF[, !(names(testingDF) %in% c("logitRegPred",
                                                      "logitRegVariablePred"))]

   logitRegTP <- logitRegCM[2,2]
   logitRegTN <- logitRegCM[1,1]
   logitRegFP <- logitRegCM[2,1]
   logitRegFN <- logitRegCM[1,2]

   logitRegSensitivity <- (logitRegTP / (logitRegTP + logitRegFN))
   logitRegSpecificity <- (logitRegTN / (logitRegTN + logitRegFP))
   logitRegAccuracy <- ((logitRegTP + logitRegTN) /
                        (logitRegTP + logitRegTN + logitRegFP + logitRegFN))
   logitRegBalancedAccuracy <- ((logitRegSensitivity + logitRegSpecificity) / 2)

   #Prints out the Logistic Regression Model results
   if (printCMResults == TRUE) {
      print("Confusion Matrix of the Logistic Regression Model: ")
      print(addmargins(logitRegCM))

      print(paste("Sensitivity of the Logistic Regression Model: ",
                  logitRegSensitivity))
      print(paste("Specificity of the Logistic Regression Model: ",
                  logitRegSpecificity))
      print(paste("Accuracy of the Logistic Regression Model: ",
                  logitRegAccuracy))
      print(paste("Balanced Accuracy of the Logistic Regression Model: ",
                  logitRegBalancedAccuracy))
   }

   logitRegResults <- list('lgPred' = logitRegPred,
                           'lgSen' = logitRegSensitivity,
                           'lgSpe' = logitRegSpecificity,
                           'lgAcc' = logitRegAccuracy,
                           'lgBalAcc' = logitRegBalancedAccuracy,
                           'lgTP' = logitRegTP, 'lgTN' = logitRegTN,
                           'lgFP' = logitRegFP, 'lgFN' = logitRegFN)

   return(logitRegResults)
}

#K-Nearest Neighbors Model
createKNearestNeighborsModel <- function(trainTestDFsList, 
                                         variableToPredict, 
                                         printCMResults) {
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
   
   #Explicitly selecting a specific column directly versus indirectly via a 
   #variable results in very minor variations in final output predictive results
   #knnActual <- testingDF$Churn
   
   knnCM <- table(knnPred, knnActual)

   knnTP <- knnCM[2,2]
   knnTN <- knnCM[1,1]
   knnFP <- knnCM[2,1]
   knnFN <- knnCM[1,2]

   knnSensitivity <- (knnTP / (knnTP + knnFN))
   knnSpecificity <- (knnTN / (knnTN + knnFP))
   knnAccuracy <- (knnTP + knnTN) / (knnTP + knnTN + knnFP + knnFN)
   knnBalancedAccuracy <- ((knnSensitivity + knnSpecificity) / 2)

   #Prints out the K-Nearest Neighbor Model results
   if (printCMResults == TRUE) {
      print("Confusion Matrix of the K-Nearest Neighbor Model: ")
      print(addmargins(knnCM))

      print(paste("Sensitivity of the K-Nearest Neighbor Model: ",
                  knnSensitivity))
      print(paste("Specificity of the K-Nearest Neighbor Model: ",
                  knnSpecificity))
      print(paste("Accuracy of the K-Nearest Neighbor Model: ", knnAccuracy))
      print(paste("Balanced Accuracy of the K-Nearest Neighbor Model: ",
                  knnBalancedAccuracy))
   }

   knnResults <- list('knnPred' = knnPred,
                      'knnSen' = knnSensitivity,
                      'knnSpe' = knnSpecificity,
                      'knnAcc' = knnAccuracy,
                      'knnBalAcc' = knnBalancedAccuracy,
                      'knnTP' = knnTP, 'knnTN' = knnTN,
                      'knnFP' = knnFP, 'knnFN' = knnFN)

   return(knnResults)
}

#Naive Bayes Classifier Model
createNaiveBayesClassifierModel <- function(trainTestDFsList, 
                                            variableToPredict, 
                                            printCMResults) {
   testingDF <- trainTestDFsList$testDF
   trainingDF <- trainTestDFsList$trainDF

   nBayesFormula <- as.formula(paste(variableToPredict, ' ~ .', sep = ''))
   
   nBayesFit <- naiveBayes(formula = nBayesFormula,
                           data = trainingDF)

   nBayesPred <- predict(nBayesFit, testingDF, type = "raw")
   nBayesPredClass <- predict(nBayesFit, testingDF, type = "class")
   nBayesActual <- testingDF[, variableToPredict]
   nBayesCM <- table(nBayesPredClass, nBayesActual)

   nBayesTP <- nBayesCM[2,2]
   nBayesTN <- nBayesCM[1,1]
   nBayesFP <- nBayesCM[2,1]
   nBayesFN <- nBayesCM[1,2]

   nBayesSensitivity <- (nBayesTP / (nBayesTP + nBayesFN))
   nBayesSpecificity <- (nBayesTN / (nBayesTN + nBayesFP))
   nBayesAccuracy <- ((nBayesTP + nBayesTN) /
                      (nBayesTP + nBayesTN + nBayesFP + nBayesFN))
   nBayesBalancedAccuracy <- ((nBayesSensitivity + nBayesSpecificity) / 2)

   #Prints out the Naive Bayes Classifier Model results
   if (printCMResults == TRUE) {
      print("Confusion Matrix of the Naive Bayes Classifier Model: ")
      print(addmargins(nBayesCM))

      print(paste("Sensitivity of the Naive Bayes Classifier Model: ",
                  nBayesSensitivity))
      print(paste("Specificity of the Naive Bayes Classifier Model: ",
                  nBayesSpecificity))
      print(paste("Accuracy of the Naive Bayes Classifier Model: ",
                  nBayesAccuracy))
      print(paste("Balanced Accuracy of the Naive Bayes Classifier Model: ",
                  nBayesBalancedAccuracy))
   }

   nBayesResults <- list('nbPred' = nBayesPredClass,
                         'nbSen' = nBayesSensitivity,
                         'nbSpe' = nBayesSpecificity,
                         'nbAcc' = nBayesAccuracy,
                         'nbBalAcc' = nBayesBalancedAccuracy,
                         'nbTP' = nBayesTP, 'nbTN' = nBayesTN,
                         'nbFP' = nBayesFP, 'nbFN' = nBayesFN)

   return(nBayesResults)
}

#Ensemble Methods Model
createEnsembleMethodsModel <- function(dtR, lgR, knnR, nbR, bR,
                                       trainTestDFsList, printCMResults) {

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
   treeLogReg <- dtR$dtAcc + lgR$lgAcc #1100
   treeKnn <- dtR$dtAcc  + knnR$knnAcc #1010
   treeNBayes <- dtR$dtAcc  + nbR$nbAcc #1001
   logRegKnn <- lgR$lgAcc + knnR$knnAcc #0110
   logRegNBayes <- lgR$lgAcc + nbR$nbAcc #0101
   knnNBayes <- knnR$knnAcc + nbR$nbAcc #0011

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
                                  "Actual Churn Results")

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
                             "Actual Churn Results")

   eResultsComplete <- rbind(eResultsOverall, eResultsCM)
   eResultsComplete <- as.data.frame.matrix(eResultsComplete)

   if (printCMResults == TRUE) {
      #Prints out the Ensemble Method results
      print("Model Results Comparison Tables: ")
      #print(eResultsCM)
      #print(eResultsOverall)
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

main <- function(autoTest) {
   if (autoTest) {
      baseDF <- createBaseDF('Telco_Customer_Churn.csv')
      
      predefinedToRemoveColumnsList <- c('customerID', 'TotalCharges', 
                                         'PaperlessBilling', 'PaymentMethod')
      
      adjustedDF <- adjustBaseDF(predefinedToRemoveColumnsList, baseDF)
      
      variableToPredict <- 'Churn'
   }
   
   else {
      csvFileName <- readline(prompt = 'Type in the name of .csv file you would like to have read: ')
      baseDF <- createBaseDF(csvFileName)
      adjustedDF <- adjustBaseDF(NULL, baseDF)
      variableToPredict <- selectVariableToPredict(adjustedDF)
   }

   factorizedDF <- factorizeDF(adjustedDF)
   trainTestDFsList <- splitIntoTrainingTestingDFs(factorizedDF)

   printResults = TRUE
   if (!autoTest) {
      printResults <- printCMResultsQuestionaire()
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

main(TRUE)

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
