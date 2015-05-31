#' Cross validation functionality for use with h2o
#' Creates the folds and gets the parameter grid to optimise over for:
#'   - LR: forward variable selection 
#'   - GBM: 
#'   - RF
#' 
#' 



# library checks ----------------------------------------------------------
if (!("caret" %in% rownames(installed.packages()))){
  error("The caret package is not installed.  Please install before proceeding.")
}

if (!("h2o" %in% rownames(installed.packages()))){
  error("The h2o package is not installed.  Please see http://0xdata.com/download/ for more information.")
}



# Function to get subsets of data -----------------------------------------
#' function to create test/training datasets and generate folds
#' uses caret functions to create the datasets
createDataSplits <- function(outputVec, seedVal = 2048, p = 0.8, nFolds = 10){
  
  if (!is.na(seedVal)){
    warning("Setting seed value")
    set.seed(seedVal)
  }
  
  trainIdx <- caret::createDataPartition(y = outputVec, 
                                         p = p, 
                                         list = FALSE)
  
  # create folds with the training set 
  foldsIdx <- createFolds(outputVec[trainIdx], 
                          k = nFolds, 
                          list = TRUE)
  
  # create the output dataframe
  isTrain <- rep(FALSE, length(outputVec))
  isTrain[trainIdx] <- TRUE
  
  foldDef <- rep(NA, length(outputVec))
  for (iF in 1:nFolds){
    tmpIdx <- trainIdx[foldsIdx[[iF]]]  # map the folds index back to the original dataset
    foldDef[tmpIdx] <- iF
  }
  output <- data.frame(isTrain, foldDef)
  
  return(output)
  
}



# test create data splits -------------------------------------------------
#' 
#' max(foldDef) = 
testCreateDataSplits <- function(dataSplitsDf, p = 0.8, nFolds = 10){
  
  # test 1
  # number of isTrain = FALSE should be equal to number of NA's in foldDef
  test1 <- sum(!dataSplitsDf$isTrain) == sum(is.na(dataSplitsDf$foldDef))  # should be true
  if (test1 == FALSE){
    error("Problem with dataSplits.  Train and cv indicis mismatching.")
  }
  
  # test 2:
  # check number of folds
  test2 <- max(dataSplitsDf$foldDef, na.rm = TRUE) == nFolds
  if (test2 == FALSE){
    error("Problem with dataSplits.  More folds than specified.")
  }
  
  # test 3:
  # check length of each fold is about p (after rounding)
  test3 <- all(round(table(dataSplits$foldDef)/(nrow(dataSplitsDf)/nFolds),1) == p)
  if (test3 == FALSE){
    warning("Check fold lengths.  They might be issues.")
  }
  
}



