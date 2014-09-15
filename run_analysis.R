
## ----download------------------------------------------------------------
data_url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
zip_file <- 'UCI HAR Dataset.zip'
zip_subdir <- "UCI HAR Dataset"
if (!file.exists(zip_subdir)) {
  if (!file.exists(zip_file)) {
    download.file(data_url, zip_file, method='curl') 
  }
  unzip(zip_file)
}


## ----metadata------------------------------------------------------------
library(stringr)

# Commun function for loading the activity and features files that describe the activity names and the columns in the data sets
load_meatdata_1 <- function(file_name) {
  
  # load file
  meta <- read.table(file_name,stringsAsFactors=FALSE)
  # check for just two columns
  if (ncol(meta) != 2) stop("Failed: Expected only two columns in a metadata table")
  # check that we have a simple data frame with row number in first column
  if (!identical(meta[,1], 1:nrow(meta))) stop("Unexpected format metadata in table.")
  # Extract the character lables as a simple vector from the second column
  meta <- meta[,2]
  
  if (length(unique(meta)) == length(meta)) {
    # Build into a factor with sympathetic numbering
    meta <- factor(meta, levels=meta)
    # Check that we got that right!
    if (!identical(as.integer(meta), 1:length(meta))) {
      stop("Internal Error: Assigned factor levels incorrectly.")
    }
  }
  meta  
}

# Load both shared meta-data files (activity names and column / feature names)
load_metadata <- function() {
  activities = load_meatdata_1('activity_labels.txt')
  features = load_meatdata_1('features.txt')
  # Point 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
  cols_to_keep = str_detect(features, fixed("-mean(")) | str_detect(features, fixed("-std("))
  col_names_to_keep = features[cols_to_keep]
  list( activities=activities
        , features=features
        , cols_to_keep=cols_to_keep
        , col_names_to_keep=col_names_to_keep
        , subjects = factor(1:30))
}


## ----data_load-----------------------------------------------------------

# Load data from test or train.
# Pass in metadata from "load_metadata" to have propper comun names and activities as factors
# Allow an 'nrows' for sub-setting and making for quicker debugging
load_data <- function(test_or_train,meta,nrows=-1) {
  
  setwd(test_or_train)
  # Get subjects as simple vector
  subject <- read.table(paste0('subject_',test_or_train,'.txt'),stringsAsFactors=FALSE, nrows=nrows)[,1]
  # Ditto for position numbers
  y <- read.table(paste0('y_',test_or_train,'.txt'), stringsAsFactors=FALSE, nrows=nrows)[,1]
  # Read in main dataset, passing in explicit column names from our metadata
  X <- read.table(paste0('X_',test_or_train,'.txt'), stringsAsFactors=FALSE
                  # Point 3: Uses descriptive activity names to name the activities in the data set
                  , col.names = meta$features
                  , nrows=nrows)
  setwd('..')

  # Point 4: Appropriately labels the data set with descriptive variable names
  result <- cbind( 
    subject = meta$subjects[subject] # Ensures we have consistent subject factors
    , activity = meta$activities[y]
    , is_training_data = test_or_train == 'test'
    , X[,meta$cols_to_keep]
  )

  # Tidy up
  rm(y,X,subject)
  
  # Return result
  result
}


## ----load_all------------------------------------------------------------

setwd("UCI HAR Dataset")

meta <- load_metadata()
test <- load_data('test',meta)
train <- load_data('train',meta)
# Point 1: Merges the training and the test sets to create one data set.
combined <- rbind(test,train)
rm(test,train)

setwd("..")


## ----averages------------------------------------------------------------
require(reshape2)
averages <- recast(combined[,-3] # need to avoid my "is_training_data" column
                   , subject + activity ~ ..., mean)
write.table(averages, "averages.txt", row.name=FALSE)


## ----copybook------------------------------------------------------------
meta$col_names_to_keep


