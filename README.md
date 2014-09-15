Project Brief
=============

This is a Project within the ["Getting and Cleaning
Data"](https://class.coursera.org/getdata-007) course within
[Coursera](https://www.coursera.org/).

The objective is to clean and represent a data set regarding sensor
measurments collected from Galaxy S smart-phones. A full description is
available at the site where the data was sourced:
<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>
The data set being cleaned was actually sourced from
<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>
as per the request of the course assignment.

The brief is:

> You should create one R script called run\_analysis.R that does the
> following.
>
> 1.  Merges the training and the test sets to create one data set.
> 2.  Extracts only the measurements on the mean and standard deviation
>     for each measurement.
> 3.  Uses descriptive activity names to name the activities in the data
>     set
> 4.  Appropriately labels the data set with descriptive variable names.
> 5.  From the data set in step 4, creates a second, independent tidy
>     data set with the average of each variable for each activity and
>     each subject.
>
> Good luck!

Downloading and examining data
==============================

    data_url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
    zip_file <- 'UCI HAR Dataset.zip'
    zip_subdir <- "UCI HAR Dataset"
    if (!file.exists(zip_subdir)) {
      if (!file.exists(zip_file)) {
        download.file(data_url, zip_file, method='curl') 
      }
      unzip(zip_file)
    }

The zip file unpacks to the following set of files:

    .
    +-- activity_labels.txt
    +-- features.txt
    +-- features_info.txt
    +-- README.txt
    +-- test
    ¦   +-- Inertial Signals
    ¦   ¦   +-- body_acc_x_test.txt
    ¦   ¦   +-- body_acc_y_test.txt
    ¦   ¦   +-- body_acc_z_test.txt
    ¦   ¦   +-- body_gyro_x_test.txt
    ¦   ¦   +-- body_gyro_y_test.txt
    ¦   ¦   +-- body_gyro_z_test.txt
    ¦   ¦   +-- total_acc_x_test.txt
    ¦   ¦   +-- total_acc_y_test.txt
    ¦   ¦   +-- total_acc_z_test.txt
    ¦   +-- subject_test.txt
    ¦   +-- X_test.txt
    ¦   +-- y_test.txt
    +-- train
        +-- Inertial Signals
        ¦   +-- body_acc_x_train.txt
        ¦   +-- body_acc_y_train.txt
        ¦   +-- body_acc_z_train.txt
        ¦   +-- body_gyro_x_train.txt
        ¦   +-- body_gyro_y_train.txt
        ¦   +-- body_gyro_z_train.txt
        ¦   +-- total_acc_x_train.txt
        ¦   +-- total_acc_y_train.txt
        ¦   +-- total_acc_z_train.txt
        +-- subject_train.txt
        +-- X_train.txt
        +-- y_train.txt

The four top-level text files are descriptinve, and have been included
in this repository. See:

1.  [README.txt](./UCI%20HAR%20Dataset/README.txt)
2.  [activity\_labels.txt](./UCI%20HAR%20Dataset/activity_labels.txt)
3.  [features.txt](./UCI%20HAR%20Dataset/features.txt)
4.  [features\_info.txt](./UCI%20HAR%20Dataset/features_info.txt)

Merges training and test data
=============================

The basic approach is to load the two-column `activity_labels.txt` and
`features.txt` tables into a metadata structure so that it is available
while loading each of the test and training data sets, then load and
augment the test and training data. And finally combine and anlyse them
per the assignemtn brief.

Shared Metadata Functions
-------------------------

The shared function `load_meatdata_1` loads a two-column table, checks
that the first column is just a sequence number, and hands back the
second column as an atomic vector. The vecor will be a factor vector if
the items are unique (which `activity_labels.txt` is), or a character
vector if they are not (which `features.txt` is).

In `load_metadata` these two are loaded. We also explicitly find the
columns that descibe means and standard deviations, both as a logical
vector for subsetting (`cols_to_keep`) and as character array
(`col_names_to_keep`). The various elements of metadata are returned in
a list.

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

Loading Core Data Function
--------------------------

The data itself is loaded via a utility function that can load either
the `test` or `train` data.

The core numbers are loaded from the 'X\_' file, with the explicit set
of column names being supplied.  
This is then subsetted to the columns that are mean or standard
devistions, using the `cols_to_keep` metadata created above. This data
is then augmented with the subject factor and activity factor, together
with an `is_training_data` column (currently unused).

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

Bringing it all together
------------------------

Load meta, test and training data, and combine the latter two.

    setwd("UCI HAR Dataset")

    meta <- load_metadata()
    test <- load_data('test',meta)
    train <- load_data('train',meta)
    # Point 1: Merges the training and the test sets to create one data set.
    combined <- rbind(test,train)
    rm(test,train)

    setwd("..")

Calculating Averages
--------------------

Calculating the averages for all columns, by subject and activity, is
trivial using the `recast` function from the
[reshape2](http://cran.r-project.org/web/packages/reshape2/index.html)
package.

    require(reshape2)
    averages <- recast(combined[,-3] # need to avoid my "is_training_data" column
                       , subject + activity ~ ..., mean)
    write.table(averages, "averages.txt", row.name=FALSE)

The file averages.txt details the averages of the means and standard
deviations, for each subject and activity. The first two columns are
`subject'' and`activity'', then followed by a further 66 columns with
the averages for the following factors:

    meta$col_names_to_keep

     [1] "tBodyAcc-mean()-X"           "tBodyAcc-mean()-Y"          
     [3] "tBodyAcc-mean()-Z"           "tBodyAcc-std()-X"           
     [5] "tBodyAcc-std()-Y"            "tBodyAcc-std()-Z"           
     [7] "tGravityAcc-mean()-X"        "tGravityAcc-mean()-Y"       
     [9] "tGravityAcc-mean()-Z"        "tGravityAcc-std()-X"        
    [11] "tGravityAcc-std()-Y"         "tGravityAcc-std()-Z"        
    [13] "tBodyAccJerk-mean()-X"       "tBodyAccJerk-mean()-Y"      
    [15] "tBodyAccJerk-mean()-Z"       "tBodyAccJerk-std()-X"       
    [17] "tBodyAccJerk-std()-Y"        "tBodyAccJerk-std()-Z"       
    [19] "tBodyGyro-mean()-X"          "tBodyGyro-mean()-Y"         
    [21] "tBodyGyro-mean()-Z"          "tBodyGyro-std()-X"          
    [23] "tBodyGyro-std()-Y"           "tBodyGyro-std()-Z"          
    [25] "tBodyGyroJerk-mean()-X"      "tBodyGyroJerk-mean()-Y"     
    [27] "tBodyGyroJerk-mean()-Z"      "tBodyGyroJerk-std()-X"      
    [29] "tBodyGyroJerk-std()-Y"       "tBodyGyroJerk-std()-Z"      
    [31] "tBodyAccMag-mean()"          "tBodyAccMag-std()"          
    [33] "tGravityAccMag-mean()"       "tGravityAccMag-std()"       
    [35] "tBodyAccJerkMag-mean()"      "tBodyAccJerkMag-std()"      
    [37] "tBodyGyroMag-mean()"         "tBodyGyroMag-std()"         
    [39] "tBodyGyroJerkMag-mean()"     "tBodyGyroJerkMag-std()"     
    [41] "fBodyAcc-mean()-X"           "fBodyAcc-mean()-Y"          
    [43] "fBodyAcc-mean()-Z"           "fBodyAcc-std()-X"           
    [45] "fBodyAcc-std()-Y"            "fBodyAcc-std()-Z"           
    [47] "fBodyAccJerk-mean()-X"       "fBodyAccJerk-mean()-Y"      
    [49] "fBodyAccJerk-mean()-Z"       "fBodyAccJerk-std()-X"       
    [51] "fBodyAccJerk-std()-Y"        "fBodyAccJerk-std()-Z"       
    [53] "fBodyGyro-mean()-X"          "fBodyGyro-mean()-Y"         
    [55] "fBodyGyro-mean()-Z"          "fBodyGyro-std()-X"          
    [57] "fBodyGyro-std()-Y"           "fBodyGyro-std()-Z"          
    [59] "fBodyAccMag-mean()"          "fBodyAccMag-std()"          
    [61] "fBodyBodyAccJerkMag-mean()"  "fBodyBodyAccJerkMag-std()"  
    [63] "fBodyBodyGyroMag-mean()"     "fBodyBodyGyroMag-std()"     
    [65] "fBodyBodyGyroJerkMag-mean()" "fBodyBodyGyroJerkMag-std()" 

Production Notes
================

This file was authored in [RMarkdown](http://rmarkdown.rstudio.com/) in
[RStudio](http://www.rstudio.com/products/RStudio/) with in-line R code
to execute the project.  
The README.md was then converted to GitHub style Markdown by invoking
`knit("README.Rmd", output="README.md")` function from
[knitr](http://yihui.name/knitr/) package, and the `run_analysis.R`
generated by running `purl("README.Rmd","run_analysis.R")`
