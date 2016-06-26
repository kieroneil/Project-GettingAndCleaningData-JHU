# Create empty environment
rm(list=ls())

# Set project working directory - will be commented out in final verion
# wd <- "~/Analytics Course/02_GettingCleaningData/Project"
# setwd(wd)
##################################################################
#
#                 Project - Getting and Cleaning Data
#
##################################################################
# Get activity names from features.txt
##################################################################
# Load libraries
library(dplyr)
library(stringr)
library(tidyr)

# Get Feature names and format them to insert as colNames
vnames_raw <- read.table("features.txt", header = FALSE, sep = " ")
vn1 <- as.data.frame(vnames_raw[2]) 
vn2 <- unlist(vn1)

vn3 <- chartr(vn2, old = ",", new = "-")
vn4 <- chartr(vn3, old = "()", new = "  ")
vnames <- gsub("[[:space:]]", "", vn4) # Remove all whitespace

# Take care of those duplicate variable names
v <- c(303:316, 382:396, 461:474)

vnames[v] <- paste0(vnames[v], "_a")
vnames[v+14] <- paste0(vnames[v+14], "_b")
vnames[v+28] <- paste0(vnames[v+28], "_c")

#######################################################################
# Get the Train data
#######################################################################
path_pre <- "./train/"
trn_data_raw <- read.table(paste0(path_pre,"x_train.txt"), header=FALSE, col.names = vnames,
                           stringsAsFactors = FALSE)

########################################################################
# Get the Test data
########################################################################
path_pre <- "./test/"

tst_data_raw <- read.table(paste0(path_pre,"x_test.txt"), header=FALSE, col.names = vnames, 
                       stringsAsFactors = FALSE)

# Bind Train and Test sets together
step_one <- rbind(trn_data_raw, tst_data_raw)

#  WHOO HOO!!!  We've got a tidy full data set
#  This completes step 1:  Merges the train and test sets to create one data set.
########################################################################

step_two <- step_one %>%
  select(contains("mean"), contains("std"))

# This completes step 2:  Extracts only the measurements on the mean 
# and standard deviation for each measurement.
# ** Printing occurs at bottom **
########################################################################
# Start of step 3:  Uses descriptive activity names to name the 
# activities in the data set
all_data <- step_two
# Get Activity names to replace in data sets
activity_names <- read.table("activity_labels.txt", sep = " ", header = FALSE,
                             col.names = c("activity_index", "activity_description"))

# Get Activity and Subject Data
path_pre <- "./train/"
trn_feature <- read.table(paste0(path_pre,"y_train.txt"), col.names = "activity_index", 
                          header=FALSE, stringsAsFactors = FALSE)
trn_subject <- read.table(paste0(path_pre,"subject_train.txt"), col.names = "subject", 
                          header=FALSE, stringsAsFactors = FALSE)

path_pre <- "./test/"
tst_feature <- read.table(paste0(path_pre,"y_test.txt"), col.names = "activity_index", 
                          header=FALSE, stringsAsFactors = FALSE)
tst_subject <- read.table(paste0(path_pre,"subject_test.txt"), col.names = "subject", 
                          header=FALSE, stringsAsFactors = FALSE)

# Combine activity and subject the same way as above; trn then tst
all_features <- rbind(trn_feature, tst_feature)
all_subjects <- rbind(trn_subject, tst_subject)

# Create full data set of activities, subjects, and data
ptd1 <- cbind(all_features, all_subjects, all_data)

# Merge in the activity names to full data set using inner.join
ptd2 <- as.tbl(inner_join(ptd1, activity_names))

# Group by activity and subject, then calculate the means of each variable
step_five <- ptd2 %>%
  select(-activity_index) %>%
  group_by(activity_description, subject) %>%
  summarise_each(funs(mean))

# Print data from step two
step_two

# Print data from step 5
step_five
#  Step 4 was done on import: Appropriately labels the 
#    data set with descriptive variable names.
################################################################
#  Create codebooks - will be commented out in final version
# library(memisc)
# Write(codebook(step_one),
#       file="step_one_codebook.txt")
# Write(codebook(step_two),
#       file="step_two_codebook.txt")
# Write(codebook(step_five),
#       file="step_five_codebook.txt")
##############  END  ###########################################
