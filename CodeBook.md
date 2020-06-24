---
title: "CodeBook"
author: "Eduardo Zornoff"
date: "6/23/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Course Project Getting and Cleaning Data - Johns Hopkins University

# 1. Merging the files and creating one dataset

The first step was setup the variables to be able to collect the data and manipulate the data

"ver" is the two types of data train and test

"name" is the file names to access the data

"features" is the name vector of the data variables

"activities" is the data frame (df) that contains the description of the activities

* temporary variables are not described

```{r}

library(dplyr)

ver = c("train", "test") # two sets of data

name = c("subject", "X", "Y") #, "body_acc_x", "body_acc_y", "body_acc_z",
         #"body_gyro_x", "body_gyro_y", "body_gyro_z",
         #"total_acc_x", "total_acc_y", "total_acc_z") Define which file to be read

features = read.csv("./data/features.txt", sep = " ", header = FALSE)[,2] # extract features variables names

activities = read.csv("./data/activity_labels.txt", sep = " ", header = FALSE) # extract activities
names(activities) = c("activity", "activity description")

```

Next we read the files that were need and assemble the data df

"data" is the desired table

"n" is the selector of the file order

"v" is the selector of the train or test file

```{r}

data = data.frame() # initializing data data frame

readFile = function(n) { # n is the file to be read / readi train and test files and combine
        n = n
        # read train
        v = 1
        dFile = paste("./data/", ver[v], "/", name[n], "_", ver[v], ".txt", sep = "")
        if (!file.exists(dFile)) {dFile = paste("./data/", ver[v], "/Inertial Signals/", 
                                        name[n], "_", ver[v], ".txt", sep = "")}
        train = read.csv(dFile)
        names(train)[1] = name[n]
        # read test
        v = 2
        dFile = paste("./data/", ver[v], "/", name[n], "_", ver[v], ".txt", sep = "")
        if (!file.exists(dFile)) {dFile = paste("./data/", ver[v], "/Inertial Signals/", 
                                        name[n], "_", ver[v], ".txt", sep = "")}

        test = read.csv(dFile)
        names(test)[1] = name[n]
        # bind rows
        temp = rbind(train, test)
        temp
}

# initialize data data.frame with subject column
data = readFile(1)

# populate data.frame with all the column data from the other variables

for (i in (2:3)) { # i is the file from the list / there's is still other 9 basic features totalling 12
        temp = readFile(i)
        data = cbind(data, temp)
}
rm("temp") # releasing memory

```

# 2. Extracting measurements (variables) / 4. Labelling the data set

Indentifing and selecting the variables that contains 'mean' or 'std'

"mn" is the boolean vector that identifies which variables contains "mean"

"st" is the boolean vector that identifies which variables contains "std"

"cl" is the boolean vector that combines both mn and st

"feat" is the desired names for the columns that were selected

```{r}

library(stringi)

# identify which variables contains mean or std

mn = stri_detect_fixed(features, "mean") & !stri_detect_fixed(features, "meanFreq")
st = stri_detect_fixed(features, "std")
cl = mn | st            # aggregate both criteria
feat = features[cl]     # create variable name vector

```

Split the variables into independent numeric columns because all the variables are into a consolidated character string

"Xdata" subset of "data" containing only the variables data

"i" is the line being processed

"Xsplit" is the resulting df of this stage

```{r}
# splitinng X into variables

Xdata = data[,"X"] # selecting only the variable column

# cut X data from data, rename Y column to create a correspondence with activities df
data = data %>% select(subject, activity = Y)

# function to character split to numeric
spt = function(i) {
        t = stri_trim(Xdata[i]) # trim the sting to remove more than one spaces 
        t = as.numeric(simplify2array(strsplit(t, " "))) # extract the numbers from the string
        t = t[!is.na(t)] # remove NAs
        t = t[cl] # get only the info for the desired variables (mean and std variables)
        t
}

Xsplit = spt(1) # initialize Xplit

# run spt for all lines of X data

for (i in (2:10297)) { # i is the line of the list
        
        temp = spt(i)
        Xsplit = rbind(Xsplit, temp)

}
rm("temp") # releasing memory
rm("Xdata")
Xsplit = as.data.frame(Xsplit)

library(plyr)

Xsplit = unrowname(Xsplit) # strip row names
names(Xsplit) = feat # naming columns of Xsplit

```

Next we add the Xsplit to the data and get the tidy data needed

```{r}
# adding Xsplit to data

data = cbind(data, Xsplit)
rm("Xsplit") # releasing memory

```

# 3. Naming the activities by the descriptive name

```{r}

# adding activity description
data = left_join(data, activities, by = "activity")
# moving next to activity code and elimination the activity code
data = data %>% relocate(`activity description`, .after = activity) %>% 
        select(-activity)

print(head(data))

```

Now we have data df with identifiers "subject" and "activity description" on 1st and 2nd columns and variables on the next 66 columns in numeric form

# 5. Create an independent dataset for the mean of the variables per subject and activity


```{r}

# creating the new data frame with the mean for each combination of subject and activity 
# and each variable
dataSum = data %>% group_by(subject, `activity description`) %>% summarise_all(mean)

print(head(dataSum))

```

et voilà

