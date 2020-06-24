library(dplyr)

ver = c("train", "test") # two sets of data

name = c("subject", "X", "Y") #, "body_acc_x", "body_acc_y", "body_acc_z",
         #"body_gyro_x", "body_gyro_y", "body_gyro_z",
         #"total_acc_x", "total_acc_y", "total_acc_z") Define which file to be read

features = read.csv("./data/features.txt", sep = " ", header = FALSE)[,2] # extract features variables names

activities = read.csv("./data/activity_labels.txt", sep = " ", header = FALSE) # extract activities
names(activities) = c("activity", "activity description")

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
rm("temp")

library(stringi)

# identify which variables contains mean or std

mn = stri_detect_fixed(features, "mean") & !stri_detect_fixed(features, "meanFreq")
st = stri_detect_fixed(features, "std")
cl = mn | st            # aggregate both criteria
feat = features[cl]     # create variable name vector

# splitinng X into variables

Xdata = data[,"X"]

# cut X data from data, rename Y column 
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

# run Xplit for all lines of X data

for (i in (2:10297)) { # i is the line of the list
        
        temp = spt(i)
        Xsplit = rbind(Xsplit, temp)

}
rm("temp") # releasing memory
rm("Xdata") # releasing memory
Xsplit = as.data.frame(Xsplit)

library(plyr)

Xsplit = unrowname(Xsplit) # strip row names
names(Xsplit) = feat # naming columns of Xsplit

# adding Xsplit to data

data = cbind(data, Xsplit)
rm("Xsplit") # releasing memory

# adding activity description
data = left_join(data, activities, by = "activity")
# moving next to activity code and elimination the activity code
data = data %>% relocate(`activity description`, .after = activity) %>% 
        select(-activity)

print(head(data))

# creating the new data frame with the mean for each combination of subject and activity 
# and each variable
dataSum = data %>% group_by(subject, `activity description`) %>% summarise_all(mean)

print(head(dataSum))