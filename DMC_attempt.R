#DMC-Attempt
getwd()
setwd("/home/chinmay/Desktop/TUM/Sem-1/Business Analytics");
train_data <- read.csv("DMC_training_data.csv")
test_data <- read.csv("DMC_test_data.csv")
str(train_data)
#Size
nrow(train_data)
ncol(train_data)

ncol(test_data)
nrow(test_data)

#Peek at first and last rows
head(train_data)
tail(train_data)

head(test_data)
tail(test_data)
#Look at missing value columns
colSums(is.na(train_data))
colSums(is.na(test_data))
#Convert the format of data
timestamp_format = "%Y-%m-%d %H:%M:%S"
date_format = "%Y-%m-%d"
time_format = "%H"
bin_points <- c(3,6,9,12,15,18,21,24)
train_data$TimeStamp <- strftime(train_data$TimeStamp,timestamp_format)
Train_Time_of_Day <- strtoi(strftime(train_data$TimeStamp, time_format),10L)
train_data$Train_Time_of_Day <-cut(Train_Time_of_Day,bin_points,labels=1:7)

#table(train_data$Train_Time_of_Day)

train_data$LAST_MODIFIED <- as.Date(train_data$LAST_MODIFIED,date_format)
train_data$VALIDATION_LAST_MODIFIED <- as.Date(train_data$VALIDATION_LAST_MODIFIED,date_format)
test_data$TimeStamp <- strftime(test_data$TimeStamp,timestamp_format)
Test_Time_of_Day = strtoi(strftime(test_data$TimeStamp, time_format),10L)
test_data$Test_Time_of_day <- cut(Test_Time_of_Day,bin_points,labels=1:7)

#table(test_data$Test_Time_of_day)

test_data$LAST_MODIFIED <- as.Date(test_data$LAST_MODIFIED,date_format)
test_data$VALIDATION_LAST_MODIFIED <- as.Date(test_data$VALIDATION_LAST_MODIFIED,date_format)


table(Train_Time_of_Day)
#Since the houehold count field is NA all through, we drop it
train_data = subset(train_data,select = -(HOUSEHOLD_COUNT))
#Let us touch the test_data later

#See the structure again
str(train_data)

#Explore Address_Street
table(train_data$ADDR_STREET)

#Look at preferred partner
table(train_data$PREFERRED_PARTNER)
table(test_data$PREFERRED_PARTNER)
#train_data$PREFERRED_PARTNER <- NULL #All values are Yes only, no use
train_data = subset(train_data,select = -(PREFERRED_PARTNER))

#Unify the ADDR_REGION column
table(train_data$ADDR_REGION)
train_data$ADDR_REGION[train_data$ADDR_REGION == "California"] = "CA"

table(train_data$ADDR_MUNICIPALITY)
train_data$ADDR_MUNICIPALITY[train_data$ADDR_MUNICIPALITY == "San Francisco"] = "SF"

table(train_data$FREECHARGE,useNA = "always")
levels(train_data$FREECHARGE) = c(levels(train_data$FREECHARGE),"NO")
train_data$FREECHARGE[train_data$FREECHARGE == ""] = "NO"
table(train_data$FREECHARGE,useNA = "always")

table(train_data$PREFERRED_PARTNER)
table(train_data$HOUSEHOLD_COUNT)


table(train_data$IS_LSC_VALIDATED)
range(train_data$TimeStamp)
#We can use the idea of separating the weekdays from the weekends during our processing

train_data$weekday = weekdays(as.Date(train_data$TimeStamp,date_format))
test_data$weekday = weekdays(as.Date(test_data$TimeStamp,date_format))

#Checking if some numeric variables should be converted to factors
#Status is our dependent variable

#-------Doing same for port number check---------
unique(train_data$portNumber)
train_data$portNumber <- factor(train_data$portNumber)
test_data$portNumber <- factor(test_data$portNumber)

#-------And now for the Type1 count
unique(train_data$TYP1_COUNT)
train_data$TYP1_COUNT <- factor(train_data$TYP1_COUNT)
test_data$TYP1_COUNT <- factor(test_data$TYP1_COUNT)

# Multicollinearity
install.packages('caret')
install.packages("stringi")
library(caret)
numeric_columns = c("EI65_GEO_ID","ADDR_LATITUDE","ADDR_LONGITUDE","ADDR_POSTALCODE")
numeric_columns_correlation <- cor(train_data[, numeric_columns], use="pairwise.complete.obs")
numeric_columns_correlation

#Better to remove the ID attribute from the data
train_data <- subset(train_data,select = -(ID))
test_data <- subset(test_data,select = -(ID))

# Feature Selection
install.packages("FSelector")
#dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_122.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(FSelector)
# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(status ~ ., data=train_data)
weights_info_gain
weights_gain_ratio = gain.ratio(status ~ ., data=train_data)
weights_gain_ratio

# Select the most important attributes based on Gain Ratio
most_important_attributes <- cutoff.k(weights_gain_ratio, 7)
most_important_attributes
formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "status")
formula_with_most_important_attributes


# 4. Training & Evaluation
# Balanced samples using the "ROSE" package
table(train_data$status)
install.packages("ROSE")
install.packages("e1071")
library(ROSE)
library(RWeka)
library(e1071)


train_data_check <- ovun.sample(status ~ ., data=train_data, method="over",  na.action="na.pass")$data
table(train_data_check$status)

# 2 x 5-fold cross validation
fitCtrl <- trainControl(method="repeatedcv", number=2, repeats=1)

# training a decision tree model using the metric "Accuracy"
model = train(formula_with_most_important_attributes, data=train_data, method="J48", trControl=fitCtrl, metric="Accuracy",  na.action=na.omit)


# Show results and metrics
model
model$results

# Show decision tree
model$finalModel

# Show confusion matrix (in percent)
confusionMatrix(model)
