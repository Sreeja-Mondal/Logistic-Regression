### SETTING WORKING DIRECTORY ### 
setwd("C:/Users/Sreeja Mondal/Desktop/TRAINING/PRACTICAL")
getwd()

### READING THE DATAASET ###
data <- read.csv("sample data.csv", header = T) 


### NUMBER OF ROWS AND COLUMNS IN DATASET ###
nrow(data) ##  2260668
ncol(data) ##  145

### HEAD OF DATA - All columns ### 
head(data)
names(data)

### DATATYPE ###
data_type <- as.data.frame(sapply(data, typeof))

### NUMBER OF NULLS IN THE DATASET ### 
null_count <- as.data.frame(colSums(is.na(data)))
null_count$datatype <- sapply(data,typeof)

### Descriptive Statistics ###
install.packages("pastecs")
library(pastecs)
options(scipen=100)
options(digits=2)
summary_statistics <- as.data.frame(t((stat.desc(data))))

#### Checking for N/A in a column and replacing it with null ###
data$dummy_na <- "NOT PRESENT"
data[data == "NOT PRESENT"] <- NA 

#### IMPUTING MISSING VALUES IN THE DATASET #### 
## Best to replace with Median in order to deal with extreme values ## 
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))


#### CHECKING FOR CLASS IMBALANCE ####
## Finding imbalance - Group the data based on Class value using dplyr package containing “group by function”
data_class_imbalance <- data %>% group_by(target_variable) %>% summerize (class_count = n())
print(head(data_class_imbalance)) 
## Class imbalance can be treated by the method of Oversampling or Undersampling or using SMOT :https://www.analyticsvidhya.com/blog/2017/03/imbalanced-classification-problem/ ### 

#### CORRELATION MATRIX #### 

#### logistic regression ####
install.packages('caTools')
library(caTools)
set.seed(88)
split <- sample.split(train$Recommended, SplitRatio = 0.75) 
########dividing the data set into training and test data##########
DropedData <- data
smp_size <- floor(0.75 * nrow(DropedData))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(DropedData)), size = smp_size)

train <- DropedData[train_ind, ]
head(train,3)
test <- DropedData[-train_ind, ]

####################logistic regression##########################

model <- glm(target_variable ~.,family=binomial(link='logit'),data=train)
s=summary(model)
############# Extracting the p value #####################
coef(summary(model))[,4]
i= as.data.frame(coef(summary(model))[,4]) 

######## Calculating the confusion matrix ####
table(train_data$target_variable, predict > 0.5)

## Accuracy ## 
Accuracy <-  (TP+TN)/(TN+TP+FN+FP) 
Precision <- TP/(TP+FP)
Recall <- (TP)/(TP+FN)
F1 <- (2*TP)/(2*TP + FP +FN)




