# Title : HW 3 - IDS 572 - Earning manipulation by Indian Firms
# Authors: Priyanshi Jignesh Patel: UIN: (650927804) 
          #Ajay Mahadev Pawar; UIN: (676955899)
          #Darshan Radadiya: UIN: (656230601) 

library(readxl)
library(caret)
library(pROC)
library(ROSE)
library(robustbase)
library(smotefamily)
library(ROCR)
library(rpart)
library(rpart.plot)

#Read the DataSet from Excel
dataSet <- read_excel("C:/Users/DELL/Desktop/UIC/Sem 3/IDS 572/Case Study/IMB579-XLS-ENG.xlsx",
                      sheet = "Sample for Model Development")

#Replace '-' in the column name 'C-MANIPULATOR' to BLANK
names(dataSet)[names(dataSet) == "C-MANIPULATOR"] <- "CMani"
str(dataSet)

colSums(is.na(dataSet)) #Check for NA values

#Change the Manipulator column values to numeric along with its type
dataSet$Manipulator <- ifelse(dataSet$Manipulator == "Yes", 1, 0)
df <- data.frame(dataSet)

#df$CMani <- as.factor(df$CMani)
str(df)

table(df$CMani)
prop.table(table(df$CMani))

####### Q3- Developing Logistic Regression Model #######

#Divide the data into Training and Testing
set.seed(1234)
splitIndex <- createDataPartition(df$CMani, p = .50,
                                  list = FALSE, times = 1)
trainingSet <- df[splitIndex, ]
testingSet <- df[-splitIndex, ]
str(trainingSet)
table(trainingSet$CMani)
prop.table(table(trainingSet$CMani))

#Using Treebag Model by Cross Validation, a simple bagging model is created.
#The Treebag Model will provide the Accuracy for training data
ctrl <- trainControl(method = "cv", number = 5)
tbModel <- train(CMani ~ ., data = trainingSet,
                 method = "treebag", trControl = ctrl)
predictors <- names(trainingSet)[names(trainingSet) != 'CMani']
pred <- predict(tbModel$finalModel, testingSet[, predictors])
auc <- roc(testingSet$CMani, pred)
print(auc)

#Using Treebag Model, the following determines the accuracy on Test Data
str(testingSet)
table(testingSet$CMani)
prop.table(table(testingSet$CMani))

ctrlTest <- trainControl(method = "cv", number = 5)
tbTestModel <- train(CMani ~ ., data = testingSet,
                 method = "treebag", trControl = ctrl)
predictorsTest <- names(testingSet)[names(testingSet) != 'CMani']
predTest <- predict(tbTestModel$finalModel, testingSet[, predictorsTest])
aucTest <- roc(testingSet$CMani, predTest)
print(aucTest)

#Using SMOTE - will design a new dataset for the modeling
smoteTrain <- trainingSet
smoteTrain$CMani <- as.factor(smoteTrain$CMani)
table(smoteTrain$CMani)
prop.table(table(smoteTrain$CMani))
str(smoteTrain)

smoteTrain <- SMOTE(smoteTrain[-11], smoteTrain$CMani)
smoteNew <- smoteTrain$data
table(smoteNew$class)
prop.table(table(smoteNew$class))

######## Q4-Performance on Test&Train #########

# Logistic Regression for the data Balanced by Smote
# We use Accuracy measure to evaluate performance. 
# Along AIC. 
smoteLogistic <- glm(as.factor(class) ~ ., data = smoteNew, family = "binomial")
summary(smoteLogistic)

smotePredict <- predict(smoteLogistic, type = "response")
smotePredict <- ifelse(smotePredict > 0.50, 1, 0)
confusionMatrix(as.factor(smoteNew$class), as.factor(smotePredict), positive = "1")
## The model gives 100% accuracy of Training Data

#Logistic Regression on Test Data
#Using SMOTE - will design a new Test dataset for the modeling
smoteTest <- testingSet
smoteTest$CMani <- as.factor(smoteTest$CMani)
table(smoteTest$CMani)
prop.table(table(smoteTest$CMani))
str(smoteTest)

smoteTest <- SMOTE(smoteTest[-11], smoteTest$CMani)
smoteTestNew <- smoteTest$data
table(smoteTestNew$class)
prop.table(table(smoteTestNew$class))

#Logistic Regression for the data Balanced by Smote
smoteTestLogistic <- glm(as.factor(class) ~ ., data = smoteTestNew, family = "binomial")
summary(smoteTestLogistic)

smoteTestPredict <- predict(smoteTestLogistic, type = "response")
smoteTestPredict <- ifelse(smoteTestPredict > 0.50, 1, 0)
confusionMatrix(as.factor(smoteTestNew$class), as.factor(smoteTestPredict), positive = "1")
## The model gives 100% accuracy for Test Data

######### Q7- Decision Trees ##########

dtree <- rpart(CMani~ ., data = trainingSet, 
               control = rpart.control(cp = -1, minsplit = 0, minbucket = 0), 
               parms = list(split = "gini"))
print(dtree)
rpart.plot(dtree)

# Decision Tree with testing data
dtree_test <- rpart(CMani~ ., data = testingSet, 
               control = rpart.control(cp = -1, minsplit = 0, minbucket = 0), 
               parms = list(split = "gini"))
print(dtree_test)
rpart.plot(dtree_test)

# Optimal CP
optCP <- which.min(dtree_test$cptable[,"xerror"] )
optCP
# the best CP value
CP <- dtree_test$cptable[optCP, "CP"]
CP

#Now we can prune the tree based on this best CP value
dtree_pruned  <- prune(dtree_test, cp = CP)
summary(dtree_pruned)

#Confusion matrix for the DecisionTree
train_dt<-table(predict(dtree_pruned, type="class", trainingSet$CMani))

confusionMatrix(train_dt, positive = "1")

tab_test_dt<-table(predict(dt_bal_pruned, type="class", newdata = sample_test), sample_test$C_MANIPULATOR, dnn = c("Predicted","Actual"))
confusionMatrix(tab_test_dt, positive = "1")

###### Random Forest Model #######

library(randomForest)
install.packages("ROSE")
library(ROSE)

# Sampling Test and Train data 
dataSet$`Company ID`<- NULL
set.seed(1234)
index <- sample(2, nrow(dataSet), replace = TRUE, prob = c(0.65,0.35))
rf_train <- dataSet[index == 1,]
rf_test <- dataSet[index == 2,]

# Balancing the data by Oversampling 
over_sample_rf <- ovun.sample(CMani~., data = rf_train, method = "over", N= 248)$data
over_sample_rf
table(over_sample_rf$CMani)
rf = randomForest(CMani~., 
                             data = over_sample_rf, ntree = 100, 
                             proximity = TRUE, replace= TRUE, 
                             importance = TRUE, 
                             mtry = sqrt(ncol(over_sample_rf)))
rf_variable = randomForest(CMani~ DSRI + SGI + ACCR, 
                  data = over_sample_rf, ntree = 100, 
                  proximity = TRUE, replace= TRUE, 
                  importance = TRUE, 
                  mtry = sqrt(ncol(over_sample_rf)))

print(rf)
plot(rf)
plot(rf_variable)

rf_test_pred <- predict(rf, newdata = rf_test)
rf_test_pred
rf_table <- table(rf_test_pred, newdata = rf_test$CMani)

# Predicting Model Accuracy 
confusionMatrix(rf_table, positive = "1")

##### Q8-Linear Regression model for Complete data ######

dataset_complete <- read_excel("C:/Users/DELL/Desktop/UIC/Sem 3/IDS 572/Case Study/IMB579-XLS-ENG.xlsx",
                      sheet = "Complete Data")
dataset_complete$`Company ID`<- NULL

# Replace '-' in the column name 'C-MANIPULATOR' to BLANK
names(dataset_complete)[names(dataset_complete) == "C-MANIPULATOR"] <- "CMani"
str(dataset_complete)

table(dataset_complete$CMani)
colSums(is.na(dataset_complete))

# Change the Manipulator column values to numeric along with its type
dataset_complete$Mani_New <- ifelse(dataset_complete$Manipulater == "Yes", 1, 0)
str(dataset_complete)

# Splitting the complete data into Training and Testing 
set.seed(123)
index = sample(2, nrow(dataset_complete), replace = TRUE, prob = c(0.8,0.2))
Train_complete = dataset_complete[index == 1, ]
nrow(Train_complete)
table(Train_complete$CMani)
Test_complete = dataset_complete[index == 2,]
nrow(Test_complete)
table(Test_complete$CMani)

# Building Logistic Regression model 
LTrain_complete <- glm(CMani ~ ., 
                      data = Train_complete, 
                      family = "binomial")
summary(LTrain_complete)

#Using oversampling for predicting 
Ltrain_over <- ovun.sample(CMani~ .,
                          data = Train_complete,
                          method = "over", 
                          N=1566)$data
table(Ltrain_over$CMani)

# Variable selection for Logistic Model 
var = glm(CMani~1, data = Ltrain_over, family = "binomial")
var2 = glm(CMani~., data = Ltrain_over, family = "binomial")
#using forward Method to select  variable
step(var, scope = list(lower=var, upper=var2), direction = "forward")
LTrain_complete_variable <- glm(CMani ~  DSRI + ACCR + SGI + AQI,
                       data = Ltrain_over, 
                       family = "binomial")
summary(LTrain_complete_variable)

# Deviance for the model
Lt <- summary(LTrain_complete_variable)$deviance

# Predict test data based on model
Tpred_variable = predict.glm(LTrain_complete_variable, newdata = Test_complete, type="response")
Tpred_variable

#Plotting ROC Curve
roc_pred = prediction(Tpred_variable, Test_complete$CMani)
roc_perf = performance(roc_pred, "tpr", "fpr")
plot(roc_perf, col = "blue")

#calculating Optimal Cutoff
opt.cut = function(roc_perf, roc_pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], cutoff = p[[ind]])
  }, roc_perf@x.values, roc_perf@y.values, roc_pred@cutoffs)}
print(opt.cut(roc_perf, roc_pred))
#sensitivity 0.9000000
#specificity 0.8146552
#cutoff      0.2960608

#Using the cutoff Point to Plot Confusion Matrix
Tpred_variable$CMani = ifelse(Tpred_variable> 0.2960608,1,0)

ptab<-table(Tpred_variable$CMani, Test_complete$CMani, dnn = c("Predicted","Actual"))
ptab
confusionMatrix(ptab,positive = "1")
# We Get 95% Accuracy
