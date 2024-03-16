library(data.table)
library(ggplot2)
library(randomForest)
library(leaps)
library(glmnet)
library(caret)


d1 = fread('C:/Users/aylar/Downloads/Uni Courses/1401-2 Regression - Mirsadeghi/Aylar/CHW2/diabetes_012_health_indicators_BRFSS2015.csv')
d2 = fread('C:/Users/aylar/Downloads/Uni Courses/1401-2 Regression - Mirsadeghi/Aylar/CHW2/diabetes_binary_5050split_health_indicators_BRFSS2015.csv')
d3 = fread('C:/Users/aylar/Downloads/Uni Courses/1401-2 Regression - Mirsadeghi/Aylar/CHW2/diabetes_binary_health_indicators_BRFSS2015.csv')

#Q1
d1$Diabetes_012 = as.factor(d1$Diabetes_012)
d2$Diabetes_binary = as.factor(d2$Diabetes_binary)
d3$Diabetes_binary = as.factor(d3$Diabetes_binary)

ggplot(d2, aes(BMI, fill = Diabetes_binary))+
  geom_density(alpha = .75)


d1$one = 1
ds = d1[, .(n = sum(one)), .(Diabetes_012, HeartDiseaseorAttack)]
ds[, n_total := sum(n), .(HeartDiseaseorAttack)]
ds[, n_percent := n/n_total]
ggplot(ds, aes(as.factor(HeartDiseaseorAttack), n_percent, fill = Diabetes_012)) + geom_bar(stat = 'identity',)

d1$one = 1
ds = d1[, .(n = sum(one)), .(Diabetes_012, HighBP)]
ds[, n_total := sum(n), .(HighBP)]
ds[, n_percent := n/n_total]
ggplot(ds, aes(as.factor(HighBP), n_percent, fill = Diabetes_012)) + geom_bar(stat = 'identity',)


d1$one = 1
ds = d1[, .(n = sum(one)), .(Diabetes_012, Sex)]
ds[, n_total := sum(n), .(Sex)]
ds[, n_percent := n/n_total]
ggplot(ds, aes(as.factor(Sex), n_percent, fill = Diabetes_012)) + geom_bar(stat = 'identity',)


#Q2


# Assuming you want to build a random forest with 100 trees
rf_model <- randomForest(as.factor(Diabetes_binary)~ ., data=d2, importance=TRUE, proximity
                         = FALSE)
# Use varImp() function to obtain feature importance
importance <- varImp(rf_model)

# Plot the feature importance
plot(importance)



#Q3
# best subset selection
best_subset <- regsubsets(d2$Diabetes_binary ~ ., data = d2, nvmax = 21, method = "exhaustive")
summarY<-summary(best_subset)
ggplot(data=data.frame(summarY$cp), aes(x=1:21, y=summarY$cp, )) +
  geom_line(color="yellow")+
  geom_point(color="orange")+
  labs(x="number of predictors", y="CP")

best_subset <- regsubsets(d2$Diabetes_binary ~ ., data = d2, nvmax = 11, method = "exhaustive")
best_subset_index <- which.min(summary(best_subset)$cp)

best_subset_predictors <- names(coef(best_subset, id = best_subset_index))
print(best_subset_predictors)

#Q4
#random forest based on Q3 features
set.seed(6000)
trainIndex <- createDataPartition(y = d2$Diabetes_binary, p = 0.5, list = FALSE)
trainData <- d2[trainIndex, ]
test <- d2[-trainIndex, ]

rF <- randomForest(as.factor(Diabetes_binary) ~ HighBP+HighChol+CholCheck
                   +BMI+HeartDiseaseorAttack+HvyAlcoholConsump
                   +GenHlth+PhysHlth+Sex+Age+Income, data = trainData,importance = TRUE , proximity = FALSE)
summary(rF)
#test :
test$predict <- predict(rF,test)
confusionMatrix(
  factor(test$predict, levels = 0:1),
  factor(test$Diabetes_binary, levels = 0:1))

