setwd("D:/Study/Hackathon analytics vidhya/Jobathon 26022021/data")

train = read.csv('train_Df64byy.csv', header = T)
test = read.csv('test_YCcRUnU.csv', header =T)

summary(train)
library(dplyr)

train<- train %>% mutate_if(is.character, as.factor)
test<- test %>% mutate_if(is.character, as.factor)

summary(train)

train$Health.Indicator[train$Health.Indicator==""]<-NA
train$Holding_Policy_Duration[train$Holding_Policy_Duration==""]<-NA

test$Health.Indicator[test$Health.Indicator=='']<- NA
test$Holding_Policy_Duration[test$Holding_Policy_Duration=='']<-NA

summary(train)
summary(test)
anyNA(train)

library(VIM)
install.packages('caret')
install.packages('mice')
library(caret)
library(mice)
str(train)
colnames(train)
df <- rbind(train[,-14],test)

str(df)

df1 <- preProcess(train[,c('Health.Indicator', 'Holding_Policy_Duration',"Holding_Policy_Type")], method='knnImpute')
df1 <- preProcess(train, method='knnImpute')

tempData <- mice(train,m=5,maxit=50,meth='pmm',seed=500)
anyNA(tempData)
df_train<- complete(tempData)
summary(df_train)
tempData1 <- mice(test,m=5,maxit=25,meth='pmm',seed=500)
anyNA(tempData1)
df_test<- complete(tempData1)
class(tempData)
tempData<- as.data.frame(tempData)
summary(tempData$data)
summary(tempData$chainMean)

write.csv(df_train, 'train_mice_imputed.csv', row.names = F)
write.csv(df_test,'test_mice_imputed.csv', row.names = F)

summary(df1$data)

# KNN imputation
df1 <- kNN(df, k = 5) # perform knn imputation.
anyNA(data_msme_impute_knn)
#Check for missing
sapply(data_msme_impute_knn, function(x) sum(is.na(x)))