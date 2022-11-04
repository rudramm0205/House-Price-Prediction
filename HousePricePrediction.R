df<-read.csv("house-data.csv")
sum(is.na(df))
sapply(df, function(x) sum(is.na(x)))
sapply (df, function(x) (sum(is.na(x))/nrow(df))*100)
drop<- c("Alley","PoolQC","Fence","MiscFeature","SaleType","ExterCond","BsmtQual","BsmtCon d","GarageType",
         "GarageCond","LotConfig","Neighborhood","Condition1","Condition2","BldgType","House Style",
         "RoofStyle","RoofMatl","Exterior1st","Foundation","Heating","Functional","PoolQC","Fenc e","MiscFeature","SaleCondition")
unique(df$Neighborhood)
df = df[,!(names(df) %in% drop)]
dim(df)
names(df)
str(df)
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE) }
unique(df$PavedDrive) unique(df$OverallCond) str(df)
unique(df$Street) unique(df$Utilities) unique(df$ExterQual) unique(df$KitchenQual) unique(df$PavedDrive)
# Converting Five character variables into dummy.
df$Street<- ifelse(df$Street == "Pave", 0 ,1)
df$Utilities <- ifelse(df$Utilities == "AllPub", 0 ,1)
df$ExterQual <- ifelse(df$ExterQual == "Gd", 0, ifelse(df$ExterQual=="TA",1, ifelse(df$ExterQual=="Ex",2, 3)))
df$KitchenQual <- ifelse(df$KitchenQual == "Gd", 0, ifelse(df$KitchenQual=="TA",1, ifelse(df$KitchenQual=="Ex",2, 3)))
df$PavedDrive <- ifelse(df$PavedDrive == "Y", 0,ifelse(df$PavedDrive== "N", 1, 2)) View(df)
Page | 18
#Correlation Matrix
install.packages("corrplot") library(corrplot)
ColMat3 <- cor(df) print(ColMat3) ColMat3.corr <- cor(df)
# Heatmap of Correlation matrix
corrplot(ColMat3.corr) class(df$OverallCond) df$OverallCond<-as.factor(df$OverallCond) #Q.2a)
df$OverallCond<- ifelse(df$OverallCond <= 3, "Poor", ifelse(df$OverallCond <= 6, "Average","Good"))
df$OverallCond<- ifelse(df$OverallCond == "Poor", 0 , ifelse(df$OverallCond == "Average", 1, 2))
dflog1<- sort(sample(nrow(df), nrow(df)*.7)) train <- df[dflog1,]
test <- df[-dflog1,]
names(df)
str(df)
is.na(df$OverallCond)
df$OverallCond <- as.numeric(overallcond)
dflog <- glm(OverallCond ~ YearBuilt + TotalBsmtSF + X1stFlrSF + GrLivArea + FullBath + TotRmsAbvGrd + GarageArea + Utilities + OverallQual + ExterQual, data = train, family=binomial(link=logit))
summary(dflog)
dflog$y
saaa <- ifelse(dflog>0.5, 1, 0)
bbb <- mean(aaa != test$OverallCond)
print(paste('Accuracy', 1- bbb))
class(df$OverallCond)
#Q.2b We use Linear Discriminant Analysis to predict the overall condition of the houses.
install.packages("MASS") library(MASS)
library(dplyr)
lll <- lda(OverallCond ~ ., data = train) kkk <- lll %>% predict(test) mean(kkk$class==test$OverallCond)
as.factor("dflog")
class("dflog")
df$OverallCond
df$OverallCond <- as.factor(df$OverallCond) class(df$OverallCond)
predicted=predict(dflog,test,type = "response") summary(dflog)
Page | 19

predicted
sensitivity<-sensitivity(df,) specificity(test$OverallCond,predicted,threshold=optCutOff) ?specificity
class(df$OverallCond)
# Converting Coefficient/ Response Variables/ X to Exponential exp(coef(dflog))
#
head(round(fitted(dflog), 2))
dflog_predicted <- predict(dflog, newdata = train, "link") tab <- table(train$OverallCond, dflog_predicted) round((sum(diag(tab))/sum(tab))*100,2)
#Q.3)a) We use Linear Regression and Random Foresting for house price prediction
library(corrplot)
# Removing Correlation matrix
ColMat3 <- cor(df)
print(ColMat3)
ColMat3.corr <- cor(df)
# Heatmap of Correlation matrix to Remove Some Variables from Model corrplot(ColMat3.corr)
# Method 1
# Linear Regression Model
linear_model <- lm(df$SalePrice ~ df$OverallQual + df$YearBuilt + df$MasVnrArea + df$TotalBsmtSF + df$X1stFlrSF + df$GrLivArea + df$FullBath + df$TotRmsAbvGrd + df$Fireplaces + df$GarageArea)
summary(linear_model)
# Method 2 – Random Forest
#Removing Missing value of OverallCond from the train dataset
train$OverallCond <- as.character(train$OverallCond) train$OverallCond[is.na(train$OverallCond)] <- 'No OverallCond' train$OverallCond <- factor(train$OverallCond)
Page | 20

# Checking if we have some missing values in the dataset
mvc = 0
for (i in 1:ncol(train)) {
  m = sum(is.na(train[,i]))
  print(paste("Column ",colnames(train[i])," has ",m," missing values")) if(m>0){
    mvc = mvc+1 }
  else{ mvc
  } }
print(paste("Dataset has overall ",mvc," columns with missing values"))
# Using Random Forest Method
library(randomForest)
# Lets omit variables which are not important from the model
trainn <- train[1:1000,] testt <- train[1001:1451,]
set.seed(1000)
output.randomforest <- randomForest(SalePrice ~ . ,
                                    data = trainn, importance = T) print(output.randomforest)
summary(output.randomforest) importance(output.randomforest)
fitForest1 <-predict(output.forest, newdata = test) summary(test)
# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))
# To get the rank importance of the sale price Please open Rank Importance Data frame from the Environment and We will get the Ranking and Importance