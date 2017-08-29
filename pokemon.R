#load the data
pokemon<-read.csv("Pokemon.csv",header=TRUE)
attach(pokemon)
View(pokemon)

#Size of the data
nrow(pokemon);ncol(pokemon)

#I'm gonna make correlation plot and conduct Random Forest 

#Let's rename the variables

colnames(pokemon) <- c("number", "name", "type1", "type2", "total", "hp", 
                       "attack", "defense", "sp.atk", "sp.def", "speed", 
                       "generation", "legendary")
sum(is.na(pokemon)) #'0' meansno missing values


#First, let's divide the variables into categorical and numerical to make life eaiser
categorical<-pokemon[,c(1:4,12:13)]
numerical<-pokemon[,c(5:11)]

#Create the correlation plot
library(corrplot)
M<-cor(numerical)
corrplot(M, method="circle")


#Let's start Random Forest

#Step1 divide the dataset into training and test dataset 
## 70% of the sample size
smp_size <- floor(0.7 * nrow(pokemon))

## set the seed to make your partition reproductible
set.seed(123)

train_ind <- sample(seq_len(nrow(pokemon)), size = smp_size)

train <- pokemon[train_ind, ]
test <- pokemon[-train_ind, ]

#Create the model for random forest
set.seed(12345)
library(randomForest)
library(miscTools)
library(ggplot2)
class(pokemon$total)
str(pokemon)
#Create RF model

rf=randomForest(total~.,data=train[-c(1,2)],ntree=200,mtry=2,nodesize=1,rules=TRUE)
rf_test_prediction=predict(rf,test)


#Variable Importance graph

rf_var_imp=data.frame(rf$importance)
rf_var_imp$Variables=row.names(rf_var_imp)
rf_var_imp

p<-ggplot(data=rf_var_imp,aes(x=rf_var_imp$Variables,y=rf_var_imp$IncNodePurity))+ geom_bar(stat="identity", width=0.7, fill="steelblue")
  

p


#create the metrics:r squared, mse

(r2 <- rSquared(test$total, test$total - predict(rf, test[,-5])))
(mse <- mean((test$total - predict(rf, test[,-5]))^2))


#Plot R square
p1 <- ggplot(aes(x=actual, y=pred),
            data=data.frame(actual=test$total, pred=predict(rf, test[,-5])))
p1 + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("RandomForest Regression in R r^2=", round(r2,2),"MSE=",round(mse,2), sep=""))

p1 + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("RandomForest Regression in MSE=", mse, sep=""))

