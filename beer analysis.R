beer<-read.csv("I:\\Amy Ko\\Statistical learning\\beers.csv",header=TRUE)
brewery<-read.csv("I:\\Amy Ko\\Statistical learning\\breweries.csv",header=TRUE)

beer<-read.csv("beers.csv",header=TRUE)
brewery<-read.csv("breweries.csv",header=TRUE)


attach(beer)
attach(brewery)
View(beer)
View(brewery)
colnames(brewery)[1] <- "brewery_id"

library(stringr)
library(dplyr)
library(maps)
library(ggplot2)
library(caret)

# Clean up the string data a bit
beer$name <- str_trim(beer$name)
beer$style <- str_trim(beer$style)
brewery$name <- str_trim(brewery$name)
brewery$city <- str_trim(brewery$city)
brewery$state <- str_trim(brewery$state)

# Match brewery information to beer
beer.data <- merge(beer, brewery, by="brewery_id")
colnames(beer.data)[2] <- "beer_id"
colnames(beer.data)[6] <- "beer_name"
colnames(beer.data)[9] <- "brewery_name"
attach(beer.data)

# Alcohol Content
#Let's divide ABV Levels (low(<4.5%),medium(4.5%-6%),high(6-8%), and very high(>8%)
# Generate data frames for choropleths
low.abv <-  beer.data[which(abv<0.045),]
med.abv <- beer.data[which(0.045 <= abv & abv<0.06),]
high.abv <-  beer.data[which(0.06 <= abv& abv< 0.08),]
vhi.abv <- beer.data[which(0.08 <= abv),]

ggplot(data=beer.data, aes(beer.data$abv)) + geom_histogram(col="blue")

ggplot(data=chol, aes(chol$AGE)) + geom_histogram()
#Create frequency table for each state
loabv.df <- data.frame(table(factor(low.abv$state, levels = c(state.abb, "DC"))))
medabv.df <- data.frame(table(factor(med.abv$state, levels = c(state.abb, "DC"))))
hiabv.df <- data.frame(table(factor(high.abv$state, levels = c(state.abb, "DC"))))
vhiabv.df <- data.frame(table(factor(vhi.abv$state, levels = c(state.abb, "DC") )))

# Prep for mapping
loabv.df$Var1 <- tolower(state.name[match(loabv.df$Var1, c(state.abb, "DC"))])
medabv.df$Var1 <- tolower(state.name[match(medabv.df$Var1, c(state.abb, "DC"))])
hiabv.df$Var1 <- tolower(state.name[match(hiabv.df$Var1, c(state.abb, "DC"))])
vhiabv.df$Var1 <- tolower(state.name[match(vhiabv.df$Var1, c(state.abb, "DC"))])
states.map <- map_data("state")

# Put together some choropleths
loabv.map <- ggplot(loabv.df, aes(map_id = Var1)) +
  geom_map(aes(fill = Freq), map = states.map) +
  expand_limits(x = states.map$long, y = states.map$lat) + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Low Alcohol Beer (<4.5% ABV) by State")
medabv.map <- ggplot(medabv.df, aes(map_id = Var1)) +
  geom_map(aes(fill = Freq), map = states.map) +
  expand_limits(x = states.map$long, y = states.map$lat) + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Medium Alcohol Beer (4.5-6% ABV) by State")
hiabv.map <- ggplot(hiabv.df, aes(map_id = Var1)) +
  geom_map(aes(fill = Freq), map = states.map) +
  expand_limits(x = states.map$long, y = states.map$lat) + 
  labs(x = "Longitude", y = "Latitude", 
       title = "High Alcohol Beer (6-8% ABV) by State")
vhiabv.map <- ggplot(vhiabv.df, aes(map_id = Var1)) +
  geom_map(aes(fill = Freq), map = states.map) +
  expand_limits(x = states.map$long, y = states.map$lat) + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Very High Alcohol Beer (>8% ABV) by State")
plot(loabv.map)
plot(medabv.map)
plot(hiabv.map)
plot(vhiabv.map)









#Analysis
## Predict ABV Level
model.beer.data <- select(beer.data, abv, ibu, style,  brewery_name, city, state)
model.beer.data <- filter(model.beer.data, !is.na(abv))
model.beer.data$style <- factor(model.beer.data$style)
model.beer.data$brewery_name <- factor(model.beer.data$brewery_name)
model.beer.data$city <- factor(model.beer.data$city)
model.beer.data$state <- factor(model.beer.data$state)

### Create dummy variables for the regression
beer.dummy <- dummyVars(~ ., data = model.beer.data)
dummied.data <- data.frame(predict(beer.dummy, newdata = model.beer.data))
### Identify and remove near zero variance predictors
nzv <- nearZeroVar(dummied.data)
beer.final <- dummied.data[,-nzv]
###Impute missing values for IBU
pp <- preProcess(beer.final, method = c("knnImpute"))
pp.beer.data <- predict(pp, newdata = beer.final)

#Preparing data for the model, we do a little bit of pre-processing before splitting the data into test and training sets. 
#The _caret_ package has some powerful facilities for accomplishing this.
# I have opted to use K-nearest neighbors to impute missing IBU values. 
#This would also impute missing ABV values had we left them in, but not many were missing, 
#and I'd prefer to train only against provided data for the output.


### Split into training and test groups
set.seed(777)
idx <- createDataPartition(pp.beer.data$abv,
                           times = 1,
                           p = 0.85,
                           list = FALSE)
beer.train <- pp.beer.data[idx,]
beer.test <- pp.beer.data[-idx,]
beer.test.nolabel <- select(beer.test, -abv)
