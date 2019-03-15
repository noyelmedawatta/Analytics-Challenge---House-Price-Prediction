setwd("E:/WS_R/DPA/Home Price")

#install.packages("magrittr") # only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)    # alternative, this also loads %>%

# Read house price data
dataSet = read.csv("house_market_value.csv",header = TRUE)
# Remove $ marks
dataSet$Market.Value = as.numeric(gsub("[\\$,]","", dataSet$Market.Value))
# Remove commas
dataSet$Square.Feet = as.numeric(gsub("[\\,]","", dataSet$Square.Feet))
# View Summary
summary(dataSet)

# Finding Outliers
par(mfrow=c(1,3))
boxplot(dataSet$House.Age, xlab="House Age", ylab="Years", col = "Blue")
boxplot(dataSet$Square.Feet, xlab="House Area", ylab="Squre Feets", col = "Green")
boxplot(dataSet$Market.Value, xlab="House Market Value", ylab="$", col = "Red")
par(mfrow=c(1,1))

nrow(dataSet)
dataSet %>% distinct(dataSet$Square.Feet, dataSet$Market.Value, dataSet$House.Age, .keep_all = TRUE)
nrow(dataSet)
houseData <- subset(dataSet, dataSet$Market.Value < 110000)
nrow(houseData)
houseData <- subset(houseData, houseData$Square.Feet < 2200)
nrow(houseData)

# Check if outliers exits
par(mfrow=c(1,3))
boxplot(houseData$House.Age, xlab="House Age", ylab="Years", col = "Blue")
boxplot(houseData$Square.Feet, xlab="House Area", ylab="Squre Feets", col = "Green")
boxplot(houseData$Market.Value, xlab="House Market Value", ylab="$", col = "Red")
par(mfrow=c(1,1))


par(mfrow=c(1,2))
plot(houseData$Market.Value,houseData$House.Age, ylab = "House Market Value", xlab="House Age", col = "Blue")
plot(houseData$Market.Value,houseData$Square.Feet, ylab = "House Market Value", xlab="House Area", col = "Red")
par(mfrow=c(1,1))

# Find Mode
findMode <- function(x){
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x,uniqx)))]
}
findMode(houseData$House.Age)
findMode(houseData$Square.Feet)
findMode(houseData$Market.Value)

# Find standard deviation
sd(houseData$House.Age)
sd(houseData$Square.Feet)
sd(houseData$Market.Value)

par(mfrow=c(1,3))
hist(houseData$House.Age, ylab="Frequency", xlab = "House Age", main = "Age Vs Frequency", col=rainbow(12))
hist(houseData$Square.Feet, ylab="Frequency", xlab = "House Area", main = "Area Vs Frequency", col=rainbow(12))
hist(houseData$Market.Value, ylab="Frequency", xlab = "Market Value", main = "Market Value Vs Frequency", col=rainbow(12))
par(mfrow=c(1,1))

mean_age <- mean(houseData$House.Age)
sd_age <- sd(houseData$House.Age)
mean_area <- mean(houseData$Square.Feet)
sd_area <- sd(houseData$Square.Feet)

cov_age = sd_age/mean_age
cov_area = sd_area/mean_area
cov_age
cov_area

# Check corelations
cor(houseData)

# Plot the house data
plot(houseData, col= rainbow(12))


# Covariance between each Age and Feet
#cov(houseData$House.Age,houseData$Square.Feet)
cov(houseData$Square.Feet,houseData$Market.Value)
cov(houseData$House.Age,houseData$Market.Value)

par(mfrow=c(1,2))
plot(houseData$Market.Value,houseData$House.Age, ylab = "House Market Value", xlab="House Age", col = "Blue")
plot(houseData$Market.Value,houseData$Square.Feet, ylab = "House Market Value", xlab="House Area", col = "Red")
par(mfrow=c(1,1))

# Linear Regression Analysis
#============================================================
price= houseData$Market.Value
age= houseData$House.Age
area = houseData$Square.Feet

sd(area, na.rm = FALSE)
sd(age, na.rm = FALSE)

newHoseAreas = c(1650,1500,1800,2200,2400)
newHouseAge = c(26,28,29,30,31)

# Simple Linear Regression Analysis | x=area, y=market value
#============================================================

slm_1.houseData = lm(price~area)
summary(slm_1.houseData)

newHouseData = data.frame(area=newHoseAreas, age = newHouseAge)
predictedHousePrices = predict(slm_1.houseData, newHouseData, level = 0.95, interval = "confidence")

predictedHousePrices

par(mfrow=c(1,2))
plot(area,price,col="Blue", xlab = "House Area", ylab = "Market Value", main = "Simple Regression - H.Area Vs M.Value")
abline(slm_1.houseData)
plot(newHoseAreas,predictedHousePrices[,1],col="Red", xlab = "House Area", ylab = "Market Value", main = "Simple Regression - H.Area Vs M.Value")
abline(slm_1.houseData)
par(mfrow=c(1,1))


# Simple Linear Regression Analysis | x=age, y=market value
#============================================================

slm_2.houseData = lm(price~age)
summary(slm_2.houseData)

newHoseData = data.frame(area=newHoseAreas, age = newHouseAge)
predictedHousePrices = predict(slm_1.houseData, newHoseData, level = 0.95, interval = "confidence")

predictedHousePrices

par(mfrow=c(1,2))
plot(area,price,col="Blue", xlab = "House Age", ylab = "Market Value", main = "Simple Regression - H.Age Vs M.Value")
abline(slm_2.houseData)
plot(newHoseAreas,predictedHousePrices[,1],col="Red", xlab = "House Age", ylab = "Market Value", main = "Simple Regression - H.Age Vs M.Value")
abline(slm_2.houseData)
par(mfrow=c(1,1))

# Multiple Linear Regression Analysis | x1=age, x2=area, y=market value
#============================================================

lm_3.houseData = lm(price~age+area)
summary(slm_2.houseData)

newHoseData = data.frame(area=newHoseAreas, age = newHouseAge)
predictedHousePrices = predict(lm_3.houseData, newHoseData, level = 0.95, interval = "confidence")

predictedHousePrices

par(mfrow=c(1,2))
plot(area,price,col="Blue", xlab = "House Age & Area", ylab = "Market Value", main = "Multiple Regression - H.Age, H.Area Vs M.Value")
abline(lm_3.houseData)
plot(newHoseAreas,predictedHousePrices[,1],col="Red", xlab = "House Age & Area", ylab = "Market Value", main = "Multiple Regression - H.Age, H.Area Vs M.Value")
abline(lm_3.houseData)
par(mfrow=c(1,1))