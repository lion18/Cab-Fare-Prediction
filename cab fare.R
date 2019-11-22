#################################### CAB FARE PREDICTION ################################################


#Clean the environment
rm(list = ls(all=T))

#Set Working Directory
setwd("C:/Users/shrid/Downloads/edWisor/Project 2")
getwd()

#Load Libraries
library("readr")
library("plyr")
library("dplyr")
library("DMwR")
library("ggplot2")
library("corrgram")
library("rlist")
library("purrr")
library("geosphere")
library("car")
library("randomForest")

                          ###LOAD THE DATA###

#Load the training data and have a glimpse of the data 
train = read.csv(file = "train_cab.csv", header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
head(train)
class(train)
names(train)
str(train) # Checking for structure of the data
summary(train)

#Convert variables into appropriate datatypes
train$fare_amount <- as.numeric(as.character(train$fare_amount))
typeof(train$fare_amount)

summary(train) 
            
                            ###DATA PRE-PROCESSING###

#Missing value Ananlysis

#Let's check for missing values
missing_values = sapply(train, function(x){sum(is.na(x))})
missing_values
#We have 25 missing values in fare_amount and 55 in passenger_count

#Let's check it in terms of percentage
missing_values_percentage = (missing_values*100/nrow(train))
missing_values_percentage
#less than 30% so no need to delete any dependent variables. Let's just delete rows for simplicity

#Let's drop the rows with missing values for simplicity
train <- na.omit(train)
train
str(train)

missing_values = sapply(train, function(x){sum(is.na(x))})
missing_values
#We do not see any missing values after clean up


#Feature Engineering

#Let's draw features out of datetime stamp

train$pickup_datetime <- gsub(' UTC', '', train$pickup_datetime)
train$pickup_datetime

train$date <- as.Date(train$pickup_datetime)
train$date

train$year <- substr(train$pickup_datetime,1,4)
train$year

train$month <- substr(train$pickup_datetime,6,7)
train$month

train$day <- weekdays(as.POSIXct(train$date), abbreviate = F)
train$day

train$date <- substr(train$date, 9, 10)
train$date

train$hour <- substr(train$pickup_datetime, 12, 13)
train$hour

class(train$month)

#Let's check for missing values after Feature Engineering

missing_values = sapply(train, function(x){sum(is.na(x))})
missing_values

#There is one Missing row, let's remove that
train = na.omit(train)

missing_values = sapply(train, function(x){sum(is.na(x))})
missing_values

#Outlier Analysis

#Outlier Ananlysis for continous variables
numeric_data <- subset(train, select = c(fare_amount,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,passenger_count))
cnames <- colnames(numeric_data)
cnames

summary(numeric_data)

#Boxplot of all numeric variables

for(i in 1:length(cnames))
  {
  assign(paste0("b",i), ggplot(aes_string(y = cnames[i]), data = train) + 
    stat_boxplot(geom = "errorbar", width = 0.5) +
    geom_boxplot(outlier.color = "red", fill = "green", outlier.shape = 16, outlier.size = 1, notch = FALSE)+
      theme(legend.position = "bottom") + labs(cnames[i])+
      ggtitle(paste("Box plot for", cnames[i]))
    )
}

gridExtra::grid.arrange(b1,b2,b3,b4,b5,b6,ncol=2)

#Let's treat the Outliers

#Fare amount
summary(train$fare_amount)

Q1 <- quantile(train$fare_amount, 0.25)
Q1#Quantile 1
Q3 <- quantile(train$fare_amount, 0.75)
Q3#Quantile 3
UL <- Q3 + (1.5*IQR(train$fare_amount))
UL#Upper Limit
LL <- Q1 - (1.5*IQR(train$fare_amount))
LL#Lower limit

#Let's replace the values outside of range with Lower Limit and Upper Limit
train[train$fare_amount < LL, "fare_amount"] <- LL
train[train$fare_amount > UL, "fare_amount"] <- UL

summary(train$fare_amount)

#Since the fare can not be in negative and zero, let's replace those values with NA
train$fare_amount[train$fare_amount <= 0.01] <- NA

#Let's check the NA values and remove them
sum(is.na(train$fare_amount))
train <- na.omit(train)

#Pick up longitude
summary(train$pickup_longitude)

Q1 <- quantile(train$pickup_longitude, 0.25)
Q1
Q3 <- quantile(train$pickup_longitude, 0.75)
Q3
UL <- Q3 + (1.5*IQR(train$pickup_longitude))
UL
LL <- Q1 - (1.5*IQR(train$pickup_longitude))
LL

train[train$pickup_longitude < LL, "pickup_longitude"] <- LL
train[train$pickup_longitude > UL, "pickup_longitude"] <- UL

summary(train$pickup_longitude)

#Pick up lattitude
summary(train$pickup_latitude)

Q1 <- quantile(train$pickup_latitude, 0.25)
Q1
Q3 <- quantile(train$pickup_latitude, 0.75)
Q3
UL <- Q3 + (1.5*IQR(train$pickup_latitude))
UL
LL <- Q1 - (1.5*IQR(train$pickup_latitude))
LL

train[train$pickup_latitude < LL, "pickup_latitude"] <- LL
train[train$pickup_latitude > UL, "pickup_latitude"] <- UL

summary(train$pickup_latitude)

#Drop off Longitude

summary(train$dropoff_longitude)

Q1 <- quantile(train$dropoff_longitude, 0.25) 
Q1
Q3 <- quantile(train$dropoff_longitude, 0.75)
Q3
UL <- Q3 + (1.5*IQR(train$dropoff_longitude))
UL
LL <- Q1 - (1.5*IQR(train$dropoff_longitude))
LL

train[train$dropoff_longitude < LL, "dropoff_longitude"] <- LL
train[train$dropoff_longitude > UL, "dropoff_longitude"] <- UL

summary(train$dropoff_longitude)

#Drop off latitude

summary(train$dropoff_latitude)

Q1 <- quantile(train$dropoff_latitude, 0.25)
Q1
Q3 <- quantile(train$dropoff_latitude, 0.75)
Q3
UL <- Q3 + (1.5*IQR(train$dropoff_latitude))
UL
LL <- Q1 - (1.5*IQR(train$dropoff_latitude))
LL

train[train$dropoff_latitude < LL, "dropoff_latitude"] <- LL
train[train$dropoff_latitude > UL, "dropoff_latitude"] <- UL

summary(train$dropoff_latitude)

#Passenger count

summary(train$passenger_count)

Q1 <- quantile(train$passenger_count, 0.25)
Q1
Q3 <- quantile(train$passenger_count, 0.75)
Q3
UL <- Q3 + (1.5*IQR(train$passenger_count))
UL
LL <- Q1 - (1.5*IQR(train$passenger_count))
LL

train[train$passenger_count < 0, "passenger_count"] <- 0
train[train$passenger_count > 6, "passenger_count"] <- 6 #As there can be a max of 6 passengers

#Since there should be at least 1 passenger for the trip to count, let's replace all the values less than 1 with NA
train$passenger_count[train$passenger_count < 1] <- NA

sum(is.na(train$passenger_count))

#Let's remove the missing values
train = na.omit(train)

sum(is.na(train$passenger_count))

#Let's visualize the boxplot after removing outliers

for(i in 1:length(cnames))
{
  assign(paste0("b",i), ggplot(aes_string(y = cnames[i]), data = train) + 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.color = "red", fill = "grey", outlier.shape = 16, outlier.size = 1, notch = FALSE)+
           theme(legend.position = "bottom") + labs(y=cnames[i])+
           ggtitle(paste("Box plot for", cnames[i]))
  )
}

gridExtra::grid.arrange(b1,b2,b3,b4,b5,b6,ncol=3)


#Let's calculate distance between 2 geo codes

distance1 = function(long1, lat1, long2, lat2){
  loadNamespace("purrr")
  loadNamespace("geosphere")
  
  l1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  l2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  
  dist = purrr::map2(l1, l2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list.extract(dist, position = 1) 
  
  distance = distance_m / 1000.0 ;#in km
  
  distance
}
  
for( i in (1:nrow(train)))
{
  train$distance[i] = distance1(train$pickup_longitude[i], train$pickup_latitude[i], train$dropoff_longitude[i], train$dropoff_latitude[i])
}

head(train)
str(train)


# Outliers for Distance

bp_dist = ggplot(aes_string( x = "distance", y = "fare_amount"), data = train) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(outlier.color = "red", fill = "green", outlier.shape = 16, outlier.size = 1, notch = FALSE) +
  ggtitle(paste("Boxplot for Distance with Outliers"))

bp_dist

summary(train$distance)

train$distance[train$distance < 1] <- mean(train$distance)

summary(train$distance)


# Exploratory data analysis

# Frequency distribution of Numeric variables

#Fare amount
a1 <- ggplot(data = train, aes( x = train$fare_amount)) + geom_histogram(fill = "royalblue", bins = 20) +
  labs(x = "Fare Amount", y = "Frequency") + ggtitle("Distribution of Fare Amount")
a1

#Pick up Longitude
a2 <- ggplot(data = train, aes( x = train$pickup_longitude)) + geom_histogram( fill = "lightgreen") +
  labs(x = "Pick up Longitude", y = "Frequency") + ggtitle("Distribution of Pick Up Longitude")
a2

#Pick up Latitude
a3 <- ggplot(data = train, aes( x = train$pickup_latitude)) + geom_histogram(fill = "green") +
  labs(x = "Pick up Latitude", y = "Frequency") + ggtitle("Distribution of Pick Up Latitude")
a3

#Drop off Longitude
a4 <- ggplot(data = train, aes( x = train$dropoff_longitude)) + geom_histogram( fill = "springgreen") +
  labs(x = "Drop off Longitude", y = "Frequency") + ggtitle("Distribution of Drop off Longitude")
a4

#Drop off Latitude
a5 <- ggplot(data = train, aes( x = train$dropoff_latitude)) + geom_histogram( fill = "limegreen") +
  labs(x = "Drop off Latitude", y = "Frequency") + ggtitle("Distribution of Drop off Latitude")
a5

#Passenger Count
a6 <- ggplot(data = train, aes( x = train$passenger_count)) + geom_histogram(binwidth = 1, fill = "powderblue") + 
  labs(x = "Passenger count", y = "Frequency") + ggtitle("Distribution of Passenger count")
a6

#Distance
a7 <- ggplot(data = train, aes( x = train$distance)) + geom_histogram(binwidth = 1, fill = "navy") +
  labs(x = "Distance", y = "Frequency") + ggtitle("Distribution of Distance")
a7#distance is right skewed. Let's remove skewness by applying log

logof10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

train$distance = logof10(train$distance)

#Distance after Log transformation
a7 <- ggplot(data = train, aes( x = train$distance)) + geom_histogram(binwidth = 0.1, fill = "navy") +
  labs(x = "Distance", y = "Frequency") + ggtitle("Distribution of Distance after Log Transformation")
a7



#Categorical variables

#Fare amount vs Year
b1 <- ggplot(train, aes(train$year, train$fare_amount)) + geom_bar(stat = "identity", fill = "yellow") +
  labs( x = "Year", y = "Fare Amount") + ggtitle("Fare Amount vs Year")
b1

#Fare Amount vs Month
b2 <- ggplot(train, aes(train$month, train$fare_amount)) + geom_bar(stat = "identity", fill = "khaki") +
  labs( x = "Month", y = "Fare Amount") + ggtitle("Fare Amount vs Month")
b2

#Fare Amount vs Day
b3 <- ggplot(train, aes(train$day, train$fare_amount)) + geom_bar(stat = "identity", fill = "orange") +
  labs( x = "Day", y = "Fare Amount") + ggtitle("Fare Amount vs Day")
b3

#Fare Amount vs Hour

b4 <- ggplot(train, aes(train$hour, train$fare_amount)) + geom_bar(stat = "identity", fill = "peru") +
  labs( x = "Hour", y = "Fare Amount") + ggtitle("Fare Amount vs Hour")
b4


#Correlation plot

c1 <- corrgram(train, order = FALSE, main = "Correlation plot")
c1

c2 <- corrgram(train, order = FALSE, main = "Correlation plot", panel = panel.cor)
c2


#ANOVA for categorical variables

categorical <- c("date", "year", "month", "day", "hour")

for(i in categorical){
  print(i)
  anova = summary(aov(fare_amount~train[,i],train))
  print(anova)
}
#p > 0.05 for day and date. So let's remove those variables

names(train)

data1 <- subset(train, select = c(fare_amount,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,passenger_count,year,month,hour,distance))
data1

#Let's create train and test dataset

index = sample(1:nrow(data1), as.integer(0.8*nrow(data1)) )

train_data = data1[index,]
test_data = data1[-index,]

#Linear regression
model1 = lm(fare_amount~., train_data)
summary(model1)

par(mfrow = c(2,2))
plot(model1)


vif(model1) #less than 5

dwt(model1)

prediction1 <- predict(model1, test_data[,-1])

df = data.frame("actual" = test_data[,1], "pred" = prediction1)
df

MAPE = function(y, yhat){
  mean(abs((y-yhat)/y))
}

MAPE(test_data[,1],prediction1)

#Random Forest

model2 <- randomForest(fare_amount~., train_data, tree = 500)

summary(model2)

prediction2 <- predict(model2, test_data[-1])

df = cbind(df, prediction2)
df


MAPE(test_data[,1],prediction2)


#Model Evaluation

test <- read.csv(file = "test.csv", header = TRUE, sep = ",", na.strings = c("", " ", "NA"))

head(test)
str(test)
summary(test)

#Feature engineering

test$pickup_datetime <- gsub(" UTC", "", test$pickup_datetime)

test$date <- as.Date(test$pickup_datetime)

test$year <- substr(as.character(test$pickup_datetime),1,4)

test$month <- substr(as.character(test$pickup_datetime),6,7)

test$day <- weekdays.POSIXt(test$date, abbreviate = FALSE)

test$date <- substr(as.character(test$date),9,10)

test$hour <- substr(as.factor(test$pickup_datetime), 12, 13)

str(test)

# Data preprocessing

#Missing value

sum(is.na(test))

#Outlier
summary(test)

#distance


for( i in (1:nrow(test)))
{
  test$distance[i] = distance1(test$pickup_longitude[i], test$pickup_latitude[i], test$dropoff_longitude[i], test$dropoff_latitude[i])
}

head(test)
summary(test$distance)

test$distance[test$distance < 1] = mean(test$distance)

#Distribution of distance

z <- ggplot(test, aes_string(x = test$distance)) + geom_histogram() + geom_density()
z


test$distance <- logof10(test$distance)

summary(test$distance)


#Modelling

Final <- subset(test, select = c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,passenger_count,year,month,hour,distance   ))

Final_model <- randomForest(fare_amount~. , data1, ntree = 500, method = "anova")

predictions <- predict(Final_model, Final)

predictions

test$fare_amount <- predictions

summary(test)
summary(test$fare_amount)

head(test)
