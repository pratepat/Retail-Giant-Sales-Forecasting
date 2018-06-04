#---------------------------------------------------------------------------------------#
############################ Global Mart TimeSeries Forecasting #########################
#---------------------------------------------------------------------------------------#
# 1. Business Objective
# 2. Data Understanding
# 3. Data Preparation and EDA
# 5. Model Building - Classical decomposition
#     5.1 Consumer EU Sales Forecasting
#     5.2 Consumer EU Quantity Forecasting
#     5.3 Consumer APAC Sales Forecasting
#     5.4 Consumer APAC Quantity Forecasting
# 6. Model Building - ARIMA
#     6.1 Consumer EU Sales Forecasting
#     6.2 Consumer EU Quantity Forecasting
#     6.3 Consumer APAC Sales Forecasting
#     6.4 Consumer APAC Quantity Forecasting
# 7. Final Conclusion

## Load relevant libraries
library(dplyr)
library(stringr)
library(graphics)
library(forecast)
library(ggplot2)
library(tseries)
library(gridExtra)

sales_data <- read.csv("Global Superstore.csv",stringsAsFactors = FALSE)
#---------------------------------------------------------------------------------------#
################ 1. Business Objective: #################################################
#---------------------------------------------------------------------------------------#

# The objective is to forecast sales and quantity (demand) of the top two most profitable 
# and consistent segments for "Global Mart".

#---------------------------------------------------------------------------------------#
################ 2. Data Understanding: #################################################
#---------------------------------------------------------------------------------------#

head(sales_data)
# The data seems to be transactional data
# Contains daily order information

str(sales_data)
# Order date is currently a char column
# Need to convert it to date to extract 

dim(sales_data)
# No of transactions: 51290
# No of Attributes: 24

## Check for NA 
sapply(sales_data, function(y) sum(length(which(is.na(y)))))
# Postal code has a lot of NA values (41296)

# Check for discrete values
sapply(sales_data, function(y) sum(length(unique(y))))

# We are interested in the Market and Segment
# 3 Segments and 7 Market

#---------------------------------------------------------------------------------------#
################ 3. Data Preparation and EDA: ###########################################
#---------------------------------------------------------------------------------------#

## Convert the date columns to date data type
sales_data$Order.Date <- as.POSIXct(strptime(sales_data$Order.Date,"%d-%m-%Y"))
sales_data$Ship.Date <- as.POSIXct(strptime(sales_data$Ship.Date,"%d-%m-%Y"))

# Order the data based on order date as we need to create time series
sales_data <- sales_data %>%
  arrange(Order.Date)

# Extract the order month
sales_data$Order.Month <- as.numeric(format(sales_data$Order.Date , "%m"))
sales_data$Order.Year <- as.numeric(format(sales_data$Order.Date , "%Y"))

## Lets divide the data into 3 segments and further divide the data into 7 markets
unique(sales_data$Segment)

# "Consumer"    "Corporate"   "Home Office"

unique(sales_data$Market)

# "US"     "APAC"   "EU"     "Africa" "EMEA"   "LATAM"  "Canada"

#---------------------------------------------------------------------------#
# Create data sets for 21 segments
# Loop creates 21 data frames for each segment and Market
for(i in unique(sales_data$Segment)) {
  for(j in unique(sales_data$Market)) {
    
    df <- sales_data %>%
      filter(Segment == i & Market == j)
    
    assign(paste("data",str_replace(i," ",""),j,sep = ".") , df)
  }
  
}

#---------------------------------------------------------------------------#
## Finding the Coefficient of variation for all 21 datasets
r <- 1
c <- 1
Coeff_Variation <- data.frame()

for(i in unique(sales_data$Segment)) {
  for(j in unique(sales_data$Market)) {
  
    
    df <- paste("data",str_replace(i," ",""),j,sep = ".")
    df.name <- df
    df <- get(df)
    
    df <- df %>%
      select(Order.Month,Order.Year,Sales,Quantity,Profit) %>%
      group_by(Order.Month,Order.Year) %>%
      summarise(Sales = sum(Sales),Quantity = sum(Quantity),Profit = sum(Profit)) %>%
      arrange(Order.Year,Order.Month)
    
    
    Coeff_Variation[r,c] <- str_split(df.name[1], "data.")[[1]][2] 
    Coeff_Variation[r,c+1] <- mean(df$Sales)
    Coeff_Variation[r,c+2] <- sum(df$Profit)
    Coeff_Variation[r,c+3] <- sd(df$Profit)/mean(df$Profit)*100
    
    r <- r + 1
  }
  
}
colnames(Coeff_Variation) <- c("Segment","Average Sales","Total Profit","Coefficient of variation")

Coeff_Variation %>%
  arrange(`Coefficient of variation`)

#---------------------------------------------------------------------------#
# Profit for each segment
ggplot(Coeff_Variation, aes(x = reorder(Segment, -`Total Profit`), 
                            y = `Total Profit`)) +
  geom_bar(stat = "identity", fill = "tomato2") +
  labs(title = "Profit for each segment", x = "Segments", y = "Total profit") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1,face="bold"), 
        axis.text.y = element_text(face="bold")) 

# Average Sale for each segment
ggplot(Coeff_Variation, aes(x = reorder(Segment, -`Average Sales`), 
                            y = `Average Sales`)) +
  geom_bar(stat = "identity",fill="blue") +
  labs(title = "Average Sale for each segment", x = "Segments", y = "Average Sales") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1,face="bold"), 
        axis.text.y = element_text(face="bold"))

# Coefficient of variation for each segment
ggplot(Coeff_Variation, aes(x = reorder(Segment, `Coefficient of variation`), 
                            y = `Coefficient of variation`)) +
  geom_bar(stat = "identity",fill="dark green") +
  labs(title = "Coefficient of variation for each segment", x = "Segments", y = "Coefficient of variation") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1,face="bold"), 
        axis.text.y = element_text(face="bold"))

## The 2 most cosistent and profitable segments are
# 1: Segment = Consumer and Market = EU >> Total Profit = 188687.707 >> Coeff of Variation = 62.43052
# 2: Segment = Consumer and Market = APAC >> Total Profit = 222817.560 >> Coeff of Variation = 63.21323

# data.Consumer.EU
# data.Consumer.APAC

#---------------------------------------------------------------------------#
## Create data frame for both
data.Consumer.EU <- data.Consumer.EU %>%
  select(Order.Month,Order.Year,Sales,Quantity,Profit) %>%
  group_by(Order.Month,Order.Year) %>%
  summarise(Sales = sum(Sales),Quantity = sum(Quantity),Profit = sum(Profit)) %>%
  arrange(Order.Year,Order.Month)

data.Consumer.APAC <- data.Consumer.APAC %>%
  select(Order.Month,Order.Year,Sales,Quantity,Profit) %>%
  group_by(Order.Month,Order.Year) %>%
  summarise(Sales = sum(Sales),Quantity = sum(Quantity),Profit = sum(Profit)) %>%
  arrange(Order.Year,Order.Month)

#---------------------------------------------------------------------------#
## Divide the data for Sales and Quantity to create a timeseries
# 1: Consumer EU
timevals <- c(1:nrow(data.Consumer.EU))

data.Consumer.EU.Sales <- as.data.frame(cbind(timevals, data.Consumer.EU$Sales))
colnames(data.Consumer.EU.Sales) <- c('Month', 'Sales')

data.Consumer.EU.Quantity <- as.data.frame(cbind(timevals, data.Consumer.EU$Quantity))
colnames(data.Consumer.EU.Quantity) <- c('Month', 'Quantity')

# 2: Consumer APAC
timevals <- c(1:nrow(data.Consumer.APAC))

data.Consumer.APAC.Sales <- as.data.frame(cbind(timevals, data.Consumer.APAC$Sales))
colnames(data.Consumer.APAC.Sales) <- c('Month', 'Sales')

data.Consumer.APAC.Quantity <- as.data.frame(cbind(timevals, data.Consumer.APAC$Quantity))
colnames(data.Consumer.APAC.Quantity) <- c('Month', 'Quantity')

#---------------------------------------------------------------------------#
## Divide the data into train and test for modelelling and testing
# 1: Consumer EU
train.Consumer.EU.Sales <- data.Consumer.EU.Sales[1:42,]
train.Consumer.EU.Quantity <- data.Consumer.EU.Quantity[1:42,]

test.Consumer.EU.Sales <- data.Consumer.EU.Sales[43:48,]
test.Consumer.EU.Quantity <- data.Consumer.EU.Quantity[43:48,]

# 2: Consumer APAC
train.Consumer.APAC.Sales <- data.Consumer.APAC.Sales[1:42,]
train.Consumer.APAC.Quantity <- data.Consumer.APAC.Quantity[1:42,]

test.Consumer.APAC.Sales <- data.Consumer.APAC.Sales[43:48,]
test.Consumer.APAC.Quantity <- data.Consumer.APAC.Quantity[43:48,]

#---------------------------------------------------------------------------#
## Create a data frame to store and compare MAPE values
MAPE.df <- data.frame(matrix(ncol = 3, nrow = 8))
colnames(MAPE.df) <- c("Segment", "Method", "MAPE")

## Create a data frame to store future predictions for Classical Decomposition
CD.Forecast.df <- data.frame(matrix(ncol = 3, nrow = 6))
colnames(CD.Forecast.df) <- c("Month","Method", "Consumer-EU-Sales")

## Create a data frame to store future predictions for Auto ARIAM
AA.Forecast.df <- data.frame(matrix(ncol = 3, nrow = 6))
colnames(AA.Forecast.df) <- c("Month","Method", "Consumer-EU-Sales")

#---------------------------------------------------------------------------------------#
################ 5. Model Building - Classical Decomposition: ###########################
#---------------------------------------------------------------------------------------#
## Create timeseries
# 1: Consumer EU
timeser.Consumer.EU.Sales <- ts(train.Consumer.EU.Sales$Sales)
plot(timeser.Consumer.EU.Sales, lwd=2)

timeser.Consumer.EU.Quantity <- ts(train.Consumer.EU.Quantity$Quantity)
plot(timeser.Consumer.EU.Quantity, lwd=2)

# 2: Consumer APAC
timeser.Consumer.APAC.Sales <- ts(train.Consumer.APAC.Sales$Sales)
plot(timeser.Consumer.APAC.Sales, lwd=2)

timeser.Consumer.APAC.Quantity <- ts(train.Consumer.APAC.Quantity$Quantity)
plot(timeser.Consumer.APAC.Quantity, lwd=2)

#---------------------------------------------------------------------------#
## 5.1. Consumer EU Sales Forecasting ##
#---------------------------------------------------------------------------#

# Lets start with decomposing the series and analysing the trend and seasonality
timeser.Consumer.EU.Sales.decompose <- ts(timeser.Consumer.EU.Sales,frequency = 12)
timeser.Consumer.EU.Sales.decompose <- decompose(timeser.Consumer.EU.Sales.decompose)
plot(timeser.Consumer.EU.Sales.decompose, lwd=2, col = "blue")

# Observations
# 1: The sales see and upward linear trend.
# 2: The seasonality has a sin and cos trend.

# Smoothing the series - Moving Average Smoothing
plot(timeser.Consumer.EU.Sales,xlab = "Months",ylab="Sales", lwd=2)
w <- 2
smoothedseries <- stats::filter(timeser.Consumer.EU.Sales, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeser.Consumer.EU.Sales)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
lines(smoothedseries, col="blue", lwd=2)

timevals_train <- c(1:nrow(train.Consumer.EU.Sales))
smooth.Consumer.EU.Sales <- as.data.frame(cbind(timevals_train, as.vector(smoothedseries)))
colnames(smooth.Consumer.EU.Sales) <- c('Month', 'Sales')

# Model the trend
lmfit <- lm(Sales ~ sin(Month*0.55) * poly(Month,2) + 
              cos(Month*0.55)*poly(Month,2) +
              sin(Month*0.1)*Month, data=smooth.Consumer.EU.Sales)

globalpred <- predict(lmfit)

lines(globalpred, col='red', type = "l", lwd=2)

#Now, let's inspect the local component of the time series
localpred <- timeser.Consumer.EU.Sales - globalpred
plot(localpred, col='red', type = "l", lwd=2)

#Now, let's make predictions for the local component of the time series
acf(localpred)
acf(localpred, type="partial")

# AR 0 amd MA 0 model

# fiiting the weak stationarity
armafit <- auto.arima(localpred)

tsdiag(armafit)
armafit
# ARIMA(0,0,0) with zero mean

armapred <- fitted(armafit)

class_dec_pred <- ts(globalpred)
# Plotting the model on actual time series
plot(timeser.Consumer.EU.Sales, col = "black", lwd=2)
lines(class_dec_pred, col = "red", lwd=2)

# Residual series
resi <- localpred - armapred
plot(resi, lwd=2)

# Testing for stationarity
adf.test(resi,alternative = "stationary")
# Lag order = 3, p-value = 0.01
# we have to reject the null hypothesis here and go with alternative hypothesis as p-value < 0.05
kpss.test(resi)
# Truncation lag parameter = 1, p-value = 0.1
# we have to accept the null hypothesis here, as p-value > 0.05 

# Both Dickey Fuller test and KPSS test show strong stationarity in residual series

## Evaluating the model
test.Consumer.EU.Sales
timevals_test <- test.Consumer.EU.Sales$Month

global_pred_test <- predict(lmfit,data.frame(Month = timevals_test))

fcast <- global_pred_test

# Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,test.Consumer.EU.Sales[,2])[5]
MAPE_class_dec

## Insert the MAPE value in DF
MAPE.df[1,] <- c("Consumer-EU-Sales","Classical Decomposition",round(MAPE_class_dec,2))

# Let's also plot the predictions along with original values, to
complete_pred <- c(ts(globalpred),ts(global_pred_test))

plot(ts(data.Consumer.EU.Sales$Sales), col = "black",xlab = "Months",ylab="Sales", lwd=2)
lines(complete_pred, col = "red", lwd=2)

# Future forecast
fcast_month <- c(49,50,51,52,53,54)
fcast.Consumer.EU.Sales <- predict(lmfit,data.frame(Month = fcast_month))
fcast.Consumer <- data.frame(fcast_month,as.vector(fcast.Consumer.EU.Sales))

colnames(fcast.Consumer) <- c("Month", "Sales Forecast")

fcast.Consumer

## Insert the future forecast value in DF
CD.Forecast.df$Month[1:6] <- c(49,50,51,52,53,54)
CD.Forecast.df$Method[1:6] <- c("Classical Decomposition")
CD.Forecast.df$`Consumer-EU-Sales`[1:6] <- as.vector(fcast.Consumer$`Sales Forecast`)

#---------------------------------------------------------------------------#
## 5.2. Consumer EU Quantity Forecasting ##
#---------------------------------------------------------------------------#

# Lets start with decomposing the series and analysing the trend and seasonality
timeser.Consumer.EU.Quantity.decompose <- ts(timeser.Consumer.EU.Quantity,frequency=12)
timeser.Consumer.EU.Quantity.decompose <- decompose(timeser.Consumer.EU.Quantity.decompose, type = c("multiplicative"))
plot(timeser.Consumer.EU.Quantity.decompose, lwd=2, col = "Blue")

# Observations
# 1: The sales see and upward linear trend.
# 2: The seasonality has a sin and cos trend.

# Smoothing the series - Moving Average Smoothing
plot(timeser.Consumer.EU.Quantity,xlab = "Months",ylab="Quantity", lwd=2)
w <- 2
smoothedseries <- stats::filter(timeser.Consumer.EU.Quantity, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeser.Consumer.EU.Quantity)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
lines(smoothedseries, col="blue", lwd=2)

timevals_train <- c(1:nrow(train.Consumer.EU.Quantity))
smooth.Consumer.EU.Quantity <- as.data.frame(cbind(timevals_train, as.vector(smoothedseries)))
colnames(smooth.Consumer.EU.Quantity) <- c('Month', 'Quantity')

# Model the trend
lmfit <- lm(Quantity ~ sin(0.6*Month) * poly(Month,2) + 
              cos(0.6*Month)*poly(Month,2) +
              sin(0.01*Month)*Month, data=smooth.Consumer.EU.Quantity)

globalpred <- predict(lmfit)

lines(globalpred, col='red', type = "l", lwd=2)

#Now, let's inspect the local component of the time series
localpred <- timeser.Consumer.EU.Quantity - globalpred
plot(localpred, col='red', type = "l", lwd=2)

#Now, let's make predictions for the local component of the time series
acf(localpred)
acf(localpred, type="partial")
armafit <- auto.arima(localpred)

tsdiag(armafit)
armafit

armapred <- fitted(armafit)

class_dec_pred <- ts(globalpred)
plot(timeser.Consumer.EU.Quantity, col = "black", lwd=2)
lines(class_dec_pred, col = "red", lwd=2)

resi <- localpred - armapred
plot(resi, lwd=2)

# Testing for stationarity
adf.test(resi,alternative = "stationary")
# Lag order = 3, p-value = 0.01118
# we have to reject the null hypothesis here and go with alternative hypothesis as p-value < 0.05
kpss.test(resi)
# Truncation lag parameter = 1, p-value = 0.1
# we have to accept the null hypothesis here, as p-value > 0.05 

# Both Dickey Fuller test and KPSS test show strong stationarity in residual series

## Evaluating the model
test.Consumer.EU.Quantity
timevals_test <- test.Consumer.EU.Quantity$Month

global_pred_test <- predict(lmfit,data.frame(Month = timevals_test))

fcast <- global_pred_test

# Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,test.Consumer.EU.Quantity[,2])[5]
MAPE_class_dec

## Insert the MAPE value in DF
MAPE.df[2,] <- c("Consumer-EU-Quantity","Classical Decomposition",round(MAPE_class_dec,2))

# Let's also plot the predictions along with original values, to
complete_pred <- c(ts(globalpred),ts(global_pred_test))

plot(ts(data.Consumer.EU.Quantity$Quantity), col = "black", lwd=2,xlab = "Months",ylab="Quantity")
lines(complete_pred, col = "red", lwd=2)

# Future forecast
fcast_month <- c(49,50,51,52,53,54)
fcast.Consumer.EU.Quantity <- predict(lmfit,data.frame(Month = fcast_month))
fcast.Consumer <- data.frame(fcast_month,as.vector(fcast.Consumer.EU.Quantity))
colnames(fcast.Consumer) <- c("Month", "Forecast Quantity")
fcast.Consumer

## Insert the future forecast value in DF
CD.Forecast.df <- cbind(CD.Forecast.df,fcast.Consumer$`Forecast Quantity`)
colnames(CD.Forecast.df)[4] <- c("Consumer-EU-Quantity")

#---------------------------------------------------------------------------#
## 5.3. Consumer APAC Sales Forecasting ##
#---------------------------------------------------------------------------#

# Lets start with decomposing the series and analysing the trend and seasonality
timeser.Consumer.APAC.Sales.decompose <- ts(timeser.Consumer.APAC.Sales,frequency=12)
timeser.Consumer.APAC.Sales.decompose <- decompose(timeser.Consumer.APAC.Sales.decompose)
plot(timeser.Consumer.APAC.Sales.decompose, lwd=2, col = "Blue")

# Observations
# 1: The sales see and upward linear trend.
# 2: The seasonality has a sin and cos trend.

# Smoothing the series - Moving Average Smoothing
plot(timeser.Consumer.APAC.Sales, lwd=2,xlab = "Months",ylab="Sales")
w <- 2
smoothedseries <- stats::filter(timeser.Consumer.APAC.Sales, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeser.Consumer.APAC.Sales)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
lines(smoothedseries, col="blue", lwd=2)

timevals_train <- c(1:nrow(train.Consumer.APAC.Sales))
smooth.Consumer.APAC.Sales <- as.data.frame(cbind(timevals_train, as.vector(smoothedseries)))
colnames(smooth.Consumer.APAC.Sales) <- c('Month', 'Sales')

# Model the trend
lmfit <- lm(Sales ~ sin(0.6*Month) * poly(Month,2) + 
              cos(0.6*Month)*poly(Month,2) +
              sin(Month)*Month, data=smooth.Consumer.APAC.Sales)

globalpred <- predict(lmfit)

lines(globalpred, col='red', type = "l", lwd=2)

#Now, let's inspect the local component of the time series
localpred <- timeser.Consumer.APAC.Sales - globalpred
plot(localpred, col='red', type = "l", lwd=2)

#Now, let's make predictions for the local component of the time series
acf(localpred)
acf(localpred, type="partial")
armafit <- auto.arima(localpred)

tsdiag(armafit)
armafit

armapred <- fitted(armafit)

class_dec_pred <- ts(globalpred)
plot(timeser.Consumer.APAC.Sales, col = "black", lwd=2)
lines(class_dec_pred, col = "red", lwd=2)

resi <- localpred - armapred
plot(resi, lwd=2)

## Testing for stationarity
adf.test(resi,alternative = "stationary")
# Lag order = 3, p-value = 0.01279
# we have to reject the null hypothesis here and go with alternative hypothesis as p-value < 0.05
kpss.test(resi)
# Truncation lag parameter = 1, p-value = 0.1
# we have to accept the null hypothesis here, as p-value > 0.05 

# Both Dickey Fuller test and KPSS test show strong stationarity in residual series

## Evaluating the model
test.Consumer.APAC.Sales
timevals_test <- test.Consumer.APAC.Sales$Month

global_pred_test <- predict(lmfit,data.frame(Month = timevals_test))

fcast <- global_pred_test

# Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,test.Consumer.APAC.Sales[,2])[5]
MAPE_class_dec

## Insert the MAPE value in DF
MAPE.df[3,] <- c("Consumer-APAC-Sales","Classical Decomposition",round(MAPE_class_dec,2))

# Let's also plot the predictions along with original values, to
complete_pred <- c(ts(globalpred),ts(global_pred_test))

plot(ts(data.Consumer.APAC.Sales$Sales), col = "black", lwd=2, xlab = "Months",ylab="Sales")
lines(complete_pred, col = "red", lwd=2)

# Future forecast
fcast_month <- c(49,50,51,52,53,54)
fcast.Consumer.APAC.Sales <- predict(lmfit,data.frame(Month = fcast_month))
fcast.Consumer <- data.frame(fcast_month,as.vector(fcast.Consumer.APAC.Sales))
colnames(fcast.Consumer) <- c("Month", "Forecast Sales")
fcast.Consumer

## Insert the future forecast value in DF
CD.Forecast.df <- cbind(CD.Forecast.df,fcast.Consumer$`Forecast Sales`)
colnames(CD.Forecast.df)[5] <- c("Consumer-APAC-Sales")

#---------------------------------------------------------------------------#
## 5.4. Consumer APAC Quantity Forecasting ##
#---------------------------------------------------------------------------#

# Lets start with decomposing the series and analysing the trend and seasonality
timeser.Consumer.APAC.Quantity.decompose <- ts(timeser.Consumer.APAC.Quantity,frequency=12)
timeser.Consumer.APAC.Quantity.decompose <- decompose(timeser.Consumer.APAC.Quantity.decompose)
plot(timeser.Consumer.APAC.Quantity.decompose, lwd=2, col = "Blue")

# Observations
# 1: The sales see and upward linear trend.
# 2: The seasonality has a sin and cos trend.

# Smoothing the series - Moving Average Smoothing
plot(timeser.Consumer.APAC.Quantity, lwd=2, xlab = "Months", ylab = "Quantity")
w <- 3
smoothedseries <- stats::filter(timeser.Consumer.APAC.Quantity, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeser.Consumer.APAC.Quantity)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
lines(smoothedseries, col="blue", lwd=2)

timevals_train <- c(1:nrow(train.Consumer.APAC.Quantity))
smooth.Consumer.APAC.Quantity <- as.data.frame(cbind(timevals_train, as.vector(smoothedseries)))
colnames(smooth.Consumer.APAC.Quantity) <- c('Month', 'Quantity')

# Model the trend
lmfit <- lm(Quantity ~ sin(0.4*Month) * poly(Month,2) + 
              cos(0.4*Month)*poly(Month,2) +
              sin(4*Month)*Month, data=smooth.Consumer.APAC.Quantity)

globalpred <- predict(lmfit)

lines(globalpred, col='red', type = "l", lwd=2)

#Now, let's inspect the local component of the time series
localpred <- timeser.Consumer.APAC.Quantity - globalpred
plot(localpred, col='red', type = "l", lwd=2)

#Now, let's make predictions for the local component of the time series
acf(localpred)
acf(localpred, type="partial")
armafit <- auto.arima(localpred)

tsdiag(armafit)
armafit

armapred <- fitted(armafit)

class_dec_pred <- ts(globalpred)
plot(timeser.Consumer.APAC.Quantity, col = "black", lwd=2)
lines(class_dec_pred, col = "red", lwd=2)

resi <- localpred - armapred
plot(resi, lwd=2)

## Testing for stationarity
adf.test(resi,alternative = "stationary")
# Lag order = 3, p-value = 0.01
# we have to reject the null hypothesis here and go with alternative hypothesis as p-value < 0.05
kpss.test(resi)
# Truncation lag parameter = 1, p-value = 0.1
# we have to accept the null hypothesis here, as p-value > 0.05 

# Both Dickey Fuller test and KPSS test show strong stationarity in residual series 

## Evaluating the model
test.Consumer.APAC.Quantity
timevals_test <- test.Consumer.APAC.Quantity$Month

global_pred_test <- predict(lmfit,data.frame(Month = timevals_test))

fcast <- global_pred_test

# Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,test.Consumer.APAC.Quantity[,2])[5]
MAPE_class_dec

## Insert the MAPE value in DF
MAPE.df[4,] <- c("Consumer-APAC-Quantity","Classical Decomposition",round(MAPE_class_dec,2))

# Let's also plot the predictions along with original values, to
complete_pred <- c(ts(globalpred),ts(global_pred_test))

plot(ts(data.Consumer.APAC.Quantity$Quantity), col = "black", lwd=2, xlab = "Months", ylab = "Quantity")
lines(complete_pred, col = "red", lwd=2)

# Future forecast
fcast_month <- c(49,50,51,52,53,54)
fcast.Consumer.APAC.Quantity <- predict(lmfit,data.frame(Month = fcast_month))
fcast.Consumer <- data.frame(fcast_month,as.vector(fcast.Consumer.APAC.Quantity))
colnames(fcast.Consumer) <- c("Month", "Forecast Quantity")
fcast.Consumer

## Insert the future forecast value in DF
CD.Forecast.df <- cbind(CD.Forecast.df,fcast.Consumer$`Forecast Quantity`)
colnames(CD.Forecast.df)[6] <- c("Consumer-APAC-Quantity")

#---------------------------------------------------------------------------------------#
################ 6. Model Building - Auto ARIMA: ########################################
#---------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------#
## 6.1 Consumer EU Sales Forecasting ##
#---------------------------------------------------------------------------#

plot(timeser.Consumer.EU.Sales, lwd=2, xlab = "Months", ylab = "Sales")

## Building final model in R
autoarimafit <- auto.arima(timeser.Consumer.EU.Sales)
autoarimafit

tsdiag(autoarimafit)
in_data_pred <-fitted(autoarimafit)

## Plot the actual and predicted values
plot(autoarimafit$x, col="black", lwd=2,main = "Actual V/S Predicted", xlab = "Months", ylab = "Sales")
lines(in_data_pred, col="red", lwd=2)

## Testing if the residual series is white noise
resi_auto_arima <- timeser.Consumer.EU.Sales - fitted(autoarimafit)

## Testing for stationarity
adf.test(resi_auto_arima,alternative = "stationary")
# Lag order = 3, p-value = 0.01
# we have to reject the null hypothesis here and go with alternative hypothesis as p-value < 0.05
kpss.test(resi_auto_arima)
# Truncation lag parameter = 1, p-value = 0.1
# we have to accept the null hypothesis here, as p-value > 0.05 

# Both Dickey Fuller test and KPSS test show strong stationarity in residual series

## Test the model
fcast_auto_arima <- predict(autoarimafit, n.ahead = 6)

outdata <- data.Consumer.EU.Sales$Sales[43:48]

## Evaluate the model using MAPE
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima

## Insert the MAPE value in DF
MAPE.df[5,] <- c("Consumer-EU-Sales","Auto ARIMA",round(MAPE_auto_arima,2))

## Lastly, let's plot the predictions along with original values, to
## get a visual feel of the fit
total_timeser <- ts(data.Consumer.EU.Sales$Sales)

auto_arima_pred <- c(fitted(autoarimafit),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black", lwd=2, xlab = "Months", ylab = "Sales")
lines(auto_arima_pred, col = "red", lwd=2)

## Future Forecast for months 49 to 54 (Jan 2015 to June 2015) 
autoarima_total <- auto.arima(total_timeser)
tsdiag(autoarima_total)
autoarima_forecast <- predict(autoarima_total,n.ahead = 6)
plot(forecast(autoarima_total,h=6), lwd=2, xlab = "Months", ylab = "Sales")

## Insert the future forecast value in DF
AA.Forecast.df$Month[1:6] <- c(49,50,51,52,53,54)
AA.Forecast.df$Method[1:6] <- c("Auto ARIMA")
AA.Forecast.df$`Consumer-EU-Sales`[1:6] <- as.vector(autoarima_forecast$pred)

#---------------------------------------------------------------------------#
## 6.2 Consumer EU Quantity Forecasting ##
#---------------------------------------------------------------------------#
plot(timeser.Consumer.EU.Quantity, lwd=2)

## Building final model in R
autoarimafit <- auto.arima(timeser.Consumer.EU.Quantity)
autoarimafit

tsdiag(autoarimafit)
in_data_pred <-fitted(autoarimafit)

## Plot the actual and predicted values
plot(autoarimafit$x, col="black", lwd=2,main = "Actual V/S Predicted", xlab = "Months", ylab = "Quantity")
lines(in_data_pred, col="red", lwd=2)

## Testing if the residual series is white noise
resi_auto_arima <- timeser.Consumer.EU.Quantity - fitted(autoarimafit)

## Testing for stationarity
adf.test(resi_auto_arima,alternative = "stationary")
# Lag order = 3, p-value = 0.04521
# we have to reject the null hypothesis here and go with alternative hypothesis as p-value < 0.05
kpss.test(resi_auto_arima)
# Truncation lag parameter = 1, p-value = 0.1
# we have to accept the null hypothesis here, as p-value > 0.05 

# Both Dickey Fuller test and KPSS test show strong stationarity in residual series

## Test the model
fcast_auto_arima <- predict(autoarimafit, n.ahead = 6)

outdata <- data.Consumer.EU.Quantity$Quantity[43:48]

## Evaluate the model using MAPE
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima

## Insert the MAPE value in DF
MAPE.df[6,] <- c("Consumer-EU-Quantity","Auto ARIMA",round(MAPE_auto_arima,2))

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(data.Consumer.EU.Quantity$Quantity)

auto_arima_pred <- c(fitted(autoarimafit),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black", lwd=2)
lines(auto_arima_pred, col = "red", lwd=2)

## Future Forecast for months 49 to 54 (Jan 2015 to June 2015) 
autoarima_total <- auto.arima(total_timeser)
tsdiag(autoarima_total)
autoarima_forecast <- predict(autoarima_total,n.ahead = 6)
plot(forecast(autoarima_total,h=6), lwd=2)

## Insert the future forecast value in DF
AA.Forecast.df <- cbind(AA.Forecast.df,as.data.frame(autoarima_forecast$pred))
colnames(AA.Forecast.df)[4] <- c("Consumer-EU-Quantity")

#---------------------------------------------------------------------------#
## 6.3 Consumer APAC Sales Forecasting ##
#---------------------------------------------------------------------------#

plot(timeser.Consumer.APAC.Sales, lwd=2)

## Building final model in R
autoarimafit <- auto.arima(timeser.Consumer.APAC.Sales)
autoarimafit

tsdiag(autoarimafit)
in_data_pred <-fitted(autoarimafit)

## Plot the actual and predicted values
plot(autoarimafit$x, col="black", lwd=2,main = "Actual V/S Predicted", xlab = "Months", ylab = "Sales")
lines(in_data_pred, col="red", lwd=2)

## Testing if the residual series is white noise
resi_auto_arima <- timeser.Consumer.APAC.Sales - fitted(autoarimafit)

## Testing for stationarity
adf.test(resi_auto_arima,alternative = "stationary")
# Lag order = 3, p-value = 0.01
# we have to reject the null hypothesis here and go with alternative hypothesis as p-value < 0.05
kpss.test(resi_auto_arima)
# Truncation lag parameter = 1, p-value = 0.1
# we have to accept the null hypothesis here, as p-value > 0.05 

# Both Dickey Fuller test and KPSS test show strong stationarity in residual series

## Test the model
fcast_auto_arima <- predict(autoarimafit, n.ahead = 6)

outdata <- data.Consumer.APAC.Sales$Sales[43:48]

## Evaluate the model using MAPE
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima

## Insert the MAPE value in DF
MAPE.df[7,] <- c("Consumer-APAC-Sales","Auto ARIMA",round(MAPE_auto_arima,2))

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(data.Consumer.APAC.Sales$Sales)

auto_arima_pred <- c(fitted(autoarimafit),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black", lwd=2)
lines(auto_arima_pred, col = "red", lwd=2)

## Future Forecast for months 49 to 54 (Jan 2015 to June 2015) 
autoarima_total <- auto.arima(total_timeser)
tsdiag(autoarima_total)
autoarima_forecast <- predict(autoarima_total,n.ahead = 6)
plot(forecast(autoarima_total,h=6), lwd=2)

## Insert the future forecast value in DF
AA.Forecast.df <- cbind(AA.Forecast.df,as.data.frame(autoarima_forecast$pred))
colnames(AA.Forecast.df)[5] <- c("Consumer-APAC-Sales")

#---------------------------------------------------------------------------#
## 6.4 Consumer APAC Quantity Forecasting ##
#---------------------------------------------------------------------------#

plot(timeser.Consumer.APAC.Quantity, lwd=2)

## Building final model in R
autoarimafit <- auto.arima(timeser.Consumer.APAC.Quantity)
autoarimafit

tsdiag(autoarimafit)
in_data_pred <-fitted(autoarimafit)

## Plot the actual and predicted values
plot(autoarimafit$x, col="black", lwd=2,main = "Actual V/S Predicted", xlab = "Months", ylab = "Quantity")
lines(in_data_pred, col="red", lwd=2)

## Testing if the residual series is white noise
resi_auto_arima <- timeser.Consumer.APAC.Quantity - fitted(autoarimafit)

## Testing for stationarity
adf.test(resi,alternative = "stationary")
# Lag order = 3, p-value = 0.01
# we have to reject the null hypothesis here and go with alternative hypothesis as p-value < 0.05
kpss.test(resi)
# Truncation lag parameter = 1, p-value = 0.1
# we have to accept the null hypothesis here, as p-value > 0.05 

# Both Dickey Fuller test and KPSS test show strong stationarity in residual series

## Test the model
fcast_auto_arima <- predict(autoarimafit, n.ahead = 6)

outdata <- data.Consumer.APAC.Quantity$Quantity[43:48]

## Evaluate the model using MAPE
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima

## Insert the MAPE value in DF
MAPE.df[8,] <- c("Consumer-APAC-Quantity","Auto ARIMA",round(MAPE_auto_arima,2))

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(data.Consumer.APAC.Quantity$Quantity)

auto_arima_pred <- c(fitted(autoarimafit),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black", lwd=2)
lines(auto_arima_pred, col = "red", lwd=2)

## Future Forecast for months 49 to 54 (Jan 2015 to June 2015) 
autoarima_total <- auto.arima(total_timeser)
tsdiag(autoarima_total)
autoarima_forecast <- predict(autoarima_total,n.ahead = 6)
plot(forecast(autoarima_total,h=6), lwd=2)

## Insert the future forecast value in DF
AA.Forecast.df <- cbind(AA.Forecast.df,as.data.frame(autoarima_forecast$pred))
colnames(AA.Forecast.df)[6] <- c("Consumer-APAC-Quantity")

#---------------------------------------------------------------------------------------#
################ 7. Conclusion: #########################################################
#---------------------------------------------------------------------------------------#

## Plotting model accuracy (MAPE) across all models
ggplot(MAPE.df, aes(Segment,MAPE, fill=Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Classical Decomposition V/S Auto ARIMA", x = "Segments", y = "MAPE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,face="bold"), 
        axis.text.y = element_text(face="bold"))

# Clearly in all cases Classical decompositioin has shown a better accuracy than Auto ARIMA
forecast.df <- rbind(CD.Forecast.df,AA.Forecast.df)

## Plotting forecasted vales for both models
plot1 <- ggplot(forecast.df, aes(x = as.factor(Month), y = forecast.df$`Consumer-EU-Sales`,fill=Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Segments", y = "EU Sales Forecast") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1,face="bold"), 
        axis.text.y = element_text(face="bold"))

plot2 <- ggplot(forecast.df, aes(x = as.factor(Month), y = forecast.df$`Consumer-APAC-Sales`,fill=Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Segments", y = "APAC Sales Forecast") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1,face="bold"), 
        axis.text.y = element_text(face="bold"))

plot3 <- ggplot(forecast.df, aes(x = as.factor(Month), y = forecast.df$`Consumer-EU-Quantity`,fill=Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Segments", y = "EU Quantity Forecast") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1,face="bold"), 
        axis.text.y = element_text(face="bold"))

plot4 <- ggplot(forecast.df, aes(x = as.factor(Month), y = forecast.df$`Consumer-APAC-Quantity`,fill=Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Segments", y = "APAC Quantity Forecast") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1,face="bold"), 
        axis.text.y = element_text(face="bold"))

grid.arrange(plot1, plot2 ,plot3 ,plot4, ncol=2,nrow=2,top="Classical Decomposition V/S Auto ARIMA")

## Final Forecats by Classical Decomposition
CD.Forecast.df

## Final Forecats by Auto ARIMA
AA.Forecast.df

# write.csv(CD.Forecast.df,"CD Forecasts.csv")
# write.csv(AA.Forecast.df,"AA Forecasts.csv")
