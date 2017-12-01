install.packages("dygraphs")
library("dygraphs")

install.packages("forecast")
library("forecast")
install.packages("fpp2")
library("fpp2")
install.packages("xts")
library("xts")
library(dplyr)


# 1.a Pull the DAX  index
help(EuStockMarkets) 
data(EuStockMarkets)
dax <- EuStockMarkets[,"DAX"]

# 1.b plot data and   add a vertical red line to your plot which divides pre-1997 and post-1997 
plot(dax,main="Stock Data", sub="DAX Index",xlab="Year", ylab="Stock Value")
abline(v=1997, col="red")
plot(EuStockMarkets,main="European Stock Data",xlab="Year", ylab="Stock Value")
abline(v=1997, col="red")

# 1.c Decompose the time series into its components
dax.comp <- decompose(dax, type="mult")
plot(dax.comp)

#HoltWinters filtering on births
dax.hw <- HoltWinters(dax)
dax.hw
plot(dax.hw,main="Stock Data", sub="DAX Index",xlab="Year", ylab="Stock Value")

# 2.a  load fpp2 and look at ffp2 temperature dataset
help(maxtemp)

#2.b Subset after 1990 year
temp.data<-window(maxtemp, start=1991,initial=c("optimal"))

#2.c SES to predict the next five years of maximum temperatures in Melbourne. plot this infromation
plot(temp.data,main="Max Temp Data",xlab="Year", ylab="max tempValue")
fcast.ses <- ses(temp.data,h=5)
plot(fcast.ses,main="Max Temp Data",xlab="Year", ylab="max tempValue")


#2.d damped Holt’s linear trend to also predict out five years
fcast.holt <- holt(temp.data,h=5,initial=c("optimal"),damped=TRUE)
plot(fcast.holt,main="Max Temp Data",xlab="Year", ylab="max tempValue")


#2.e Compare the AICc - Best value in models is to choose the one with the minimum AIC value.SES perefed as it has Low AICc value
summary(fcast.ses)
summary(fcast.holt)

#3.a Read the files and rename columns to meaningful name
ts.gregorovitch <-  read.csv("data/Unit11TimeSeries_Gregorovitch.csv")
ts.ollivander <-  read.csv("data/Unit11TimeSeries_Ollivander.csv")

colnames(ts.gregorovitch) <- c("Date","WandsSold")
colnames(ts.ollivander) <- c("Date","WandsSold")

#3.b covert to Date class
ts.gregorovitch$Date <- as.Date(ts.gregorovitch$Date , "%m/%d/%y")
ts.ollivander$Date <- as.Date(ts.ollivander$Date , "%m/%d/%y")
ts.gregorovitch$Maker <- "Gregorovitch"
ts.ollivander$Maker <- "Voldemort"

#3.c make each data frame an xts object 
greg.xts <- xts(ts.gregorovitch$WandsSold,ts.gregorovitch$Date,order.by = ts.gregorovitch$Date )
olliv.xts <- xts(ts.ollivander$WandsSold,ts.ollivander$Date,order.by = ts.ollivander$Date )

#3.d Bind the two xts objects together and create a dygraph

ollivander <- ts(log(olliv.xts), start = start(olliv.xts), end = end(olliv.xts), frequency = 1)
gregorovitch <- ts(log(greg.xts), start = start(greg.xts), end = end(greg.xts), frequency = 1)
bind.dyg <- cbind(ollivander,gregorovitch)
dygraph(as.ts(bind.dyg))
dygraph(bind.dyg) %>% dyRangeSelector(height=100) 

dygraph(ollivander) %>% dyRangeSelector(height=100) %>% dyShading(from = "1955-1-1", to = "1959-1-1", color = "#CCEBD6")
