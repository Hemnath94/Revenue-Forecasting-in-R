library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)
install.packages("padr")
library(padr)
install.packages("anytime")
library("anytime")   
rm(list=ls())

#Reading Dataset
Exp <- read.csv("Monthly_T&T_10Stores.csv")
Exp

#Filtering Storenumber
ExpNew <- Exp[Exp$STORE_NUM==502,]
ExpNew
#checking NA and INF
#ExpNew[is.na(ExpNew) | ExpNew=="Inf"] = NA
#ExpNew

#ExpNew[ExpNew==0] <- NA
#ExpNew <- na.omit(object = ExpNew)
#summary(object = ExpNew)

str(ExpNew)
head(ExpNew, n = 5)

#converting to date forma
ExpS <- anydate(ExpNew$month_year)
head(ExpS)
class(ExpNew$month_year)

#Extracting Year, Month, Day from Date column
Y <- as.numeric(format(ExpS,'%Y'))
head(Y)

M <- as.numeric(format(ExpS,'%m'))
head(M)

D <- as.numeric(format(ExpS,'%d'))
head(D)

#Binding Monthyear and Revenue
EXPTS <- cbind(ExpNew[,c("month_year","Revenue.Qty.Amt")],Y,M)
EXPTS

#Aggregating based on Month and year
DMAMT_aggr <- aggregate(Revenue.Qty.Amt ~ M + Y,       # Aggregate data
                        EXPTS,
                        FUN = sum)
DMAMT_aggr


#Creating Timeseries for Month
Month <- ts(DMAMT_aggr$Revenue.Qty.Amt,frequency=12, start=c(2020,9),end=c(2022,5)) 
Month

#Spliting the dataset.

#Train <- window(salesTS,  start = c(2020,9),
  #                    end = c(2022,2))
#Test <- window(salesTS, start = c(2022,3), end = c(2022,5))
#salesTS.Hold <- window(salesTS, start = c(2022, 152))
#Train
#Test

Simple <- list()
Double <- list()
Triple <- list()
Overall <- list()
SinglePred <- list()
DoublePred <- list()
TriplePred <- list()
#Train <- ts(DMAMT_aggr$Revenue.Qty.Amt[1:(length(DMAMT_aggr$Revenue.Qty.Amt)-14)], frequency = 7)
#Train
#Test <- DMAMT_aggr$Revenue.Qty.Amt[(length(DMAMT_aggr$Revenue.Qty.Amt)-13):length(DMAMT_aggr$Revenue.Qty.Amt)]
#Test

#SES <- ses(Train, lamba = 'auto', biasadj = TRUE, h = 14)
#SimpleACC <- accuracy(SES, Test)
#SimpleACC
for(i in 1:3)
{
  
Train <- ts(Month[1:(length(Month)-i)],start = c(2020,9), frequency=12)
Test <- Month[(length(Month)-i+1)]
#Train

#simple Exponential Smoothing Model
SES <- ses(Train, lamba = 'auto', biasadj = FALSE, h = 1)
SinglePred[[i]] <- SES$mean
SimpleACC <- accuracy(SES, Test)

SimpleACC
Simple[[i]] <- SimpleACC[2,]

#Holt Method (Trends)
Holt <- holt(Train, lamba = 'auto', h = 1, biasadj = FALSE)
DoublePred [[i]]<- Holt$mean
#Accuracy for Double
HoltACC <- accuracy(Holt, Test)
HoltACC
Double[[i]] <- HoltACC[2,]

#Holt winter Seasonal & Trend
HW <- hw(Train, seasonal ='additive',lamba = 'auto', h = 1,biasadj = FALSE)
TriplePred[[i]] <- HW$mean
## Accuracy for Triple
HWACC <- accuracy(HW, Test)
Triple[[i]] <- HWACC[2,]

}

SingePd <- rev(unlist(SinglePred))
SingePd
DoublePd <- rev(unlist(DoublePred))
DoublePd
TriplePd <-rev(unlist(TriplePred))
TriplePd

Simple
SESOverall <- do.call(rbind, Simple)
round(colMeans(SESOverall[,c(1:6)]),2)


HoltOverall <- do.call(rbind, Double)
HoltOverall
round(colMeans(HoltOverall[,c(1:6)]),2)

TripleOverall <- do.call(rbind, Triple)
round(colMeans(TripleOverall[,c(1:6)]),2)


