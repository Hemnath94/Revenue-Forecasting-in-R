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
Exp <- read.csv("train_and_test.csv")
Exp

#Filtering Storenumber
ExpNew <- Exp[Exp$STORE_NUM==567,]
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
ExpS <- anydate(ExpNew$ORDER_DT)
head(ExpS)
class(ExpNew$ORDER_DT)

#Extracting Year, Month, Day from Date column
Y <- as.numeric(format(ExpS,'%Y'))
head(Y)

M <- as.numeric(format(ExpS,'%m'))
head(M)

D <- as.numeric(format(ExpS,'%d'))
head(D)

#Binding Monthyear and Revenue
EXPTS <- cbind(ExpNew[,c("ORDER_DT","Revenue.Qty.Amt")],Y,M,D)
EXPTS

#Aggregating based on Month and year
DMAMT_aggr <- aggregate(Revenue.Qty.Amt ~ D + M + Y,       # Aggregate data
                        EXPTS,
                        FUN = sum)
DMAMT_aggr

Daily <- ts(DMAMT_aggr$Revenue.Qty.Amt, frequency=7, start=c(2021,95),end=c(2021,314))
Daily
#Creating Timeseries for Month
#salesTS <- ts(DMAMT_aggr$Revenue.Qty.Amt, frequency = 12 start = c(2020,9), end = c(2022,8))
#salesTS
#class(salesTS)

#Spliting the dataset.

#Train <- window(salesTS,  start = c(2020,9),
#                end = c(2022,2))
#Test <- window(salesTS, start = c(2022,3), end = c(2022,5))
#salesTS.Hold <- window(salesTS, start = c(2022, 152))
#Train
#Test

#Train <- ts(DMAMT_aggr$Revenue.Qty.Amt[1:(length(DMAMT_aggr$Revenue.Qty.Amt)-14)], frequency = 7)
#Test <- DMAMT_aggr$Revenue.Qty.Amt[(length(DMAMT_aggr$Revenue.Qty.Amt)-13):length(DMAMT_aggr$Revenue.Qty.Amt)]
#Train
#Test

#Creating list for 3 ETS models
Simple <- list()
Double <- list()
Triple <- list()
SinglePred <- list()
DoublePred <- list()
TriplePred <- list()



#SimpleACC

#start<-(len_tr_data+((i-1)*7)+1)
#rm(Train)
#len_tr_data <-206
for(i in 1:14)
{
Train <- ts(Daily[1:(length(Daily)-i)],start =c(2021,95), frequency=7)
Test <- Daily[(length(Daily)-i+1)]

#simple Exponential Smoothing Model
SES <- ses(Train, lamba = 'auto', biasadj = FALSE, h = 1)
SinglePred[[i]] <- SES$mean
SimpleACC <- accuracy(SES, Test)
Simple[[i]] <- SimpleACC[2,]

#Holt Method (Trends)
Holt <- holt(Train, lamba = 'auto', h = 1, biasadj = FALSE)
DoublePred [[i]]<- Holt$mean
#Accuracy for Double
HoltACC <- accuracy(Holt, Test)
#HoltACC
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

SESOverall <- do.call(rbind, Simple)
#SESOverall
SESOverall
round(colMeans(SESOverall[,c(1:6)]),2)


HoltOverall <- do.call(rbind, Double)
HoltOverall
round(colMeans(HoltOverall[,c(1:6)]),2)

TripleOverall <- do.call(rbind, Triple)
round(colMeans(TripleOverall[,c(1:6)]),2)


