setwd("/Users/bahadir/Documents/BahadirAyan")
getwd()
library(dplyr)
library(data.table)
library(readxl)
library(lubridate)
library(ggplot2)
library("xts")
library("zoo")
library("tidyverse")
library(stats)
library(forecast)
electricity_consumption<-read.csv("electricty.csv")
electricity_consumption


electricity_consumption_2<-as.data.frame(electricity_consumption$Tüketim.Miktarý..MWh.) 

names(electricity_consumption_2)[names(electricity_consumption_2)=="electricity_consumption$Tüketim.Miktarý..MWh."]<-"consumption"


electricity_consumption_2$consumption<-as.character(electricity_consumption_2$consumption)

electricity_consumption_2$consumption<-substr(electricity_consumption_2$consumption,1,nchar(electricity_consumption_2$consumption)-3)

electricity_consumption_2$consumption<-as.numeric(electricity_consumption_2$consumption)


#To sum consumption data to create daily,weekly,monthly data
n.colsum = function(df, n = 24){
  aggregate(x = df, by = list(gl(ceiling(nrow(df)/n), n)[1:nrow(df)]),FUN = sum)
}



#Create Hourly Data

electricity_consumption_ts_hourly<-ts(electricity_consumption_2,start=c(2016,1),freq=365*24)

ts.plot(electricity_consumption_ts_hourly,
        xlab = "Year", ylab = "Electricity Consumption",
        main="Electricity Consumption Hourly (2016-2020)"
        )
legend("topleft",col="blue",legend=c("ECx1000(MwH)"),cex=0.5)



#Create Daily Data

ec_nothour_daily<-n.colsum(electricity_consumption_2,n=24)
ec_nothour_daily$Group.1<-NULL

electricity_consumption_ts_daily<-ts(ec_nothour_daily,start=c(2016,1),freq=365)

ts.plot(electricity_consumption_ts_daily, 
        xlab = "Year", 
        ylab = "Electricity Consumption",
        main="Electricity Consumption Daily (2016-2020)")
legend("topleft",col="blue",legend=c("ECx1000(MwH)"),cex=0.5)


#Create Weekly Data

ec_nothour_weekly<-n.colsum(electricity_consumption_2,n=168)
ec_nothour_weekly$Group.1<-NULL



ec_nothour_weekly<-ec_nothour_weekly[-c(226),]

electricity_consumption_ts_weekly<-ts(ec_nothour_weekly,start=c(2016,1),freq=52)

ts.plot(electricity_consumption_ts_weekly,
        xlab = "Year",
        ylab = "Electricity Consumption",
        main="Electricity Consumption Weekly (2016-2020)")
legend("topleft",col="blue",legend=c("ECx1000(MwH)"),cex=0.5)


#Create Monthly Data

ec_nothour_monthly<-n.colsum(electricity_consumption_2,n=672)
ec_nothour_monthly$Group.1<-NULL

ec_nothour_monthly<-ec_nothour_monthly[-c(57),]

electricity_consumption_ts_monthly<-ts(ec_nothour_monthly,start=c(2016,1),freq=12)

ts.plot(electricity_consumption_ts_monthly, xlab = "Year", 
        ylab = "Electricity Consumption",
        main="Electricity Consumption Monthly (2016-2020)")
legend("topleft",col="blue",legend=c("ECx1000(MwH)"),cex=0.5)



##Decompose Hourly Data

electricity_consumption_ts_hourly
is.ts(electricity_consumption_ts_hourly)

#Multiplicative
ec_hourly_multip<-decompose(electricity_consumption_ts_hourly,type="multiplicative")

ec_hourly_multip

plot(ec_hourly_multip)


#Additive

electricity_consumption_ts_hourly_log<-log(electricity_consumption_ts_hourly)

ec_hourly_additive<-decompose(electricity_consumption_ts_hourly,type="additive")

ec_hourly_additive

plot(ec_hourly_additive)

#ACF's
acf(ec_hourly_multip$random,na.action=na.omit,lag.max = 500)
acf(ec_hourly_additive$random,na.action=na.omit,lag.max=500)


#####Decompose Daily data 

electricity_consumption_ts_daily

is.ts(electricity_consumption_ts_daily)

#Multiplicative

ec_daily_multip<-decompose(electricity_consumption_ts_daily,type="multiplicative")

ec_daily_multip

plot(ec_daily_multip)



#Additive

ec_daily_additive<-decompose(electricity_consumption_ts_daily,type="additive")

ec_daily_additive

plot(ec_daily_additive)

#ACF's
acf(ec_daily_multip$random,na.action=na.omit,lag.max = 100)
acf(ec_daily_additive$random,na.action=na.omit,lag.max = 100)

#####################D
##Decompose weekly Data

ec_weekly_multip<-decompose(electricity_consumption_ts_weekly,type="multiplicative")

ec_weekly_multip

plot(ec_weekly_multip)


##Decompose Monthly Data

ec_monthly_multip<-decompose(electricity_consumption_ts_monthly,type="multiplicative")

ec_monthly_multip

plot(ec_monthly_multip)

########################################
### Auto Correlations

#12-24 Hour
acf(electricity_consumption_ts_hourly,lag.max = 50,main="Autocorrelation of Hourly Electric Consumption")



#7 days
acf(electricity_consumption_ts_daily,lag.max = 50,main="Autocorrelation of Daily Electric Consumption")

#24 week
acf(electricity_consumption_ts_weekly,lag.max = 50,main="Autocorrelation of Weekly Electric Consumption")


#3 Months
acf(electricity_consumption_ts_monthly,lag.max = 24,main="Autocorrelation of Monthly Electric Consumption")

##########################



###########################################################Q3 

#Deseasonalize hourly data
deseasonalized_ec_hourly<-electricity_consumption_ts_hourly/ec_hourly_multip$seasonal




#Raw Hourly Data

ts.plot(electricity_consumption_ts_hourly,xlab = "Year", ylab = "Electricity Consumption",main="Electricity Consumption Hourly (2016-2020)")
acf(electricity_consumption_ts_hourly,main="Autocorrelation of Hourly Electric Consumption")

#Deseasonalized hourly data
ts.plot(deseasonalized_ec_hourly,xlab = "Year", ylab = "Electricity Consumption",main="Deseasonalized Electricity Consumption Hourly (2016-2020)")
acf(deseasonalized_ec_hourly,lag.max = 1000)
deseasonalized_ec_hourly

ec_hourly_multip$seasonal

#Detrend Hourly data

detrend_ec_hourly<-deseasonalized_ec_hourly/ec_hourly_multip$trend

ts.plot(detrend_ec_hourly,xlab = "Year", ylab = "Electricity Consumption",main="Detrend Electricity Consumption Hourly (2016-2020)")

acf(electricity_consumption_ts_hourly,lag.max = 500)
acf(detrend_ec_hourly,na.action=na.omit,lag.max = 500)

detrend_ec_hourly

#############################Q3
#AR model

ARmodel_hourly<-arima(detrend_ec_hourly,order=c(7,0,0))
print(ARmodel_hourly)
AIC(ARmodel_hourly)



ARmodel_hourly_fitted<- detrend_ec_hourly-residuals(ARmodel_hourly)

ts.plot(detrend_ec_hourly,xlab = "Year", ylab = "Electricity Consumption",main="Detrend Electricity Consumption Hourly (2016-2020)")

points(ARmodel_hourly_fitted, type="l",col=2, lty=2)




#########################Q4
#MA model
MAmodel_hourly<-arima(detrend_ec_hourly,order=c(0,0,7))
print(MAmodel_hourly)
AIC(MAmodel_hourly)



MAmodel_hourly_fitted<-detrend_ec_hourly-residuals(MAmodel_hourly)

ts.plot(detrend_ec_hourly,xlab = "Year", ylab = "Electricity Consumption",main="Detrend Electricity Consumption Hourly (2016-2020)")

points(MAmodel_hourly_fitted,type="l",col=2,lty=2)




#####################
seasonalavg<-mean(ec_hourly_multip$seasonal)
seasonalavg

na.omitted<-na.omit(ec_hourly_multip$trend)
trendavg<-mean(na.omitted)
trendavg



#######################
############Q5
ts.plot(electricity_consumption_ts_hourly,xlab = "Year", ylab = "Electricity Consumption",main="Electricity Consumption Hourly (2016-2020)")

ARmodel_hourly_forecast<-predict(ARmodel_hourly,n.ahead=2400)$pred

ARmodel_hourly_forecast_se<-predict(ARmodel_hourly,n.ahead=2400)$se

ARmodel_hourly_forecast_new<-ARmodel_hourly_forecast*seasonalavg*trendavg
ARmodel_hourly_forecast_new

ARmodel_hourly_forecast_se_new<-ARmodel_hourly_forecast_se*seasonalavg*trendavg
ARmodel_hourly_forecast_se_new

points(ARmodel_hourly_forecast_new,type="l",col=2)
points(ARmodel_hourly_forecast_new - 1.96*ARmodel_hourly_forecast_se_new, type = "l", col = 2, lty = 2)
points(ARmodel_hourly_forecast_new + 1.96*ARmodel_hourly_forecast_se_new, type = "l", col = 2, lty = 2)


#########









