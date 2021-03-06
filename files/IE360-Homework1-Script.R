

library(dplyr)
library(data.table)
library(readxl)
library(lubridate)
library(ggplot2)
library("xts")
library("zoo")
library("tidyverse")


#USD/TL Exchange Rates
#Reading USD/TL Rates from excel
doviz=read_xlsx('/Users/bahadir/Documents/BahadirAyan/dovizaylik.xlsx')

#Converting data to data.table format
doviz_df<-as.data.table(doviz,keep.rownames = FALSE)

#Adjusting the date data 
doviz_df$Tarih<-as.yearmon(doviz_df$Tarih)

doviz_df$Tarih<-as.Date(doviz_df$Tarih,format=c("%y-%m"))

#Adjusting values to numeric data to prevent any further problems
doviz_df$`TP DK USD A YTL`<-as.numeric(doviz_df$`TP DK USD A YTL`)

#To create xts object
doviz_ts<-xts(doviz_df$`TP DK USD A YTL`,doviz_df$Tarih) 


plot.xts(doviz_ts,
         main ="USD/TL Rate",
         yaxis.right = FALSE,
         col = 4,
         grid.ticks.on = "months",
        
         )
addLegend("topleft",
          
          legend.names = c("USD/TL Rate"),
          col=c("blue"),
          lty=c(1,1), lwd=c(1,1),
          cex=0.65,
          bty="o"
          
)


# Find Difference of USD/TL Rate


dovizdiff_ts<-diff(doviz_ts)
dovizdiff_ts<-na.omit(dovizdiff_ts)


plot.xts(dovizdiff_ts,
         main ="Difference of USD/TL Rate",
         yaxis.right = FALSE,
         col = 4,
         grid.ticks.on = "months",
         
)
addLegend("topleft",
          
          legend.names = c("Difference of USD/TL Rate"),
          col=c("blue"),
          lty=c(1,1), lwd=c(1,1),
          cex=0.65,
          bty="o"
          
)






#CPI 

#Reading CPI from excel
fiyatendeksi=read_xlsx('/Users/bahadir/Documents/BahadirAyan/fiyatendeksi.xlsx')

#Converting data to data.table format
fiyatendeksi_df<-as.data.table(fiyatendeksi,keep.rownames = FALSE)

#Adjusting the date data 
fiyatendeksi_df$Tarih<-as.yearmon(fiyatendeksi_df$Tarih)

fiyatendeksi_df$Tarih<-as.Date(fiyatendeksi_df$Tarih, format=c("%m-%y"))

#Adjusting values to numeric data to prevent any further problems. 
#I divided values by 100 to plot graphs logically.
fiyatendeksi_df$`TP FG J0`<-as.numeric(fiyatendeksi_df$`TP FG J0`/100)

#To create xts object
fiyatendeksi_ts<-xts(fiyatendeksi_df$`TP FG J0`,fiyatendeksi_df$Tarih)

plot.xts(fiyatendeksi_ts,
         main ="CPI Index",
         yaxis.right = FALSE,
         col = 4,
         grid.ticks.on = "months",
         ylim=c(2.5,5)
         
)
addLegend("topleft",
          
          legend.names = c("CPI Index"),
          col=c("blue"),
          lty=c(1,1), lwd=c(1,1),
          cex=0.65,
          bty="o"
          
)

#FInd Diff of CPI

cpidiff_ts<-diff(fiyatendeksi_ts)

cpidiff_ts<-na.omit(cpidiff_ts)
plot.xts(cpidiff_ts,
         main ="Difference CPI Index",
         yaxis.right = FALSE,
         col = 4,
         grid.ticks.on = "months",
         
         
)
addLegend("topleft",
          
          legend.names = c("CPI Index"),
          col=c("blue"),
          lty=c(1,1), lwd=c(1,1),
          cex=0.65,
          bty="o"
          
)











#PRICE INDICES) BIST-100, According to Closing Price 

#Reading USD/TL Rates from excel
bist=read_xlsx('/Users/bahadir/Documents/BahadirAyan/bistaylik.xlsx')

#Converting data to data.table format
bist_df=as.data.table(bist,keep.rownames = FALSE)

#Adjusting values to numeric data to prevent any further problems. 
#I divided values by 10000 to plot graphs logically.
bist_df$`TP MK F BILESIK`<-as.numeric(bist_df$`TP MK F BILESIK`/10000)

#Adjusting the date data 
bist_df$Tarih<-as.yearmon(bist_df$Tarih)

bist_df$Tarih<-as.Date(bist_df$Tarih, format=c("%m-%y"))

#To create xts object
bist_ts<-xts(bist_df$`TP MK F BILESIK`,bist_df$Tarih)


plot.xts(bist_ts,
         main ="BIST Index",
         yaxis.right = FALSE,
         col = 4,
         grid.ticks.on = "months",
         
         
)
addLegend("topleft",
          
          legend.names = c("BIST Index"),
          col=c("blue"),
          lty=c(1,1), lwd=c(1,1),
          cex=0.65,
          bty="o"
          
)
#Find Difference of BIST Index
bistdiff_ts<-diff(bist_ts)
bistdiff_ts<-na.omit(bistdiff_ts)
plot.xts(bistdiff_ts,
         main ="Diff of BIST Index",
         yaxis.right = FALSE,
         col = 4,
         grid.ticks.on = "months",
         
         
)
addLegend("topleft",
          
          legend.names = c("Diff of BIST Index"),
          col=c("blue"),
          lty=c(1,1), lwd=c(1,1),
          cex=0.65,
          bty="o"
          
)







# Merge BIST and USD/TL rate data in order to make analysis
bistvsdoviz<-merge(bist_ts,doviz_ts)

#plot merged data
plot.xts(bistvsdoviz, 
         
         main ="BIST Index&USD/TL Rate vs Time",
         yaxis.right = FALSE,
         col = c(2,4),
         grid.ticks.on = "months",
         ylim = c(0,15)
     
)
addLegend("topleft",
          
          legend.names = c("BIST(x10000)","USD/TL"),
          col=c("red","blue"),
          lty=c(1,1), lwd=c(1,1),
          cex=0.65,
          bty="o"
         
           )

# Find correlation & covariance
cor(bistvsdoviz$bist_ts,bistvsdoviz$doviz_ts)
cov(bistvsdoviz$bist_ts,bistvsdoviz$doviz_ts)



#Merge Diff BIST and USD/TL rate
diff_bistvsdoviz<-merge(bistdiff_ts,dovizdiff_ts)
diff_bistvsdoviz<-na.omit(diff_bistvsdoviz)

plot.xts(diff_bistvsdoviz, 
         
         main ="Diff of BIST Index&USD/TL Rate vs Time",
         yaxis.right = FALSE,
         col = c(2,4),
         grid.ticks.on = "months",
        
         
)
addLegend("topleft",
          
          legend.names = c("BIST(x10000)","USD/TL"),
          col=c("red","blue"),
          lty=c(1,1), lwd=c(1,1),
          cex=0.65,
          bty="o"
          
)
#Find Correlation and Covariance of diff() applied BIST and USD/TL Exchange rate
cor(diff_bistvsdoviz$bistdiff_ts,diff_bistvsdoviz$dovizdiff_ts)
cov(diff_bistvsdoviz$bistdiff_ts,diff_bistvsdoviz$dovizdiff_ts)

#Merge BIST and CPI data in order to make analysis
bistvsfiyatendeksi<-merge(bist_ts,fiyatendeksi_ts)


plot.xts(bistvsfiyatendeksi, 
         
         main ="BIST Index vs CPI",
         yaxis.right = FALSE,
         col = c(2,4),
         grid.ticks.on = "months",
        
         
)
addLegend("topleft",
          
          legend.names = c("BIST(x10000)","CPI(x100)"),
          col=c("red","blue"),
          lty=c(1,1), lwd=c(1,1),
          cex=0.65,
          bty="o"
          
)

#Find Correlation & Covarinace of BIST and CPI
cor(bistvsfiyatendeksi$bist_ts,bistvsfiyatendeksi$fiyatendeksi_ts)
cov(bistvsfiyatendeksi$bist_ts,bistvsfiyatendeksi$fiyatendeksi_ts)


## Merge diff() applied BIST and CPI data
diff_bistvscpi<-merge(bistdiff_ts,cpidiff_ts)

diff_bistvscpi<-na.omit(diff_bistvscpi)

#plot 

plot.xts(diff_bistvscpi, 
         
         main ="diff() applied BIST Index vs CPI",
         yaxis.right = FALSE,
         col = c(2,4),
         grid.ticks.on = "months",
         
         
)
addLegend("topleft",
          
          legend.names = c("diff() BIST","diff() CPI"),
          col=c("red","blue"),
          lty=c(1,1), lwd=c(1,1),
          cex=0.65,
          bty="o"
          
)

#Find Correlation & Covariance of diff() applied BIST and CPI
cor(diff_bistvscpi$bistdiff_ts,diff_bistvscpi$cpidiff_ts)
cov(diff_bistvscpi$bistdiff_ts,diff_bistvscpi$cpidiff_ts)


################################ Calculating percent change of parameters
PC_bist<-(bist_ts/stats::lag(bist_ts,-1) - 1)*100
PC_doviz<-(doviz_ts/stats::lag(doviz_ts,-1) - 1)*100
PC_cpi<-(fiyatendeksi_ts/stats::lag(fiyatendeksi_ts,-1) - 1)*100

#Omitting the NA's
PC_bist<-na.omit(PC_bist)
PC_doviz<-na.omit(PC_doviz)
PC_cpi<-na.omit(PC_cpi)
############################


## Percentage Change Analysis of BIST and USD/TL Exchange Rate
# Merging BIST and USD/TL percentage changes
PC_bistvsdoviz<-as.xts(merge(PC_bist,PC_doviz,join="inner"))

#plotting new data
plot.xts(PC_bistvsdoviz, 
         
         main ="% Change of BIST Index vs USD/TL Exchange Rate",
         yaxis.right = FALSE,
         col = c(2,4),
         grid.ticks.on = "months",
         
         
)
addLegend("topleft",
          
          legend.names = c("% Change BIST","% Change USD/TL"),
          col=c("red","blue"),
          lty=c(1,1), lwd=c(1,1),
          cex=0.65,
          bty="o"
          
)

#Finding Correlation&Covariance of Percentage changes
cor(PC_bistvsdoviz$PC_bist,PC_bistvsdoviz$PC_doviz)
cov(PC_bistvsdoviz$PC_bist,PC_bistvsdoviz$PC_doviz)


# Merging BIST and CPI percentage changes

PC_bistvscpi<-as.xts(merge(PC_bist,PC_cpi, join="inner"))

#plotting data

plot.xts(PC_bistvscpi, 
         
         main ="% Change of BIST Index vs CPI",
         yaxis.right = FALSE,
         col = c(2,4),
         grid.ticks.on = "months",
         
         
)
addLegend("topleft",
          
          legend.names = c("% Change BIST","% Change CPI"),
          col=c("red","blue"),
          lty=c(1,1), lwd=c(1,1),
          cex=0.65,
          bty="o"
          
)

#Finding Correlation&Covariance of Percentage changes

cor(PC_bistvscpi$PC_bist,PC_bistvscpi$PC_cpi)
cov(PC_bistvscpi$PC_bist,PC_bistvscpi$PC_cpi)



