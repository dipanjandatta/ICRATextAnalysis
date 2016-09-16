# Clean memory
rm(list = ls())

# Set working directory
setwd("C:/Users/dipanjand/Desktop/RMF")

# install.packages("data.table")
library(data.table)

# Read Book1
temp.Book1 = fread("Book1.csv") #99999 obs. of 38 var.

# Read Book2
temp.Book2 = fread("Book2.csv") #100000 obs. of 38 var.

# Read Book3
temp.Book3 = fread("Book3.csv") #58365 obs.of 38 var.

# Merge the data together
RMFdata = rbind(temp.Book1,temp.Book2,temp.Book3)

# Remove temporary tables
rm(temp.Book3,temp.Book2,temp.Book1)

# Summary of the data
summary(RMFdata)

# Change the period for character to date
library(zoo)

RMFdata$PERIOD = as.Date(as.yearmon(RMFdata$PERIOD,"%B-%y"))

# Electing only data for whoch RMF asset class = Equity
RMFdata1 = RMFdata[tolower(RMFdata$RMF_ASSETCLASS1) %in% "equity"]

# Calculate correlated terms

colnames(RMFdata1)

CorrData = RMFdata1[,15:38, with = FALSE]

# Use Spearman Rank Correlation
M = abs(cor(CorrData, method = "spearman"))

# List all the correlated variables
diag(M) = 0; which(M > 0.80, arr.ind = T)

# We prefer MAAUM over AUM, 
# DROP RMF_AUM,IND_AUM

RMFdata2 = RMFdata1[,c("RMF_AUM","IND_AUM"):=NULL]

# We prefer Total_pur over Sales
# DROP SALES_RMF, SALES_IND

RMFdata2 = RMFdata2[,c("SALES_RMF","SALES_IND"):=NULL]

# We prefer Total_RED over RED
# DROP RED_RMF, RED_IND

RMFdata2 = RMFdata2[,c("RED_RMF","RED_IND"):=NULL]

# We prefer New SIP Amount over New SIP
# DROP NEWSIP_RMF, NEWSIP_IND

RMFdata2 = RMFdata2[,c("NEWSIP_RMF","NEWSIP_IND"):=NULL]

# We prefer TotalSIP over SIP value
# DROP SIPVALUE_RMF, SIPVALUE_IND

RMFdata2 = RMFdata2[,c("SIPVALUE_RMF","SIPVALUE_IND"):=NULL]

#Drop demographic variables, only keep broker code and period

RMFdata2 = RMFdata2[,colnames(RMFdata2)[c(2:7,9:14)]:=NULL]


dps_dt[,list(AVERAGE=.Internal(mean(RT)), MEDIAN=median(RT),
             SE= sqrt(var(RT)/length(RT))),by=list(Subject,Class)]

