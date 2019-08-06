# ch10-makelibrary(tidyverse)

library(tidyverse)

##################################
######  SSP2015データセットの作成
##################################

SSP2015<-read.csv("SSPI2015_20150923_ver2.csv")
# age<-SSP2015$age #2014年12月末時点age
income<-SSP2015$q31_1R #個人年収　
income[income==25]<-12.5 #修正シンタックスリコード
length(income)# 回答者数 3575
income[income==0 | income>4600]<-NA #所得0（344人）,収入1億円（3人）を欠損値指定
table(income) 

income_log <- log(income) #所得対数化
sex<-SSP2015$q1_1
sex[sex==2]<-0#head(sex,50) 女性0,男性1

SSP2015$eduy[SSP2015$ID==9301] <- 12
SSP2015$eduy[SSP2015$ID==10401] <- 16
SSP2015$eduy[SSP2015$ID==11832] <- 12
SSP2015$eduy[SSP2015$ID==13737] <- 12
SSP2015$eduy[SSP2015$ID==14216] <- 16
SSP2015$eduy[SSP2015$ID==14918] <- 12

eduy<-SSP2015$eduy #years of education

# SSP2015$eduy[SSP2015$ID==9301]
# SSP2015$eduy[SSP2015$ID==10401]
# SSP2015$eduy[SSP2015$ID==11832] 
# SSP2015$eduy[SSP2015$ID==13737]
# SSP2015$eduy[SSP2015$ID==14216] 
# SSP2015$eduy[SSP2015$ID==14918] 

data<-data.frame(income_log,eduy,sex,income)
data<-na.omit(data)#remove NA for Stan.

length(data$income)#2841

head(data)

data2 <- sample_n(data,1000)

data3 <-data.frame(income_log=data2$income_log) 
data3
write.csv(data3, "income.csv")


