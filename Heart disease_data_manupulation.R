#subset dataset using various conditions
#sorting the data
#Add new variables if necessary
#Handle Date variables if necessary
#Form Tabulations
#Do an aggregate analysis on the data
#using dplyr and piping to perform a seriesof tasks


getwd()
setwd("C:\\Users\\Nabin Gope\\Desktop\\Jigsaw\\Case study\\")
getwd()
hd<-read.csv("case_study_heart_disease_data_set.csv")
head(hd,2)
str(hd)
hd$Sex<- as.factor(hd$Sex)
hd$cp<- as.factor(hd$cp)

hd$fbs<- as.factor(hd$restecg)
hd$exang<- as.factor(hd$exang)
hd$slope<- as.factor(hd$slope)
hd$DV<- as.factor(hd$DV)

str(hd)

##subsetting the data
dat1<- hd[hd$thal=='3',]
head(dat1)

dat2<-head(hd[hd$thal=='3' & hd$Sex==1,c("Sex","thal")])
head(dat2)
str(dat2)

dim(dat2)

##sorting the data descending order:thalach maximum heart rate achieved

dat3<- hd[order(-hd$thalach),]
head(dat3)


##  Tabulation
table(hd$thal)

hd1<-na.omit(hd)
table(hd1[,c("cp","Sex","DV")])

#Finding aggregates
## mean of resting blood pressure
aggregate(trestbps ~ Sex,data=hd1,FUN=mean)

##mean of Maximum heart rate

aggregate(thalach ~ exang+Sex,data=hd1,FUN=mean)

##To find out the patients with heart disease grouped by summary of heart condition (thal) and chest pain type(CP)
library(sqldf)

sqldf("Select thal,cp,count(DV) from hd group by thal,cp")


## using dplyr and piping symbols to do a "Series" of task quickly
## subset the dataset to thal=3 and cp=1 and find out the %people with/without heart disease

library(dplyr)
hd1%>% filter(thal=="3",cp==4)%>%group_by(DV)%>% summarise(n())


