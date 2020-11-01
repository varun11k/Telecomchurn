setwd("C:\\Users\\vk\\Desktop\\R\\Data Visualizaton using R\\Capstone")
getwd()
telecom=read.csv("sampletelecomfinal.csv")

#reading the file for analysis
setwd("C:\\DS Full stack\\Graded Assignments\\09 - Capstone Project  and Certitication")
getwd()
telecom<-read.csv("telecomfinal.csv",stringsAsFactors = T)


#Basic sanity checks
dim(telecom)
# 66297 rows  81 variables
names(telecom)
str(telecom)
summary(telecom)
colSums(is.na(telecom))



#Data Quality Report
library(dplyr)
library(dataQualityR)
checkDataQuality(telecom,out.file.num = "Numeric.csv",out.file.cat = "Character.csv")


#Removing  the variables with more than 15% missing values and creating new data set
telecom_new<-select(telecom,-c(dwlltype,dwllsize,mailordr,occu1,wrkwoman,solflag,proptype,mailresp,cartype,children,div_type,numbcars,retdays,income))
names(telecom_new)
#67 variables


#Data visualization to understand the distribution of data
library(ggplot2)
# alpha 0 (transparent) to 1 (opaque). 
#area(churn rate high in some area may be because of network issues in particular area)
q<-ggplot()+geom_bar(data = telecom_new,mapping = aes(x=area,fill=factor(churn)),alpha=0.8,stat = "count")
q+theme(axis.text.x = element_text(angle = 70,hjust = 1))

#age
r<-ggplot()+geom_bar(data = telecom_new,mapping = aes(x=age1,fill=factor(churn)),alpha=0.8,stat = "count")
r+theme(axis.text.x = element_text(angle = 70,hjust = 1))+ylim(0,5000)

#marital status
s<-ggplot()+geom_bar(data = telecom_new,mapping = aes(x=marital,fill=factor(churn)),alpha=0.8,stat = "count")
s+theme(axis.text.x = element_text(angle = 70,hjust = 1))

#ethnic
t<-ggplot()+geom_bar(data = telecom_new,mapping = aes(x=ethnic,fill=factor(churn)),alpha=0.8,stat = "count")
t+theme(axis.text.x = element_text(angle = 70,hjust = 1))

#uniqsubs
t<-ggplot()+geom_bar(data = telecom_new,mapping = aes(x=uniqsubs,fill=factor(churn)),alpha=0.8,stat = "count")
t+theme(axis.text.x = element_text(angle = 70,hjust = 1))+xlim(0,8)
unique(telecom_new$uniqsubs)


#scatter plot to see the relationship between 2 cont variables
ggplot(telecom_new,aes(x=totrev,y=totcalls))+geom_point()


ggplot(telecom_new,aes(x=ovrrev_Mean,y=totmrc_Mean))+geom_point()

ggplot(telecom_new,aes(x=drop_vce_Mean,y=drop_dat_Mean))+geom_point()

ggplot(telecom_new,aes(x=drop_blk_Mean,y=drop_vce_Range))+geom_point()

ggplot(telecom_new,aes(x=roam_Mean,y=drop_vce_Mean))+geom_point()

z<-ggplot(telecom_new,aes(x=totmrc_Mean,y=totrev))
z+geom_point()

#continuous vs categorical

a<-ggplot()+geom_bar(data = telecom_new,mapping = aes(y=totcalls,x=area),col="blue",alpha=0.8,stat = "identity")
a+theme(axis.text.x = element_text(angle = 70,hjust = 1))

names(telecom_new)
b<-ggplot()+geom_bar(data = telecom_new,mapping = aes(y=hnd_price,x=marital),col="blue",alpha=0.8,stat = "identity")
b+theme(axis.text.x = element_text(angle = 70,hjust = 1))

c<-ggplot()+geom_bar(data = telecom_new,mapping = aes(y=drop_vce_Mean,x=area),col="red",alpha=0.8,stat = "identity")
c+theme(axis.text.x = element_text(angle = 70,hjust = 1))

d<-ggplot()+geom_bar(data = telecom_new,mapping = aes(y=totmrc_Mean,x=area),col="light pink",stat = "identity")
d+theme(axis.text.x = element_text(angle = 70,hjust = 1))


e<-ggplot()+geom_bar(data = telecom_new,mapping = aes(y=drop_blk_Mean,x=area),col="light blue",stat = "identity")
e+theme(axis.text.x = element_text(angle = 70,hjust = 1))
#cor(telecom_new[c("mou_Mean","totmrc_Mean","rev_Range","mou_Range")])



#But retdays seems significant variable  
#as mentioned in the data documentation ,missing value can be assumed as customers who did not call customer support(no retention calls from customers)
# so imputing 0 to all missing values and 1 to non NA values.
telecom_new$retdays1<-ifelse(is.na(telecom$retdays)==TRUE,0,1)
class(telecom_new$retdays1)
telecom_new$retdays1<-as.factor(telecom_new$retdays1)
dim(telecom_new)
#68 variables 


# summary(telecom_new$blck_dat_Mean)
# summary(telecom_new$drop_dat_Mean)
# summary(telecom_new$drop_vce_Mean)
# summary(telecom_new$drop_vce_Range)
# summary(telecom_new$drop_blk_Mean)



names(telecom_new)

#Decile Analysis for continuous variable


telecom_new%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat1
dat1$N<-unclass(telecom_new%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-dat1$n/dat1$N
dat1$Min<-unclass(telecom_new%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat1$Max<-unclass(telecom_new%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat1$varname<-rep("mou_Mean",nrow(dat1))
dat1

telecom_new%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat2
dat2$N<-unclass(telecom_new%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-dat2$n/dat2$N
dat2$Min<-unclass(telecom_new%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat2$Max<-unclass(telecom_new%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat2$varname<-rep("totmrc_Mean",nrow(dat2))
dat2


telecom_new%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat3
dat3$N<-unclass(telecom_new%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-dat3$n/dat3$N
dat3$Min<-unclass(telecom_new%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat3$Max<-unclass(telecom_new%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat3$varname<-rep("rev_Range",nrow(dat3))
dat3

telecom_new%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat4
dat4$N<-unclass(telecom_new%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$Min<-unclass(telecom_new%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat4$Max<-unclass(telecom_new%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat4$varname<-rep("mou_Range",nrow(dat4))
dat4

telecom_new%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat5
dat5$N<-unclass(telecom_new%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$Min<-unclass(telecom_new%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat5$Max<-unclass(telecom_new%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat5$varname<-rep("change_mou",nrow(dat5))
dat5

telecom_new%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat6
dat6$N<-unclass(telecom_new%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat6$churn_perc<-dat6$n/dat6$N
dat6$Min<-unclass(telecom_new%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dat6$Max<-unclass(telecom_new%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dat6$varname<-rep("drop_blk_Mean",nrow(dat6))
dat6

telecom_new%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat7
dat7$N<-unclass(telecom_new%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat7$churn_perc<-dat7$n/dat7$N
dat7$Min<-unclass(telecom_new%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dat7$Max<-unclass(telecom_new%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat7$varname<-rep("drop_vce_Range",nrow(dat7))
dat7

telecom_new%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat8
dat8$N<-unclass(telecom_new%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat8$churn_perc<-dat8$n/dat8$N
dat8$Min<-unclass(telecom_new%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat8$Max<-unclass(telecom_new%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat8$varname<-rep("owylis_vce_Range",nrow(dat8))
dat8

telecom_new%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat9
dat9$N<-unclass(telecom_new%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat9$churn_perc<-dat9$n/dat9$N
dat9$Min<-unclass(telecom_new%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat9$Max<-unclass(telecom_new%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat9$varname<-rep("mou_opkv_Range",nrow(dat9))
dat9

telecom_new%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat10
dat10$N<-unclass(telecom_new%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$Min<-unclass(telecom_new%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat10$Max<-unclass(telecom_new%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat10$varname<-rep("months",nrow(dat10))
dat10

telecom_new%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat11
dat11$N<-unclass(telecom_new%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat10$N
dat11$Min<-unclass(telecom_new%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat11$Max<-unclass(telecom_new%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat11$varname<-rep("totcalls",nrow(dat11))
dat11

#Getting less than 4 deciles

summary(telecom_new$eqpdays)

telecom_new%>%mutate(dec=ntile(eqpdays,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat12
dat12$N<-unclass(telecom_new%>%mutate(dec=ntile(eqpdays,n=4))%>%count(dec)%>%unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$Min<-unclass(telecom_new%>%mutate(dec=ntile(eqpdays,n=4))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
dat12$Max<-unclass(telecom_new%>%mutate(dec=ntile(eqpdays,n=4))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
dat12$varname<-rep("eqpdays",nrow(dat12))
dat12

#Getting less than 4 deciles

telecom_new%>%mutate(dec=ntile(custcare_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat13
dat13$N<-unclass(telecom_new%>%mutate(dec=ntile(custcare_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat13$churn_perc<-dat13$n/dat13$N
dat13$Min<-unclass(telecom_new%>%mutate(dec=ntile(custcare_Mean,n=4))%>%group_by(dec)%>%summarise(min(custcare_Mean)))[[2]]
dat13$Max<-unclass(telecom_new%>%mutate(dec=ntile(custcare_Mean,n=4))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]
dat13$varname<-rep("custcare_Mean",nrow(dat13))
dat13

telecom_new%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat14
dat14$N<-unclass(telecom_new%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$Min<-unclass(telecom_new%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
dat14$Max<-unclass(telecom_new%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
dat14$varname<-rep("callwait_Mean",nrow(dat14))
dat14

telecom_new%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat15
dat15$N<-unclass(telecom_new%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$Min<-unclass(telecom_new%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat15$Max<-unclass(telecom_new%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat15$varname<-rep("iwylis_vce_Mean",nrow(dat15))
dat15

#Getting less than 4 deciles
unique(telecom_new$callwait_Range)
telecom_new%>%mutate(dec=ntile(callwait_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat16
dat16$N<-unclass(telecom_new%>%mutate(dec=ntile(callwait_Range,n=4))%>%count(dec)%>%unname())[[2]]
dat16$churn_perc<-dat16$n/dat16$N
dat16$Min<-unclass(telecom_new%>%mutate(dec=ntile(callwait_Range,n=4))%>%group_by(dec)%>%summarise(min(callwait_Range)))[[2]]
dat16$Max<-unclass(telecom_new%>%mutate(dec=ntile(callwait_Range,n=4))%>%group_by(dec)%>%summarise(max(callwait_Range)))[[2]]
dat16$varname<-rep("callwait_Range",nrow(dat16))
dat16

#Getting less than 4 deciles
telecom_new%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat17
dat17$N<-unclass(telecom_new%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(dec)%>%unname())[[2]]
dat17$churn_perc<-dat17$n/dat17$N
dat17$Min<-unclass(telecom_new%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]]
dat17$Max<-unclass(telecom_new%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]
dat17$varname<-rep("ccrndmou_Range",nrow(dat17))
dat17

telecom_new%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat18
dat18$N<-unclass(telecom_new%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$Min<-unclass(telecom_new%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat18$Max<-unclass(telecom_new%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat18$varname<-rep("adjqty",nrow(dat18))
dat18

telecom_new%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat19
dat19$N<-unclass(telecom_new%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$Min<-unclass(telecom_new%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat19$Max<-unclass(telecom_new%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat19$varname<-rep("ovrrev_Mean",nrow(dat19))
dat19

telecom_new%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(telecom_new%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$Min<-unclass(telecom_new%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat20$Max<-unclass(telecom_new%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat20$varname<-rep("rev_Mean",nrow(dat20))
dat20

telecom_new%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat21
dat21$N<-unclass(telecom_new%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$Min<-unclass(telecom_new%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat21$Max<-unclass(telecom_new%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat21$varname<-rep("ovrmou_Mean",nrow(dat21))
dat21

telecom_new%>%mutate(dec=ntile(comp_vce_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat22
dat22$N<-unclass(telecom_new%>%mutate(dec=ntile(comp_vce_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$Min<-unclass(telecom_new%>%mutate(dec=ntile(comp_vce_Mean,n=4))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat22$Max<-unclass(telecom_new%>%mutate(dec=ntile(comp_vce_Mean,n=4))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat22$varname<-rep("comp_vce_Mean",nrow(dat22))
dat22

telecom_new%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat23
dat23$N<-unclass(telecom_new%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat23$churn_perc<-dat23$n/dat23$N
dat23$Min<-unclass(telecom_new%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat23$Max<-unclass(telecom_new%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat23$varname<-rep("plcd_vce_Mean",nrow(dat23))
dat23

telecom_new%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat24
dat24$N<-unclass(telecom_new%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat24$churn_perc<-dat24$n/dat24$N
dat24$Min<-unclass(telecom_new%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat24$Max<-unclass(telecom_new%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat24$varname<-rep("avg3mou",nrow(dat24))
dat24

telecom_new%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat25
dat25$N<-unclass(telecom_new%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$Min<-unclass(telecom_new%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat25$Max<-unclass(telecom_new%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat25$varname<-rep("avgmou",nrow(dat25))
dat25

telecom_new%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat26
dat26$N<-unclass(telecom_new%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$Min<-unclass(telecom_new%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat26$Max<-unclass(telecom_new%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat26$varname<-rep("avg3qty",nrow(dat26))
dat26

telecom_new%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat27
dat27$N<-unclass(telecom_new%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$Min<-unclass(telecom_new%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat27$Max<-unclass(telecom_new%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat27$varname<-rep("avgqty",nrow(dat27))
dat27

telecom_new%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(telecom_new%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$Min<-unclass(telecom_new%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat28$Max<-unclass(telecom_new%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat28$varname<-rep("avg6mou",nrow(dat28))
dat28


telecom_new%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(telecom_new%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$Min<-unclass(telecom_new%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat29$Max<-unclass(telecom_new%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat29$varname<-rep("avg6qty",nrow(dat29))
dat29



telecom_new%>%mutate(dec=ntile(age1,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat30
dat30$N<-unclass(telecom_new%>%mutate(dec=ntile(age1,n=6))%>%count(dec)%>%unname())[[2]]
dat30$churn_perc<-dat30$n/dat30$N
dat30$Min<-unclass(telecom_new%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
dat30$Max<-unclass(telecom_new%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
dat30$varname<-rep("age1",nrow(dat30))
dat30

#Getting less tha 4 deciles,but consider.
telecom_new%>%mutate(dec=ntile(age2,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat31
dat31$N<-unclass(telecom_new%>%mutate(dec=ntile(age2,n=4))%>%count(dec)%>%unname())[[2]]
dat31$churn_perc<-dat31$n/dat31$N
dat31$Min<-unclass(telecom_new%>%mutate(dec=ntile(age2,n=4))%>%group_by(dec)%>%summarise(min(age2)))[[2]]
dat31$Max<-unclass(telecom_new%>%mutate(dec=ntile(age2,n=4))%>%group_by(dec)%>%summarise(max(age2)))[[2]]
dat31$varname<-rep("age2",nrow(dat31))
dat31

#Getting less tha 4 deciles,but consider.
telecom_new%>%mutate(dec=ntile(models,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat32
dat32$N<-unclass(telecom_new%>%mutate(dec=ntile(models,n=4))%>%count(dec)%>%unname())[[2]]
dat32$churn_perc<-dat32$n/dat32$N
dat32$Min<-unclass(telecom_new%>%mutate(dec=ntile(models,n=4))%>%group_by(dec)%>%summarise(min(models)))[[2]]
dat32$Max<-unclass(telecom_new%>%mutate(dec=ntile(models,n=4))%>%group_by(dec)%>%summarise(max(models)))[[2]]
dat32$varname<-rep("models",nrow(dat32))
dat32


telecom_new%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat33
dat33$N<-unclass(telecom_new%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]]
dat33$churn_perc<-dat33$n/dat33$N
dat33$Min<-unclass(telecom_new%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
dat33$Max<-unclass(telecom_new%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
dat33$varname<-rep("hnd_price",nrow(dat33))
dat33

#Getting less tha 4 deciles,but consider.
telecom_new%>%mutate(dec=ntile(actvsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat34
dat34$N<-unclass(telecom_new%>%mutate(dec=ntile(actvsubs,n=4))%>%count(dec)%>%unname())[[2]]
dat34$churn_perc<-dat34$n/dat34$N
dat34$Min<-unclass(telecom_new%>%mutate(dec=ntile(actvsubs,n=4))%>%group_by(dec)%>%summarise(min(actvsubs)))[[2]]
dat34$Max<-unclass(telecom_new%>%mutate(dec=ntile(actvsubs,n=4))%>%group_by(dec)%>%summarise(max(actvsubs)))[[2]]
dat34$varname<-rep("actvsubs",nrow(dat34))
dat34


#Getting less tha 4 deciles,but consider.
telecom_new%>%mutate(dec=ntile(uniqsubs,n=3))%>%count(churn,dec)%>%filter(churn==1)->dat35
dat35$N<-unclass(telecom_new%>%mutate(dec=ntile(uniqsubs,n=3))%>%count(dec)%>%unname())[[2]]
dat35$churn_perc<-dat35$n/dat35$N
dat35$Min<-unclass(telecom_new%>%mutate(dec=ntile(uniqsubs,n=3))%>%group_by(dec)%>%summarise(min(uniqsubs)))[[2]]
dat35$Max<-unclass(telecom_new%>%mutate(dec=ntile(uniqsubs,n=3))%>%group_by(dec)%>%summarise(max(uniqsubs)))[[2]]
dat35$varname<-rep("uniqsubs",nrow(dat35))
dat35

#Getting less tha 4 deciles,but consider.
telecom_new%>%mutate(dec=ntile(forgntvl,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat36
dat36$N<-unclass(telecom_new%>%mutate(dec=ntile(forgntvl,n=10))%>%count(dec)%>%unname())[[2]]
dat36$churn_perc<-dat36$n/dat36$N
dat36$Min<-unclass(telecom_new%>%mutate(dec=ntile(forgntvl,n=10))%>%group_by(dec)%>%summarise(min(forgntvl)))[[2]]
dat36$Max<-unclass(telecom_new%>%mutate(dec=ntile(forgntvl,n=10))%>%group_by(dec)%>%summarise(max(forgntvl)))[[2]]
dat36$varname<-rep("forgntvl",nrow(dat36))
dat36

#Getting less than 4 deciles
telecom_new%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat37
dat37$N<-unclass(telecom_new%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat37$churn_perc<-dat37$n/dat37$N
dat37$Min<-unclass(telecom_new%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%group_by(dec)%>%summarise(min(opk_dat_Mean)))[[2]]
dat37$Max<-unclass(telecom_new%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%group_by(dec)%>%summarise(max(opk_dat_Mean)))[[2]]
dat37$varname<-rep("opk_dat_Mean",nrow(dat37))
dat37

#Getting less tha 4 deciles,but consider.
telecom_new%>%mutate(dec=ntile(mtrcycle,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$N<-unclass(telecom_new%>%mutate(dec=ntile(mtrcycle,n=10))%>%count(dec)%>%unname())[[2]]
dat38$churn_perc<-dat38$n/dat38$N
dat38$Min<-unclass(telecom_new%>%mutate(dec=ntile(mtrcycle,n=10))%>%group_by(dec)%>%summarise(min(mtrcycle)))[[2]]
dat38$Max<-unclass(telecom_new%>%mutate(dec=ntile(mtrcycle,n=10))%>%group_by(dec)%>%summarise(max(mtrcycle)))[[2]]
dat38$varname<-rep("mtrcycle",nrow(dat38))
dat38

#Getting less tha 4 deciles,but consider.
telecom_new%>%mutate(dec=ntile(truck,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat39
dat39$N<-unclass(telecom_new%>%mutate(dec=ntile(truck,n=10))%>%count(dec)%>%unname())[[2]]
dat39$churn_perc<-dat39$n/dat39$N
dat39$Min<-unclass(telecom_new%>%mutate(dec=ntile(truck,n=10))%>%group_by(dec)%>%summarise(min(truck)))[[2]]
dat39$Max<-unclass(telecom_new%>%mutate(dec=ntile(truck,n=10))%>%group_by(dec)%>%summarise(max(truck)))[[2]]
dat39$varname<-rep("truck",nrow(dat39))
dat39

#Getting less than 4 deciles
telecom_new%>%mutate(dec=ntile(roam_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat40
dat40$N<-unclass(telecom_new%>%mutate(dec=ntile(roam_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat40$churn_perc<-dat40$n/dat40$N
dat40$Min<-unclass(telecom_new%>%mutate(dec=ntile(roam_Mean,n=4))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
dat40$Max<-unclass(telecom_new%>%mutate(dec=ntile(roam_Mean,n=4))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
dat40$varname<-rep("roam_Mean",nrow(dat40))
dat40

#Getting less than 4 deciles
telecom_new%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat41
dat41$N<-unclass(telecom_new%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat41$churn_perc<-dat41$n/dat41$N
dat41$Min<-unclass(telecom_new%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%group_by(dec)%>%summarise(min(recv_sms_Mean)))[[2]]
dat41$Max<-unclass(telecom_new%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%group_by(dec)%>%summarise(max(recv_sms_Mean)))[[2]]
dat41$varname<-rep("recv_sms_Mean",nrow(dat41))
dat41


#Getting less than 4 deciles
#blck_dat_Mean
telecom_new%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat430
dat430$N<-unclass(telecom_new%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat430$churn_perc<-dat430$n/dat430$N
dat430$Min<-unclass(telecom_new%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(blck_dat_Mean)))[[2]]
dat430$Max<-unclass(telecom_new%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(blck_dat_Mean)))[[2]]
dat430$varname<-rep("blck_dat_Mean",nrow(dat430))
dat430



#Getting less than 4 deciles
telecom_new%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat42
dat42$N<-unclass(telecom_new%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat42$churn_perc<-dat42$n/dat42$N
dat42$Min<-unclass(telecom_new%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%group_by(dec)%>%summarise(min(mou_pead_Mean)))[[2]]
dat42$Max<-unclass(telecom_new%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%group_by(dec)%>%summarise(max(mou_pead_Mean)))[[2]]
dat42$varname<-rep("mou_pead_Mean",nrow(dat42))
dat42


telecom_new%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat43
dat43$N<-unclass(telecom_new%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat43$churn_perc<-dat43$n/dat43$N
dat43$Min<-unclass(telecom_new%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat43$Max<-unclass(telecom_new%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat43$varname<-rep("da_Mean",nrow(dat43))
dat43

telecom_new%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat44
dat44$N<-unclass(telecom_new%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
dat44$churn_perc<-dat44$n/dat44$N
dat44$Min<-unclass(telecom_new%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat44$Max<-unclass(telecom_new%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat44$varname<-rep("da_Range",nrow(dat44))
dat44


#Getting less than 4 deciles
telecom_new%>%mutate(dec=ntile(datovr_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$N<-unclass(telecom_new%>%mutate(dec=ntile(datovr_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$Min<-unclass(telecom_new%>%mutate(dec=ntile(datovr_Mean,n=4))%>%group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]
dat45$Max<-unclass(telecom_new%>%mutate(dec=ntile(datovr_Mean,n=4))%>%group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]
dat45$varname<-rep("datovr_Mean",nrow(dat45))
dat45


#Getting less than 4 deciles
telecom_new%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat46
dat46$N<-unclass(telecom_new%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(dec)%>%unname())[[2]]
dat46$churn_perc<-dat46$n/dat46$N
dat46$Min<-unclass(telecom_new%>%mutate(dec=ntile(datovr_Range,n=4))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
dat46$Max<-unclass(telecom_new%>%mutate(dec=ntile(datovr_Range,n=4))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
dat46$varname<-rep("datovr_Range",nrow(dat46))
dat46


#Getting less than 4 deciles
telecom_new%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat47
dat47$N<-unclass(telecom_new%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat47$churn_perc<-dat47$n/dat47$N
dat47$Min<-unclass(telecom_new%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%group_by(dec)%>%summarise(min(drop_dat_Mean)))[[2]]
dat47$Max<-unclass(telecom_new%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%group_by(dec)%>%summarise(max(drop_dat_Mean)))[[2]]
dat47$varname<-rep("drop_dat_Mean",nrow(dat47))
dat47

telecom_new%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat48
dat48$N<-unclass(telecom_new%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat48$churn_perc<-dat48$n/dat48$N
dat48$Min<-unclass(telecom_new%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat48$Max<-unclass(telecom_new%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
dat48$varname<-rep("drop_vce_Mean",nrow(dat48))
dat48

telecom_new%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat49
dat49$N<-unclass(telecom_new%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat49$churn_perc<-dat49$n/dat49$N
dat49$Min<-unclass(telecom_new%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat49$Max<-unclass(telecom_new%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat49$varname<-rep("adjmou",nrow(dat49))
dat49



telecom_new%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat50
dat50$N<-unclass(telecom_new%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat50$churn_perc<-dat50$n/dat50$N
dat50$Min<-unclass(telecom_new%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat50$Max<-unclass(telecom_new%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat50$varname<-rep("totrev",nrow(dat50))
dat50


telecom_new%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat51
dat51$N<-unclass(telecom_new%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat51$churn_perc<-dat51$n/dat51$N
dat51$Min<-unclass(telecom_new%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat51$Max<-unclass(telecom_new%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat51$varname<-rep("adjrev",nrow(dat51))
dat51


telecom_new%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat52
dat52$N<-unclass(telecom_new%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat52$churn_perc<-dat52$n/dat52$N
dat52$Min<-unclass(telecom_new%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat52$Max<-unclass(telecom_new%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat52$varname<-rep("avgrev",nrow(dat52))
dat52

#Getting less than 4 deciles
telecom_new%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat53
dat53$N<-unclass(telecom_new%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat53$churn_perc<-dat53$n/dat53$N
dat53$Min<-unclass(telecom_new%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%group_by(dec)%>%summarise(min(comp_dat_Mean)))[[2]]
dat53$Max<-unclass(telecom_new%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%group_by(dec)%>%summarise(max(comp_dat_Mean)))[[2]]
dat53$varname<-rep("comp_dat_Mean",nrow(dat53))
dat53

#Getting less than 4 deciles
telecom_new%>%mutate(dec=ntile(plcd_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat54
dat54$N<-unclass(telecom_new%>%mutate(dec=ntile(plcd_dat_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat54$churn_perc<-dat54$n/dat54$N
dat54$Min<-unclass(telecom_new%>%mutate(dec=ntile(plcd_dat_Mean,n=4))%>%group_by(dec)%>%summarise(min(plcd_dat_Mean)))[[2]]
dat54$Max<-unclass(telecom_new%>%mutate(dec=ntile(plcd_dat_Mean,n=4))%>%group_by(dec)%>%summarise(max(plcd_dat_Mean)))[[2]]
dat54$varname<-rep("plcd_dat_Mean",nrow(dat54))
dat54


#as per documentation Binding all  objects to dat and written into csv file  for the further analysis
dat<-rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9,dat10,
           dat11,dat14,dat15,dat18,dat19,dat20,
           dat21,dat22,dat23,dat24,dat25,dat26,dat27,dat28,dat29,dat30,dat33,dat35,
           dat43,dat44,dat48,dat49,dat50,dat51,dat52)
write.csv(dat,"Deciled Continuous variables.csv ")


#Removing some variables that could not be deciled as it will come insignificant in the model
names(telecom_new)
telecom_new<-telecom_new[,-c(13,16,17,48,49,51,58)]
#61 variables

# qn=quantile(telecom_new$churn,c(0.05,0.95),na.rm = TRUE)
# y=IQR(churn)
# q1=quantile(churn,0.25)
# q3=quantile(churn,0.75)
# telecom_new<-within(telecom_new,{churn1=ifelse(churn<(q1-1.5*y),qn[1],churn)
# churn1=ifelse(churn>(q3+1.5*y),qn[2],churn)})


#####################Data preparation###########################

#Mean imputation for missing value variables
summary(telecom_new)


telecom_new$mou_Mean[is.na(telecom_new$mou_Mean)]<-mean(telecom_new$mou_Mean,na.rm = T)

telecom_new$totmrc_Mean[is.na(telecom_new$totmrc_Mean)]<-mean(telecom_new$totmrc_Mean,na.rm = T)

telecom_new$rev_Range[is.na(telecom_new$rev_Range)]<-mean(telecom_new$rev_Range,na.rm = T)

telecom_new$mou_Range[is.na(telecom_new$mou_Range)]<-mean(telecom_new$mou_Range,na.rm = T)

telecom_new$change_mou[is.na(telecom_new$change_mou)]<-mean(telecom_new$change_mou,na.rm = T)

telecom_new$eqpdays[is.na(telecom_new$eqpdays)]<-mean(telecom_new$eqpdays,na.rm = T)

telecom_new$ovrrev_Mean[is.na(telecom_new$ovrrev_Mean)]<-mean(telecom_new$ovrrev_Mean,na.rm = T)

telecom_new$rev_Mean[is.na(telecom_new$rev_Mean)]<-mean(telecom_new$rev_Mean,na.rm = T)

telecom_new$ovrmou_Mean[is.na(telecom_new$ovrmou_Mean)]<-mean(telecom_new$ovrmou_Mean,na.rm = T)

telecom_new$da_Range[is.na(telecom_new$da_Range)]<-mean(telecom_new$da_Range,na.rm = T)

telecom_new$da_Mean[is.na(telecom_new$da_Mean)]<-mean(telecom_new$da_Mean,na.rm = T)

telecom_new$age1[is.na(telecom_new$age1)]<-mean(telecom_new$age1,na.rm = T)

telecom_new$age2[is.na(telecom_new$age2)]<-mean(telecom_new$age2,na.rm = T)

telecom_new$avg6mou[is.na(telecom_new$avg6mou)]<-mean(telecom_new$avg6mou,na.rm = T)

telecom_new$avg6qty[is.na(telecom_new$avg6qty)]<-mean(telecom_new$avg6qty,na.rm = T)

telecom_new$hnd_price[is.na(telecom_new$hnd_price)]<-mean(telecom_new$hnd_price,na.rm = T)

telecom_new$forgntvl[is.na(telecom_new$forgntvl)]<-mean(telecom_new$forgntvl,na.rm = T)

telecom_new$mtrcycle[is.na(telecom_new$mtrcycle)]<-mean(telecom_new$mtrcycle,na.rm = T)

telecom_new$truck[is.na(telecom_new$truck)]<-mean(telecom_new$truck,na.rm = T)

telecom_new$models[is.na(telecom_new$models)]<-mean(telecom_new$models,na.rm = T)

telecom_new$datovr_Mean[is.na(telecom_new$datovr_Mean)]<-mean(telecom_new$datovr_Mean,na.rm = T)


#removing the categorical variables.to plot boxplot for continuous variables
names(telecom_new)

list<-names(telecom_new)
list
#also removing some continuous variables which doesn't seem to have outliers 


list<-list[-c(27:34,36:48,58:61,16,18,52,51)]
list
#32 variables


#outlier plots and impute with them by mean
par(mar=c(1,1,1,1))
#store the names of the dataset in a list format
par(mfrow=c(4,9))
#plot the boxplots of all the variables and shortlist which ones need outlier treatment
for(i in 1:length(list))
{
  boxplot(telecom_new[,list[i]],main=list[i])#This allows us to plot 36 plots in a single page.
}


#Run 10 times to remove outliers from all the variables
#here I am replacing outliers by respective mean 

par(mar=c(1,1,1,1))
par(mfrow=c(4,9))
for(i in 1:length(list))
{
  x<-boxplot(telecom_new[,list[i]],main=list[i])
  out<-x$out
  index<-which(telecom_new[,list[i]]%in% x$out)
  telecom_new[index,list[i]]<-mean(telecom_new[,list[i]])
  rm(x)
  rm(out)
}
#after running it for 10 times all the outliers will be removed
write.csv(telecom_new,"telecom_new.csv")

#Restore the par parameters to normal
dev.off()




# decile analysis for categorical variables
#also Imputation by observing the event rate


telecom_new%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC1
datC1$N<-unclass(telecom_new%>%filter(prizm_social_one%in%datC1$levels)%>%count(prizm_social_one))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("prizm_social_one",nrow(datC1))
datC1
telecom_new$prizm_social_one[is.na(telecom_new$prizm_social_one)]<-"T"



telecom_new%>%count(churn,levels=area)%>%filter(churn==1)->datC2
datC2$N<-unclass(telecom_new%>%filter(area%in%datC2$levels)%>%count(area))[[2]]
datC2$ChurnPerc<-datC2$n/datC2$N
datC2$Var.Name<-rep("area",nrow(datC2))
datC2
telecom_new$area[is.na(telecom_new$area)]<-"HOUSTON AREA"


telecom_new%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC3
datC3$N<-unclass(telecom_new%>%filter(hnd_webcap%in%datC3$levels)%>%count(hnd_webcap))[[2]]
datC3$ChurnPerc<-datC3$n/datC3$N
datC3$Var.Name<-rep("hnd_webcap",nrow(datC3))
datC3
telecom_new$hnd_webcap[is.na(telecom_new$hnd_webcap)]<-"WC"



telecom_new%>%count(churn,levels=marital)%>%filter(churn==1)->datC4
datC4$N<-unclass(telecom_new%>%filter(marital%in%datC4$levels)%>%count(marital))[[2]]
datC4$ChurnPerc<-datC4$n/datC4$N
datC4$Var.Name<-rep("marital",nrow(datC4))
datC4
telecom_new$marital[is.na(telecom_new$marital)]<-"S"





telecom_new%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC5
datC5$N<-unclass(telecom_new%>%filter(ethnic%in%datC5$levels)%>%count(ethnic))[[2]]
datC5$ChurnPerc<-datC5$n/datC5$N
datC5$Var.Name<-rep("ethnic",nrow(datC5))
datC5
telecom_new$ethnic[is.na(telecom_new$ethnic)]<-"M"




telecom_new%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC6
datC6$N<-unclass(telecom_new%>%filter(car_buy%in%datC6$levels)%>%count(car_buy))[[2]]
datC6$ChurnPerc<-datC6$n/datC6$N
datC6$Var.Name<-rep("car_buy",nrow(datC6))
datC6
telecom_new$car_buy[is.na(telecom_new$car_buy)]<-"New"


summary(telecom_new$csa)

#telecom_new$csa
#unique(telecom_new$csa)
telecom_new%>%count(churn,levels=csa)%>%filter(churn==1)->datC7
datC7$N<-unclass(telecom_new%>%filter(csa%in%datC7$levels)%>%count(csa))[[2]]
datC7$ChurnPerc<-datC7$n/datC7$N
datC7$Var.Name<-rep("csa",nrow(datC7))
datC7
telecom_new$csa[is.na(telecom_new$csa)]<-"AIRORA803"



telecom_new%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC8
datC8$N<-unclass(telecom_new%>%filter(marital%in%datC8$levels)%>%count(refurb_new))[[2]]
datC8$ChurnPerc<-datC8$n/datC8$N
datC8$Var.Name<-rep("marital",nrow(datC8))
datC8
telecom_new$refurb_new[is.na(telecom_new$refurb_new)]<-"N"

unique(telecom_new$crclscod)
telecom_new%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC9
datC9$N<-unclass(telecom_new%>%filter(crclscod%in%datC9$levels)%>%count(crclscod))[[2]]
datC9$ChurnPerc<-datC9$n/datC9$N
datC9$Var.Name<-rep("crclscod",nrow(datC9))
datC9



telecom_new%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC10
datC10$N<-unclass(telecom_new%>%filter(asl_flag%in%datC10$levels)%>%count(asl_flag))[[2]]
datC10$ChurnPerc<-datC10$n/datC10$N
datC10$Var.Name<-rep("asl_flag",nrow(datC10))
datC10




telecom_new%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC10
datC10$N<-unclass(telecom_new%>%filter(asl_flag%in%datC10$levels)%>%count(asl_flag))[[2]]
datC10$ChurnPerc<-datC10$n/datC10$N
datC10$Var.Name<-rep("asl_flag",nrow(datC10))
datC10

names(telecom_new)

telecom_new%>%count(churn,levels=retdays1)%>%filter(churn==1)->datC11
datC11$N<-unclass(telecom_new%>%filter(retdays1%in%datC11$levels)%>%count(retdays1))[[2]]
datC11$ChurnPerc<-datC11$n/datC11$N
datC11$Var.Name<-rep("retdays1",nrow(datC11))
datC11

#few variables were getting less deciles so doing data transformation or delete them
# here i am checking event rate and then convert them into factor for the further analysis
#not giving any error so continue doing the same


unique(telecom_new$models)
telecom_new%>%count(churn,levels=models)%>%filter(churn==1)->datC12
datC12$N<-unclass(telecom_new%>%filter(models%in%datC12$levels)%>%count(models))[[2]]
datC12$ChurnPerc<-datC12$n/datC12$N
datC12$Var.Name<-rep("models",nrow(datC12))
datC12

unique(telecom_new$actvsubs)
telecom_new%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC13
datC13$N<-unclass(telecom_new%>%filter(actvsubs%in%datC13$levels)%>%count(actvsubs))[[2]]
datC13$ChurnPerc<-datC13$n/datC13$N
datC13$Var.Name<-rep("actvsubs",nrow(datC13))
datC13

unique(telecom_new$uniqsubs)
telecom_new%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC14
datC14$N<-unclass(telecom_new%>%filter(uniqsubs%in%datC14$levels)%>%count(uniqsubs))[[2]]
datC14$ChurnPerc<-datC14$n/datC14$N
datC14$Var.Name<-rep("uniqsubs",nrow(datC14))
datC14

unique(telecom_new$forgntvl)
telecom_new%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC15
datC15$N<-unclass(telecom_new%>%filter(forgntvl%in%datC15$levels)%>%count(forgntvl))[[2]]
datC15$ChurnPerc<-datC15$n/datC15$N
datC15$Var.Name<-rep("forgntvl",nrow(datC15))
datC15

unique(telecom_new$mtrcycle)
telecom_new%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC16
datC16$N<-unclass(telecom_new%>%filter(mtrcycle%in%datC16$levels)%>%count(mtrcycle))[[2]]
datC16$ChurnPerc<-datC16$n/datC16$N
datC16$Var.Name<-rep("mtrcycle",nrow(datC16))
datC16

unique(telecom_new$hnd_price)
telecom_new%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC17
datC17$N<-unclass(telecom_new%>%filter(hnd_price%in%datC17$levels)%>%count(hnd_price))[[2]]
datC17$ChurnPerc<-datC17$n/datC17$N
datC17$Var.Name<-rep("hnd_price",nrow(datC17))
datC17

datC_1<-rbind.data.frame(datC1,datC2,datC3,datC4,datC5,datC6,datC7,datC8,datC9,datC10)
datC_2<-rbind.data.frame(datC11,datC12,datC13,datC14,datC15,datC16,datC17)

write.csv(datC_1,"Categorical variable_1.csv")
write.csv(datC_2,"Categorical variable_2.csv")
str(telecom_new)


#since csa and crclscod has many levels. using decision tree,  reducing the levels


# data=data.frame(csa=rownames(telecom_new),target=telecom_new$churn)
# unique(data$csa)
# unique(data$target)
# library(rpart)
# modd<-rpart(target~csa,data = data,method = "class")
# unique(modd$where)
# data$Nodes<-modd$where
# data
# 
# 
# names(telecom_new)
# 
# telecom_new$csa_new<-ifelse(data$Nodes == 2,2,3)
# unique(telecom_new$csa_new)
# telecom_new<-telecom_new[,-46]
# 
# 
# #Dt2
# data=data.frame(crclscod=rownames(telecom_new),target=telecom_new$churn)
# unique(data$crclscod)
# unique(data$target)
# library(rpart)
# modd<-rpart(target~crclscod,data = data,method = "class")
# unique(modd$where)
# data$Nodes<-modd$where
# data
# 
# telecom_new$crclscod_new<-ifelse(data$Nodes == 2,2,3)
# unique(telecom_new$crclscod_new)
# 
# telecom_new<-telecom_new[,-27]





names(telecom_new)
telecom_new<-telecom_new[,-c(27,48)]
#If we include these 2 variables in model resulting in insignificant variables hence we can ignore 
#59 variables


#Age can be coverted to categorical variables 
summary(telecom_new)
unique(telecom$age1)
telecom_new$age1_new<-ifelse(telecom_new$age1==0,"default value",ifelse(telecom_new$age1<=30,"young",
                                                           ifelse(telecom_new$age1>30 & telecom_new$age1<=60,"Mid age","old")))
telecom_new$age1_new<-as.factor(telecom_new$age1_new)

names(telecom_new)
telecom_new<-telecom_new[,-34]

unique(telecom$age2)
telecom_new$age2_new<-ifelse(telecom_new$age2==0,"default value",ifelse(telecom_new$age2<=30,"young",
                                                                  ifelse(telecom_new$age2>30 & telecom_new$age2<=60,"Mid age","old")))
telecom_new$age2_new<-as.factor(telecom_new$age2_new)
names(telecom_new)
telecom_new<-telecom_new[,-34]

unique(telecom_new$age1_new)
table(telecom_new$age1_new)
unique(telecom_new$age2_new)
table(telecom_new$age2_new)


#(as per documentation)since it cannot be deciled and  has few levels and try converting them into factors and seeing the result
#if it's coming significant taking into consideration or else ignore

telecom_new$models<-as.factor(telecom_new$models)

telecom_new$hnd_price<-as.factor(telecom_new$hnd_price)

telecom_new$actvsubs<-as.factor(telecom_new$actvsubs)

telecom_new$uniqsubs<-as.factor(telecom_new$uniqsubs)

telecom_new$forgntvl<-as.factor(telecom_new$forgntvl)

telecom_new$mtrcycle<-as.factor(telecom_new$mtrcycle)

telecom_new$truck<-as.factor(telecom_new$truck)

write.csv(telecom_new,"Telcom_new.csv")

###Building the model
#spliting the data into trainig & testing data set
set.seed(900)
index00<-sample(nrow(telecom_new),0.70*nrow(telecom_new))
train<-telecom_new[index00,]
test<-telecom_new[-index00,]



names(telecom_new)
mod<-glm(churn~.,data = train[,-54],family = "binomial")
summary(mod)


#     Null deviance: 51015  on 46406  degrees of freedom
# Residual deviance: 49029  on 46261  degrees of freedom
# AIC: 49321
# 
# Number of Fisher Scoring iterations: 10



#creating Dummies for significant variables
train$asl_flag_Y<-ifelse(train$asl_flag == "Y",1,0)
test$asl_flag_Y<-ifelse(test$asl_flag == "Y",1,0)


train$area_texas<-ifelse(train$area == "CENTRAL/SOUTH TEXAS AREA",1,0)
test$area_texas<-ifelse(test$area == "CENTRAL/SOUTH TEXAS AREA",1,0)

train$area_houtson<-ifelse(train$area == "HOUSTON AREA",1,0)
test$area_houtson<-ifelse(test$area == "HOUSTON AREA",1,0)



train$area_northwest<-ifelse(train$area == "NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
test$area_northwest<-ifelse(test$area == "NORTHWEST/ROCKY MOUNTAIN AREA",1,0)


train$area_south_florida<-ifelse(train$area == "SOUTH FLORIDA AREA",1,0)
test$area_south_florida<-ifelse(test$area == "SOUTH FLORIDA AREA",1,0)


train$area_tennesse<-ifelse(train$area == "TENNESSEE AREA",1,0)
test$area_tennesse<-ifelse(test$area == "TENNESSEE AREA",1,0)



train$refurb_new_R<-ifelse(train$refurb_new == "R",1,0)
test$refurb_new_R<-ifelse(test$refurb_new == "R",1,0)




train$ethnic_C<-ifelse(train$ethnic == "C",1,0)
test$ethnic_C<-ifelse(test$ethnic == "C",1,0)


train$ethnic_F<-ifelse(train$ethnic == "F",1,0)
test$ethnic_F<-ifelse(test$ethnic == "F",1,0)


train$ethnic_G<-ifelse(train$ethnic == "G",1,0)
test$ethnic_G<-ifelse(test$ethnic == "G",1,0)


train$ethnic_H<-ifelse(train$ethnic == "H",1,0)
test$ethnic_H<-ifelse(test$ethnic == "H",1,0)


train$ethnic_I<-ifelse(train$ethnic == "I",1,0)
test$ethnic_I<-ifelse(test$ethnic == "I",1,0)


train$ethnic_N<-ifelse(train$ethnic == "N",1,0)
test$ethnic_N<-ifelse(test$ethnic == "N",1,0)


train$ethnic_P<-ifelse(train$ethnic == "P",1,0)
test$ethnic_P<-ifelse(test$ethnic == "P",1,0)


train$ethnic_S<-ifelse(train$ethnic == "S",1,0)
test$ethnic_S<-ifelse(test$ethnic == "S",1,0)


train$ethnic_U<-ifelse(train$ethnic == "U",1,0)
test$ethnic_U<-ifelse(test$ethnic == "U",1,0)
 

train$ethnic_Z<-ifelse(train$ethnic == "Z",1,0)
test$ethnic_Z<-ifelse(test$ethnic == "Z",1,0)



train$hnd_price_105.16<-ifelse(train$hnd_price == "105.165056386471",1,0)
test$hnd_price_105.16<-ifelse(test$hnd_price == "105.165056386471",1,0)


train$hnd_price_199.98<-ifelse(train$hnd_price == "199.9899902",1,0)
test$hnd_price_199.98<-ifelse(test$hnd_price == "199.9899902",1,0)


train$hnd_price_249.98<-ifelse(train$hnd_price == "249.9899902",1,0)
test$hnd_price_249.98<-ifelse(test$hnd_price == "249.9899902",1,0)


train$uniqsubs_2<-ifelse(train$uniqsubs == "2",1,0)
test$uniqsubs_2<-ifelse(test$uniqsubs == "2",1,0)

train$uniqsubs_3<-ifelse(train$uniqsubs == "3",1,0)
test$uniqsubs_3<-ifelse(test$uniqsubs == "3",1,0)

train$uniqsubs_4<-ifelse(train$uniqsubs == "4",1,0)
test$uniqsubs_4<-ifelse(test$uniqsubs == "4",1,0)

train$uniqsubs_5<-ifelse(train$uniqsubs == "5",1,0)
test$uniqsubs_5<-ifelse(test$uniqsubs == "5",1,0)

train$uniqsubs_6<-ifelse(train$uniqsubs == "6",1,0)
test$uniqsubs_6<-ifelse(test$uniqsubs == "6",1,0)

train$uniqsubs_7<-ifelse(train$uniqsubs == "7",1,0)
test$uniqsubs_7<-ifelse(test$uniqsubs == "7",1,0)

train$uniqsubs_8<-ifelse(train$uniqsubs == "8",1,0)
test$uniqsubs_8<-ifelse(test$uniqsubs == "8",1,0)

train$uniqsubs_9<-ifelse(train$uniqsubs == "9",1,0)
test$uniqsubs_9<-ifelse(test$uniqsubs == "9",1,0)



train$prizm_social_one_R<-ifelse(train$prizm_social_one == "R",1,0)
test$prizm_social_one_R<-ifelse(test$prizm_social_one == "R",1,0)

train$prizm_social_one_T<-ifelse(train$prizm_social_one == "T",1,0)
test$prizm_social_one_T<-ifelse(test$prizm_social_one == "T",1,0)

train$prizm_social_one_S<-ifelse(train$prizm_social_one == "S",1,0)
test$prizm_social_one_S<-ifelse(test$prizm_social_one == "S",1,0)



train$age1_new_Mid_age<-ifelse(train$age1_new == "Mid age",1,0)
test$age1_new_Mid_age<-ifelse(test$age1_new == "Mid age",1,0)

train$age1_new_old<-ifelse(train$age1_new == "old",1,0)
test$age1_new_old<-ifelse(test$age1_new == "old",1,0)



train$truck_1<-ifelse(train$truck == "1",1,0)
test$truck_1<-ifelse(test$truck == "1",1,0)


train$models_3<-ifelse(train$models == "3",1,0)
test$models_3<-ifelse(test$models == "3",1,0)


names(train)
mod1<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+drop_vce_Range+mou_opkv_Range+months+eqpdays+iwylis_vce_Mean+
            ovrrev_Mean+rev_Mean+plcd_vce_Mean+avg3mou+avgmou+avgqty+retdays1+adjmou+avgrev+totrev+asl_flag_Y+area_texas+area_houtson+
            area_northwest+area_south_florida+area_tennesse+refurb_new_R+ethnic_C+ethnic_F+ethnic_G+ethnic_H+ethnic_I+
            ethnic_N+ethnic_P+ethnic_S+ethnic_U+ethnic_Z+hnd_price_105.16+hnd_price_199.98+hnd_price_249.98+uniqsubs_2+
            uniqsubs_3+uniqsubs_4+uniqsubs_5+uniqsubs_6+uniqsubs_7+uniqsubs_8+uniqsubs_9+prizm_social_one_R+prizm_social_one_T+
            prizm_social_one_S+age1_new_Mid_age+age1_new_old+truck_1+models_3,data = train,family = "binomial")

summary(mod1)



mod2<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+drop_vce_Range+mou_opkv_Range+months+eqpdays+iwylis_vce_Mean+
            ovrrev_Mean+rev_Mean+plcd_vce_Mean+avg3mou+avgmou+avgqty+retdays1+adjmou+avgrev+totrev+asl_flag_Y+area_texas+area_houtson+
            area_northwest+area_south_florida+area_tennesse+refurb_new_R+ethnic_C+ethnic_F+ethnic_G+ethnic_H+ethnic_I+
            ethnic_N+ethnic_P+ethnic_S+ethnic_U+ethnic_Z+hnd_price_105.16+hnd_price_199.98+hnd_price_249.98+uniqsubs_2+
            uniqsubs_3+uniqsubs_4+uniqsubs_5+uniqsubs_7+uniqsubs_8+uniqsubs_9+prizm_social_one_R+prizm_social_one_T+
            prizm_social_one_S+age1_new_Mid_age+models_3+age1_new_old+truck_1,data = train,family = "binomial")

summary(mod2)


 
# 
# Null deviance: 51015  on 46406  degrees of freedom
# Residual deviance: 49243  on 46352  degrees of freedom
# AIC: 49353
# 
# Number of Fisher Scoring iterations: 4


#step(mod,direction = "both")




#all variables coming significant
#Checking for multicolliniarity,ignore variable if value>5
library(car)
vif(mod2)
#all variables have vif values below 5,so we can go for mod2



#predict probability of customer churning
prob=predict(mod2,newdata = test,type="response")
head(prob)

#Confusion matrix will help us in understand how the model behaves at a give point.
#Confusion matrix(i.e. checking accuracy at given probability point)
class=ifelse(prob>=0.5,1,0)
t=table(class,test$churn)
t
sum(diag(t))/nrow(test)

library(ROCR)
pred=prediction(prob,test$churn)
roc=performance(pred,"tpr","fpr")
plot(roc)
abline(0,1)


#Accuracy of the model
auc=performance(pred,"auc")
auc=unlist(slot(auc,"y.values"))
auc
#The auc is 0.6274849 approximately equal to 0.63
#so the model is fine and acceptable 


#Business Questions
#Q1)T
#we could see top 5 factors driving liklihood of churn at mobicom through graph (using ggplot)

library(ggplot2)
var_names=c("mou_Mean","totmrc_Mean","rev_Range","mou_Range","change_mou","drop_vce_Range","mou_opkv_Range","months","eqpdays","iwylis_vce_Mean",
            
            "ovrrev_Mean","rev_Mean","plcd_vce_Mean","avg3mou","avgmou","avgqty","retdays1","adjmou","avgrev","totrev","asl_flag_Y","area_texas","area_houtson",
            
            "area_northwest","area_south_florida","area_tennesse","refurb_new_R","ethnic_C","ethnic_F","ethnic_G","ethnic_H","ethnic_I",
            
            "ethnic_N","ethnic_P","ethnic_S","ethnic_U","ethnic_Z","hnd_price_105.16","hnd_price_199.98","hnd_price_249.98","uniqsubs_2",
            
            "uniqsubs_3","uniqsubs_4","uniqsubs_5","uniqsubs_6","uniqsubs_7","uniqsubs_8","uniqsubs_9","prizm_social_one_R","prizm_social_one_T",
            
            "prizm_social_one_S","age1_new_Mid_age","models_3","age1_new_old"," truck_1 " )
#53

coef_values=c(-0.00036669159,-0.00882016085,0.00254740809,0.00025995037, -0.00066581273,0.01686478758, -0.00060571319,-0.01070315339,0.00092968174,-0.01736971903,
              
              0.00268740066,0.00332001529,-0.00099504709,-0.00036201941,0.00041509007,0.00073915322,0.73402201549,0.00002314758,0.00329854178,0.00015353861,-0.28572118783,
              
              -0.15545808143, -0.15402027728,0.24277809961,0.19104063682, -0.17420410511,0.20037642394,-1.04719821480, -0.16597405648, -0.21652181182,-0.12456107333,
              
              -0.20487093058,-0.25614614962, -0.49492981086,-0.22480535144,-0.20158430501,-0.63850033359,-0.26791844118,-0.21679060596, -0.86198237342,0.21167664015,
              
              0.19041664074,0.22448242710,0.20867014825,0.19721176851,0.67384248317,0.87033944582,2.13869569166,0.15883990798,0.08096804323,-0.05819337110,-0.14498659967,
              
              -0.12898202933,-0.34855835,0.05678211)
#53 

final_data<-data.frame(var_names,coef_values)
p<-ggplot(final_data,aes(as.factor(x=var_names),y=coef_values,angle=45))+geom_bar(stat = "identity",fill="dark blue")
p+theme(axis.text.x = element_text(angle = 70,hjust = 1),panel.grid.major.x = element_blank() )+ggtitle("Beta Coefficient")

#53
#coess<-mod2$coefficients







#top 5 factors driving liklihood of churn at mobicom 
head(sort(abs(mod2$coefficients),decreasing = T),6)
summary(mod2)
# There are 9 no.'s of unique subscribers in the household
# Current handset price is 239.9899902
# There are 8 no.'s of unique subscribers in the household




#uniqsubs_9        2.1535397              
#ethnic_C          - 1.0655293
#hnd_price_249.98  - 0.8721436
#uniqsubs_8       0.8299417 
#retdays1          0.7332340
  

#only positive coefficients
head(sort((mod2$coefficients),decreasing = T),5)

# uniqsubs_9          2.1535397
# uniqsubs_8          0.8299417
# retdays1            0.7332340
# uniqsubs_7          0.6730534
# area_northwest      0.2350440


#since all factors have positive beta coefficients a one unit increase in these factors will result in an average increase
#in the churn rate.
#_________________________________________________________________________

#Q2


#totmrc _mean
#Cost is analyzed by analyzing monthly recurring charge (MRC). 
#The chart indicates that as the monthly recurring charge of a customer increases,
#the churn rate drops steadily. One of the reasons for this could be that higher 	MRC points to 
#more number of family members in the same plan which makes it more difficult to churn. 

#The model gives us a positive coefficient "0.002260397" for the factor
# "REV_RANGE"
# ..........this implies it has a postitive impact,
# i.e. a unit increase in the revenue will have a unit increase in the churn 
# behaviour, thus leading to churn


# Factor "OVRREV_MEAN" gives us a coefficient of value "0.008319130"
# ..........this implies it has a postitive impact,
# i.e. a unit increase in the overage revenue will have a unit increase in the 
# churn behaviour, thus leading to churn


# Factor "TOTREV" gives us a coefficient of value "0.0001651900"
# ..........this implies it has a postitive impact,
# i.e. a unit increase in the total revenue earned,will have a unit increase in 
# the churn behaviour, thus leading to churn


# Conclusion : 
# Since the unit increase is very miniscule, the "cost and billing" is
# not a really important factor influencing churn behaviour

#______________________________________________________________________________

#Q3



#retdays : Higher the number of days since last retention call, 
#          higher is the churn rate

#mou_Mean : Less number of monthly minutes of usage higher is the churn rate

#months : less no. of months the plan is in service, the higher is the churn rate





#COMPLETE_MEAN	Mean number of completed calls
#Decrease in the number of completed calls will increase the churn rate


# DROP_BLK_MEAN	Mean number of dropped or blocked calls



# DROP_VCE_RANGE	Range of number of dropped (failed) voice calls
# Increase in the number of dropped (failed) voice calls, results in high churn rate



#iwylis_vce_Mean : Providing less service of wireless voice calls will attract 
#                  higher churn rate

# IWYLIS_VCE_MEAN	Mean number of inbound wireless to wireless voice calls
# Decrease in the number of inbound wireless voice calls will increase churn rate


#_____________________________________________________________________________________

g<-telecom_new$comp_vce_Mean/telecom_new$totcalls
summary(g)
g<-mean(g)
sum(g)/nrow(g)
# 
# comp_dat_Mean : 0.02450105374(Mean no. of completed data calls) 
# plcd_dat_Mean : -0.02254696754(Mean number of attempted data calls placed)
# opk_dat_Mean: -0.00275570300(Mean number of off-peak data calls)
# blck_dat_Mean: 0.02149221933(Mean no. of blocked / failed data calls)
# drop_dat_Mean : 0.01071048883(Mean no. of dropped / failed data calls)


#if we look at the values we  could see less numberof 
#of completed calls, data and voice calls, off-peak calls and wireless calls.


#Q

#If the customer incurs overage charges consistently, either because of data overage or 
#minutes overage, then such customers should be migrated to a different rate plan as a 
#proactive retention strategy.

#_______________________________________________________________________________
#Q4)

#gains

library(gains)
gains(test$churn,predict(mod2,type = "response",newdata = test),groups = 10)
#to take action on particular segment of people we use gain char
#the gains chart shows top 20% of the probabbilities contain 30.0%customers that are highly likely to churn at mobicom
#gains chart shows that top 30% probabilities contain 42.6 % cutomers that are likely to churn and so on

test$prob<-predict(mod2,type="response",newdata = test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

# top 20%(80 to 100) of the probabbilities lie between  0.3028321 and 0.7662783  
#highly likely to churn

#Applying condition to predict customers who will likely to  churn 
predict1<-predict(mod2,type="response",newdata = test)



#applying cut off
predict2<-ifelse(predict1>=0.3028321,1,0)
table(predict2,test$churn)

target_cust<-test[test$prob>0.3028321 & test$prob<=0.7662783 & test$churn=="1","Customer_ID"]
target_cust<-as.data.frame(target_cust)
nrow(target_cust)
#1434

write.csv(target_cust,"Target_customers(likely to churn).csv")





#using  customer ID we can predict the customers who likely to churn

predict3<-predict(mod2,type="response",newdata = test)
#testing on test data set
test$prob<-predict(mod2,type="response",newdata = test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
predict4<-ifelse(predict3<0.20,"Low",ifelse(predict3>=0.20 & predict3<0.30,"Medium","High"))
table(predict4,test$churn)

## This Result can help to select the level of customer need to be targeted 
#basesd on Totrev
str(test$totrev)
summary(test$totrev)
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
#randomly choose cut off
revenue_split<-ifelse(test$totrev<668.3580,"Low Revenue",ifelse(test$totrev>=668.3580 & test$totrev<1114.8960,"Medium Revenue","High Revenue"))
table(revenue_split)
table(predict4,revenue_split)#checking the prediction quality


#Probability of churn scores vs revenue :

test$prob_levels<-ifelse(predict3<0.20,"Low",ifelse(predict3>=0.20 & predict3<0.30,"Medium","High"))
#putting it in testing data set
test$revenue_split<-ifelse(test$totrev<668.3580,"Low Revenue",ifelse(test$totrev>=668.3580 & test$totrev<1114.8960,"Medium Revenue","High Revenue"))

target_cust1<-test[test$prob_levels=="High" & test$revenue_split=="High Revenue" ,"Customer_ID"]
target_cust1<-as.data.frame(target_cust1)
nrow(target_cust1)
#1291
View(target_cust1)
write.csv(target_cust1,"High revenue target customers(likely to churn).csv")

        



######################End######








# 
# predict3<-predict(mod2,type="response",newdata = test)
# test$prob<-predict(mod2,type="response",newdata = test)
# quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
# predict4<-ifelse(predict3<0.20,"Low",ifelse(predict3>=0.20 & predict3<0.30,"Medium","High"))
# table(predict4,test$churn)
# 
# 
# #basesd on Totrev
# str(test$totrev)
# summary(test$totrev)
# quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
# revenue_split<-ifelse(test$totrev<453.3340,"Low Revenue",ifelse(test$totrev>=453.3340 & test$totrev<556.3940 ,"Medium Revenue","High Revenue"))
# table(revenue_split)
# table(predict4,revenue_split)
# 
# 
# 
# 
# test$prob_levels<-ifelse(predict3<0.20,"Low",ifelse(predict3>=0.20 & predict3<0.30,"Medium","High"))
# test$revenue_split<-ifelse(test$totrev<453.3340,"Low Revenue",ifelse(test$totrev>=453.3340 & test$totrev<556.3940 ,"Medium Revenue","High Revenue"))
# 
# target_cust1<-test[test$prob_levels=="High" & test$revenue_split=="High Revenue" ,"Customer_ID"]
# target_cust1<-as.data.frame(target_cust1)
# nrow(target_cust1)
# View(target_cust1)
# write.csv(target_cust1,"High revenue target customers.csv")

