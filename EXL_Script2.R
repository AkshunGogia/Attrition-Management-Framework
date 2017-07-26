
library(dplyr)
library(plyr)
library(readxl)
library(tidyr)
library(xlsx)
library(randomForest)
library(e1071)
library(caret)
library(rpart)
library(ggplot2)


study<-read_excel("Study Case.xlsx", sheet=1, col_names = TRUE, skip=5)

study<-study[-c(14953:15000),]

View(study)

summary(study)

lapply(study, class)

study_churned<-study
View(study_churned)
study_churned$Churned<-0
study_churned$Churned<-ifelse(!is.na(study_churned$`Churn Date`), 1,0)
study_churned<-filter(study_churned,Churned==1)
write.xlsx(study_churned,file="study_churned.xlsx")

study_2<-study
View(study_2)
study_2$Churned<-0
study_2$Churned<-ifelse(!is.na(study_2$`Churn Date`), 1,0)
study_2<-study[-c(9967:nrow(study_2)),]

write.csv(study_2,file="study_2.csv")

stu<-study_2

stu<-left_join(stu,u)
write.csv(stu,file="stu.csv")

st<-study[,c(1,15)]
View(st)
lapply(st, class)
colSums(is.na(st))
summary(st)

st$Churned<-0

st$Churned<-ifelse(!is.na(st$`Churn Date`), 1,0)

st<-st[,-2]

st<-st[-c(9967:nrow(st)),]




payment<-read_excel("Study Case.xlsx", sheet=3, col_names = TRUE, skip=5)
payment<-payment[,-5]
library(lubridate)
payment$Month<-format(payment$`Realization Date`, "%m")
pay<-payment
pay<-pay[-c(27758:nrow(pay)),]


pay$Month<-as.numeric(pay$Month)

z<-aggregate(Month~`Account Number`, pay, max)
z<-filter(z,Month!=3)

#pay1<-filter(pay,Month==3)
#new_pay1<-full_join(pay1,z)
#View(new_pay1)

z1<-inner_join(z,pay)
z2<-filter(pay,Month==3)
z_final<-full_join(z1,z2)

z_new<-aggregate(Sum~`Account Number`,z_final,sum)
View(z_new)
zz<-inner_join(z_new,st)

subset(st, !(`Account Number` %in% zz$`Account Number`))  #to check those accounts that are in d1 but not st

zz_churned<-filter(zz,Churned==1)
View(zz_churned)
zz_not_churned<-filter(zz,Churned==0)
View(zz_not_churned)


sd(zz_churned$Sum)
range(zz_churned$Sum)
mean(zz_churned$Sum)

sd(zz_not_churned$Sum)
range(zz_not_churned$Sum)
mean(zz_not_churned$Sum)







View(payment)
lapply(payment, class)

payment$`Account Number`<-as.factor(payment$`Account Number`)
payment$`Mode `<-as.factor(payment$`Mode `)

payment$`Mode `<-revalue(payment$`Mode `, c(" Card I TYPE"=1, " Card II TYPE"=2,
                                            "Paper based/Post"=3, "Other"=4))


Mo <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

pay1<-aggregate(Sum~`Account Number`, payment, mean)

payment$`Mode `<-as.numeric(as.character(payment$`Mode `))
pay2<-aggregate(`Mode `~`Account Number`, payment, Mo)

View(pay1)
View(pay2)

pay<-cbind(pay1,pay2)
View(pay)
pay<-pay[,-3]
pay$`Mode `<-as.factor(pay$`Mode `)




default<-read_excel("Study Case.xlsx", sheet=4, col_names = TRUE, skip=5)
View(default)
default<-default[-c(2472:nrow(default)),]

summary(default)
colSums(is.na(default))
lapply(default,class)
default$Defaulted<-"Yes"
default$`date of default2`<-default$`date of default`
default$Defaulted<-as.factor(default$Defaulted)
View(default)
default<-separate(default, col=`date of default2`, into = c("Year","Month","Date"),sep="-")

default$Year<-as.numeric(default$Year)
default$Month<-as.numeric(default$Month)
default$Date<-as.numeric(default$Date)
default$`Account Number`<-as.factor(default$`Account Number`)

default$No_of_Defaults<-1



d1<-aggregate(`Default sum` ~`Account Number`, default, max)  
View(d1)

d1<-d1[-c(1186:1731),]



j<-join(d1,st,type="inner")

subset(d1, !(`Account Number` %in% j$`Account Number`))  #to check those accounts that are in d1 but not st

j_churned<-filter(j,Churned==1)
View(j_churned)
j_not_churned<-filter(j,Churned==0)
View(j_not_churned)


sd(j_churned$`Default sum`)
range(j_churned$`Default sum`)
mean(j_churned$`Default sum`)

sd(j_not_churned$`Default sum`)
range(j_not_churned$`Default sum`)
mean(j_not_churned$`Default sum`)


use<-read_excel("Study Case.xlsx", sheet=5, col_names = TRUE, skip=5)

colnames(use)[1]<- "Account Number"

View(use)
summary(use)
use<-use[-c(41731:nrow(use)),]
lapply(use,class)

use<-separate(use, col='YYYYMM', into=c("Year", "Month"), sep="2015")
use$Year<-2015

lapply(use, class)
use$Month<-as.numeric(use$Month)
use$`Account Number `<-as.factor(use$`Account Number `)

u<-aggregate(`Used pattern in hours` ~ `Account Number`,use, mean)
View(u)

u_join<-join(u,st,type="inner")

subset(st, !(`Account Number` %in% u_join$`Account Number`))

u_join_churned<-filter(u_join,Churned==1)
View(u_join_churned)

u_join_not_churned<-filter(u_join,Churned==0)
View(u_join_not_churned)

sd(u_join_churned$`Used pattern in hours`)
range(u_join_churned$`Used pattern in hours`)
mean(u_join_churned$`Used pattern in hours`)

sd(u_join_not_churned$`Used pattern in hours`)
range(u_join_not_churned$`Used pattern in hours`)
mean(u_join_not_churned$`Used pattern in hours`)





u_post<-aggregate(`Usage Post Limit Utilization in hrs` ~ `Account Number`,use, mean)
View(u_post)

u_post_join<-join(u_post,st,type="inner")
View(u_post_join)

u_post_join_churned<-filter(u_post_join,Churned==1)
View(u_post_join_churned)

u_post_join_not_churned<-filter(u_post_join,Churned==0)
View(u_post_join_not_churned)

sd(u_post_join_churned$`Usage Post Limit Utilization in hrs`)
range(u_post_join_churned$`Usage Post Limit Utilization in hrs`)
mean(u_post_join_churned$`Usage Post Limit Utilization in hrs`)

sd(u_post_join_not_churned$`Usage Post Limit Utilization in hrs`)
range(u_post_join_not_churned$`Usage Post Limit Utilization in hrs`)
mean(u_post_join_not_churned$`Usage Post Limit Utilization in hrs`)

call<-read_excel("Study Case.xlsx", sheet=2, col_names = TRUE, skip=5)
View(call)
lapply(call,class)



study2<-read_excel("Study Case.xlsx", sheet=1, col_names = TRUE, skip=5)
study2<-study2[-c(14953:15000),]

View(study2)
summary(study2)
lapply(study2, class)
which(lapply(study2, is.character)==TRUE)


class()




default2<-read_excel("Study Case.xlsx", sheet=4, col_names = TRUE, skip=5)
View(default2)
default2<-default2[-c(2472:nrow(default2)),]

default2$Defaulted<-"Yes"
default2$Default_Times<-1

de1<-aggregate(Default_Times~`Account Number`,default2, sum)
View(de1)
lapply(de1, class)

study2<-left_join(study2,de1)  
View(study2)
study2$Default_Times<-ifelse(is.na(study2$Default_Times),0,study2$Default_Times)


call2<-read_excel("Study Case.xlsx", sheet=2, col_names = TRUE, skip=5)
View(call2)
call2<-na.omit(call2)

call2$Query_Times<-1

ce1<-aggregate(Query_Times~`Account Number`,call2,sum)
study2<-left_join(study2,ce1)
study2$Query_Times<-ifelse(is.na(study2$Query_Times),0,study2$Query_Times)

Mo <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

study2$`Professional Info`<-ifelse(study2$`Professional Info`==13, 12,study2$`Professional Info`)
study2$`Professional Info`<-ifelse(study2$`Professional Info`==14, 12,study2$`Professional Info`)
study2$`Professional Info`<-ifelse(study2$`Professional Info`==16, 12,study2$`Professional Info`)
study2$`Professional Info`<-ifelse(study2$`Professional Info`==18, 12,study2$`Professional Info`)

study2$`Professional Info`<-ifelse(study2$`Professional Info`==15, 5,study2$`Professional Info`)
study2$`Professional Info`<-ifelse(study2$`Professional Info`==6, 5,study2$`Professional Info`)
study2$`Professional Info`<-ifelse(study2$`Professional Info`==8, 5,study2$`Professional Info`)
study2$`Professional Info`<-ifelse(study2$`Professional Info`==20, 5,study2$`Professional Info`)

study2$Sal<-0

study2$Sal<-ifelse(study2$`Salary Slab`>=1 & study2$`Salary Slab`<=3,"Very Low",study2$Sal)
study2$Sal<-ifelse(study2$`Salary Slab`>=4 & study2$`Salary Slab`<=6,"Low",study2$Sal)
study2$Sal<-ifelse(study2$`Salary Slab`>=7 & study2$`Salary Slab`<=9,"Medium",study2$Sal)
study2$Sal<-ifelse(study2$`Salary Slab`>=10 & study2$`Salary Slab`<=13,"High",study2$Sal)


cols<-c(3,4,5,6,7,8,10,18)
study2[cols] <- lapply(study2[cols],as.factor)
View(study2)
lapply(study2, class)
which(lapply(study2, is.character)==TRUE)
which(lapply(study2, is.numeric)==TRUE)
cols2<-c(11,12)
study2[cols2] <- lapply(study2[cols2],as.factor)
colSums(is.na(study2))

study2<-separate(study2,col=`Commence Date`,into=c("Commence_Year","Commence_Month","Commence_Date"),
              sep="-")

study2<-separate(study2,col=`Equipment Warranty Expiry Date`,
              into=c("Expiry_Year","Expiry_Month","Expiry_Date"),sep="-")

cols3<-c(2,3,4,16,17,18)
study2[cols3] <- lapply(study2[cols3],as.numeric)



study2$Commence_Year[is.na(study2$Commence_Year)] <- Mo(study2$Commence_Year)
study2$Commence_Month[is.na(study2$Commence_Month)] <- Mo(study2$Commence_Month)
study2$Commence_Date[is.na(study2$Commence_Date)] <- Mo(study2$Commence_Date)

study2$Expiry_Year[is.na(study2$Expiry_Year)] <- Mo(study2$Expiry_Year)
study2$Expiry_Month[is.na(study2$Expiry_Month)] <-Mo(study2$Expiry_Month)
study2$Expiry_Date[is.na(study2$Expiry_Date)] <- 16


study2$Address[is.na(study2$Address)] <- Mo(study2$Address)
study2$Gender[is.na(study2$Gender)] <- Mo(study2$Gender)
study2$`Equipment Warranty`[is.na(study2$`Equipment Warranty`)] <-  Mo(study2$`Equipment Warranty`)

study2$`Salary Slab`[is.na(study2$`Salary Slab`)] <-Mo(study2$`Salary Slab`)
study2$`Professional Info`[is.na(study2$`Professional Info`)] <- Mo(study2$`Professional Info`)

study2$Age[is.na(study2$Age)] <- median(study2$Age,na.rm=TRUE)
study2$`Age of Home`[is.na(study2$`Age of Home`)] <- Mo(study2$`Age of Home`)
study2$Sal[is.na(study2$Sal)]<-Mo(study2$Sal)


payment2<-payment
View(payment2)
payment2$Paid_Times<-1
py1<-aggregate(Paid_Times~`Account Number`,payment2,max)
py2<-aggregate(Paid_Times~`Account Number`,payment2,sum)

View(py2)
study2<-left_join(study2,py1)

study2<-study2[-4057,]
study2<-study2[-2128,]



study2$New_Age<-0

study2$New_Age<-ifelse(study2$Age>=10 & study2$Age<=30, "Young", study2$New_Age)
study2$New_Age<-ifelse(study2$Age>=31 & study2$Age<=50, "Middle Age", study2$New_Age)
study2$New_Age<-ifelse(study2$Age>=51 & study2$Age<=70, "Old", study2$New_Age)
study2$New_Age<-ifelse(study2$Age>=71 & study2$Age<=100, "Very Old", study2$New_Age)

study2$Home_Age<-0

study2$Home_Age<-ifelse(study2$`Age of Home`>=1 & study2$`Age of Home`<=5,"New Home",study2$Home_Age)
study2$Home_Age<-ifelse(study2$`Age of Home`>=6 & study2$`Age of Home`<=10," Middle Home",study2$Home_Age)
study2$Home_Age<-ifelse(study2$`Age of Home`>=11 & study2$`Age of Home`<=25,"Old Home",study2$Home_Age)

study2$Paid_Times[is.na(study2$Paid_Times)]<-Mo(study2$Paid_Times)

study2$New_Age<-as.factor(study2$New_Age)
study2$Home_Age<-as.factor(study2$Home_Age)


sa<-study2
View(sa)

sa<-sa[,-c(11,13,15,23)]

u2<-aggregate(`data used Gb`~`Account Number`,use,sum)

sa<-left_join(sa,u2)
sa$`data used Gb`[is.na(sa$`data used Gb`)]<-median(sa$`data used Gb`, na.rm = TRUE)
sa<-left_join(sa,py1)
sa$Paid_Times[is.na(sa$Paid_Times)]<-0
sa_model<-sa[1:9964,]
sa_valid<-sa[9965:nrow(sa),]
View(sa_model)
View(sa_valid)

sa_valid<-sa_valid[,-16]
sa_model$Churned<-0
sa_model$Churned<-ifelse(!is.na(sa_model$`Churn Date`), 1,0)
sa_model<-sa_model[,-16]

sa_model$Churned<-as.factor(sa_model$Churned)

sa_model2<-sa_model
sa_valid2<-sa_valid

sa_model<-sa_model[,-1]
sa_valid<-sa_valid[,-1]


sa_model_churned<-filter(sa_model2,Churned==1)
sa_model_not_churned<-filter(sa_model2,Churned==0)

summary(sa_model_churned$Scheme)
summary(sa_model_not_churned$Scheme)

sa_model$Scheme[2]<-"PRS"

model <- glm (Churned ~ ., data = sa_model, family = binomial)
model2 <- glm (Churned ~ ., data = sa_model2, family = binomial)

summary(model)
predict2 <- predict(model,sa_valid, type = 'response')
pred<-predict(model,type = "response")
fitted.results<-pred
fitted.results <- ifelse(fitted.results >= 0.3,1,0)

pred5<-predict(model5,type = "response")
fitted.results5<-pred5
fitted.results5 <- ifelse(fitted.results5 >= 0.3,1,0)
confusionMatrix(fitted.results5,sa_model5$Churned)
predict5 <- predict(model5,sa_valid, type = 'response')

confusionMatrix(fitted.results,sa_model$Churned)

fitted.results2<-predict2
fitted.results2 <- ifelse(fitted.results2 >= 0.3,1,0)


submit2<-sa_valid2[,1]
View(submit2)
submit2<-cbind(submit2,predict2)
submit2<-cbind(submit2,fitted.results2)
submit2$predict2<-format(submit2$predict2,scientific = FALSE)

colnames(submit2)[1]<-"ACCT_NUM"
colnames(submit2)[2]<-"PREDICTED_RESP_PROBABILITY"
colnames(submit2)[3]<-"PREDICTED_CLASS"

write.xlsx(submit2,"sol_submit.xlsx")

library(ROCR)
ROCRpred <- prediction(pred, sa_model$Churned)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

ggplot(sa_model2,aes(Churned==1,fill=Sal))+geom_bar()

