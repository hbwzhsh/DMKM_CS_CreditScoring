#------- Case Study---------#
#----CREDIT SCORING PROBLEM-------#
#Author : SHALINI GANGWAR & ARPAPORN SKUNKITTIYUT
#----------------------------------------#
rm(list=ls())
#importing Libraries 
library(FactoMineR)
library(mice)
library(kernlab)
library(MASS)
library(class)
library(tree)
library(e1071)
library(rpart)
library(car)

#reading the data 
setwd("E:\\EMDMKM 3rd Semester\\Case study\\repo")
cmp3 <- read.csv("credsco_final_1.csv")

test <- read.csv("credsco_test1.csv")

#summary of the data
#summ <- summary(input)
test$funding.ratio <- (test$amount.of.money.solicitated / test$price.of.good.to.buy) * 100
test$saving.capacity<- ((test$income- test$Expenses - (test$mortgage.left/100)))/(test$amount.of.money.solicitated/test$term.in.months)

test$income[test$income=="99999999"] <- NA
test$mortgage.left [test$mortgage.left =="99999999"] <- NA
test$total.wealth [test$total.wealth =="99999999"] <- NA
test$total.wealth [test$total.wealth =="99999999"] <- NA
test$Type.of.employment [test$Type.of.employment =="0"] <- NA
test$House.type [test$House.type =="0"] <- NA


test$House.type <- factor(test$House.type,levels=c(1:6),labels=c("rented","ownerwithdeed","private","ignore","parents","others"))
test$Marital.status <- factor(test$Marital.status,levels=c(1:5),labels=c("single","married","widower","separated","divorced"))
test$Registers <- factor(test$Registers,levels=c(1:2),labels=c("No","Yes"))
test$Type.of.employment <- factor(test$Type.of.employment,levels=c(1:4),labels=c("Permanent","Temporaray","Self","Others"))
test$Opinion <- factor(test$Opinion,levels=c(1:2),labels=c("Positive","Negative"))
test$Expenses <- factor(test$Expenses,levels=c(1:4),labels=c("miser","less miser","less spenthrift","spenthrift"))
test$funding.ratio [test$funding.ratio< 70 ] <- 1
test$funding.ratio [test$funding.ratio>=70 & test$funding.ratio< 98 ] <- 2
test$funding.ratio [test$funding.ratio>=99 ] <- 3
test$funding.ratio <- factor(test$funding.ratio,levels=c(1:3),labels=c("1","2","3"))
test$saving.capacity [test$saving.capacity< 1 ] <- 1
test$saving.capacity [test$saving.capacity>=1 ] <- 2
test$saving.capacity <- factor(test$saving.capacity,levels=c(1:2),labels=c("1","2"))

#todo check also precision to see if model well predicts for the hard class also 


n<-2986
cmp3<-cmp3[,-16]
trdata1<-cmp3[1:n,2:17]
trdata2<-cmp3[(n+1):(2*n),2:17]
trdata3<-cmp3[((2*n)+1):(3*n),2:17]
trdata4<-cmp3[((3*n)+1):(4*n),2:17]
trdata5<-cmp3[((4*n)+1):(5*n),2:17]

testdata1 <- rbind(trdata1, test)
testdata2 <- rbind(trdata2, test)
testdata3 <- rbind(trdata3, test)
testdata4 <- rbind(trdata4, test)
testdata5 <- rbind(trdata5, test)

imp <- mice(testdata1,maxit=1,m=1)
test1 <- complete(imp) 


imp <- mice(testdata2,maxit=1,m=1)
test2 <- complete(imp)


imp <- mice(testdata3,maxit=1,m=1)
test3 <- complete(imp)

imp <- mice(testdata4,maxit=1,m=1)
test4 <- complete(imp)

imp <- mice(testdata5,maxit=1,m=1)
test5 <- complete(imp)

#dim(test1[2987:nrow(test1),])
test1 <- test1[2987:nrow(test1),]
test2 <- test2[2987:nrow(test2),]
test3 <- test3[2987:nrow(test3),]
test4 <- test4[2987:nrow(test4),]
test5 <- test5[2987:nrow(test5),]



testfinal <- rbind(test1, test2,test3,test4,test5)
dim(test2)
data<-testfinal

tree<-rpart(data$Opinion~.,data=data,control=rpart.control(cp=0.0001,maxdepth = 4, method="class"))
pred <- predict(tree,newdata=data)
pred2 <- predict(tree,newdata=data,type="class")
mc<-table(data$Opinion,pred2)
err.tree <- 1.0-(mc[1,1]+mc[2,2])/sum(mc)

neta<-log(pred[,2]/(1-pred[,2]))

neta_pos<-neta[pred2=="Positive"]
neta_neg<-neta[pred2=="Negative"]
length(neta_neg)
length(neta_pos)

hist(neta,prob=T,border="white")
lines(density(neta_neg),col="red")
lines(density(neta_pos),col="darkgreen")
#############################################
# Cost computation

# no need to test normality for this case 

#priori probability
prob_pri_pos<-0.55
prob_pri_neg<-0.53

#threshold
thres_pos<-log(prob_pri_pos/(1-prob_pri_pos))
thres_pos
thres_neg<-log(prob_pri_neg/(1-prob_pri_neg))
thres_neg

prob_pos_neg<-sum(neta_neg>thres_pos)/length(neta_neg)
prob_pos_neg
prob_neg_pos<-sum(neta_pos<=thres_neg)/length(neta_pos)
prob_neg_pos

prob_post_coun<-0.13
prob_man<-((sum(neta_pos<=thres_pos)/length(neta_pos))
          +(sum(neta_neg>=thres_neg)/length(neta_neg))
          -(prob_neg_pos*(1-prob_post_coun))
          -(prob_pos_neg*(prob_post_coun)))
prob_man

#Cost computation

cost_man<-20*prob_man
cost_man
cost_pos_neg<-1000*prob_pos_neg*(prob_post_coun)
cost_pos_neg
cost_neg_pos<-1000*0.14*prob_neg_pos*(1-prob_post_coun)
cost_neg_pos

#Total Cost 
total_cost<-cost_man+cost_pos_neg+cost_neg_pos
total_cost
