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

#reading the data 
setwd("E:\\EMDMKM 3rd Semester\\Case study\\repo")
input <- read.csv("credsco_train.csv")

#summary of the data
summ <- summary(input)

#changing the values to NA
#income
input$income[input$income=="99999999"] <- NA
input$mortgage.left [input$mortgage.left =="99999999"] <- NA
input$total.wealth [input$total.wealth =="99999999"] <- NA
input$total.wealth [input$total.wealth =="99999999"] <- NA
input$Type.of.employment [input$Type.of.employment =="0"] <- NA
input$House.type [input$House.type =="0"] <- NA


input$House.type <- factor(input$House.type,levels=c(1:6),labels=c("rented","ownerwithdeed","private","ignore","parents","others"))
input$Marital.status <- factor(input$Marital.status,levels=c(1:5),labels=c("single","married","widower","separated","divorced"))
input$Registers <- factor(input$Registers,levels=c(1:2),labels=c("No","Yes"))
input$Type.of.employment <- factor(input$Type.of.employment,levels=c(1:4),labels=c("Permanent","Temporaray","Self","Others"))
input$Opinion <- factor(input$Opinion,levels=c(1:2),labels=c("Positive","Negative"))

input$Na_countinrow <- 0

for (i in 1:nrow(input))
  for (j in 1: ncol(input))
    if (is.na(input[i,j]))
      input$Na_countinrow <- input$Na_countinrow + 1



#contiDesc <- condes(input,num.var=10)

#catDesc <- catdes(input,num.var=0)

#inputinput$Na_count = input[apply(is.na(input), 1, any) ]
#input$Na_count = c(rowSums (input, )
#Description of variables
#contiDesc <- condes(input,num.var=2)

#HouseType = catdes(input,num.var=14)

#cat1 <-HouseType$test.chi2

#MICE Imputation
md.pattern(input)
imp <- mice(input)
cmp1 <- complete(imp,action = "long") 
weight <- rep(1/5,5*nrow(input))

  
cmp2 <- cbind(cmp1,weight)
mean_Number.of.employment.years=mean(cmp2$Number.of.employment.years)

catdes(cmp2,num.var =1,row.w=18)
names(cmp2)
cmp3=cmp2[,c(-1,-2,-17)]
cat3<-catdes(cmp3,num.var =1,row.w=15)
cat3$test.chi2
cat3$category
cat3$quanti.var
cat3$quanti

cmp3$funding.ratio <- (cmp2$amount.of.money.solicitated / cmp2$price.of.good.to.buy) * 100
cmp3$saving.capacity<- ((cmp2$income- cmp2$Expenses - (cmp2$mortgage.left/100)))/(cmp2$amount.of.money.solicitated/cmp2$term.in.months)

###########################################################
output <- write.csv(cmp3,"credsco_final.csv")

#cmp3 <- read.csv("credsco_full.csv")


#For age, we take it as linear 
tree.full<-rpart(cmp3$Opinion~cmp3$Age,data=cmp3,control=rpart.control(cp=0.0001,maxdepth = 4, method="class"))
print(tree.full)
p=predict(tree.full)
plot(cmp3$Age, p[,1],pch=20)

#For income, we take it as non-linear 
tree.full<-rpart(cmp3$Opinion~cmp3$income,data=cmp3,control=rpart.control(cp=0.0001,maxdepth = 4, method="class"))
print(tree.full)
p=predict(tree.full)
plot(cmp3$income, p[,1],pch=20)

#For amount.of.money.solicitated we take it as linear 
tree.full<-rpart(cmp3$Opinion~cmp3$amount.of.money.solicitated,data=cmp3,control=rpart.control(cp=0.0001,maxdepth = 4, method="class"))
print(tree.full)
p=predict(tree.full)
plot(cmp3$amount.of.money.solicitated, p[,1],pch=20)

#For total.wealth we take it as non linear and we will need to tranform it to binary if it is not linear and have discrete variables
tree.full<-rpart(cmp3$Opinion~cmp3$total.wealth,data=cmp3,control=rpart.control(cp=0.0005,maxdepth = 4, method="class"))
print(tree.full)
p=predict(tree.full)
plot(cmp3$total.wealth, p[,1],pch=20)

cmp3$total.wealth [p[,1]>0.4] <- 1
cmp3$total.wealth [p[,1]<=0.4] <- 0

#For mortgage we take it as linear
tree.full<-rpart(cmp3$Opinion~cmp3$mortgage.left,data=cmp3,control=rpart.control(cp=0.0005,maxdepth = 4, method="class"))
print(tree.full)
p=predict(tree.full)
plot(cmp3$mortgage.left, p[,1],pch=20)


#For price.of.good.to.buy we take it as linear

tree.full<-rpart(cmp3$Opinion~cmp3$price.of.good.to.buy,data=cmp3,control=rpart.control(cp=0.002,maxdepth = 4, method="class"))
print(tree.full)
p=predict(tree.full)
plot(cmp3$price.of.good.to.buy, p[,1],pch=20)

#For expenses we take it as non-linear 
tree.full<-rpart(cmp3$Opinion~cmp3$Expenses,data=cmp3,control=rpart.control(cp=0.0005,maxdepth = 4, method="class"))
print(tree.full)
p=predict(tree.full)
plot(cmp3$Expenses, p[,1],pch=20)

cmp3$Expenses [cmp3$Expenses< 44.5] <- 1
cmp3$Expenses [cmp3$Expenses>=44.5 & cmp3$Expenses< 85.5] <- 2
cmp3$Expenses [cmp3$Expenses>= 85.5 & cmp3$Expenses < 145] <- 3
cmp3$Expenses [cmp3$Expenses>= 145] <- 4

#Factorization
cmp3$Expenses <- factor(cmp3$Expenses,levels=c(1:4),labels=c("miser","less miser","less spenthrift","spenthrift"))
input$Marital.status <- factor(input$Marital.status,levels=c(1:5),labels=c("single","married","widower","separated","divorced"))
input$Registers <- factor(input$Registers,levels=c(1:2),labels=c("No","Yes"))
input$Type.of.employment <- factor(input$Type.of.employment,levels=c(1:4),labels=c("Permanent","Temporaray","Self","Others"))
input$Opinion <- factor(input$Opinion,levels=c(1:2),labels=c("Positive","Negative"))
input$Number.of.employment.years <- factor(input$Opinion,levels=c(1:3),labels=c("1","2","3"))
input$term.in.months <- factor(input$Opinion,levels=c(1:5),labels=c("1","2","3","4","5"))


#For Number.of.employment.years we make factor
tree.full<-rpart(cmp3$Opinion~cmp3$Number.of.employment.years,data=cmp3,control=rpart.control(cp=0.001,maxdepth = 4, method="class"))
print(tree.full)
p=predict(tree.full)
plot(cmp3$Number.of.employment.years, p[,1],pch=20)


cmp3$Number.of.employment.years [cmp3$Number.of.employment.years<1 ] <- 1
cmp3$Number.of.employment.years [cmp3$Number.of.employment.years>=1 & cmp3$Number.of.employment.years< 5] <- 2
cmp3$Number.of.employment.years [cmp3$Number.of.employment.years>=5 ] <- 3

cmp3$term.in.months [cmp3$term.in.months==12 ] <- 1
cmp3$term.in.months [cmp3$term.in.months==24 ] <- 2
cmp3$term.in.months [cmp3$term.in.months==36 ] <- 3
cmp3$term.in.months [cmp3$term.in.months==48 ] <- 4
cmp3$term.in.months [cmp3$term.in.months==60 ] <- 5


#For funding.ratio we make factor
tree.full<-rpart(cmp3$Opinion~cmp3$funding.ratio,data=cmp3,control=rpart.control(cp=0.001,maxdepth = 4, method="class"))
print(tree.full)
p=predict(tree.full)
plot(cmp3$funding.ratio, p[,1],pch=20)


cmp3$funding.ratio [cmp3$funding.ratio< 70 ] <- 1
cmp3$funding.ratio [cmp3$funding.ratio>=70 & cmp3$funding.ratio< 98 ] <- 2
cmp3$funding.ratio [cmp3$funding.ratio>=99 ] <- 3


#For saving.capacity we make factor
tree.full<-rpart(cmp3$Opinion~cmp3$saving.capacity,data=cmp3,control=rpart.control(cp=0.001,maxdepth = 4, method="class"))
print(tree.full)
p=predict(tree.full)
plot(cmp3$saving.capacity, p[,1],pch=20)


cmp3$saving.capacity [cmp3$saving.capacity< 1 ] <- 1
cmp3$saving.capacity [cmp3$saving.capacity>=1 ] <- 2

#try with different models

tree.full<-rpart(cmp3$Opinion~.,data=cmp3,control=rpart.control(cp=0.003,maxdepth = 4, method="class"))
tree.full

mylogit <- glm(Opinion ~ Number.of.employment.years + House.type + term.in.months + Age + Marital.status + Registers + Type.of.employment + Expenses + income + total.wealth + mortgage.left + amount.of.money.solicitated + price.of.good.to.buy + saving.capacity + funding.ratio, data = cmp3, family = "binomial"(link="logit"))
summary(mylogit)

myprobit <- glm(Opinion ~ Number.of.employment.years + House.type + term.in.months + Age + Marital.status + Registers + Type.of.employment + Expenses + income + total.wealth + mortgage.left + amount.of.money.solicitated + price.of.good.to.buy + saving.capacity + funding.ratio, data = cmp3, family = "binomial"(link = "probit"))
summary(myprobit)

mycloglog <- glm(Opinion ~ Number.of.employment.years + House.type + term.in.months + Age + Marital.status + Registers + Type.of.employment + Expenses + income + total.wealth + mortgage.left + amount.of.money.solicitated + price.of.good.to.buy + saving.capacity + funding.ratio, data = cmp3, family = "binomial"(link = "cloglog"))
summary(mycloglog)



myksvm<-ksvm(Opinion ~ Number.of.employment.years + House.type + term.in.months + Age + Marital.status + Registers + Type.of.employment + Expenses + income + total.wealth + mortgage.left + amount.of.money.solicitated + price.of.good.to.buy+ saving.capacity + funding.ratio, data = cmp3, C=5, na.action = na.omit, scaled = TRUE)
myksvm

#LDA
mylda<- lda(formula = cmp3$Opinion~cmp3$Age+cmp3$income+cmp3$amount.of.money.solicitated+cmp3$price.of.good.to.buy,data=cmp3,CV=FALSE)
mylda
tab1=table(predict(mylda)$class,cmp3$Opinion)

par(mfrow=c(1,1))
#Simple histogram by the grouping variable
ldahist(data = cmp3$income, g = cmp3$Opinion)




#Assign fold to data set
for (i in 1:nrow(input)){
  n <- sample(1:10,1)
  cmp3$fold[i]<-n
  for (j in 1:4){
    cmp3$fold[i+(j*nrow(input))]<-n
  }
}

#Cross Validation
all.err.tree<-numeric(0)
all.err.glm<-c()
all.err.lda<-c()
all.err.svm<-c()

for(i in 1:10){
  
  w<-which(cmp3$fold==i)
  data<-cmp3[w,]
  data <-cmp3
  #Classification Tree
  tree<-rpart(data$Opinion~.,data=data,control=rpart.control(cp=0.0001,maxdepth = 4, method="class"))
  pred <- predict(tree,newdata=data,type="class")
  mc<-table(data$Opinion,pred)
  err.tree <- 1.0-(mc[1,1]+mc[2,2])/sum(mc)
  all.err.tree <- rbind(all.err.tree,err.tree)
  
  #Generalized linear model
  glm<-glm(data$Opinion ~ data$Number.of.employment.years + data$House.type + data$term.in.months + data$Age + data$Marital.status + data$Registers + data$Type.of.employment + data$Expenses + data$income + data$total.wealth + data$mortgage.left + data$amount.of.money.solicitated + data$price.of.good.to.buy +data$saving.capacity+data$funding.ratio, data = data, family = "binomial"(link="logit"))

  predglm <- 1-predict(glm,newdata=data,type="response")
  
  data.pred.class <- ifelse(predglm > 0.5,"1","2") 
  data.pred.class <- as.factor(data.pred.class)
  
  mc<-table(data$Opinion,data.pred.class)
  errglm <- 1.0-(mc[1,1]+mc[2,2])/sum(mc)
  all.err.glm <- rbind(all.err.glm,errglm)
  
  #Linear discriminant analysis
  mylda<- lda(formula = data$Opinion~data$Age+data$income+data$amount.of.money.solicitated+data$price.of.good.to.buy,data=data,CV=FALSE)
  mylda
  tab1=table(predict(mylda)$class,data$Opinion)
  
  
  predlda <- predict(mylda,newdata=data)
  mc<-table(data$Opinion,predlda$class)
  err.lda <- 1.0-(mc[1,1]+mc[2,2])/sum(mc)
  all.err.lda <- rbind(all.err.lda,err.lda)
  
  #Kernel support vector machine
  myksvm<-ksvm(Opinion ~ Number.of.employment.years + House.type + term.in.months + Age + Marital.status + Registers + Type.of.employment + Expenses + income + total.wealth + mortgage.left + amount.of.money.solicitated + price.of.good.to.buy+ saving.capacity + funding.ratio, data = data, C=3)
  myksvm
  pred.svm <- predict(myksvm,newdata=data)
  mc<-table(data$Opinion,pred.svm)
  err.svm <- 1.0-(mc[1,1]+mc[2,2])/sum(mc)
  print(err.svm)  
  all.err.svm <- rbind(all.err.svm,err.svm)

}

#Cross Validation Error
print(mean(all.err.tree))
print(mean(all.err.glm))
print(mean(all.err.lda))
print(mean(all.err.svm))



