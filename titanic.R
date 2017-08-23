setwd("/Users/Pan/Google Drive/Data Science/SYS 6018/competition1_titanic/sys6018-competition-titanic")
library(readr)
library(dplyr)
test<-read.csv("test.csv")
train<-read.csv("train.csv")
test$Survived<-NA
test$Set<-'test'
train$Set<-'train'
comb<-rbind(train,test)

comb$Sex<-factor(comb$Sex)
comb$Pclass<-factor(comb$Pclass)
comb$Embarked<-factor(comb$Embarked)
comb$SibSp<-factor(comb$SibSp)
comb$Parch<-factor(comb$Parch)

sum(is.na(comb$Fare))#1
sum(is.na(comb$Embarked))
round(sum(comb$Cabin=='')/nrow(comb)*100) #77% missing: too much, throw away
sum(is.na(comb$Age)) #263: too much, throw away

names(comb)

lm1<-lm(Survived~Pclass+Sex+SibSp+Parch+Embarked,comb[comb$Set=='train',])
summary(lm1) #0.3844


glm2<-glm(Survived~Pclass+Sex+SibSp+Parch+Embarked,data=comb[comb$Set=='train',],family='binomial')
summary(glm2) #AIC 825.18

glm3<-glm(Survived~Pclass+Sex+SibSp,data=comb[comb$Set=='train',],family='binomial')
summary(glm3) #AIC 828.21

glm3<-glm(Survived~Pclass+Sex,data=comb[comb$Set=='train',],family='binomial')
summary(glm3) #AIC 834.89

probs<-as.vector(predict(glm3,newdata=comb[comb$Set=='train',], type="response"))
preds <- rep(0,nrow(comb[comb$Set=='train',]))  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds,comb[comb$Set=='train',]$Survived) 
(468+233)/(nrow(comb[comb$Set=='train',])) #0.787

myprobs<-as.vector(predict(glm3,newdata=comb[comb$Set=='test',], type="response"))
mypreds<-rep(0,nrow(comb[comb$Set=='test',]))
mypreds[myprobs>0.5] <- 1 # p>0.5 -> 1
myoutput<-cbind(comb[comb$Set=='test',1],mypreds)
write.table(myoutput, file = "hp4zw-entry-titanic.csv",col.names=c("PassengerId","Survived"), sep=",",row.names = F)



