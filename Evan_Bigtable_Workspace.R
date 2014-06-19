install.packages("PostgreSQL")
library("PostgreSQL")

require(RPostgreSQL)
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, host='hostname', port='port', dbname='dbname',
                 user='username', password='password')  # change to your password


######


bigtble <- dbSendQuery(con, "select * from kiva_bigtble")
bigtble1 <- fetch(bigtble,n=-1)

dbListTables(con) #list tables
 
kiva_loans <- dbReadTable(con,"kiva_loans")



bigtable1 <- dbReadTable(con,"kiva_bigtble")
str(bigtable1)

bt1.stat<-bigtable1$status
table(bt1.stat)
bt1.ln.id<-bigtable1$loan_id
bt1.ln.amt<-bigtable1$loan_amount
bt1.pd.amt<-bigtable1$paid_amount
bt1.act<-bigtable1$activity
bt1.sect<-bigtable1$sector
bt1.fund.date<-bigtable1$funded_date
bt1.disb.date<-bigtable1$disbursal_date

bigtable2 <- dbReadTable(con,"kiva_bigtble2")
str(bigtable2)

bt2.ln.id<-bigtable2$loan_id
bt2.sex<-bigtable2$gender
bt2.cntry<-bigtable2$country
bt2.cntry.cd<-bigtable2$country_code
bt2.geo.lat<-bigtable2$geo_lat
bt2.geo.lng<-bigtable2$geo_lng

bigtable1 <- dbReadTable(con,"kiva_bigtble")
str(bigtable1)

gen.tbl<-read.csv("C:/Users/EVAN EZ/Desktop/Data-Tactics/gender.csv", sep=";")
gen.tbl[is.na(gen.tbl)] <- 0
summary(gen.tbl)
str(gen.tbl)

gen.tbl$total<-gen.tbl$males+gen.tbl$females
summary(gen.tbl$total)
gen.tbl$group<-gen.tbl$total>1
summary(gen.tbl$group)

bt1.gen <- merge(bigtable1,gen.tbl,by="loan_id")
summary(bt1.gen)



bt1.gen$defaulted <- bt1.gen$status == "defaulted"
plot(table(bt1.gen$defaulted, bt1.gen$group),main="Group vs Individual",xlab="defaulted",ylab="group")


#########################################

###############################
######BATTLE OF THE SEXES######
###############################
males<-bt1.gen$males
females<-bt1.gen$females
total<-bt1.gen$total

loan.amt<-bt1.gen$loan_amount

group.loan<-bt1.gen$total>1
table(group.loan)

stat<-bt1.gen$status
table(stat)

ind.loan<-(bt1.gen$total==1)+0
table(ind.loan)
#1 is ind loans, 0 is group
table(ind.loan,stat)
#loan stats for single and group loans

male.loan<-(males==1)+(total==1)
table(male.loan)
male.loan.amt<-(male.loan==2)+0
table(male.loan.amt)
hist(loan.amt)

female.loan<-(females==1)+(total==1)
table(female.loan)
female.loan.amt<-(female.loan==2)
#2=single sex loan, 1=i think one of the above criteria satisfied, 0=dude group loans
table(male.loan,stat)

cor(ind.loan,male.loan)
cor(ind.loan,female.loan)

ind.male.stat.rough.tab<-table(male.loan,stat)
ind.male.stat<-ind.male.stat.rough.tab[3,c(2,6)]
#individual man loan status

ind.male.stat.total<-sum(ind.male.stat)
ind.male.stat.paid<-ind.male.stat["paid"]
ind.male.stat.def<-ind.male.stat["defaulted"]
ind.male.stat/ind.male.stat.total
prop.test(ind.male.stat.paid,ind.male.stat.total)
prop.test(ind.male.stat.def,ind.male.stat.total)


ind.female.stat.rough.tab<-table(female.loan,stat)
ind.female.stat<-ind.female.stat.rough.tab[3,c(2,6)]
#individual man loan status

ind.female.stat.total<-sum(ind.female.stat)
ind.female.stat.paid<-ind.female.stat["paid"]
ind.female.stat.def<-ind.female.stat["defaulted"]
ind.female.stat/ind.female.stat.total
prop.test(ind.female.stat.paid,ind.female.stat.total)
prop.test(ind.female.stat.def,ind.female.stat.total)


###SEXES WORKBENCH
par(mfrow=c(1,1))
hist(log10(loan.amt[male.loan>1]), col="red",xlim=c(1,4),ylim=c(0,90000),xlab="Dollar Amount (Log Scale)",ylab="Amount of Loans",main="Loan Size Given to Males")
hist(log10(loan.amt[female.loan>1]),col="orange",xlim=c(1,4),ylim=c(0,90000),xlab="Dollar Amount (Log Scale)",ylab="Amount of Loans",main="Loan Size Given to Females")

hist(log10(loan.amt[female.loan>1]), col="orange",xlim=c(1,4), ylim=c(0,90000),
     xlab="Dollar Amount (Log Scale)",ylab="Amount of Loans",main="Loan Size Given by Sex")
hist(log10(loan.amt[male.loan>1]), col="red", add=T)



pro.def<-c(.02389584,.0209089)




stat<-bt1.gen$status
hist(male.loan[stat=="defaulted"])
hist(ind.male.stat)


#############
library(ggplot2)
library(reshape2)

data.pro <- textConnection("Sex,Paid, Default
Male,.9761,.0238
Female,.9790,.0209")

data.pro <- textConnection("Sex,Paid, Default
Male,102612,.0238
Female,281746,.0209")

data.pro <- read.csv(data.pro, h=T)

p <- ggplot(aes(x=Sex, fill=Default), data=data.pro)
p + geom_bar() +
labs(x="Sex of Borrower", y="Proportion", title="Status Proportions by Sex")





p <- ggplot(aes(x=Sex), data=data.pro)
p + geom_bar(position='fill') +
labs(x="Sex", y="Proportion", title="Status Proportions by Sex")



p <- ggplot(aes(x=Sex, weight=("Paid:Default"), fill=("Paid:Default")), data=data.pro)
p + geom_bar(position='fill') + 
  scale_fill_discrete("Legend Title") + 
  labs(x="X Label", y="Y Label", title="An Example Stacked Column Percentage Chart")


p <- ggplot(aes(x=factor(1), fill=Paid, weight=Paid), data=data.pro)
p + geom_bar(width = 1) +
  coord_polar(theta="y") +
  scale_fill_discrete("Legend Title") +
  labs(x="X Label", y="Y Label", title="An Example Pie Chart")
#################
hist(loan.amt)


male.loan.amt.tab<-table(loan.amt,male.loan)
male.loan.amt<-male.loan.amt.tab[,3]


plot(table(loan.amt,male.loan.amt))
plot(ind.male.stat,ind.female.stat)




plot(male.loan.amt,loan.amt)



cor(male.loan,female.loan)
female.amt<-table(female.loan,bt1.gen$loan_amount)
female.amt<-female.amt[3,]
table(female.amt)
male.amt<-table(male.loan,bt1.gen$loan_amount)
male.amt<-male.amt[3,]
cor(female.loan,bt1.gen$loan_amount)
cor(male.loan,bt1.gen$loan_amount)
cor(male.amt,female.amt) 
table(male.amt,female.amt)

female.amt
plot(female.amt,bt1.gen$loan_amount)
cor(male.loan,bt1.gen$loan_amount)


















####################
####################

require(RPostgreSQL)
 
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, host='host', port='port', dbname='dbname',
                 user='username', password='password')  # change to your password
 
dbListTables(con) #list tables
 
bigtable1 <- dbReadTable(con,"kiva_bigtble")
summary(bigtable1)
bigtable1$loan_id <- factor(bigtable1$loan_id)
bigtable1$activity <- factor(bigtable1$activity)
bigtable1$status <- factor(bigtable1$status)
bigtable1$sector <- factor(bigtable1$sector)
 
 
 
load(url('http://eeyore.ucdavis.edu/stat135/data/Kiva_Loans.rda'))
#saves it as loans
loans$id = factor(loans$id)
 
#all of the ids in loans match to the ids in bigtable:
matches<- bigtable1$loan_id %in% loans$id
summary(matches)
 
summary(bigtable1[matches,])
summary(bigtable1[-matches,])

#######################

#LOAD THE BIGTABLE and TYPECAST
dbListTables(con) #list tables
bigtable1 <- dbReadTable(con,"kiva_bigtble")
summary(bigtable1)
bigtable1$loan_id <- factor(bigtable1$loan_id)
bigtable1$activity <- factor(bigtable1$activity)
bigtable1$status <- factor(bigtable1$status)
bigtable1$sector <- factor(bigtable1$sector)
 
 
#LOAD THE UCDAVIS DATA AND TYPECAST
load(url('http://eeyore.ucdavis.edu/stat135/data/Kiva_Loans.rda'))
#saves it as loans
loans$id = factor(loans$id)
 
#FIND MATCHING IDS
#all of the ids in loans match to the ids in bigtable:
matches<- bigtable1$loan_id %in% loans$id
summary(matches)
 
#SUMMARIES FOR THE TWO SETS
summary(bigtable1[matches,])
summary(bigtable1[-matches,])
 
#COMPARING THE LOAN AMOUNTS FOR THE TWO GROUPS
par(mfrow=c(2,1))
hist(log10(bigtable1[matches,"funded_amount"]) , breaks=seq(1,5,.1))
hist(log10(bigtable1[-matches,"funded_amount"]), breaks=seq(1,5,.1))
 
#COMPARING THE DATES FOR THE TWO GROUPS
par(mfrow=c(2,1))
hist(bigtable1[matches,"funded_date"], breaks=seq(ISOdate(2005, 1, 1), ISOdate(2013, 1, 1), "months"))
hist(bigtable1[-matches,"funded_date"], breaks=seq(ISOdate(2005, 1, 1), ISOdate(2013, 8, 1), "months"))
 
#BECAUSE THE DATES ARE SELECTED, SOME OF THE VARIABLES LIKE STATUS ARE SKEWED IN THE UCDAVIS SAMPLE
table(bigtable1[matches,"status"]) /dim(bigtable1[matches,])[1]
table(bigtable1[-matches,"status"])/dim(bigtable1[-matches,])[1]
