# User may not:
#
#(i) sell, redistribute, encumber, give, lend, rent, lease, sublicense, or otherwise transfer the code, #or any portions of the code, to anyone without the prior written consent of Data Tactics Corporation.
#
#(ii) reverse engineer, decompile, disassemble, modify, translate, or create derivative works from the #code, or
#
#(iii) use the code for any commercial purpose
#
#Warranty: Except as expressly stated in the immediately preceding sentence, the Original Work is #provided on an "AS IS" BASIS and WITHOUT WARRANTY, either express or implied, including, without #limitation, the warranties of non-infringement, merchantability or fitness for a particular purpose. THE #ENTIRE RISK AS TO THE QUALITY OF THE ORIGINAL WORK IS WITH YOU. This DISCLAIMER OF WARRANTY constitutes #an essential part of this Use License. No license to the Original Work is granted by this License except #under this disclaimer.
# 
#Data Tactics Corporation ©2013
#Created 9/19/13
# This program is used to 'transfer' Kiva decision tree process from RapidMiner to R
# steps 1) data preparing a) input data; b) feature selection(no activity because of format error; 
#       no load-amount becuae of duplicate with founded_amount);c) removing missing value; d) split data;
#   2) biuld decision model
#   3) eveluation (predic) model

#paid_ratio <- c(0.6,0.4)
#repay_ration <- c(0.8. 0.2)

# 1-a
#data <- read.csv('F:/KIVA/Data/Kiva_short_m4.csv')
data <- read.csv('F:/KIVA/Data/Kiva_short_m5_R.csv')
#1.b(ID(fro checking purpose), lable, founded_amount, sector,country,numFemale, numPayments)
#new_data <- data[, c(2,4:5,7,21,25:26)]   
new_data <- data[, c(2:7)]   
#1.c
rmInd <- 0
 missValue <- is.na(new_data[,])
for (i in 1:ncol(missValue)){
  I <- which(missValue[,i] == TRUE)
  if (length(I)> 0){
    if (rmInd == 0){
    # need to remove those rows from new-data
     rmInd <- I
    } else {
      rmInd<-c(rmInd,I)
    }
  }
}

# need to remove rows from new_data with index list in'rmInd'

#1-d

set.seed(1234)
# have to choose "TRUE"
ind <- sample(2,nrow(new_data), replace=TRUE, prob=c(0.7,0.3))

trainData <- new_data[ind==1,]
testData <- new_data[ind==2,]
# need to sampling to solve data stew problem

# work on 'defaulted class
I <- which(trainData$lable == 'defaulted')
dd <- trainData[I,]


# work on 'paid' class
I <- which(trainData$lable == 'paid')
s <- sample(I, length(I)*(0.6), replace = FALSE)
dp <- trainData[s,]

# work on 'repay' class
I <- which(trainData$lable == 'in_repayment')
s <- sample(I, length(I)*(0.8), replace = FALSE)
dr <- trainData[s,]

#put together

t <- dd
t <-rbind(t,dp)
t <-rbind(t,dr)
#mix up
I <- sample(length(t[,1]),replace = FALSE)
newData <- t[I,]


#collect 'in_repayment' 
#nl=0
#for (i in 1:nrow(trainData)){
 # if (trainData[i,2] == 'in_repayment'){
  #  if (nl == 0) {
   #   nl=1
    #  dr <- trainData[i,]
    #}
    #else {
     # dr <- rbind(dr, trainData[i,])
    #}
  #} else{
    
  #} 
#}  
#################################

# sample paid examples
#ind <- sample(2,nrow(dp), replace=TRUE, prob=c(0.6,0.4))
#trainPaid<- dp[ind==1,]
# sample in_repayment examples
#ind <- sample(2,nrow(dr), replace=TRUE, prob=c(0.8,0.2))
#trainRepay<- dr[ind==1,]

# mixed-up
# new_trainData <-dd
 #new_trainData <- rbind(new_trainData, rbind(dp,dr))

 #train <- sample(new_trainData, replace = FALSE)

# build model
myFormula <- lable ~ funded_amount +sector + country  + numFemale + numPayments
#kiva_ctree <- ctree(myFormula, data=new_train)
kiva_ctree <- ctree(myFormula, data=newData)
# check the prediction, using training data
table(predict(kiva_ctree), newData$lable)


# using test
testPred <- predict(kiva_ctree, newdata = testData)
table(testPred, testData$lable)

# 
#print(kiva_ctree)

#plot(kiva_ctree)
#plot(kiva_ctree, type = 'simple')
