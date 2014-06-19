################################
########Colin's ################

a <- loans$status
b <- loans$sector
c <- loans$country
plot(a)
str(loans)
individual <- loans$numBorrowers == 1
group <- loans$numBorrowers >1

individual+0
hist(individual+0)

table(individual)

hist(loans$numBorrowers, xlab="Number of Borrowers", main="Ammount of Borrowers")
hist(loans$numFemale, scale="percent", breaks="Sturges", col="darkgray")
install.packages("ggplot2")
library(ggplot2)
ggplot(loans,aes(x=status), xlab="Status") + geom_bar()
defaulted <- loans$status == "defaulted"
defaulted
summary(defaulted)
summary(individual)
summary(group) 

######################################
############Rich's 1##################

loans <- load(url('http://eeyore.ucdavis.edu/stat135/data/Kiva_Loans.rda'))
class(loans)
library(lattice)
library(maps)

# how many countries got any (at least one) Kiva loans.
length(unique(loans$country))
dotplot(table(loans$country))

densityplot(loans$funded_amount, groups = loans$country, xlim = c(0, 2000))

loans$numMale = loans$numBorrowers - loans$numFemale
loans$hasMales = loans$numMale > 0
loans$delinquent = !is.na(loans$delinquent)
theme = list(superpose.symbol = list(col = c(rgb(0, 0, 1, 0.2), rgb(1, 0, 0, 0.2))))
loans$myfac = paste(c("All Female Borrowers", "Male Borrower Present")[loans$hasMale + 1], 
                    c("Non-Delinquent", "Delinquent")[loans$delinquent + 1], sep = " - ")

xyplot(funded_amount ~ numBorrowers | myfac, groups = hasMales, data = loans, 
       par.settings = theme, pch = 19, key = list(space = "right", points = list(pch = 19,
                                                                                 col = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5))), text = list(c("Male Borrower Present",
                                                                                                                                               "All Female Borrowers"))), layout = c(2, 2), xlab = "Number of Borrowers")

sets = split(loans, loans$myfac)
par(mfrow = c(2, 2))
sapply(names(sets), function(nm) smoothScatter(sets[[nm]]$numBorrowers, sets[[nm]]$funded_amount, 
                                               xlim = range(loans$numBorrowers), ylim = range(loans$funded_amount), main = nm, 
                                               xlab = "Number of Borrowers", ylab = "Funded Amount (USD)"))

loans$delinqfac = c("Non-Delinquent", "Delinquent")[loans$delinquent + 1]
loans$hasMalesFac = c("All Female", "Males Present")[loans$hasMales + 1]
mosaicplot(table(loans$delinqfac, loans$hasMalesFac))

loans$groupFac = "All Female"
loans$groupFac[loans$hasMales & (loans$numFemale > 0)] = "Mixed"
loans$groupFac[loans$hasMales & (loans$numFemale == 0)] = "All Male"
mosaicplot(table(loans$delinqfac, loans$groupFac), main = "Mosaic Plot of Borrower Gender and Delinquency", las = 1)

t = table(loans$delinqfac, loans$groupFac)
t[1, ]/t[2, ]

smoothScatter(loans$long, loans$lat, main = "Density of Kiva Loans by Location", 
              xlab = "Longitude", ylab = "Latitude", bandwidth = c(5, 1))
map(xlim = range(loans$long), ylim = range(loans$lat), add = TRUE)

######################################
############Rich's 2##################

load(url('http://eeyore.ucdavis.edu/stat135/data/Kiva_Loans.rda'))

dat<-loans

# table of column sector w counts. 
table(dat$sector)

# two way table of sector and status w counts.
table(dat$sector, dat$status)

# xtabs is just a nicer way of looking at things. 
xtabs( ~ status + sector, data=dat)

# cross tabs - X2 analysis 
res <-  xtabs( ~ status + sector, data=dat)
summary(res)


# sector = rows, status = columns 
dat.tbl <- table(dat2$sector, dat2$status)

# row %
prop.table(dat.tbl, 1)
# column %
prop.table(dat.tbl, 2)

# row frequencies summed over columns
margin.table(dat.tbl, 1)
# column frequencies summed over rows
margin.table(dat.tbl, 2)

##################################################################
##################################################################

install.packages("PostgreSQL")
library("PostgreSQL")
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, host='hostname', port='port', dbname='dbname',
                 user='username', password='password')  # change to your password


######


bigtble <- dbSendQuery(con, "select * from kiva_bigtble")
bigtble1 <- fetch(bigtble,n=-1)

dbListTables(con) #list tables
 
kiva_loans <- dbReadTable(con,"kiva_loans")
bigtable2 <- dbReadTable(con,"kiva_bigtble2")


bigtable1 <- dbReadTable(con,"kiva_bigtble")
str(bigtable1)
bt1.stat<-bigtable1$status
table(bt1.stat)

bt1.ln.amt<-bigtable1$loan_amount
bt1.pd.amt<-bigtable1$paid_amount
bt1.act<-bigtable1$activity
bt1.sect<-bigtable1$sector
bt1.fund.date<-bigtable1$funded_date
bt1.disb.date<-bigtable1$disbursal_date


PostgreSQL(max.con = 16, fetch.default.rec = 500, force.reload = TRUE)


###
kloans <- dbSendQuery(con, "select * from kiva_loans")
###kloans100 <- fetch(kloans,n=100)   ## fetch first 100 rows from result set
klo <- fetch(kloans,n=-1)
summary(klo)
dim(klo)
str(klo)
klo.stat<-klo$status

?fetch
kloans.status<-kloans@status
class(kloans100)
colnames(kloans100)
str(kloans100)
table(kloans100)





PostgreSQL(max.con = 16, fetch.default.rec = 500, force.reload = TRUE)








###CORRELATION###

load(url('http://eeyore.ucdavis.edu/stat135/data/Kiva_Loans.rda'))

dat<-loans
str(dat)
stat<- loans$status

cor(dat[,"loan_amount"],dat[,"disbursal_amount"])
###


#########

load(url('http://eeyore.ucdavis.edu/stat135/data/Kiva_Loans.rda'))

dat<-loans
str(dat)
stat<-loans$status

###Payment Information

funded<-loans$funded_amount
paid.amt<-loans$paid_amount
summary(funded)
summary(paid.amt)
table(funded)
table(paid.amt)
plot(funded,paid.amt)
boxplot(funded,paid.amt)
hist(funded)
hist(paid.amt)
?hist()


table(stat)
#quantity of each payment status


#"paid" and "def" are highly useless

paid<-(stat=="paid")+0
#1=paid, 0=all others
table(paid)

def<-(stat=="defaulted")+0
#0=all others 1=def
table(def)

###############################
######BATTLE OF THE SEXES######
###############################

ind.loan<-(loans$numBorrowers==1)+0
table(ind.loan)
#1=individual loan
table(ind.loan, stat)
#loan stats for single and group loans

women.in.group<-loans$numFemale
men.in.group<-loans$numBorrowers-women.in.group
table(women.in.group,ind.loan)
table(men.in.group,ind.loan)
cor(men.in.group,women.in.group)

man.loan<-(men.in.group==1)+(loans$numBorrowers==1)
table(man.loan)
woman.loan<-(women.in.group==1)+(loans$numBorrowers==1)
table(woman.loan)
#2=single sex loan, 1=i think one of the above criteria satisfied, 0=dude group loans
cor(ind.loan,man.loan)
cor(ind.loan,woman.loan)

ind.man.stat.rough.tab<-table(man.loan,stat)
ind.man.stat<-ind.man.stat.rough.tab[3,1:2]
#individual man loan status
ind.man.stat.total<-sum(ind.man.stat)
ind.man.stat.paid<-ind.man.stat["paid"]
ind.man.stat.def<-ind.man.stat["defaulted"]
ind.man.stat/ind.man.stat.total
prop.test(ind.man.stat.paid,ind.man.stat.total)
prop.test(ind.man.stat.def,ind.man.stat.total)

ind.wom.stat.rough.tab<-table(woman.loan, stat)
ind.wom.stat<-ind.wom.stat.rough.tab[3,1:2]
#individual woman loan status
ind.wom.stat.total<-sum(ind.wom.stat)
ind.wom.stat.paid<-ind.wom.stat["paid"]
ind.wom.stat.def<-ind.wom.stat["defaulted"]
ind.wom.stat/ind.wom.stat.total
prop.test(ind.wom.stat.paid,ind.wom.stat.total)
prop.test(ind.wom.stat.def,ind.wom.stat.total)



###MEN V. WOMEN

table(men.in.group,women.in.group)
#tells me the the quantity of loans given to the male:female ratio of group 
exp.men.v.wom<-table(men.in.group,women.in.group)
exp.men.v.wom[1:2,1:2]
singles.tab<-exp.men.v.wom[1:2,1:2]
#amount of single men and/or women w/ loans

###SEXES WORKBENCH

cor(men.in.group,women.in.group)
table(women.in.group,dat$loan_amount)
cor(women.in.group,dat$loan_amount)
table(men.in.group,dat$loan_amount)
cor(men.in.group,dat$loan_amount)



##################
####TIME STATS####
##################

###DEFAULT DATE

def.date<-dat$defaulted_date

def.ymd<-as.Date(def.date)
table(def.ymd)
#Breaks initial dtg into y/m/d format

def.year<-substr(def.date,1,4)
table(def.year)
#function rips the first four characters from the string and creates the year
def.vector<-c(15,1811,1698)
#normalize it by year


###DISBURSAL DATE

dis.date<-loans$disbursal_date

dis.year<-substr(dis.date,1,4)
table(dis.year)

paid.date<-loans$paid_date
paid.year<-substr(paid.date,1,4)
table(paid.year)

###TIME WORKBENCH

table(def.year,loans$sector)
table(dis.year,loans$sector)
exp.def.sec<-table(def.year,loans$sector)
exp.dis.sec<-table(dis.year,loans$sector)
exp.dis.sec2<-exp.dis.sec[3:5,]
exp.paid.sec<-table(paid.year,loans$sector)
exp.paid.sec2<-exp.paid.sec[3:5,] 
exp.tot.sec<-sum(exp.paid.sec2,exp.def.sec)

exp.def.sec/exp.tot.sec

exp.def.sec/exp.dis.sec2
#proportion of sector loans that failed that year

table(def.year,dis.year)
#amount loans defaulted in a given year from a starting year

exp.def.sec/def.vector
#proportion of defaults of a given year per sec



##############################################################
##############################################################
##############################################################
install.packages(reshape2)
install.packages(xtable)
install.packages(hwriter)
#Packages for special graphics#
##############################################################

load(url('http://eeyore.ucdavis.edu/stat135/data/Kiva_Loans.rda'))

dat<-loans
str(dat)
stat<-loans$status

#how to call specific elements of a data frame
keepcols<-c("defaulted","paid")
table(loans$activity,loans$sector,loans$status)
obj<-table(loans$activity,loans$sector,loans$status)
obj2<-obj[,,keepcols]

sum(stat)
table(stat)
table(men.in.group,stat)
colnames(loans)
dim(loans)
loans$numMale[1:100]
summary(loans$funded_amount)
summary(loans$paid_amount)

plot(loans$funded_amount,loans$paid_amount)



?textConnection()

library(ggplot2)
library(reshape2)

data <- textConnection("Sex, defaulted, paid
Men, 910,18452
Women, 2464,66839
")
data <- read.csv(data, h=T)

p <- ggplot(aes(x=Sex, weight=value, fill=variable), data=data)
p + geom_bar() + 
  coord_flip() +
  scale_x_discrete("Sex") +
  labs(x="Sex", y="Number of Loans", title="Status of Loan by Sex")













library(ggplot2)

#Pie chart
data <- textConnection("Category,Value
Category A,5
Category B,4
Category C,3
Category D,2
Category E,1
")
data <- read.csv(data, h=T)

p <- ggplot(aes(x=factor(1), fill=Category, weight=Value), data=data)
p + geom_bar(width = 1) +
  coord_polar(theta="y") +
  scale_fill_discrete("Legend Title") +
  labs(x="X Label", y="Y Label", title="An Example Pie Chart")





library(ggplot2)
library(reshape2)

data <- textConnection("Month,Series 1,Series 2,Series 3,Series 4
Jan,7.41,9.38,5.52,6.25
Feb,5.74,8.27,7.29,3.39
Mar,6.52,5.42,7.51,6.20
Apr,2.02,0.70,0.24,1.88
May,7.90,0.35,9.99,6.84
Jun,3.22,8.01,0.91,1.61
Jul,1.43,8.54,8.08,7.62
Aug,9.80,7.79,8.71,8.21
Sep,2.36,8.17,5.70,4.48
Oct,4.39,9.71,7.19,4.96
Nov,3.24,0.26,7.65,1.37
Dec,8.44,7.78,9.44,3.65
")

data <- read.csv(data, h=T)
orderedMonths <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
data$Month <- factor(data$Month, levels=orderedMonths)
data.lng <- melt(data, id=c("Month"))

p <- ggplot(aes(x=Month, weight=value, fill=variable), data=data.lng)
p + geom_bar() + 
  scale_fill_discrete("Legend Title") + 
  labs(x="X Label", y="Y Label", title="An Example Stacked Column Chart")


install.packages("hwriter")
library(hwriter)

fdir <- "/Users/glamp/repository/blog/CactusBlog/static/img/03/large"

data <- textConnection("Name,AB,AVG,HR,BB,K
Nomar Garciaparra,305,334,12,28,17
Albert Pujols,291,330,32,57,26
Scott Hatteberg,268,321,9,43,21
Lance Berkman,335,316,26,50,64
Travis Hafner,325,311,29,72,74
Justin Morneau,337,309,25,27,60
Lyle Overbay,357,305,16,34,57
Paul Konerko,348,302,24,39,56
Nick Johnson,324,302,14,63,53
Shea Hillenbrand,296,301,12,14,40
Adrian Gonzalez,338,299,18,21,62
Jim Thome,317,297,32,61,99
Kevin Youkilis,353,289,11,60,76
Mike Jacobs,296,284,14,30,63
Todd Helton,292,284,11,59,37
Todd Walker,297,283,5,35,26
Ryan Howard,342,281,31,37,104
Doug Mientkiewicz,307,280,4,35,49
Prince Fielder,358,279,17,29,82
Michael Cuddyer,299,271,12,37,71
Chris Shelton,332,271,16,31,92
Adam LaRoche,293,270,17,33,72
Mark Teixeira,383,269,13,53,75
Conor Jackson,276,264,8,40,37
Jeff Conine,280,261,7,26,38
Nick Swisher,337,258,21,62,90
Jason Giambi,294,255,28,68,74
Carlos Delgado,336,250,24,39,84
Adam Dunn,349,246,30,73,113
Brad Wilkerson,280,239,15,37,99
Richie Sexson,361,224,19,35,99")

data <- read.csv(data, h=T)

data$Name_cell_color <- "white"
for(col in names(data[,c(-1, -7)])) {
  ntiles <- quantile(data[,c(col)], probs=c(.25, .75))
  color_col <- paste(col, "cell_color", sep="_")
  columnData <- data[,c(col)]
  data[,c(color_col)] <- ifelse(columnData<=ntiles[1], "red",
                                ifelse(columnData>=ntiles[2], "green",
                                       "white"))
}

# transpose the color columns of the data frame and convert it to a list
# goes from dataframe to matrix to dataframe to list
# see http://stackoverflow.com/questions/3492379/data-frame-rows-to-a-list
row.colours <- as.list(as.data.frame(t(data[1:nrow(data),7:12])))

hwrite(data[1:nrow(data), 1:6],
       paste(fdir, "QuartilesTable.html", sep="/"),
       center=TRUE,
       row.bgcolor=row.colours)
