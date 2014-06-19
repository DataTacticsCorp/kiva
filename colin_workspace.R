#############kivabigtable(kiva_loans.loan_id, funded_amount, activity, status, kiva_loans_terms.loan_amount, paid_amount, sector, funded_date, use, disbursal_date)
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, host='hostname', port='port', dbname='dbname',
                 user='username', password='password')  # change to your password

bigtable1<-dbReadTable(con,"kiva_bigtble")

bigtable2<-dbReadTable(con,"kiva_bigtble2")

bigtable1$status

### a big thing to note is that most of the graphics that i created, i got by transfering table data over to excell, finding the proportions, and then returning them back in the form of matrices.

defaulted <- bigtable1$status == "defaulted"
summary(defaulted)

library(ggplot2)
ggplot(data=bigtable1, aes(status))+geom_bar()+opts(title=expression("Loan Performance"))
ggplot(data=bigtable1, aes(sector))+geom_bar()+ opts(title = expression("Focus of Loans"))
?ggplot()
res4<- xtabs(~ sector + (defaulted+0), data=bigtable1)
summary(res4)
table(bigtable1$sector, defaulted)

probs.sector<-c(0.01782,
0.02009,
0.02649,
0.01823,
0.00589,
0.02513,
0.02064,
0.02797,
0.00982,
0.01776,
0.00870,
0.01749,
0.02083,
0.01214,
0.01918)

num.sector<-seq(1,15)
plot(num.sector,probs
prop.test(2045,114746)
prop.test(237,11799)
prop.test(986,37218)
prop.test(189,10369)
prop.test(34,5777)
prop.test(25,995)
prop.test(2915,141239)
prop.test(124,4434)
prop.test(155,15791)
prop.test(135,7601)
prop.test(48,5519)
prop.test(2171,124151)
prop.test(891,42765)
prop.test(220,18124)
prop.test(24,1251)

plot(fundamt.nums,fundamt.probs, main="Fund Ammount by Default Rate", xlab="Funded Ammount", ylab="Default Rate")



gen.table<-read.csv("C:/Users/x47527/Desktop/Data tactics work/gender.csv",sep=";")
summary(gen.table)
gen.table[is.na(gen.table)] <- 0
str(gen.table)
gen.table$total<-gen.table$males+gen.table$females
summary(gen.table$total)
gen.table$group<-gen.table$total>1
summary(gen.table$group)
total <- merge(bigtable1,gen.table,by="loan_id")
total$defaulted <- total$status == "defaulted"
summary(total)
table(total$defaulted, total$group))
table(total$group)
plot(table(total$defaulted, total$group),main="Group vs Individual",xlab="Defaulted",ylab="Group                                                       Individual")

prop.test(1856,72865)
prop.test(8343,468453)

probs.groupind<-c(0.025471763, 0.017809684)
num.groupind<-seq(1,2)
?plot()
plot(num.groupind,probs.groupind, main="Group vs Individual", xlab="Group                                                                                 Individual", ylab="Default Rate")
segments(1,0.02434585,1,0.02664801)
segments(2,0.01743382,2,0.01819348)

### scater plot w/ CI's of sector default prob

prop.test(96,8171)
prop.test(2859,185108)
prop.test(6853,301660)
prop.test(403,47199)
prop.test(17,545)



fundamt.nums<-c(0,100,400,1600,6400)

fundamt.probs<-c(0.0117488679476196,
0.0154450374916265,
0.0227176291188756,
0.00853831648975614,
0.0311926605504587)


plot(fundamt.nums,fundamt.probs, main="Fund Ammount by Default Rate", xlab="Funded Ammount", ylab="Default Rate")

segments(0,0.009576062,0,0.014392502)
segments(100,0.0148906,100,0.01601969)
segments(400,0.02219032,400,0.02325713)
segments(1600,0.007737248,1600,0.009420397)
segments(6400,0.01885591,6400,0.05049301)

summary(gianttable$funded_amount)

###########
#Countries#
###########
loan_locations <- dbReadTable(con,"kiva_loans_locations")

loan_locations$loan_id <- factor(loan_locations$loan_id)

loan_locations$country <- factor(loan_locations$country)

loan_locations$country_code <- factor(loan_locations$country_code)

loan_locations <- subset(loan_locations, select = c(loan_id, country, country_code, geo_lng, geo_lat))

summary(loan_locations)

 

matches<- bigtable1$loan_id %in% loan_locations$loan_id

summary(matches)

 

gianttable<- merge(bigtable1, loan_locations, by = "loan_id")
gianttable$defaulted <- gianttable$status == "defaulted"
table(gianttable$country,gianttable$defaulted)

nums.country<-seq(1,70)
probs.country<-c(0.248820248820249,
0,
0.000402576489533011,
0.000138869601444244,
0.0153846153846154,
0.0055621771950735,
0.00424972956266419,
0,
0.0203389830508475,
0.0227743271221532,
0.00162074554294976,
0.000224681233499972,
0.00470035252643948,
0,
0.00845410628019324,
0.0224514563106796,
0,
0.00573300573300573,
0.135690148167403,
0.0540208319855082,
0.0171115173674589,
0.000385059684251059,
0.00891470250557169,
0.0322836729293464,
0.020066889632107,
0.00707818930041152,
0,
0.0142061281337047,
0.00103305785123967,
0.037037037037037,
0.00399885746929449,
0.0630082184632778,
0.00555555555555556,
0.00026246719160105,
0.00182505540346761,
0.15017825311943,
0,
0.018339072635541,
0,
0.00185972369819341,
0.00036101083032491,
0,
0.00741742321807997,
0,
0.00196099793005774,
0.00529100529100529,
0,
0.00866713516045912,
0.00137449651595014,
0.00987525987525988,
0.000610966855048114,
0.00937413840639647,
0.0458930137386729,
0,
0.00728332119446468,
0,
0.000293927458703192,
0.100305326246749,
0.00165152766308836,
0,
0.0934924994717938,
0.0227272727272727,
0.014705104469717,
0,
0.0454065469904963,
0,
0,
0,
0.138686131386861,
0.0188331880696675)
length(probs.country)


plot(nums.country,probs.country, xlab="Country", ylab="Probability of Default",main="Default Probability by Country")
segments(0,0.018833188,70,0.018833188, col="red")
segments(1,0.2314834,1,0.2669937)
segments(2,0,2,0.009356138)
segments(3,0.00002101491,3,0.002610486)
segments(4,0.000007249056,4,0.0009014557)
segments(5,0.0008036029,5,0.0940325439)
segments(6,0.003769927,6,0.008144308)
segments(7,0.003233067,7,0.005570657)
segments(8,0,8,0.007827069)
segments(9,0.00829448,9,0.04591891)
segments(10,0.01202042,10,0.04162097)
segments(11,0.00008460896,11,0.010458)
segments(12,0.000104476,12,0.0004619083)
segments(13,0.002187182,13,0.009634886)
segments(14,0,14,0.07380976)
segments(15,0.003709466,15,0.018150916)
segments(16,0.01907363,16,0.0263973)
segments(17,0,17,0.006369678)
segments(18,0.003265491,18,0.009855812)
segments(19,0.1251057,19,0.1470102)
segments(20,0.05053528,20,0.05773001)
segments(21,0.01503533,21,0.01946348)
segments(22,0.0000201005,22,0.0024970762)
segments(23,0.007492004,23,0.010597806)
segments(24,0.02698505,24,0.03855444)
segments(25,0.008183164,25,0.045314698)
segments(26,0.005189154,26,0.009614272)
segments(27,0,27,0.009411295)
segments(28,0.01070112,28,0.01878668)
segments(29,0.000178955,29,0.004157538)
segments(30,0.01732278,30,0.07433355)
segments(31,0.002277148,31,0.006879701)
segments(32,0.06081325,32,0.06527654)
segments(33,0.0009628519,33,0.0221456558)
segments(34,0.00001370098,34,0.001702921)
segments(35,0.001038945,35,0.003142789)
segments(36,0.1399177,36,0.1610435)
segments(37,0,37,0.0009752336)
segments(38,0.01609203,38,0.0208874)
segments(39,0,39,0.01607758)
segments(40,0.001058686,40,0.003202441)
segments(41,0.00001884511,41,0.00234135)
segments(42,0,42,0.003721593)
segments(43,0.006424791,43,0.008558932)
segments(44,0,44,0.000569209)
segments(45,0.001198676,45,0.003164446)
segments(46,0.003133618,46,0.00878164)
segments(47,0,47,0.0005213716)
segments(48,0.007920619,48,0.00948246)
segments(49,0.00113988,49,0.001655984)
segments(50,0.007839863,50,0.012413996)
segments(51,0.0001957362,51,0.0016790763)
segments(52,0.007340805,52,0.011942862)
segments(53,0.04111514,53,0.05118802)
segments(54,0,54,0.01857134)
segments(55,0.00527627,55,0.01000672)
segments(56,0,56,0.01787808)
segments(57,0.0001082172,57,0.0007288774)
segments(58,0.09416168,58,0.10679921)
segments(59,0.0002861107,59,0.0066378582)
segments(60,0,60,0.01388657)
segments(61,0.08774035,61,0.09957742)
segments(62,0.001187511,62,0.135090458)
segments(63,0.01306027,63,0.01655015)
segments(64,0,64,0.001307867)
segments(65,0.03342984,65,0.061201)
segments(66,0,66,0.0006346849)
segments(67,0,67,0.004048798)
segments(68,0,68,0.05921002)
segments(69,0.1141434,69,0.1673717)

text(1,.24,"AFG")
text(19,.145,"DR")
text(20,.065,"ECU")
text(33,.07,"KEN")
text(36,.17,"LIB")
text(58,.11,"TAN")
text(61.5, .1,"TOGO")
text(69,.15, "ZIM")

prop.test(580,2331)
prop.test(0,508)
prop.test(1,2484)
prop.test(1,7201)
prop.test(1,65)
prop.test(28,5034)
prop.test(55,12942)
prop.test(0,608)
prop.test(0,1)
prop.test(6,295)
prop.test(11,483)
prop.test(1,617)
prop.test(8,35606)
prop.test(8,1702)
prop.test(0,61)
prop.test(7,828)
prop.test(148,6592)
prop.test(0,748)
prop.test(14,2442)
prop.test(522,3847)
prop.test(835,15457)
prop.test(234,13675)
prop.test(7,8)
prop.test(1,2597)
prop.test(132,14807)
prop.test(122,3779)
prop.test(6,299)
prop.test(43,6075)
prop.test(0,505)
prop.test(51,3590)
prop.test(2,1936)
prop.test(8,216)
prop.test(14,3501)
prop.test(2898,45994)
prop.test(2,360)
prop.test(1,3810)
prop.test(14,7671)
prop.test(674,4488)
prop.test(0,4907)
prop.test(229,12487)
prop.test(0,294)
prop.test(14,7528)
prop.test(1,2770)
prop.test(0,1)
prop.test(0,1283)
prop.test(192,25885)
prop.test(0,8410)
prop.test(18,9179)
prop.test(16,3024)
prop.test(0,9182)
prop.test(481,55497)
prop.test(115,83667)
prop.test(76,7696)
prop.test(4,6547)
prop.test(68,7254)
prop.test(314,6842)
prop.test(0,254)
prop.test(40,5492)
prop.test(0,264)
prop.test(5,17011)
prop.test(887,8843)
prop.test(0,9)
prop.test(2,1211)
prop.test(0,341)
prop.test(885,9466)
prop.test(1,44)
prop.test(278,18905)
prop.test(0,3658)
prop.test(43,947)
prop.test(0,7542)
prop.test(0,1179)
prop.test(0,77)
prop.test(95,685)


library(lattice)
library(maps)


dim(table(gianttable$country))
length(unique(gianttable$country))
countrytable <- table(gianttable$country)
sum(table(gianttable$country)[table(gianttable$country)<1000])
summary(table(gianttable$country)[table(gianttable$country)<20000])
countrytable <- c( table(gianttable$country)[table(gianttable$country)>1000],
      other.26 = sum(table(gianttable$country)[table(gianttable$country)<1000]) )
    
table(gianttable$country)<2000

dotplot(table(gianttable$country)[table(gianttable$country)>15000])


#### this added data was from the practice data set.  some of it is entirely worthless, but a lot of it shows how i worked through and slowly learned how to work with R

load(url('http://eeyore.ucdavis.edu/stat135/data/Kiva_Loans.rda'))
#somewhat unimportant
 a <- loans$status
b <- loans$sector
c <- loans$country
plot(a)

str(loans)
#shows all of the variables we are working with


individual <- loans$numBorrowers == 1
group <- loans$numBorrowers >1
#set's the number of borrowers to either individual or groups

hist(individual+0)

hist(loans$numBorrowers, xlab="Number of Borrowers", main="Ammount of Borrowers")
Hist(loans$numFemale, scale="percent", breaks="Sturges", col="darkgray")


library(ggplot2)
ggplot(loans,aes(x=sector), xlab="Sector") + geom_bar()


defaulted <- loans$status == "defaulted"
#set's all of the defaulted loaner's as their own function
summary(defaulted)
summary(individual)
summary(group)
#gives you a summary of these sections

ggplot(data=loans, aes(x=defaulted, fill=individual),xlab="default",main="groupvind default")+geom_bar()
#stacked bar chart attempt... didn't work

#######################################################3
#######################################################
##############Rich's Stuff############################
#######################################################
#######################################################

# return rows 1-5 and all columns.
loans[1:5,]

# table of column sector w counts. 
table(loans$sector)

# two way table of sector and status w counts.
table(loans$sector, loans$status)

# xtabs is just a nicer way of looking at things. 
xtabs( ~ status + sector, data=loans)

# cross tabs - X2 analysis 
res <-  xtabs( ~ status + sector, data=loans)
summary(res)

# sector = rows, status = columns 
loans.tbl <- table(loans$sector, loans$status)

# row %
prop.table(loans.tbl, 1)
# column %
prop.table(loans.tbl, 2)

# row frequencies summed over columns
margin.table(loans.tbl, 1)
# column frequencies summed over rows
margin.table(loans.tbl, 2)

#####################################################
#####################################################
#############New Attempts############################
#####################################################


res2 <- xtabs( ~ numBorrowers+ (defaulted+0), data=loans)
summary(res2)
?xtabs()
res3 <- xtabs( ~ (individual+0)+ (defaulted+0), data=loans)
summary(res3)
res4<- xtabs(~ sector + (defaulted+0), data=loans)
summary(res4)
table(loans$individual, loans$defaulted)
table(loans$status, loans$numBorrowers)

country.tbl <- table(loans$country, loans$status)
prop.table(country.tbl, 1)
summary(loans$country)
countrysector.tbl<- table(loans$country, loans$sector)
prop.table(countrysector.tbl)
table(loans$country, loans$sector)
kenya <- loans$country == "Kenya"
table(kenya, loans$sector, defaulted)
unitedstates <-loans$country == "United States"
ecuador <-loans$country == "Ecuador"

ggplot(data=loans, aes(x=sector, fill=status["defaulted"])+geom_bar()]
library(relimp, pos=4)
showData(loans, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, 
  maxheight=30)

##################################################
#################Me Aug 2#######################
##################################################

str(loans)
table(unitedstates, loans$sector, defaulted)
table(ecuador, loans$sector, defaulted)
ecuador.tbl <- table(ecuador, loans$sector)
prop.table(ecuador.tbl)
kenya.tbl <- table(kenya, loans$status)
prop.table(kenya.tbl)
.0132+.008
.0132/.0212

summary(loans$delinquent+0==1)
delinquent <- loans$delinquent+0==1
table(delinquent, loans$status=="defaulted", kenya)
table( loans$country, delinquent)
table( loans$country, defaulted+0==1)

kenyasector.tbl <- table(kenya, loans$sector)
prop.table(kenyasector.tbl)
countrysector.tbl<- table(loans$country, loans$sector)
prop.table(countrysector.tbl,2)
delinqpayamt.tbl <- table(delinquent, loans$paid_amount)
prop.table(delinqpayamt.tbl,2)

library(lattice)
library(maps)
smoothScatter(loans$long, loans$lat, main = "Density of Kiva Loans by Location", 
              xlab = "Longitude", ylab = "Latitude", bandwidth = c(5, 1))
map(xlim = range(loans$long), ylim = range(loans$lat), add = TRUE)\

delinquent = !is.na(loans$delinquent)
#sets delinquent to no NA's, meaning that it will only show when it is true

scatterplot(numBorrowers~loan_amount | delinquent, reg.line=lm, smooth=TRUE, 
  spread=TRUE, boxplots='xy', span=0.5, by.groups=TRUE, data=loans, main = "Delinquencies by # Borrowers and Loan Ammount")

library(ggplot2)
ggplot(loans, aes(x=sector,fill=loans$delinquent))+geom_bar()


xtabs(~defaulted + funded_amount, data=loans)

######################################################
########################Aug5##########################
######################################################

numMales <- loans$numBorrowers-loans$numFemales

table(defaulted, loans$numBorrowers)
table(loans$activity, defaulted)
#table of defaults vs activity

mosaicplot(table(loans$sector, defaulted))
#mosaic plot of sector vs default

cor(defaulted, loans$numBorrowers)
cor(defaulted, loans$avgPayment)
round(cor(defaulted,loans$activity),2)
#attempts at correlation tests

table(defaulted, loans$avgPayment)
table(loans$country, loans$sector, defaulted)
countrysectordefault.tbl <- table(loans$country, loans$sector, defaulted)
prop.table(countrysectordefault.tbl)

prop.test(defaulted, loans$sector)

#finding intervals for Status vs Sector
prop.test(776,28796)
prop.test(81,4402)
prop.test(402,13003)
prop.test(57,3882)
prop.test(4,178)
prop.test(958,48276)
prop.test(34,1542)
prop.test(23,2993)
prop.test(54,2536)
prop.test(738,38965)
prop.test(344,14259)
prop.test(40,5042)
prop.test(5,443)
prop.test(0,560)
prop.test(7,566)
prop.test(0,9)

#finding proportions for Status w/ group vs ind
prop.test(3374,146384)

prop.test(149,19068)

#countries by default
prop.test(0,1465)
prop.test(0,59)
prop.test(0,4540)
prop.test(0,1660)
prop.test(0,4525)
prop.test(2,576)
prop.test(6,295)
prop.test(0,15386)
prop.test(4,601)
prop.test(0,9)
prop.test(0,20)
prop.test(0,269)
prop.test(0,202)
prop.test(485,2265)
prop.test(734,2412)
prop.test(0,1406)
prop.test(7,7)
prop.test(0,8334)
prop.test(0,915)
prop.test(0,61)
prop.test(0,1809)
prop.test(0,1940)
prop.test(0,196)
prop.test(2186,4960)
prop.test(0,136)
prop.test(0,2733)
prop.test(0,447)
prop.test(0,1414)
prop.test(11,5729)
prop.test(0,259)
prop.test(0,1278)
prop.test(0,1173)
prop.test(0,685)
prop.test(8,8573)
prop.test(0,7245)
prop.test(0,2562)
prop.test(0,317)
prop.test(0,3101)
prop.test(0,20264)
prop.test(0,14448)
prop.test(0,582)
prop.test(0,3589)
prop.test(0,2515)
prop.test(0,1641)
prop.test(0,1444)
prop.test(0,8768)
prop.test(0,7148)
prop.test(0,205)
prop.test(67,5555)
prop.test(13,4414)
prop.test(0,1732)
prop.test(0,118)
prop.test(0,3467)

######################################
#############Aug 6###################
####################################


library(ggplot2)
ggplot(loans, aes(x=sector))+geom_bar()

#plot of probability of default per sector
default.probs<- c(0.026948187, 0.018400727, 0.030915943, 0.014683153, 0.02247191, 0.019844229, 0.022049287, 0.007684597, 0.021293375, 0.018940074, 0.024125114, 0.00793336
,0.011286682
,0
,0.012367491
,0)

nums<- seq(1,16)
nums

sector.names<- c("Agriculture",
"Arts",
"Clothing",
"Construction",
"Entertainment",
"Food",
"Health",
"Housing",
"Manufacturing",
"Retail",
"Services",
"Transportation",
"Education",
"Personal Use",
"Wholesale",
"Green")

length(sector.names)

### scater plot w/ CI's of sector default prob
plot(nums,default.probs, xlab="sector",main="Probability of Default per Sector")

segments(1,0.02512326,1,0.02890049)
segments(2,0.01472853,2,0.02293662)
segments(3,0.02803997,3,0.03407257)
segments(4,0.01123833,4,0.01911991)
segments(5,0.007222921,5,0.060232132)
segments(6,0.01862778,6,0.02113772)
segments(7,0.01554966,7,0.03102796)
segments(8,0.004992507,8,0.011704849)
segments(9,0.01618502,9,0.02790016)
segments(10,0.01762088,10,0.02035501)
segments(11,0.02169863,11,0.0268115)
segments(12,0.005747598,12,0.010898336)
segments(13,0.004162571,13,0.027696671)
segments(14,0,14,0.008493339)
segments(15,0.005429929,15,0.026477604)
segments(16,0,16,0.3711903)
text(1.5, .028, "Agriculture")
text(2,.02, "Art")
text(3,.029,"Clothing")
text(4,.017,"Construction")
text(5, .025,"Entertainment")
text(6, .022,"Food")
text(7, .024,"Health")
text(8, .01,"Housing")
text(9,.023,"Manufacturing")
text(10,.02,"Retail")
text(11,.025,"Services")
text(12,.01,"Transportation")
text(13,.0125,"Education")
text(14,.003,"Personal Use")
text(15,.015,"Wholesale")
text(16,.0025,"Green")

?plot()


#below is to make a plot for probability of default per country
names<-c("Argentina",
"Armenia",
"Azerbaijan",
"Benin",
"Bolivia",
"Bosnia and Herzegovina",
"Bulgaria",
"Cambodia",
"Cameroon",
"Chile",
"Colombia",
"Costa Rica",
"Cote D'Ivoire",
"Dominican Republic",
"Ecuador",
"El Salvador",
"Gaza",
"Ghana",
"Guatemala",
"Haiti",
"Honduras",
"Indonesia",
"Iraq",
"Kenya",
"Kyrgyzstan",
"Lebanon",
"Liberia",
"Mali",
"Mexico",
"Moldova",
"Mongolia",
"Mozambique",
"Nepal",
"Nicaragua",
"Nigeria",
"Pakistan",
"Palestine",
"Paraguay",
"Peru",
"Philippines",
"Rwanda",
"Samoa",
"Senegal",
"Sierra Leone",
"South Sudan",
"Tajikistan",
"Tanzania",
"Congo",
"Togo",
"Uganda",
"Ukraine",
"United States",
"Viet Nam")

country.probs <-c(0,
0,
0,
0,
0,
0.0034722222,
0.0203389831,
0,
0.006655574,
0,
0,
0,
0,
0.2141280353,
0.3043117745,
0,
1,
0,
0,
0,
0,
0,
0,
0.4407258065,
0,
0,
0,
0,
0.0019200559,
0,
0,
0,
0,
0.0009331623,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0.0120612061,
0.0029451744,
0,
0,
0)
length(names)
nums.country<-seq(1,53)
plot(nums.country,country.probs, xlab="Country", ylab="probability of default")
segments(14,0.1975116,14,0.2317251)
segments(15,0.2860692,15,0.3231837)
segments(17,0.5609339,17,1)
segments(24,0.4268599,24,0.4546842)
text(24,.5, "Kenya")
text(14,.25, "Domincan Republic")
text(15,.4, "Ecuador")
text(17, .95, "Gaza")

table(defaulted, individual)
plot(table(defaulted, individual))
sessionInfo()


#############kivabigtable(kiva_loans.loan_id, funded_amount, activity, status, kiva_loans_terms.loan_amount, paid_amount, sector, funded_date, use, disbursal_date)
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, host='analytics.data-tactics-corp.com', port='5432', dbname='kiva',
                 user='cfisk', password='47Njt**')  # change to your password

rs <- dbSendQuery(con, "select * from kiva_bigtble")
dat <- fetch(rs,n=100)   ## fetch first 100 rows from result set

PostgreSQL(max.con = 16, fetch.default.rec = 500, force.reload = TRUE)

rs2 <- dbSendQuery(con, "select * from kiva_bigtble2")
dat2 <- fetch(rs2,n=-1)   ## fetch all rows of result set


### Funded amt default

funded.amt<-c(0,
25,
50,
75,
100,
125,
150,
175,
200,
210,
225,
250,
275,
300,
325,
350,
375,
400,
425,
450,
475,
500,
525,
550,
575,
600,
625,
650,
675,
700,
725,
750,
775,
800,
825,
850,
875,
900,
925,
950,
975,
1000,
1025,
1050,
1075,
1100,
1125,
1150,
1175,
1200,
1225,
1250,
1275,
1300,
1325,
1350,
1375,
1400,
1425,
1450,
1475,
1500,
1525,
1550,
1575,
1600,
1625,
1650,
1675,
1700,
1725,
1750,
1775,
1800,
1825,
1850,
1875,
1900,
1925,
1950,
1975,
2000)

prob.default.fundamt<- c(0,
0.0714285714285714,
0.00911854103343465,
0.0520381613183001,
0.0122448979591837,
0.00349514563106796,
0.060272536687631,
0.0113502009931426,
0.0164130110414802,
0,
0.0127373227589522,
0.0205050723073602,
0.00476730987514188,
0.0582450832072617,
0.00519031141868512,
0.00930576070901034,
0.0144889357218124,
0.0262738853503185,
0.00526052104208417,
0.054640522875817,
0.0045045045045045,
0.010791788856305,
0.0101721439749609,
0.0198360222163449,
0.0145419292292778,
0.0423312883435583,
0.00253699788583509,
0.0231776956667786,
0.0225538971807629,
0.0240187463386057,
0.00905432595573441,
0.0437768240343348,
0.00363636363636364,
0.0232001913417843,
0.049373618275608,
0.0282931354359926,
0.00504201680672269,
0.0395861448493027,
0.0172532781228433,
0.0470420527441197,
0.00937709310113865,
0.0160487683503359,
0.00491266375545852,
0.02937576499388,
0.0452961672473868,
0.0220635950681376,
0.0134310134310134,
0.0159924741298213,
0.0108588351431392,
0.0457782299084435,
0.0234375,
0.0142857142857143,
0.03125,
0.03125,
0.0240963855421687,
0.0324074074074074,
0,
0.0181159420289855,
0.0182648401826484,
0.028169014084507,
0.0709677419354839,
0.03584229390681,
0.0072463768115942,
0.0137931034482759,
0.00735294117647059,
0.0450819672131148,
0,
0.00735294117647059,
0.00847457627118644,
0.0211267605633803,
0.00699300699300699,
0.0078125,
0.0225563909774436,
0.109243697478992,
0,
0.00934579439252336,
0.00714285714285714,
0.018348623853211,
0,
0,
0,
0.012565445026178)




