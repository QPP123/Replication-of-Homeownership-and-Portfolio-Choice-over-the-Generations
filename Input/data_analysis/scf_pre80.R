# scf_pre80.R
# analyses the SCF waves before 1989

# In order to produce:

# - Data for Figure 1 
#       saved as stockp_data_oldscf and loaded by scf_participation_actual
#       and by Matlab for Figures 4 and 8

# - Data for Figure 10 
#       saved as wtoy_data_oldscf_95.csv and wtoy_data_oldscf_med.csv
#       loaded by Matlab

# Note that for this file you need the ICPSR data files
# If not available, a few datapoints in the graphs above will not show
# But the missing part will be minor

rm(list=setdiff(ls(), c("main.folder","da.folder")))

source(paste0(main.folder,"code/data_analysis/functions/gon_graphs.R"))
source(paste0(main.folder,"code/data_analysis/functions/pre80_functions.R"))
data.path <- paste0(main.folder,"data/datapre80/")
folder <- paste0(main.folder,"results/Figures/")
folder.data <- paste0(main.folder,"data/")
folder.to <- paste0(main.folder,"working/Model/")

# changed folder to from Values to working


## CHECK IF DATA IS DOWNLOADED

isdownloaded <- file.exists(paste0(data.path,'ICPSR_07443/DS0001/07443-0001-Data.txt'))

if (isdownloaded) {

path63 <- paste0(data.path,"ICPSR_07443/DS0001/")
varnames63 <- varlines(path63)

path68 <- paste0(data.path,"ICPSR_07448/DS0001/")
varnames68 <- varlines(path68)

path69 <- paste0(data.path,"ICPSR_07449/DS0001/")
varnames69 <- varlines(path69)

path70 <- paste0(data.path,"ICPSR_07450/DS0001/")
varnames70 <- varlines(path70)

path71 <- paste0(data.path,"ICPSR_07451/DS0001/")
varnames71 <- varlines(path71)

path77 <- paste0(data.path,"ICPSR_09752/DS0001/")
varnames77 <- varlines(path77)



df63 <- read.fwf(paste0(path63,'07443-0001-Data.txt'),widths=varnames63$col.length)
names(df63) <- varnames63$col.name

df68 <- read.fwf(paste0(path68,'07448-0001-Data.txt'),widths=varnames68$col.length)
names(df68) <- varnames68$col.name

df69 <- read.fwf(paste0(path69,'07449-0001-Data.txt'),widths=varnames69$col.length)
names(df69) <- varnames69$col.name

df70 <- read.fwf(paste0(path70,'07450-0001-Data.txt'),widths=varnames70$col.length)
names(df70) <- varnames70$col.name

df71 <- read.fwf(paste0(path71,'07451-0001-Data.txt'),widths=varnames71$col.length)
names(df71) <- varnames71$col.name

df77 <- read.fwf(paste0(path77,'09752-0001-Data.txt'),widths=varnames77$col.length)
names(df77) <- varnames77$col.name

save.image(file=paste0(folder.data,"SCF_pre_Sample.RData"))

# Variables that I want in every survey
# Birth year
# Whether has stocks
# Now, net worth

# For 1963
# age is 36 (already bracketed)
# v3: hold stock (0: no, 1-9 yes)

load(paste0(folder.data,"SCF_pre_Sample.RData"))

df63$agroup <- df63$V36
df63$agroup <- as.factor(df63$agroup)
df63$agroup <- plyr::mapvalues(df63$agroup,from=seq(1,6),to=seq(20,70,by=10))
df63$agroup2 <- as.numeric(as.character(df63$agroup))

df63$hstocks <- 0
df63$hstocks[df63$V3!=0] <- 1


df63 <- as.data.table(df63)

stocks.byage.63 <- df63[,mean(hstocks),by="agroup2"]
std.byage.63 <- df63[,sqrt(var(hstocks)/length(hstocks)),by="agroup2"]

mycoh.63 <- subset(stocks.byage.63,agroup2==20)
mycoh.63 <- mycoh.63$V1
mycoh.std.63 <- std.byage.63$V1[std.byage.63$agroup2==20]


df63$networth <- df63$V443
df63$income <- as.factor(df63$V35)
df63$income <- plyr::mapvalues(df63$income,from=seq(0,9),to=c("0","1500","2500","3500","4500","5500","6750","8775","12500","17500"))
df63$income <- as.numeric(as.character(df63$income))

mean.income <- df63[,mean(income)]
df63$networth <- df63$networth/mean.income


# Just plain: wealth of this group to cross-sectional income ratio
# Get median, .95, total
df63.young <- subset(df63,agroup2==20)

bk <- df63.young[,quantile(networth,0.95)]
df63.mycoh.restrict <- subset(df63.young,networth<bk)

wtoy.63.95 <- mean(df63.mycoh.restrict$networth)
wtoy.63.med <- median(df63.young$networth)


# For 1968
# age is 22
# v442: whether stock, 5 is no, 1-8 is yes
# v133 : total family income

# v534: total liquid assets (checking, savings, bonds)
# v150: equity on house
# v292: stocks and shares (careful 999998,999999 are dks)
# v298: value of other real estate
# v299: amount borrowed in last year for real estate
# v300: amount borrowed before for real estate


df68$agroup <- cut(df68$V22,breaks=seq(20,60,by=5),labels=seq(22,57,by=5))
df68$agroup2 <- as.numeric(as.character(df68$agroup))

df68$hstocks <- 0
df68$hstocks[df68$V442<5 | (df68$V442>5 & df68$V442<9)] <- 1

df68 <- as.data.table(df68)

df68$V292[df68$V292>999997] <- 0
df68$V298[df68$V298>999997] <- 0
df68$V299[df68$V299>999997] <- 0
df68$V300[df68$V300>999997] <- 0
df68$V534[df68$V534>9999996] <- 0

df68$networth <- df68$V150+df68$V292+df68$V298-df68$V299-df68$V300+df68$V534

df68$homeo <- 0
df68$homeo[df68$V144>0 & df68$V144<5] <- 1

df68$stockval <- df68$V292

df68.mycoh <- subset(df68,V22>22 & V22<29)

mycoh.68 <- df68.mycoh[,mean(hstocks)]
mycoh.std.68 <- df68.mycoh[,sqrt(var(hstocks)/length(hstocks))]

mean.income68 <- df68[,mean(V133)]

q68 <- df68[,quantile(V133,0.95)]
mean.income68.95 <- mean(df68$V133[df68$V133<q68])

bk <- df68.mycoh[,quantile(networth,0.95)]
df68.mycoh.restrict <- subset(df68.mycoh,networth<bk)
wtoy.68.95 <- df68.mycoh.restrict[,mean(networth)]/mean.income68.95
wtoy.68.med <- df68.mycoh[,median(networth)]/mean.income68


# For 1969
# age is 29
# V468: whether stock, 5 is no, 1-8 is yes
# v471 : whether sold, purchased... as '70

df69$hstocks <- 0
df69$hstocks[df69$V468<5 | (df69$V468>5 & df69$V468<9)] <- 1

df69 <- as.data.table(df69)
df69$homeo <- 0 
df69$homeo[df69$V542==1] <- 1

df69.mycoh <- subset(df69,V29>23 & V29<30)

mycoh.69 <- df69.mycoh[,mean(hstocks)]
mycoh.std.69 <- df69.mycoh[,sqrt(var(hstocks)/length(hstocks))]

mycoh.69.homeo <- df69.mycoh[,mean(homeo)]


# For 1970
# age is 20
# V303: whether stock, 1,2,3,4,6,7,8 are yes, 5 is no
# v307: whether sold (1), purchased (2), both (3) or neither (4) stock in the last year

df70$hstocks <- 0
df70$hstocks[df70$V303<5 | (df70$V303>5 & df70$V303<9)] <- 1

df70 <- as.data.table(df70)

df70$homeo <- 0
df70$homeo[df70$V145>0 & df70$V145<5] <- 1

df70$married <- 0 
df70$married[df70$V24>1] <- 1
df70$age <- df70$V20

df70.mycoh <- subset(df70,V20>24 & V20<31)

mycoh.70 <- df70.mycoh[,mean(hstocks)]
mycoh.std.70 <- df70.mycoh[,sqrt(var(hstocks)/length(hstocks))]
mycoh.70.homeo <- df70.mycoh[,mean(homeo)]

# For 1971
# age is variable 20
# V281 : own stock in a corporation (1, 3, 7, 8 are yes, 5 is no)
# v376 : total financial ssets (savings, stocks, bonds, checkig, etc.)
# v298: house equity


df71$hstocks <- 0
df71$hstocks[df71$V281<5 | (df71$V281>5 & df71$V281<9)] <- 1

df71 <- as.data.table(df71)

df71$homeo <- 0
df71$homeo[df71$V292==1] <- 1

df71$V376[df71$V376>999996] <- 0

df71$networth <- df71$V376+df71$V298

df71$incomef <- as.factor(df71$V148)
df71$incomef <- plyr::mapvalues(df71$incomef,from=seq(0,10),to=c(1500,3500,4500,5500,6750,8750,11250,13750,20000,0,30000))
df71$income <- as.numeric(as.character(df71$incomef))

df71$stockval <- df71$V282
df71$stockval[df71$stockval>999997] <- 0


mean.income.71 <- df71[,mean(income)]


q71 <- df71[,quantile(income,0.95)]
mean.income.71.95 <- mean(df71$income[df71$income<q71])

df71.mycoh <- subset(df71,V20>25 & V20<32)

mycoh.71 <- df71.mycoh[,mean(hstocks)]
mycoh.std.71 <- df71.mycoh[,sqrt(var(hstocks)/length(hstocks))]

bk <- df71.mycoh[,quantile(networth,0.95)]
df71.mycoh.restrict <- subset(df71.mycoh,networth<bk)
wtoy.71.95 <- df71.mycoh.restrict[,mean(networth)]/mean.income.71.95
wtoy.71.med <- df71.mycoh[,median(networth)]/mean.income.71


# For 1977
# byear is v281
# hstocks is 
# 925 - directly held stocks in coporations
# 927 - mutual funds
# 929 - via investnment club

#


df77$hstocks <- 0
df77$hstocks[df77$V925==1 | df77$V927==1 | df77$V929==1] <- 1

df77 <- as.data.table(df77)

df77$homeo <- 0
df77$homeo[df77$V303>0 & df77$V303<3] <- 1

# I want to try to get wealth in 77 because it is a key point in the graph

df77$checking <- as.factor(df77$V915)

codes <- c(seq(0,19),98,99)
values <- c(0,75,175,375,750,1500,2750,4500,6250,8750,
            11250,13750,17500,22500,27500,37500,62500,
            87500,150000,250000,0,0)

df77$checking <- as.numeric(as.character(plyr::mapvalues(df77$checking,
                      from=codes,to=values)))

df77$bonds <- as.factor(df77$V918)
df77$bonds <- as.numeric(as.character(plyr::mapvalues(df77$bonds,
                    from=codes,to=values)))

df77$bonds2 <- as.factor(df77$V920)
df77$bonds2 <- as.numeric(as.character(plyr::mapvalues(df77$bonds2,
                                                from=codes,to=values)))

df77$bonds3 <- as.factor(df77$V922)
df77$bonds3 <- as.numeric(as.character(plyr::mapvalues(df77$bonds3,
                                                 from=codes,to=values)))

df77$bonds4 <- as.factor(df77$V924)
df77$bonds4 <- as.numeric(as.character(plyr::mapvalues(df77$bonds4,
                                                 from=codes,to=values)))

df77$stock <- as.factor(df77$V926)
df77$stock <- as.numeric(as.character(plyr::mapvalues(df77$stock,
                                                 from=codes,to=values)))

df77$mutual <- as.factor(df77$V928)
df77$mutual <- as.numeric(as.character(plyr::mapvalues(df77$mutual,
                                                from=codes,to=values)))

df77$iclub <- as.factor(df77$V930)
df77$iclub <- as.numeric(as.character(plyr::mapvalues(df77$iclub,
                                                 from=codes,to=values)))

df77$cdep <- as.factor(df77$V932)
df77$cdep <- as.numeric(as.character(plyr::mapvalues(df77$cdep,
                                                 from=codes,to=values)))

df77$V935[df77$V935>99995] <- 0
df77$V936[df77$V936>99995] <- 0
df77$V938[df77$V938>99995] <- 0
df77$V939[df77$V939>99995] <- 0
df77$V940[df77$V940>99995] <- 0
df77$V941[df77$V941>99995] <- 0
df77 <- as.data.table(df77)

df77$orealestate <- df77$V935-df77$V936+df77$V938-df77$V939+df77$V940-df77$V941

df77$V318[df77$V318>9995] <- 0
df77$V323[df77$V323>90] <- 0
df77$mortgage <- df77$V318*df77$V323*12

# Alternative construction

df77$V317[df77$V317>999995] <- 0 # value when took 1st
df77$V326[df77$V326>999995] <- 0 # value when took 2nd

df77$V327[df77$V327>9995] <- 0 # payments 2nd
df77$time.ago.took.2nd <- 0
df77$time.ago.took.2nd[df77$V314<97] <- 77-df77$V314[df77$V314<97]

df77$time.ago.bought <-  0
df77$time.ago.bought[df77$V301<98] <- 77-df77$V301[df77$V301<98]
df77$mortgage2 <- df77$V317-df77$V318*12*df77$time.ago.bought+df77$V327-12*df77$V327*df77$time.ago.took.2nd
df77$mortgage2[df77$mortgage2<0] <- 0


df77$V308[df77$V308>999995] <- 0
df77$hvalue <- df77$V308-df77$mortgage2

df77$networth <- df77$hvalue+ df77$orealestate + df77$cdep +
  df77$iclub + df77$mutual + df77$stock + df77$checking + 
  df77$bonds + df77$bonds2 + df77$bonds3 + df77$bonds4

df77 <- subset(df77,is.na(networth)==0)

# Need income

values2 <- c(0,1000,2500,3500,4500,5500,6750,8250,9500,10500,11750,13750,16250,
             18250,21250,23750,27500,32500,42500,60000,0,0)


df77$hhinc <- as.factor(df77$V947)
df77$hhinc <- as.numeric(as.character(plyr::mapvalues(df77$hhinc,
                                               from=codes,to=values2)))


mean.income.77 <- df77[,mean(hhinc)]
q77 <- df77[,quantile(hhinc,0.95)]
mean.income.77.95 <- mean(df77$hhinc[df77$hhinc<q77])



df77.mycoh <- subset(df77,V281>1939 & V281<1946)

mycoh.77 <- df77.mycoh[,mean(hstocks)]
mycoh.std.77 <- df77.mycoh[,sqrt(var(hstocks)/length(hstocks))]


bk <- df77.mycoh[,quantile(networth,0.95)]
df77.mycoh.restrict <- subset(df77.mycoh,networth<bk)
wtoy.77.95 <- df77.mycoh.restrict[,mean(networth)]/mean.income.77.95
wtoy.77.med <- df77.mycoh[,median(networth)]/mean.income.77


# b3005: weight
# b4502 : year of birth
# b3462 : total amount in stocks or mutual funds
# b3446: IRA or Keogh accounts total amount

# b3324: net worth
# b3201: total gross household income


df83 <- read.dta13(paste0(data.path,'scf83b.dta'))

df83$hstocks <- 0
df83$hstocks[df83$b3462>0] <- 1
df83$hstocks[df83$b3446>0] <- 1 # assumption here (no data about IRA composition)

df83 <- as.data.table(df83)

df83$networth <- df83$b3323
df83$stockval <- df83$b3462+df83$b3470+df83$b3446
# direct stocks + trusts + IRAs

df83$homeo <- 0
df83$homeo[df83$b3702==1] <- 1


df83.mycoh <- subset(df83,b4502>1939 & b4502<1945)


mycoh.83 <- df83.mycoh[,weighted.mean(hstocks,weights=b3005)]
mycoh.std.83 <- df83.mycoh[,sqrt(wtd.var(hstocks,weights=b3005)/length(hstocks))]

mmm <- df83[,mean(b3201,na.rm=T)]
mean.income.83 <- df83[,weighted.mean(b3201,b3005,na.rm=T)]

q83 <- df83[,wtd.quantile(b3201,weights=b3005,0.95,na.rm=T)]
mean.income.83.95 <- weighted.mean(df83$b3201[df83$b3201<q83],df83$b3005[df83$b3201<q83],na.rm=T)

bk <- df83.mycoh[,wtd.quantile(networth,weights=b3005,0.95)]
df83.mycoh.restrict <- subset(df83.mycoh,networth<bk)
wtoy.83.95 <- df83.mycoh.restrict[,weighted.mean(networth,b3005)]/mean.income.83.95
wtoy.83.med <- df83.mycoh[,wtd.quantile(networth,weights=b3005,0.5)]/mean.income.83


# c1012: weights
# c1602: year of birth
# c1401: stock holdings
# c1407: IRA
# c1415: other, that seem assimilable to stocks

# c1457: net worth
# c1301: total household income

df86 <- read.dta13(paste0(data.path,'scf86b.dta'))

df86$homeo <- 0
df86$homeo[df86$c1502==1] <- 1


df86$hstocks <- 0
df86$hstocks[df86$c1401>0] <- 1
df86$hstocks[df86$c1407>0] <- 1 # assumption

df86$stockval <- df86$c1401+df86$c1407

df86 <- as.data.table(df86)
df86.mycoh <- subset(df86,c1602>1939 & c1602<1945)

mycoh.86 <- df86.mycoh[,weighted.mean(hstocks,c1012)]
mycoh.std.86 <- df86.mycoh[,sqrt(wtd.var(hstocks)/length(hstocks))]


mean.income.86 <- df86[,weighted.mean(c1301,c1012)]
q86 <- df86[,wtd.quantile(c1301,weights=c1012,0.95)]
mean.income.86.95 <- weighted.mean(df86$c1301[df86$c1301<q86],df86$c1012[df86$c1301<q86])



bk <- df86.mycoh[,wtd.quantile(c1457,weights=c1012,0.95)]
df86.mycoh.restrict <- subset(df86.mycoh,c1457<bk)
wtoy.86.95 <- df86.mycoh.restrict[,weighted.mean(c1457,c1012)]/mean.income.86.95
# Now export this piece of actual data
wtoy.86.med <- df86.mycoh[,wtd.quantile(c1457,weights=c1012,0.5)]/mean.income.86



piece <- c(mycoh.63,mycoh.68,mycoh.69,mycoh.70,mycoh.71,mycoh.77,mycoh.83,mycoh.86)
age <- c(20,25,26,27,28,34,39,42)
piece.std <- c(mycoh.std.63,mycoh.std.68,mycoh.std.69,mycoh.std.70,mycoh.std.71,mycoh.std.77,mycoh.std.83,mycoh.std.86)

stockp.data.oldscf <- as.data.table(piece)
stockp.data.oldscf$age <- age
stockp.data.oldscf$std <- piece.std

write.table(stockp.data.oldscf,file=paste0(folder.to,"stockp_data_oldscf.csv"),
            row.names=F,sep=",")

piece2 <- c(wtoy.63.95,wtoy.68.95,wtoy.71.95,wtoy.77.95,wtoy.83.95,wtoy.86.95)
piece4 <- c(wtoy.63.med,wtoy.68.med,wtoy.71.med,wtoy.77.med,wtoy.83.med,wtoy.86.med)
age2 <- c(21,26,29,35,41,44)

wtoy.data.oldscf <- as.data.table(piece2)
wtoy.data.oldscf$age <- age2

write.table(wtoy.data.oldscf,file=paste0(folder.to,"wtoy_data_oldscf_95.csv"),
            row.names=F,sep=",")


wtoy.data.oldscf <- as.data.table(piece4)
wtoy.data.oldscf$age <- age2

write.table(wtoy.data.oldscf,file=paste0(folder.to,"wtoy_data_oldscf_med.csv"),
            row.names=F,sep=",")


} # If not downloaded, don't touch (extract measures provided)
