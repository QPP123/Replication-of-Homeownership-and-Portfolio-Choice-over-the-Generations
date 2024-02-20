# THIS CODE

# Complements psid_outputs to generate statistics on the total (not only SRC)
# sample first, then conditional on employment/unemployment status
# and then only for the married

# For now, written sequentially because each of the cases requires some
# specific adjustments in the code.
# Provided here to avoid cluttering psid_outputs

folder <- paste0(main.folder,"results/Figures/")
folder.from <- paste0(main.folder,"working/")
load(paste0(folder.from,'PSIDSample.RData'))
source(paste0(main.folder,'code/data_analysis/functions/gon_graphs.R'))

# dam is original, do not edit

dt <- dam

# Define different cohort types

dt$decade <- cut(dt$byear,breaks=seq(1940,1990,10),
                 labels=seq(1940,1980,10))
dt$bigcoh <- cut(dt$byear,breaks=seq(1930,1990,20))
dt$earncoh <- factor(0,levels=c(0,"1940","1960","1980"))
dt$earncoh[dt$byear>1934 & dt$byear<1950] <- "1940"
dt$earncoh[dt$byear>1949 & dt$byear<1971] <- "1960"
dt$earncoh[dt$byear>1970] <- "1980"


# Some sample selection requirements are common for both cases
# (but not for some robustness checks)

dt <- subset(dt,age>19 & age<100)
dt <- subset(dt,is.na(age)==0)
dam.weights <- dt # keep the non-SRC sample for robustness checks


dam.weights$SampleWeight <- NA
dam.weights$SampleWeight[is.na(dam.weights$WeightCore)==0] <- 
  dam.weights$WeightCore[is.na(dam.weights$WeightCore)==0]
dam.weights$SampleWeight[is.na(dam.weights$WeightNew)==0] <- 
  dam.weights$WeightNew[is.na(dam.weights$WeightNew)==0]

de <- subset(dam.weights,age>19 & age<63)
de <- subset(de,nextrelhead==1 | nextrelhead==10)
de <- subset(de,is.na(EquiScale)==0) # available hh information
de <- subset(de,labtotal>1518) # minimum earnings level


# Define some other earnings variables
de$actual.labtotal <- exp(de$loglabtotal)

# Detrend & extract average regression factor (will bring that back in model)

regre.trend <- lm(loglabtotal ~ year,data=de)
de$detlabtotal <- residuals(regre.trend)
regre.rec <- lm(detlabtotal ~ factor(rec),data=de)
de$resy <- de$detlabtotal #loglabtotal
de$logy <- de$resy


## Earnings transformations for earnings process & save results

earn <- subset(de,select=c("personid","year","resy","age","rec","byear","earncoh","SampleWeight"))
earn <- subset(earn,is.na(resy)==0)

# Perform AGE reconstructions to make our life easier
# These are not really needed for cross-sectional statistics
# But really help when looking at changes

min.point <- earn[,list(minpoint=min(year)),by="personid"]
earn <- merge(earn,min.point,by="personid",all.x=T)
earn$minage <- 0
earn$minage[earn$year==earn$minpoint] <- 1

min.age <- subset(earn,minage==1)
# We choose to believe this byear in case of conflict
min.age$byear2 <- min.age$byear
min.age <- subset(min.age,select=c("personid","byear2"))

earn <- merge(earn,min.age,by="personid",all.x=T)
earn$byear <- earn$byear2
earn$byear2 <- NULL
earn$minage <- NULL
earn$minpoint <- NULL
earn$age <- earn$year-earn$byear


earn[,lage:=shift(age,type="lead"),by="personid"]
earn[,lyear:=shift(year,type="lead"),by="personid"]
earn <- subset(earn,(age-lage)==(year-lyear) | is.na(lyear))

earn.all <- subset(earn,select=c("personid","year","resy","rec","byear","earncoh","SampleWeight"))


earn[, yearl:=shift(year,type="lag"),by="personid"]
earn$yeardif <- earn$year-earn$yearl
earn$yeardif[is.na(earn$yearl)] <- 1

cumsu <- earn[,list(year,cumsum(yeardif)),by=personid]
setkeyv(earn,c("personid","year"))
setkeyv(cumsu,c("personid","year"))
earn <- merge(earn,cumsu)
# V2 is the sum of years SINCE the first observation!! (one=it is the first)

earn[, a1:=shift(V2,type="lead",n=1),by="personid"]
earn[, a2:=shift(V2,type="lead",n=2),by="personid"]
earn[, a3:=shift(V2,type="lead",n=3),by="personid"]
earn[, a4:=shift(V2,type="lead",n=4),by="personid"]

earn$valid <- 0
earn$valid[(earn$a1==(earn$V2+2)|earn$a2==(earn$V2+2)|earn$a3==(earn$V2+2)|earn$a4==(earn$V2+2)) &
             (earn$a1==(earn$V2+4)|earn$a2==(earn$V2+4)|earn$a3==(earn$V2+4)|earn$a4==(earn$V2+4))] <- 1
earn$valid[earn$yeardif>2] <- 0

# Now just need to eliminate observations that have already been taken

earn <- subset(earn,valid==1)
earn[, prev:=shift(V2,type="lag",n=1),by="personid"]
earn[, prev2:=shift(V2,type="lag",n=2),by="personid"]

earn$dupli <- 0
earn$dupli[(earn$prev==(earn$V2-2))|(earn$prev2==(earn$V2-2))] <- 1


earn[, predupli:=shift(dupli,type="lag",n=1),by="personid"]
earn[, predupli2:=shift(dupli,type="lag",n=2),by="personid"]

# Go one by one
# If dupli and predupli=0 and corresponds, eliminate

punto <- 1
while (punto>0) {
  earn$keep <- 1
  earn$keep[earn$dupli==1 & ((earn$predupli==0 & earn$prev==(earn$V2-2)) | (earn$predupli2==0 & earn$prev2==(earn$V2-2)))] <- 0
  punto<- nrow(earn[earn$keep==0])
  earn <- subset(earn,keep==1)
  
  earn[, prev:=shift(V2,type="lag",n=1),by="personid"]
  earn[, prev2:=shift(V2,type="lag",n=2),by="personid"]
  
  earn$dupli <- 0
  earn$dupli[(earn$prev==(earn$V2-2))|(earn$prev2==(earn$V2-2))] <- 1
  
  earn[, predupli:=shift(dupli,type="lag",n=1),by="personid"]
  earn[, predupli2:=shift(dupli,type="lag",n=2),by="personid"]
}


# Good. Now we just need to get the right ones
# Re

earn.new <- merge(earn.all,earn,by=c("personid","year","resy","rec","byear","earncoh","SampleWeight"),all.x=T)
earn.new$valid[is.na(earn.new$valid)==1] <- 0
setkeyv(earn.new,c("personid","year"))
#earn.new <- subset(earn.new,is.na(age)==0)

earn.new[, l1:=shift(resy,type="lead",n=1),by="personid"]
earn.new[, l2:=shift(resy,type="lead",n=2),by="personid"]
earn.new[, l3:=shift(resy,type="lead",n=3),by="personid"]
earn.new[, l4:=shift(resy,type="lead",n=4),by="personid"]

earn.new[, r1:=shift(rec,type="lead",n=1),by="personid"]
earn.new[, r2:=shift(rec,type="lead",n=2),by="personid"]
earn.new[, r3:=shift(rec,type="lead",n=3),by="personid"]
earn.new[, r4:=shift(rec,type="lead",n=4),by="personid"]

earn.new$n1 <- NA
earn.new$n2 <- NA
earn.new$n1[earn.new$a1==(earn.new$V2+2) & is.na(earn.new$a1)==0] <- earn.new$l1[earn.new$a1==(earn.new$V2+2) & is.na(earn.new$a1)==0]
earn.new$n1[earn.new$a2==(earn.new$V2+2) & is.na(earn.new$a2)==0] <- earn.new$l2[earn.new$a2==(earn.new$V2+2) & is.na(earn.new$a2)==0]
earn.new$n1[earn.new$a3==(earn.new$V2+2) & is.na(earn.new$a3)==0] <- earn.new$l3[earn.new$a3==(earn.new$V2+2) & is.na(earn.new$a3)==0]
earn.new$n2[earn.new$a2==(earn.new$V2+4) & is.na(earn.new$a2)==0] <- earn.new$l2[earn.new$a2==(earn.new$V2+4) & is.na(earn.new$a2)==0]
earn.new$n2[earn.new$a3==(earn.new$V2+4) & is.na(earn.new$a3)==0] <- earn.new$l3[earn.new$a3==(earn.new$V2+4) & is.na(earn.new$a3)==0]
earn.new$n2[earn.new$a4==(earn.new$V2+4) & is.na(earn.new$a4)==0] <- earn.new$l4[earn.new$a4==(earn.new$V2+4) & is.na(earn.new$a4)==0]


earn.new$rec2 <- NA
earn.new$rec3 <- NA
earn.new$rec2[earn.new$a1==(earn.new$V2+2) & is.na(earn.new$a1)==0] <- earn.new$r1[earn.new$a1==(earn.new$V2+2) & is.na(earn.new$a1)==0]
earn.new$rec2[earn.new$a2==(earn.new$V2+2) & is.na(earn.new$a2)==0] <- earn.new$r2[earn.new$a2==(earn.new$V2+2) & is.na(earn.new$a2)==0]
earn.new$rec2[earn.new$a3==(earn.new$V2+2) & is.na(earn.new$a3)==0] <- earn.new$r3[earn.new$a3==(earn.new$V2+2) & is.na(earn.new$a3)==0]
earn.new$rec3[earn.new$a2==(earn.new$V2+4) & is.na(earn.new$a2)==0] <- earn.new$r2[earn.new$a2==(earn.new$V2+4) & is.na(earn.new$a2)==0]
earn.new$rec3[earn.new$a3==(earn.new$V2+4) & is.na(earn.new$a3)==0] <- earn.new$r3[earn.new$a3==(earn.new$V2+4) & is.na(earn.new$a3)==0]
earn.new$rec3[earn.new$a4==(earn.new$V2+4) & is.na(earn.new$a4)==0] <- earn.new$r4[earn.new$a4==(earn.new$V2+4) & is.na(earn.new$a4)==0]

earn.new <- subset(earn.new,valid==1)


# And now reorder for abb stuff

earn.new <- subset(earn.new,select=c("personid","age","resy","n1","n2","rec","rec2","rec3","byear","earncoh","SampleWeight"))
earn.new$age2 <- earn.new$age+2
earn.new$age3 <- earn.new$age+4
earn.new <- plyr::rename(earn.new,c("age"="age1","resy"="resy1","n1"="resy2","n2"="resy3","rec"="rec1"))
earn.new$order1 <- 1000000*earn.new$personid+1000*earn.new$age1+1
earn.new$order2 <- 1000000*earn.new$personid+1000*earn.new$age1+2
earn.new$order3 <- 1000000*earn.new$personid+1000*earn.new$age1+3

earn.new2 <- melt(earn.new,id=c("personid","byear","earncoh","SampleWeight"),measure=list(paste0("age",1:3),paste0("resy",1:3),paste0("order",1:3),
                                                                           paste0("rec",1:3)),value.name=c("age","resy","order","rec"))
setkeyv(earn.new2,c("personid","order"))



earn.new2$resy2 <- earn.new2$resy^2
sd2 <- earn.new2[,wtd.mean(resy2,SampleWeight),by=c("age","earncoh")]
sd2 <- subset(sd2,earncoh!=0)
sd2$earncoh <- droplevels(sd2$earncoh)
setkeyv(sd2,c("earncoh","age"))
sdearn.forgraph <- smooth.val.na(sd2,"V1","earncoh",5)
sdearn.forgraph <- subset(sdearn.forgraph,age<61)
sdearn.forgraph <- subset(sdearn.forgraph,!(age<25 & earncoh=="1940"))
sdearn.forgraph <- subset(sdearn.forgraph,!(age>39 & earncoh=="1980"))
# These extreme points are too noisy & selected (only few birth years can be
# observed at those ages for those generations)

g1 <- grafico.varias.lineas3(sdearn.forgraph,"age","smoo","earncoh","Age","SD of the earnings dtb",
                             "FigureC6a.pdf",c(0.4,1),seq(25,60,5),1.2,20)


# SD of earnings CHANGES
# Notice there was an error before in the code (not necessarily computing them on 2 years
# gaps before). Now, consistent w earnings process.

earn.change <- earn.new
earn.change$ng1 <- earn.change$resy2-earn.change$resy1
earn.change$ng2 <- earn.change$resy3-earn.change$resy2
earn.change$year1 <- earn.change$byear+earn.change$age1
earn.change$year2 <- earn.change$byear+earn.change$age2

earn.change <- subset(earn.change,select=c("age1","age2","earncoh","ng1","ng2","year1","year2","SampleWeight"))
earn.change <- melt(earn.change,id.vars=c("earncoh","SampleWeight"),measure=patterns(age="age",ng="ng",year="year"))
earn.change$ng.sd <- earn.change$ng^2
earn.change <- subset(earn.change,earncoh!=0)
earn.change$earncoh <- droplevels(earn.change$earncoh)

var.cohorts <- earn.change[,wtd.mean(ng.sd,SampleWeight),by=c("age","earncoh")]
var.cohorts <- subset(var.cohorts,is.na(earncoh)==0)
var.cohorts <- plyr::rename(var.cohorts,c("V1"="earn.var"))
var.cohorts$earn.sd <- sqrt(var.cohorts$earn.var)
setkeyv(var.cohorts,c("earncoh","age"))
var.cohorts <- smooth.val.na(var.cohorts,"earn.sd","earncoh",5)
var.cohorts <- subset(var.cohorts,!(earncoh=="1980" & age>36)) # note here it's 2y changes
var.cohorts <- subset(var.cohorts,!(earncoh=="1960" & age>56)) # note here it's 2y changes
var.cohorts <- subset(var.cohorts,!(age>58))

gcoh1 <- grafico.varias.lineas3(var.cohorts,"age","smoo","earncoh","Age","SD earnings change",
                                "FigureC6b.pdf",NULL,seq(25,60,5),1.2,20)



# Statistics conditional on employment/unemployment status



folder <- paste0(main.folder,"results/Figures/")
folder.from <- paste0(main.folder,"working/")
load(paste0(folder.from,'PSIDSample.RData'))
source(paste0(main.folder,'code/data_analysis/functions/gon_graphs.R'))
type <- 3

# dam is original, do not edit

dt <- dam

# Define different cohort types

dt$decade <- cut(dt$byear,breaks=seq(1940,1990,10),
                 labels=seq(1940,1980,10))
dt$bigcoh <- cut(dt$byear,breaks=seq(1930,1990,20))
dt$earncoh <- factor(0,levels=c(0,"1940","1960","1980"))
dt$earncoh[dt$byear>1934 & dt$byear<1950] <- "1940"
dt$earncoh[dt$byear>1949 & dt$byear<1971] <- "1960"
dt$earncoh[dt$byear>1970] <- "1980"
dt$alldecade <- cut(dt$byear,breaks=seq(1870,2000,10),labels=seq(1870,1990,10))


dt$WtrUnemployed <- 0
dt$WtrUnemployed[dt$TimeUnemployed>0] <- 1

dt$WtrSpouseUnemployed <- 0 
dt$WtrSpouseUnemployed[dt$SpouseTimeUnemployed>0] <- 1


# Some sample selection requirements are common for both cases
# (but not for some robustness checks)

dt <- subset(dt,age>19 & age<100)
dt <- subset(dt,is.na(age)==0)
dam.weights <- dt # keep the non-SRC sample for robustness checks
dt <- subset(dt,src==1)

# PORTFOLIO
# Only keep over-24s

dp <- subset(dt,age>24)

dp <- subset(dp,age>19 & age<100)
dp <- subset(dp,src==1)


# EARNINGS
# Keep over-19s up to 65s (to have enough observations that have several years
# and start in the late 50s)

de <- subset(dt,age>19 & age<66)
#de <- subset(de,nextrelhead==1 | nextrelhead==10)
# note all observations in sample are for Relhead==1 or Relhead==10, and we construct
# the changes in this file, so this is already guaranteed
de <- subset(de,is.na(EquiScale)==0) # available hh information
de <- subset(de,labtotal>1518) # minimum earnings level




# EARNINGS - With this restricted sample I can now find age-eff profiles

# Detrend & extract average regression factor (will bring that back in model)

regre.trend <- lm(loglabtotal ~ year,data=de)
de$detlabtotal <- residuals(regre.trend)
regre.rec <- lm(detlabtotal ~ factor(rec),data=de)
de$resy <- de$detlabtotal #loglabtotal
de$logy <- de$resy


## NOW SUBSET HERE
# 
if (type==0) {
  fname <- "f_sdshock_coh_nounemp0.pdf"
} else if (type==1) {
  de <- subset(de,WtrUnemployed==0)
  fname <- "f_sdshock_coh_nounemp1.pdf"
} else if (type==2) {
  de <- subset(de,AgeSpouse>0)
  fname <- "f_sdshock_coh_nounemp2.pdf"
} else if (type==3) {
  de <- subset(de,LaborEarningsWife>0 & WtrSpouseUnemployed==0 & WtrUnemployed==0)
  fname <- "FigureC2.pdf"
}


## Earnings transformations for earnings process & save results

extra.vars <- c("WtrUnemployed","WtrSpouseUnemployed","JobTenure","SpouseJobTenure")

de$mgroup <- 0
de$mgroup[de$WtrUnemployed==1] <- 1



earn <- subset(de,select=c("personid","year","resy","age","rec","byear","earncoh","mgroup"))
earn <- subset(earn,is.na(resy)==0)

# Perform AGE reconstructions to make our life easier
# These are not really needed for cross-sectional statistics
# But really help when looking at changes

# First a couple of ad-hoc corrections for clear errors

earn$age[earn$personid==835001 & earn$year==1969] <- 34
earn$byear[earn$personid==835001 & earn$year==1969] <- 1935

min.point <- earn[,list(minpoint=min(year)),by="personid"]
earn <- merge(earn,min.point,by="personid",all.x=T)
earn$minage <- 0
earn$minage[earn$year==earn$minpoint] <- 1

min.age <- subset(earn,minage==1)
# We choose to believe this byear in case of conflict
min.age$byear2 <- min.age$byear
min.age <- subset(min.age,select=c("personid","byear2"))

earn <- merge(earn,min.age,by="personid",all.x=T)
earn$byear <- earn$byear2
earn$byear2 <- NULL
earn$minage <- NULL
earn$minpoint <- NULL
earn$age <- earn$year-earn$byear


earn[,lage:=shift(age,type="lead"),by="personid"]
earn[,lyear:=shift(year,type="lead"),by="personid"]
earn <- subset(earn,(age-lage)==(year-lyear) | is.na(lyear))


earn.all <- subset(earn,select=c("personid","year","resy","rec","byear","earncoh","mgroup"))


earn[, yearl:=shift(year,type="lag"),by="personid"]
earn$yeardif <- earn$year-earn$yearl
earn$yeardif[is.na(earn$yearl)] <- 1

cumsu <- earn[,list(year,cumsum(yeardif)),by=personid]
setkeyv(earn,c("personid","year"))
setkeyv(cumsu,c("personid","year"))
earn <- merge(earn,cumsu)
# V2 is the sum of years SINCE the first observation!! (one=it is the first)

earn[, a1:=shift(V2,type="lead",n=1),by="personid"]
earn[, a2:=shift(V2,type="lead",n=2),by="personid"]
earn[, a3:=shift(V2,type="lead",n=3),by="personid"]
earn[, a4:=shift(V2,type="lead",n=4),by="personid"]

earn$valid <- 0
earn$valid[(earn$a1==(earn$V2+2)|earn$a2==(earn$V2+2)|earn$a3==(earn$V2+2)|earn$a4==(earn$V2+2)) &
             (earn$a1==(earn$V2+4)|earn$a2==(earn$V2+4)|earn$a3==(earn$V2+4)|earn$a4==(earn$V2+4))] <- 1
earn$valid[earn$yeardif>2] <- 0

# Now just need to eliminate observations that have already been taken

earn <- subset(earn,valid==1)
earn[, prev:=shift(V2,type="lag",n=1),by="personid"]
earn[, prev2:=shift(V2,type="lag",n=2),by="personid"]

earn$dupli <- 0
earn$dupli[(earn$prev==(earn$V2-2))|(earn$prev2==(earn$V2-2))] <- 1


earn[, predupli:=shift(dupli,type="lag",n=1),by="personid"]
earn[, predupli2:=shift(dupli,type="lag",n=2),by="personid"]

# Go one by one
# If dupli and predupli=0 and corresponds, eliminate

punto <- 1
while (punto>0) {
  earn$keep <- 1
  earn$keep[earn$dupli==1 & ((earn$predupli==0 & earn$prev==(earn$V2-2)) | (earn$predupli2==0 & earn$prev2==(earn$V2-2)))] <- 0
  punto<- nrow(earn[earn$keep==0])
  earn <- subset(earn,keep==1)
  
  earn[, prev:=shift(V2,type="lag",n=1),by="personid"]
  earn[, prev2:=shift(V2,type="lag",n=2),by="personid"]
  
  earn$dupli <- 0
  earn$dupli[(earn$prev==(earn$V2-2))|(earn$prev2==(earn$V2-2))] <- 1
  
  earn[, predupli:=shift(dupli,type="lag",n=1),by="personid"]
  earn[, predupli2:=shift(dupli,type="lag",n=2),by="personid"]
}


# Good. Now we just need to get the right ones
# Re

earn.new <- merge(earn.all,earn,by=c("personid","year","resy","rec","byear","earncoh","mgroup"),all.x=T)
earn.new$valid[is.na(earn.new$valid)==1] <- 0
setkeyv(earn.new,c("personid","year"))
#earn.new <- subset(earn.new,is.na(age)==0)

earn.new[, l1:=shift(resy,type="lead",n=1),by="personid"]
earn.new[, l2:=shift(resy,type="lead",n=2),by="personid"]
earn.new[, l3:=shift(resy,type="lead",n=3),by="personid"]
earn.new[, l4:=shift(resy,type="lead",n=4),by="personid"]

earn.new[, r1:=shift(rec,type="lead",n=1),by="personid"]
earn.new[, r2:=shift(rec,type="lead",n=2),by="personid"]
earn.new[, r3:=shift(rec,type="lead",n=3),by="personid"]
earn.new[, r4:=shift(rec,type="lead",n=4),by="personid"]

earn.new[, mg1:=shift(mgroup,type="lead",n=1),by="personid"]
earn.new[, mg2:=shift(mgroup,type="lead",n=2),by="personid"]
earn.new[, mg3:=shift(mgroup,type="lead",n=3),by="personid"]
earn.new[, mg4:=shift(mgroup,type="lead",n=4),by="personid"]

t1 <- (earn.new$a1==(earn.new$V2+2) & is.na(earn.new$a1)==0)
t2 <- (earn.new$a2==(earn.new$V2+2) & is.na(earn.new$a2)==0)
t3 <- (earn.new$a3==(earn.new$V2+2) & is.na(earn.new$a3)==0)
t4 <- (earn.new$a2==(earn.new$V2+4) & is.na(earn.new$a2)==0)
t5 <- (earn.new$a3==(earn.new$V2+4) & is.na(earn.new$a3)==0)
t6 <- (earn.new$a4==(earn.new$V2+4) & is.na(earn.new$a4)==0)

earn.new$n1 <- NA
earn.new$n2 <- NA
earn.new$n1[t1] <- earn.new$l1[t1]
earn.new$n1[t2] <- earn.new$l2[t2]
earn.new$n1[t3] <- earn.new$l3[t3]
earn.new$n2[t4] <- earn.new$l2[t4]
earn.new$n2[t5] <- earn.new$l3[t5]
earn.new$n2[t6] <- earn.new$l4[t6]



earn.new$rec2 <- NA
earn.new$rec3 <- NA
earn.new$rec2[t1] <- earn.new$r1[t1]
earn.new$rec2[t2] <- earn.new$r2[t2]
earn.new$rec2[t3] <- earn.new$r3[t3]
earn.new$rec3[t4] <- earn.new$r2[t4]
earn.new$rec3[t5] <- earn.new$r3[t5]
earn.new$rec3[t6] <- earn.new$r4[t6]

earn.new$mgroup2 <- NA
earn.new$mgroup3 <- NA 
earn.new$mgroup2[t1] <- earn.new$mg1[t1]
earn.new$mgroup2[t2] <- earn.new$mg2[t2]
earn.new$mgroup2[t3] <- earn.new$mg3[t3]
earn.new$mgroup3[t4] <- earn.new$mg2[t4]
earn.new$mgroup3[t5] <- earn.new$mg3[t5]
earn.new$mgroup3[t6] <- earn.new$mg4[t6]

earn.new <- subset(earn.new,valid==1)
earn.new <- subset(earn.new,age<61) # But in the end the ones I want are just the 60s

# And now reorder for abb stuff

earn.new <- subset(earn.new,select=c("personid","age","resy","n1","n2","rec","rec2","rec3",
                                     "byear","earncoh","mgroup","mgroup2","mgroup3"))
earn.new$age2 <- earn.new$age+2
earn.new$age3 <- earn.new$age+4
earn.new <- plyr::rename(earn.new,c("age"="age1","resy"="resy1","n1"="resy2","n2"="resy3","rec"="rec1",
                              "mgroup"="mgroup1"))
earn.new$order1 <- 1000000*earn.new$personid+1000*earn.new$age1+1
earn.new$order2 <- 1000000*earn.new$personid+1000*earn.new$age1+2
earn.new$order3 <- 1000000*earn.new$personid+1000*earn.new$age1+3

# Some age adjustments generate byears off the intial cohort, so re-subset here
earn.new <- subset(earn.new,!(earncoh=="1940" & (byear<1935 | byear>1949)))
earn.new <- subset(earn.new,!(earncoh=="1960" & (byear<1951 | byear>1970)))
earn.new <- subset(earn.new,!(earncoh=="1980" & byear<1971))

### PRODUCE GRAPHS



# SD of earnings CHANGES

earn.change <- earn.new
earn.change$ng1 <- earn.change$resy2-earn.change$resy1
earn.change$ng2 <- earn.change$resy3-earn.change$resy2
earn.change$year1 <- earn.change$byear+earn.change$age1
earn.change$year2 <- earn.change$byear+earn.change$age2

# Here I need to redefine groups

earn.change$bgroup1 <- 0
earn.change$bgroup1[earn.change$mgroup1==0 & earn.change$mgroup2==0] <- 1
earn.change$bgroup1[(earn.change$mgroup1==1 & earn.change$mgroup2==0) |
                      (earn.change$mgroup1==0 & earn.change$mgroup2==1) |
                      (earn.change$mgroup1==1 & earn.change$mgroup2==1)] <- 2


earn.change$bgroup2 <- 0
earn.change$bgroup2[earn.change$mgroup2==0 & earn.change$mgroup3==0] <- 1
earn.change$bgroup2[(earn.change$mgroup2==1 & earn.change$mgroup3==0) |
                      (earn.change$mgroup2==0 & earn.change$mgroup3==1) |
                      (earn.change$mgroup2==1 & earn.change$mgroup3==1)] <- 2

earn.change <- subset(earn.change,select=c("age1","age2","earncoh","ng1","ng2","year1","year2",
                                           "bgroup1","bgroup2"))
earn.change <- melt(earn.change,id.vars="earncoh",measure=patterns(age="age",ng="ng",year="year",bgroup="bgroup"))
earn.change$ng.sd <- earn.change$ng^2
earn.change <- subset(earn.change,earncoh!=0)
earn.change$earncoh <- droplevels(earn.change$earncoh)

earn.change$uno <- 1
tots.age.coh <- earn.change[,list(cohtot=sum(uno)),by=c("age","earncoh")]
earn.change <- merge(earn.change,tots.age.coh,by=c("age","earncoh"))

var.cohorts.group <- earn.change[,list(sum(uno/cohtot),mean(ng.sd)),
                                 by=c("age","earncoh","bgroup")]
var.cohorts.group <- subset(var.cohorts.group,age==30 & earncoh!="1960")
var.cohorts.group[,nextval:=shift(V2,1,type="lead"),by="bgroup"]
var.cohorts.group[,prevprob:=shift(V1,1,type="lag"),by="bgroup"]
var.cohorts.group$growthc <- var.cohorts.group$V2*(var.cohorts.group$V1-var.cohorts.group$prevprob)
var.cohorts.group$probc <- var.cohorts.group$V1*(var.cohorts.group$nextval-var.cohorts.group$V2)

var.cohorts <- earn.change[,mean(ng.sd),by=c("age","earncoh")]
var.cohorts <- subset(var.cohorts,is.na(earncoh)==0)
var.cohorts <- plyr::rename(var.cohorts,c("V1"="earn.var"))
var.cohorts$earn.sd <- sqrt(var.cohorts$earn.var)
setkeyv(var.cohorts,c("earncoh","age"))
var.cohorts <- smooth.val.na(var.cohorts,"earn.sd","earncoh",5)
var.cohorts <- subset(var.cohorts,!(earncoh=="1980" & age>36)) # note here it's 2y changes
var.cohorts <- subset(var.cohorts,!(earncoh=="1960" & age>56)) # note here it's 2y changes
var.cohorts <- subset(var.cohorts,!(age>58))

gcoh1a <- grafico.varias.lineas3(var.cohorts,"age","smoo","earncoh","Age","SD earnings change",
                                 fname,c(0,0.7),seq(25,60,5),1.2,20)


#THIS CODE

# Saves the earnings process for the MARRIED only
# Separated from psid_outputs for cleanliness, but reproduces what's done in there

folder <- paste0(main.folder,"results/Figures/")
folder.from <- paste0(main.folder,"working/")
load(paste0(folder.from,'PSIDSample.RData'))
source(paste0(main.folder,'code/data_analysis/functions/gon_graphs.R'))
married <- 1
folder.export <- paste0(main.folder,"working/ABB/")


# dam is original, do not edit

dt <- dam
dt <- subset(dt,AgeSpouse>0)

# Define different cohort types

dt$decade <- cut(dt$byear,breaks=seq(1940,1990,10),
                 labels=seq(1940,1980,10))
dt$bigcoh <- cut(dt$byear,breaks=seq(1930,1990,20))
dt$earncoh <- factor(0,levels=c(0,"1940","1960","1980"))
dt$earncoh[dt$byear>1934 & dt$byear<1950] <- "1940"
dt$earncoh[dt$byear>1949 & dt$byear<1971] <- "1960"
dt$earncoh[dt$byear>1970] <- "1980"
dt$alldecade <- cut(dt$byear,breaks=seq(1870,2000,10),labels=seq(1870,1990,10))


dt$WtrUnemployed <- 0
dt$WtrUnemployed[dt$TimeUnemployed>0] <- 1


# Some sample selection requirements are common for both cases
# (but not for some robustness checks)

dt <- subset(dt,age>19 & age<100)
dt <- subset(dt,is.na(age)==0)
dt <- subset(dt,src==1)


# EARNINGS
# Keep over-19s up to 65s (to have enough observations that have several years
# and start in the late 50s)

de <- subset(dt,age>19 & age<66)
de <- subset(de,is.na(EquiScale)==0) # available hh information
de <- subset(de,labtotal>1518) # minimum earnings level


# EARNINGS - With this restricted sample I can now find age-eff profiles

# Detrend & extract average regression factor (will bring that back in model)

regre.trend <- lm(loglabtotal ~ year,data=de)
de$detlabtotal <- residuals(regre.trend)
regre.rec <- lm(detlabtotal ~ factor(rec),data=de)
de$resy <- de$detlabtotal #loglabtotal
de$logy <- de$resy


## Earnings transformations for earnings process & save results

earn <- subset(de,select=c("personid","year","resy","age","rec","byear","earncoh"))
earn <- subset(earn,is.na(resy)==0)

# Perform AGE reconstructions to make our life easier
# These are not really needed for cross-sectional statistics
# But really help when looking at changes

# First a couple of ad-hoc corrections for clear errors

earn$age[earn$personid==835001 & earn$year==1969] <- 34
earn$byear[earn$personid==835001 & earn$year==1969] <- 1935

min.point <- earn[,list(minpoint=min(year)),by="personid"]
earn <- merge(earn,min.point,by="personid",all.x=T)
earn$minage <- 0
earn$minage[earn$year==earn$minpoint] <- 1

min.age <- subset(earn,minage==1)
# We choose to believe this byear in case of conflict
min.age$byear2 <- min.age$byear
min.age <- subset(min.age,select=c("personid","byear2"))

earn <- merge(earn,min.age,by="personid",all.x=T)
earn$byear <- earn$byear2
earn$byear2 <- NULL
earn$minage <- NULL
earn$minpoint <- NULL
earn$age <- earn$year-earn$byear


earn[,lage:=shift(age,type="lead"),by="personid"]
earn[,lyear:=shift(year,type="lead"),by="personid"]
earn <- subset(earn,(age-lage)==(year-lyear) | is.na(lyear))


earn.all <- subset(earn,select=c("personid","year","resy","rec","byear","earncoh"))


earn[, yearl:=shift(year,type="lag"),by="personid"]
earn$yeardif <- earn$year-earn$yearl
earn$yeardif[is.na(earn$yearl)] <- 1

cumsu <- earn[,list(year,cumsum(yeardif)),by=personid]
setkeyv(earn,c("personid","year"))
setkeyv(cumsu,c("personid","year"))
earn <- merge(earn,cumsu)
# V2 is the sum of years SINCE the first observation!! (one=it is the first)

earn[, a1:=shift(V2,type="lead",n=1),by="personid"]
earn[, a2:=shift(V2,type="lead",n=2),by="personid"]
earn[, a3:=shift(V2,type="lead",n=3),by="personid"]
earn[, a4:=shift(V2,type="lead",n=4),by="personid"]

earn$valid <- 0
earn$valid[(earn$a1==(earn$V2+2)|earn$a2==(earn$V2+2)|earn$a3==(earn$V2+2)|earn$a4==(earn$V2+2)) &
             (earn$a1==(earn$V2+4)|earn$a2==(earn$V2+4)|earn$a3==(earn$V2+4)|earn$a4==(earn$V2+4))] <- 1
earn$valid[earn$yeardif>2] <- 0

# Now just need to eliminate observations that have already been taken

earn <- subset(earn,valid==1)
earn[, prev:=shift(V2,type="lag",n=1),by="personid"]
earn[, prev2:=shift(V2,type="lag",n=2),by="personid"]

earn$dupli <- 0
earn$dupli[(earn$prev==(earn$V2-2))|(earn$prev2==(earn$V2-2))] <- 1


earn[, predupli:=shift(dupli,type="lag",n=1),by="personid"]
earn[, predupli2:=shift(dupli,type="lag",n=2),by="personid"]

# Go one by one
# If dupli and predupli=0 and corresponds, eliminate

punto <- 1
while (punto>0) {
  earn$keep <- 1
  earn$keep[earn$dupli==1 & ((earn$predupli==0 & earn$prev==(earn$V2-2)) | (earn$predupli2==0 & earn$prev2==(earn$V2-2)))] <- 0
  punto<- nrow(earn[earn$keep==0])
  earn <- subset(earn,keep==1)
  
  earn[, prev:=shift(V2,type="lag",n=1),by="personid"]
  earn[, prev2:=shift(V2,type="lag",n=2),by="personid"]
  
  earn$dupli <- 0
  earn$dupli[(earn$prev==(earn$V2-2))|(earn$prev2==(earn$V2-2))] <- 1
  
  earn[, predupli:=shift(dupli,type="lag",n=1),by="personid"]
  earn[, predupli2:=shift(dupli,type="lag",n=2),by="personid"]
}


# Good. Now we just need to get the right ones
# Re

earn.new <- merge(earn.all,earn,by=c("personid","year","resy","rec","byear","earncoh"),all.x=T)
earn.new$valid[is.na(earn.new$valid)==1] <- 0
setkeyv(earn.new,c("personid","year"))
#earn.new <- subset(earn.new,is.na(age)==0)

earn.new[, l1:=shift(resy,type="lead",n=1),by="personid"]
earn.new[, l2:=shift(resy,type="lead",n=2),by="personid"]
earn.new[, l3:=shift(resy,type="lead",n=3),by="personid"]
earn.new[, l4:=shift(resy,type="lead",n=4),by="personid"]

earn.new[, r1:=shift(rec,type="lead",n=1),by="personid"]
earn.new[, r2:=shift(rec,type="lead",n=2),by="personid"]
earn.new[, r3:=shift(rec,type="lead",n=3),by="personid"]
earn.new[, r4:=shift(rec,type="lead",n=4),by="personid"]

earn.new$n1 <- NA
earn.new$n2 <- NA
earn.new$n1[earn.new$a1==(earn.new$V2+2) & is.na(earn.new$a1)==0] <- earn.new$l1[earn.new$a1==(earn.new$V2+2) & is.na(earn.new$a1)==0]
earn.new$n1[earn.new$a2==(earn.new$V2+2) & is.na(earn.new$a2)==0] <- earn.new$l2[earn.new$a2==(earn.new$V2+2) & is.na(earn.new$a2)==0]
earn.new$n1[earn.new$a3==(earn.new$V2+2) & is.na(earn.new$a3)==0] <- earn.new$l3[earn.new$a3==(earn.new$V2+2) & is.na(earn.new$a3)==0]
earn.new$n2[earn.new$a2==(earn.new$V2+4) & is.na(earn.new$a2)==0] <- earn.new$l2[earn.new$a2==(earn.new$V2+4) & is.na(earn.new$a2)==0]
earn.new$n2[earn.new$a3==(earn.new$V2+4) & is.na(earn.new$a3)==0] <- earn.new$l3[earn.new$a3==(earn.new$V2+4) & is.na(earn.new$a3)==0]
earn.new$n2[earn.new$a4==(earn.new$V2+4) & is.na(earn.new$a4)==0] <- earn.new$l4[earn.new$a4==(earn.new$V2+4) & is.na(earn.new$a4)==0]


earn.new$rec2 <- NA
earn.new$rec3 <- NA
earn.new$rec2[earn.new$a1==(earn.new$V2+2) & is.na(earn.new$a1)==0] <- earn.new$r1[earn.new$a1==(earn.new$V2+2) & is.na(earn.new$a1)==0]
earn.new$rec2[earn.new$a2==(earn.new$V2+2) & is.na(earn.new$a2)==0] <- earn.new$r2[earn.new$a2==(earn.new$V2+2) & is.na(earn.new$a2)==0]
earn.new$rec2[earn.new$a3==(earn.new$V2+2) & is.na(earn.new$a3)==0] <- earn.new$r3[earn.new$a3==(earn.new$V2+2) & is.na(earn.new$a3)==0]
earn.new$rec3[earn.new$a2==(earn.new$V2+4) & is.na(earn.new$a2)==0] <- earn.new$r2[earn.new$a2==(earn.new$V2+4) & is.na(earn.new$a2)==0]
earn.new$rec3[earn.new$a3==(earn.new$V2+4) & is.na(earn.new$a3)==0] <- earn.new$r3[earn.new$a3==(earn.new$V2+4) & is.na(earn.new$a3)==0]
earn.new$rec3[earn.new$a4==(earn.new$V2+4) & is.na(earn.new$a4)==0] <- earn.new$r4[earn.new$a4==(earn.new$V2+4) & is.na(earn.new$a4)==0]

earn.new <- subset(earn.new,valid==1)
earn.new <- subset(earn.new,age<61) # But in the end the ones I want are just the 60s

# And now reorder for abb

earn.new <- subset(earn.new,select=c("personid","age","resy","n1","n2","rec","rec2","rec3","byear","earncoh"))
earn.new$age2 <- earn.new$age+2
earn.new$age3 <- earn.new$age+4
earn.new <- dplyr::rename(earn.new,c("age1"="age","resy1"="resy","resy2"="n1","resy3"="n2","rec1"="rec"))
earn.new$order1 <- 1000000*earn.new$personid+1000*earn.new$age1+1
earn.new$order2 <- 1000000*earn.new$personid+1000*earn.new$age1+2
earn.new$order3 <- 1000000*earn.new$personid+1000*earn.new$age1+3

# Some age adjustments generate byears off the intial cohort, so re-subset here
earn.new <- subset(earn.new,!(earncoh=="1940" & (byear<1935 | byear>1949)))
earn.new <- subset(earn.new,!(earncoh=="1960" & (byear<1951 | byear>1970)))
earn.new <- subset(earn.new,!(earncoh=="1980" & byear<1971))

earn.new2 <- melt(earn.new,id=c("personid","byear","earncoh"),measure=list(paste0("age",1:3),paste0("resy",1:3),paste0("order",1:3),
                                                                           paste0("rec",1:3)),value.name=c("age","resy","order","rec"))
setkeyv(earn.new2,c("personid","order"))

abb.export.40 <- subset(earn.new2,earncoh=="1940")
abb.export.40 <- subset(abb.export.40,select=c("resy","age","rec"))

abb.export.60 <- subset(earn.new2,earncoh=="1960")
abb.export.60 <- subset(abb.export.60,select=c("resy","age","rec"))

abb.export.80 <- subset(earn.new2,earncoh=="1980")
abb.export.80 <- subset(abb.export.80,select=c("resy","age","rec"))


write.table(abb.export.40,paste0(folder.export,"psid_2y_1940_married_rev.txt"), sep="\t",row.names=F,col.names=F)
write.table(abb.export.60,paste0(folder.export,"psid_2y_1960_married_rev.txt"), sep="\t",row.names=F,col.names=F)
write.table(abb.export.80,paste0(folder.export,"psid_2y_1980_married_rev.txt"), sep="\t",row.names=F,col.names=F)

