#THIS CODE

# Generates all of the data objects we need from the PSID
# This includes
#   - The earnings process (previously in abbpsid_cohort_2y)
#   - Figure 1 in the main text
#   - Data counterparts of Figures 4, 5
#   - Figure 7 in main text
#   - Figures in Appendix C (Fig C.1. to C.13.)
#   - Data counterparts in Appendix D (D.1., D.2.)
#   - Figure F.4.

#   It also generates:
#   - Model counterpart of Figure 5
#   - Some inputs for model:
#     - Age efficiencies and cohort means
#   - Variances and autocovariances for canonical process
#   - Modified tax parameter as in A.1.2.

# Notice that the samples differ somewhat depending on the case (as described in App A).
# In particular, for the estimation of the income process and for all graphs associated
# with income:
#   - Cohorts are broader (e.g. 1930-1950 rather than 1940-1950)
#   - Minimum earnings requirement is imposed
#   - Two consecutive years available
# These are not imposed for the portfolio measures (e.g. homeownership) to have a broader
# sample.

folder <- paste0(main.folder,"results/Figures/")
folder.values <- paste0(main.folder,"results/Values/")
folder.from <- paste0(main.folder,"working/")
folder.earnings.process <- paste0(main.folder,"working/earnings/")
folder.export <- paste0(main.folder,"working/ABB/")
folder.model <- paste0(main.folder,"working/Model/")
folder.data <- paste0(main.folder,"data/")
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
dt$alldecade <- cut(dt$byear,breaks=seq(1870,2000,10),labels=seq(1870,1990,10))


dt$WtrUnemployed <- 0
dt$WtrUnemployed[dt$TimeUnemployed>0] <- 1


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

# And now reorder for abb stuff

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


write.table(abb.export.40,paste0(folder.export,"psid_2y_1940_rev.txt"), sep="\t",row.names=F,col.names=F)
write.table(abb.export.60,paste0(folder.export,"psid_2y_1960_rev.txt"), sep="\t",row.names=F,col.names=F)
write.table(abb.export.80,paste0(folder.export,"psid_2y_1980_rev.txt"), sep="\t",row.names=F,col.names=F)


### GRAPHS IN MAIN PAPER

### Figure 1

# MEDIAN EARNINGS

med1 <- de[,median(loglabtotal),by=c("age","earncoh")]
med1 <- subset(med1,is.na(V1)==0 & earncoh!=0)
med1$earncoh <- droplevels(med1$earncoh)
setkeyv(med1,c("earncoh","age"))
med1$V1 <- exp(med1$V1)/1000
med2 <- smooth.val3(med1,"V1","earncoh","age",5)
med2 <- subset(med2,!(earncoh=="1980" & age>36))
med2 <- subset(med2,!(earncoh=="1960" & age>56))

gcoh6 <- grafico.varias.lineas3(med2,"age","smoo","earncoh","Age","Median hh earnings ('000)",
                                "Figure1a.pdf",NULL,seq(25,60,5),1.2,20)

# AVERAGE EARNINGS

avg1 <- de[,mean(labtotal,na.rm=T),by=c("age","earncoh")]
avg1 <- subset(avg1,is.na(V1)==0 & earncoh!=0)
avg1$earncoh <- droplevels(avg1$earncoh)
setkeyv(avg1,c("earncoh","age"))
avg1$V1 <- avg1$V1/1000
avg2 <- smooth.val3(avg1,"V1","earncoh","age",5)
avg2 <- subset(avg2,!(earncoh=="1980" & age>36))
avg2 <- subset(avg2,!(earncoh=="1960" & age>56))

gcoh6 <- grafico.varias.lineas3(avg2,"age","smoo","earncoh","Age","Average hh earnings ('000)",
                                "Figure1b.pdf",NULL,seq(25,60,5),1.2,20)



# CROSS-SECTIONAL STANDARD DEVIATION OF EARNINGS
# Use earn.new2 to get closest possible to the input to the model
# (both canonical and NL)

sd2 <- earn.new2[,sd(resy),by=c("age","earncoh")]
sd2 <- subset(sd2,earncoh!=0)
sd2$earncoh <- droplevels(sd2$earncoh)
setkeyv(sd2,c("earncoh","age"))
sdearn.forgraph <- smooth.val.na(sd2,"V1","earncoh",5)
sdearn.forgraph <- subset(sdearn.forgraph,age<61)
sdearn.forgraph <- subset(sdearn.forgraph,!(age<25 & earncoh=="1940"))
sdearn.forgraph <- subset(sdearn.forgraph,!(age>36 & earncoh=="1980"))
sdearn.forgraph <- subset(sdearn.forgraph,!(age>56 & earncoh=="1960"))

g1 <- grafico.varias.lineas3(sdearn.forgraph,"age","smoo","earncoh","Age","SD of the earnings dtb",
                             "Figure1c.pdf",NULL,seq(25,60,5),1.2,20)

sdearn.fortable <- dcast(subset(sdearn.forgraph,select=c("age","earncoh","smoo")),
                         age~earncoh)
sdearn.fortable$`1940`[is.na(sdearn.fortable$`1940`)] <- 0
sdearn.fortable$`1960`[is.na(sdearn.fortable$`1960`)] <- 0
sdearn.fortable$`1980`[is.na(sdearn.fortable$`1980`)] <- 0

write.table(sdearn.fortable,paste0(folder.from,"f_sdage_coh6.csv"),row.names=F,col.names=F,sep=",")


# SD of earnings CHANGES
# Notice there was an error before in the code (not necessarily computing them on 2 years
# gaps before). Now, consistent w earnings process.

earn.change <- earn.new
earn.change$ng1 <- earn.change$resy2-earn.change$resy1
earn.change$ng2 <- earn.change$resy3-earn.change$resy2
earn.change$year1 <- earn.change$byear+earn.change$age1
earn.change$year2 <- earn.change$byear+earn.change$age2

earn.change <- subset(earn.change,select=c("age1","age2","earncoh","ng1","ng2","year1","year2"))
earn.change <- melt(earn.change,id.vars="earncoh",measure=patterns(age="age",ng="ng",year="year"))
earn.change$ng.sd <- earn.change$ng^2
earn.change <- subset(earn.change,earncoh!=0)
earn.change$earncoh <- droplevels(earn.change$earncoh)

var.cohorts <- earn.change[,mean(ng.sd),by=c("age","earncoh")]
var.cohorts <- subset(var.cohorts,is.na(earncoh)==0)
var.cohorts <- dplyr::rename(var.cohorts,c("earn.var"="V1"))
var.cohorts$earn.sd <- sqrt(var.cohorts$earn.var)
setkeyv(var.cohorts,c("earncoh","age"))
var.cohorts <- smooth.val.na(var.cohorts,"earn.sd","earncoh",5)
var.cohorts <- subset(var.cohorts,!(earncoh=="1980" & age>36)) # note here it's 2y changes
var.cohorts <- subset(var.cohorts,!(age>58))
var.cohorts <- subset(var.cohorts,!(earncoh=="1960" & age>56)) # note here it's 2y changes

gcoh1a <- grafico.varias.lineas3(var.cohorts,"age","smoo","earncoh","Age","SD earnings change",
                                "Figure1d.pdf",NULL,seq(25,60,5),1.2,20)

# Homeownership over the generations
# Also data counterpart for Figure 6

dp$homeown <- 0
dp$homeown[dp$Houseval>1000] <- 1
homeown.coh <- dp[,mean(homeown),by=c("age","decade")]
homeown.coh <- subset(homeown.coh,is.na(decade)==0)
setkeyv(homeown.coh,"decade")
names(homeown.coh$decade) <- seq(1940,1980,10)

homeown.coh <- smooth.val3(homeown.coh,"V1","decade","age",3)
homeown.coh <- subset(homeown.coh,decade %in% c(1940,1960,1980))
g3.homeown.coh <- grafico.varias.lineas3(homeown.coh,"age","smoo","decade","age","Proportion of homeowners",
                                         "Figure1e.pdf",c(0.0,1.0),seq(25,60,5),1.2,20)

homeown.coh.dcast <- dcast(homeown.coh,age~decade,fill=0)
write.table(homeown.coh.dcast,file=paste0(folder.model,"homeown_coh3_data.csv"),row.names=F,col.names=F,sep=",")

# This also produces the in-text numbers in the introduction

homeown.coh$smoo[homeown.coh$decade=="1940" & homeown.coh$age==35]
homeown.coh$smoo[homeown.coh$decade=="1960" & homeown.coh$age==35]
homeown.coh$smoo[homeown.coh$decade=="1980" & homeown.coh$age==35]

## Data counterparts for Figure 4

# Homebuyers PER age
# These are movers PER YEAR (even in biennial PSID)
# (actually there are some timing issues because it asks since spring, but closer to
# annual)

setkeyv(dp,c("personid","year"))
dp[, lastval:=shift(Houseval,type="lag"),by=personid]

dam.hb <- subset(dp,decade==1940)
dam.hb <- dam.hb[,list(bought=length(personid[Houseval>0 & Moved==1 & Houseval>lastval &
                                         (YearLastMoved>year-2 | is.na(YearLastMoved))]),
                    tot=length(personid)),by="age"]
dam.hb$prop <- dam.hb$bought/dam.hb$tot

h7 <- grafico.linea(dam.hb,"age","prop","age","proportion","hbuyers_age.pdf")
setkeyv(dam.hb,"age")
write.table(dam.hb,file=paste0(folder.model,"hbuyers_age.csv"),row.names=F,sep=",")

# Mortgage holders over the LC

dp$indebt <- 0
dp$indebt[dp$Mortrem>0] <- 1
dp$indebt[is.na(dp$Mortrem)] <- NA

debt.lc <- subset(dp,decade==1940)
debt.lc <- debt.lc[,mean(indebt,na.rm=T),by="age"]
setkeyv(debt.lc,"age")

write.table(debt.lc,file=paste0(folder.model,"withmortgage_age.csv"),row.names=F,sep=",")


## Figure 5, LHS


portf.quantiles <- function(x,nx) {
  y1 <- x
  y1$dc <- xtile(y1$Wealth,n=nx)
    y2 <- y1[,list(house=mean((Houseval+OtherRE)/Assets,na.rm=T),
                   liquid=mean((Liquid+Vehicles)/Assets,na.rm=T),
                   risky=mean((Risky+Business)/Assets,na.rm=T)),by="dc"]
    y2 <- melt(y2,id="dc")
    y2 <- subset(y2,is.na(dc)==0)
    
    y2$variable <- plyr::mapvalues(y2$variable,from=c("house","risky","liquid"),
                             to=c("Housing","Risky","Liquid"))
    y3 <- ggplot(data=y2,aes(dc,value,fill=variable))
    y3 <- y3 + layer(geom="bar",stat="identity",position="stack",data=
                       y2) + xlab("Wealth decile") + ylab("% of total assets") +
      scale_fill_discrete(guide=guide_legend(title="Assets")) + theme_light() +
      scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0))    +
      theme(legend.text=element_text(size=20), text=element_text(size=20))
  return(y3)
}



pf.65b <- portf.quantiles(subset(subset(dp,decade==1940),63<age & age<66 & Business>=0),10)
ggsave(filename=paste(folder,"Figure5a.pdf",sep=""),plot=pf.65b,width=8,height=6)

# And model equivalent (right hand side of figure)

if (file.exists(paste0(folder,'../portfolioret_table_ini.csv'))) {

model.portf <- read.csv(file=paste0(folder,'../portfolioret_table_ini.csv'),header=F)
names(model.portf) <- c("Housing","Liquid","Risky")
model.portf$dc <- seq(1,10)
model.portf <- melt(model.portf,id="dc")

y3 <- ggplot(data=model.portf,aes(dc,value,fill=variable))
y3 <- y3 + layer(geom="bar",stat="identity",position="stack",data=
                   model.portf) + xlab("Wealth decile") + ylab("% of total assets") +
  scale_fill_discrete(guide=guide_legend(title="Assets")) + theme_light() +
  scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0))    +
  theme(legend.text=element_text(size=20), text=element_text(size=20))
ggsave(filename=paste(folder,"Figure5b.pdf",sep=""),
       plot=y3,width=8,height=6)

}

# Figure 7 - homeownership by percentile of the income dtb 

# Homeownership by cohort and earnings percentile
# So keep earnings sample

setkeyv(de,c("age","earncoh","personid"))
eje <- de[,xtile(labtotal,probs=seq(0,1,0.05)),by=c("age","earncoh")]
de$pc <- eje$V1
de$pc <- plyr::mapvalues(de$pc,from=seq(1,21),to=seq(0,100,by=5))

de$homeown <- 0
de$homeown[de$Houseval>1000] <- 1
homeo.coh.earn <- subset(de,age>33 & age<37)
homeo.coh.earn <- homeo.coh.earn[,list(V1=mean(homeown),V2=length(homeown)),by=c("earncoh","pc")]
homeo.coh.earn <- subset(homeo.coh.earn,earncoh!=0)
homeo.coh.earn <- subset(homeo.coh.earn,pc>0)
homeo.coh.earn$earncoh <- droplevels(homeo.coh.earn$earncoh)

fg35 <- homeo.coh.earn
setkeyv(fg35,c("earncoh","pc"))
fg35 <- smooth.val3(fg35,"V1","earncoh","pc",5)

g35 <- grafico.varias.lineas3(fg35,"pc","smoo","earncoh","Earnings percentile at age 35","Homeownership","Figure7.pdf",NULL,NULL,1.2,20)


### DATA NUMBERS IN TABLE 1

# Homeownership, plain

homeo.age40 <- homeown.coh$V1[homeown.coh$age==40 & homeown.coh$decade=="1940"]
write.table(c(format(100*homeo.age40,digits=2)),file=paste0(folder.values,"mean_houseo_40_data.tex"),row.names=F,col.names=F,quote=F)


# Homeownership, by house type (to approximate the two housing Qs)

dp$homeown.type <- 0
dp$homeown.type[dp$Houseval>0 & is.na(dp$Houseval)==0 & (dp$TypeHouse!=1)] <- 1
dp$homeown.type[dp$Houseval>0 & is.na(dp$Houseval)==0 & dp$TypeHouse==1] <- 2
dp$homeown.type[dp$Houseval>0 & is.na(dp$Houseval)==0 & dp$TypeHouse==1 &
                  dp$Nrooms<5] <- 1 # lump small houses with type 1 housing
dp$homeown.type[is.na(dp$TypeHouse)] <- NA # this var. is missing in some years

dp$renter <- 0
dp$renter[dp$homeown.type==0] <- 1
dp$flatowner <- 0
dp$flatowner[dp$homeown.type==1] <- 1
dp$houseowner <- 0
dp$houseowner[dp$homeown.type==2] <- 1
dp$typehouse.available <- 1
dp$typehouse.available[is.na(dp$TypeHouse)] <- 0

house.types.lc <- subset(dp,decade=="1940")
house.types.lc <- house.types.lc[,as.list(c(rent=mean(renter)/mean(typehouse.available),
                                            flat=mean(flatowner)/mean(typehouse.available),
                                            house=mean(houseowner)/mean(typehouse.available))),by="age"]


write.table(format(100*house.types.lc$house[house.types.lc$age==40],digits=2),file=paste0(folder.values,"mean_high_houseo_40_data.tex"),row.names=F,col.names=F,quote=F)
write.table(c(format(100*house.types.lc$flat[house.types.lc$age==40],digits=2)),file=paste0(folder.values,"mean_low_houseo_40_data.tex"),row.names=F,col.names=F,quote=F)


# Wealth to income ratios
# Note: measure naturally very sensitive to (higher) outliers etc.
# Here, try to keep cross-sectional representativity by picking a year
# (i.e., wealth info not very available before 1984, so when picking
# the 1940s cohort I would oversample older people)

dp.wealth <- subset(dp,is.na(Wealth)==0 & year==1989)
wquantiles <- quantile(dp.wealth$Wealth,probs=seq(0,1,0.01))
dp.wealth$pc <- .bincode(dp.wealth$Wealth,wquantiles)
dp.wealth <- subset(dp.wealth,pc<97)

wtoi.ratio <- mean(dp.wealth$Wealth,na.rm=T)/mean(dp.wealth$labtotal,na.rm=T)
write.table(format(wtoi.ratio,digits=2),file=paste0(folder.values,"avgwy_data.tex"),row.names=F,col.names=F,quote=F)


# Percentage moving at 40

movers.40 <- dam.hb$prop[dam.hb$age==40] 
write.table(c(format(100*movers.40,digits=2)),file=paste0(folder.values,"propbuying40_data.tex"),row.names=F,col.names=F,quote=F)



## FIGURES IN APPENDIX C, IN ORDER

# Figure C.1.: means and medians of MALE earnings

# med_coh and avg_coh and sdage_coh6 are already above (in main text)
# compute on de (earnings sample)

med3 <- de[,median(LaborEarningsHead),by=c("age","earncoh")]
med3 <- subset(med3,is.na(V1)==0 & earncoh!=0)
med3$earncoh <- droplevels(med3$earncoh)
setkeyv(med3,c("earncoh","age"))
med3$V1 <- med3$V1/1000
med3 <- smooth.val3(med3,"V1","earncoh","age",5)
med3 <- subset(med3,!(earncoh=="1980" & age>36))
med3 <- subset(med3,!(earncoh=="1960" & age>56))

gcoh7 <- grafico.varias.lineas3(med3,"age","smoo","earncoh","age","Median head earnings ('000)",
                                "FigureC1a.pdf",NULL,NULL,1.2,20)

# Average


avg3 <- de[,mean(LaborEarningsHead),by=c("age","earncoh")]
avg3 <- subset(avg3,is.na(V1)==0 & earncoh!=0)
avg3$earncoh <- droplevels(avg3$earncoh)
avg3$V1 <- avg3$V1/1000
setkeyv(avg3,c("earncoh","age"))
avg3 <- smooth.val3(avg3,"V1","earncoh","age",5)
avg3 <- subset(avg3,!(earncoh=="1980" & age>36))
avg3 <- subset(avg3,!(earncoh=="1960" & age>56))

gcoh7 <- grafico.varias.lineas3(avg3,"age","smoo","earncoh","age","Average head earnings ('000)",
                                "FigureC1b.pdf",NULL,NULL,1.2,20)


de$loghead <- log(de$LaborEarningsHead)
de$loghead[is.infinite(de$loghead)] <- NA
sd3 <- de[,sd(loghead,na.rm=T),by=c("age","earncoh")]
sd3 <- subset(sd3,is.na(V1)==0 & earncoh!=0)
sd3$earncoh <- droplevels(sd3$earncoh)
setkeyv(sd3,c("earncoh","age"))
sd3$V1[sd3$age>36 & sd3$earncoh==1980] <- NA
sd3$V1[sd3$age>56 & sd3$earncoh==1960] <- NA
sd3$V1[sd3$age<26 & sd3$earncoh==1940] <- NA
sd3 <- smooth.val3(sd3,"V1","earncoh","age",5)

gcoh7 <- grafico.varias.lineas3(sd3,"age","smoo","earncoh","age","SD of the earnings dtb",
                                "FigureC1c.pdf",NULL,NULL,1.2,20)

# Figure C.2. 
# is in psid_outputs_more.R


## FIGURE C.3., LEFT 
wtr.unemp.age <- de[,mean(WtrUnemployed),by=c("age","earncoh")]
wtr.unemp.age <- subset(wtr.unemp.age,earncoh!=0)
setkeyv(wtr.unemp.age,c("earncoh","age"))
wtr.unemp.age <- smooth.val.na(wtr.unemp.age,"V1","earncoh",5)
wtr.unemp.age$smoo[wtr.unemp.age$earncoh=="1960" & wtr.unemp.age$age>56] <- NA
wtr.unemp.age$smoo[wtr.unemp.age$earncoh=="1980" & wtr.unemp.age$age>36] <- NA


g1 <- grafico.varias.lineas(wtr.unemp.age,"age","smoo","earncoh",
                            "Age","Whether unemployed","FigureC3a.pdf")

## FIGURE C.3., RIGHT

de.unemp <- subset(de,WtrUnemployed==1)
average.unemp.age.cond <- de.unemp[,mean(TimeUnemployed/7),by=c("age","earncoh")]
average.unemp.age.cond <- subset(average.unemp.age.cond,earncoh!=0)
setkeyv(average.unemp.age.cond,c("earncoh","age"))
average.unemp.age.cond <- smooth.val.na(average.unemp.age.cond,"V1","earncoh",5)
average.unemp.age.cond$smoo[average.unemp.age.cond$earncoh=="1960" & average.unemp.age.cond$age>56] <- NA
average.unemp.age.cond$smoo[average.unemp.age.cond$earncoh=="1980" & average.unemp.age.cond$age>36] <- NA

g1 <- grafico.varias.lineas(average.unemp.age.cond,"age","smoo","earncoh",
                            "Age","Average weeks unemployed","FigureC3b.pdf")

# Figure C.4.
# Higher order moments of earnings changes 
# Compute on earn.change for consistency with income process

earn.change <- merge(earn.change,var.cohorts,by=c("age","earncoh"),all.x=T)
earn.change$sk <- (earn.change$ng/sqrt(earn.change$earn.var))^3

skew.cohorts <- earn.change[,mean(sk),by=c("age","earncoh")]
setkeyv(skew.cohorts,c("earncoh","age"))
skew.cohorts <- smooth.val.na(skew.cohorts,"V1","earncoh",5)

earn.change$sk[earn.change$earncoh=="1960" & earn.change$age>56] <- NA
earn.change$sk[earn.change$earncoh=="1980" & earn.change$age>36] <- NA

gcoh1 <- grafico.varias.lineas3(skew.cohorts,"age","smoo","earncoh","age","Skew earnings change",
                                "FigureC4a.pdf",NULL,seq(25,60,5),1.2,20)

# And ktosis

earn.change$kt <- ((earn.change$ng/sqrt(earn.change$earn.var))^4)

kurt.cohorts <- earn.change[,mean(kt),by=c("age","earncoh")]
setkeyv(kurt.cohorts,c("earncoh","age"))
kurt.cohorts <- smooth.val.na(kurt.cohorts,"V1","earncoh",5)

earn.change$kt[earn.change$earncoh=="1960" & earn.change$age>56] <- NA
earn.change$kt[earn.change$earncoh=="1980" & earn.change$age>36] <- NA

gcoh1 <- grafico.varias.lineas3(kurt.cohorts,"age","smoo","earncoh","age","Kurtosis earnings change",
                                "FigureC4b.pdf",NULL,seq(25,60,5),1.2,20)
# And kelley etc.

ii <- earn.change[,list(p025=quantile(ng,0.025),p10=quantile(ng,0.1),
                 p25=quantile(ng,0.25),p50=quantile(ng,0.5),p75=quantile(ng,0.75),
                 p90=quantile(ng,0.9),p975=quantile(ng,0.975)),by=c("age","earncoh")]
ii$p9010 <- ii$p90-ii$p10
ii$kelley <- ((ii$p90-ii$p50)-(ii$p50-ii$p10))/(ii$p90-ii$p10)
ii$kelleynum <- (ii$p90-ii$p50)
ii$kelleyden <- (ii$p50-ii$p10)
ii$crowsid <- (ii$p975-ii$p025)/(ii$p75-ii$p25)
ii <- subset(ii,age<60)
ii <- subset(ii,!(earncoh=="1980" & age>40))

ii$kelley[ii$earncoh=="1960" & ii$age>56] <- NA
ii$kelley[ii$earncoh=="1980" & ii$age>36] <- NA
ii$crowsid[ii$earncoh=="1960" & ii$age>56] <- NA
ii$crowsid[ii$earncoh=="1980" & ii$age>36] <- NA

gcoh1 <- grafico.varias.lineas3(ii,"age","kelley","earncoh","age","Kelley's skewness",
                                "FigureC4c.pdf",NULL,seq(25,60,5),1.2,20)

gcoh1 <- grafico.varias.lineas3(ii,"age","crowsid","earncoh","age","CS kurtosis",
                                "FigureC4d.pdf",NULL,seq(25,60,5),1.2,20)



# FIGURE C.5. - Alternative samples

# Note that we need to use dt

dam.weights$SampleWeight <- NA
dam.weights$SampleWeight[is.na(dam.weights$WeightCore)==0] <- 
  dam.weights$WeightCore[is.na(dam.weights$WeightCore)==0]
dam.weights$SampleWeight[is.na(dam.weights$WeightNew)==0] <- 
  dam.weights$WeightNew[is.na(dam.weights$WeightNew)==0]

dam.weights <- subset(dam.weights,age>19 & age<65)
dam.weights <- subset(dam.weights,earncoh!=0)
dam.weights$earncoh <- droplevels(dam.weights$earncoh)

# Impose sample selection restriction such as in DE because these are earnings

dam.weightsp <- dam.weights # keep before this restriction for portfolio information, as in main results
dam.weights <- subset(dam.weights,labtotal>1518)

med3 <- dam.weights[,wtd.quantile(LaborEarningsHead/1000,weights=SampleWeight,0.5),by=c("age","earncoh")]
setkeyv(med3,c("earncoh","age"))
med3 <- smooth.val3(med3,"V1","earncoh","age",5)
med3 <- subset(med3,!(earncoh=="1980" & age>36))
med3 <- subset(med3,!(earncoh=="1960" & age>56))

gcoh7 <- grafico.varias.lineas3(med3,"age","smoo","earncoh","age","Median head earnings ('000)",
                                "FigureC5a.pdf",NULL,seq(25,60,5),1.2,20)

med1 <- dam.weights[,wtd.quantile(labtotal/1000,weights=SampleWeight,0.5),by=c("age","earncoh")]
setkeyv(med1,c("earncoh","age"))
med2 <- smooth.val3(med1,"V1","earncoh","age",5)
med2 <- subset(med2,!(earncoh=="1980" & age>36))
med2 <- subset(med2,!(earncoh=="1960" & age>56))

gcoh6 <- grafico.varias.lineas3(med2,"age","smoo","earncoh","age","Median hh earnings ('000)",
                                "FigureC5b.pdf",NULL,seq(25,60,5),1.2,20)


avg3 <- dam.weights[,wtd.mean(LaborEarningsHead/1000,weights=SampleWeight),by=c("age","earncoh")]
setkeyv(avg3,c("earncoh","age"))
avg3 <- smooth.val3(avg3,"V1","earncoh","age",5)
avg3 <- subset(avg3,!(earncoh=="1980" & age>36))
avg3 <- subset(avg3,!(earncoh=="1960" & age>56))

gcoh7 <- grafico.varias.lineas3(avg3,"age","smoo","earncoh","age","Average head earnings ('000)",
                                "FigureC5c.pdf",NULL,NULL,1.2,20)


avg1 <- dam.weights[,wtd.mean(labtotal/1000,weights=SampleWeight),by=c("age","earncoh")]
setkeyv(avg1,c("earncoh","age"))
avg2 <- smooth.val3(avg1,"V1","earncoh","age",5)
avg2 <- subset(avg2,!(earncoh=="1980" & age>36))
avg2 <- subset(avg2,!(earncoh=="1960" & age>56))

gcoh6 <- grafico.varias.lineas3(avg2,"age","smoo","earncoh","age","Average hh earnings ('000)",
                                "FigureC5d.pdf",NULL,NULL,1.2,20)


# Standard deviation and sdshock for weighted sample
# This requires applying the same transformations to the earnings data
# for consistency with main results (see above) and to verify the 2-year gaps
# For cleanliness, done in psid_outputs_more.R
# (THAT IS FIGURE C.6.)

# FIGURE C.7. - Homeownership, weighted sample

dam.weightsp$homeown <- 0
dam.weightsp$homeown[dam.weightsp$Houseval>1000] <- 1
homeown.coh.wtd <- dam.weightsp[,wtd.mean(homeown,SampleWeight),by=c("age","decade")]
homeown.coh.wtd <- subset(homeown.coh.wtd,age<61)
homeown.coh.wtd <- subset(homeown.coh.wtd,decade %in% c("1940","1960","1980"))
homeown.coh.wtd$decade <- droplevels(homeown.coh.wtd$decade)
setkeyv(homeown.coh.wtd,c("decade","age"))
homeown.coh.wtd <- smooth.val3(homeown.coh.wtd,"V1","decade","age",3)

g4.homeown.coh <- grafico.varias.lineas3(homeown.coh.wtd,"age","smoo","decade","age","Proportion of homeowners",
                                         "FigureC7.pdf",c(0,1),seq(25,60,5),1.2,20)


# FIGURE C.8. - Alternative deflators

de$LaborEarningsHead.pce <- de$LaborEarningsHead*de$cpi/de$pce 
de$labtotal.pce <- de$labtotal*de$cpi/de$pce

med3 <- de[,median(LaborEarningsHead.pce),by=c("age","earncoh")]
med3 <- subset(med3,is.na(V1)==0 & earncoh!=0)
med3$earncoh <- droplevels(med3$earncoh)
setkeyv(med3,c("earncoh","age"))
med3$V1 <- med3$V1/1000
med3 <- smooth.val3(med3,"V1","earncoh","age",5)
med3 <- subset(med3,!(earncoh=="1980" & age>36))
med3 <- subset(med3,!(earncoh=="1960" & age>56))

gcoh7 <- grafico.varias.lineas3(med3,"age","smoo","earncoh","age","Median head earnings ('000)",
                                "FigureC8a.pdf",NULL,NULL,1.2,20)


med1 <- de[,median(labtotal.pce/1000),by=c("age","earncoh")]
med1 <- subset(med1,is.na(V1)==0 & earncoh!=0)
med1$earncoh <- droplevels(med1$earncoh)
setkeyv(med1,c("earncoh","age"))
med2 <- smooth.val3(med1,"V1","earncoh","age",5)
med2 <- subset(med2,!(earncoh=="1980" & age>36)) # too few observations
med2 <- subset(med2,!(earncoh=="1960" & age>56)) # too few observations

gcoh6 <- grafico.varias.lineas3(med2,"age","smoo","earncoh","Age","Median hh earnings ('000)",
                                "FigureC8b.pdf",NULL,seq(25,60,5),1.2,20)


avg3 <- de[,mean(LaborEarningsHead.pce/1000),by=c("age","earncoh")]
avg3 <- subset(avg3,is.na(V1)==0 & earncoh!=0)
avg3$earncoh <- droplevels(avg3$earncoh)
setkeyv(avg3,c("earncoh","age"))
avg3 <- smooth.val3(avg3,"V1","earncoh","age",5)
avg3 <- subset(avg3,!(earncoh=="1980" & age>36))
avg3 <- subset(avg3,!(earncoh=="1960" & age>56))

gcoh7 <- grafico.varias.lineas3(avg3,"age","smoo","earncoh","age","Average head earnings ('000)",
                                "FigureC8c.pdf",NULL,NULL,1.2,20)


avg1 <- de[,mean(labtotal.pce/1000,na.rm=T),by=c("age","earncoh")]
avg1 <- subset(avg1,is.na(V1)==0 & earncoh!=0)
avg1$earncoh <- droplevels(avg1$earncoh)
setkeyv(avg1,c("earncoh","age"))
avg2 <- smooth.val3(avg1,"V1","earncoh","age",5)
avg2 <- subset(avg2,!(earncoh=="1980" & age>36))
avg2 <- subset(avg2,!(earncoh=="1960" & age>56))

gcoh6 <- grafico.varias.lineas3(avg2,"age","smoo","earncoh","Age","Average hh earnings ('000)",
                                "FigureC8d.pdf",NULL,seq(25,60,5),1.2,20)


# Married households (FIGURE C.9. bottom)

de.married <- subset(de,AgeSpouse>0)


med1 <- de.married[,median(labtotal.pce/1000),by=c("age","earncoh")]
med1 <- subset(med1,is.na(V1)==0 & earncoh!=0)
med1$earncoh <- droplevels(med1$earncoh)
setkeyv(med1,c("earncoh","age"))
med2 <- smooth.val3(med1,"V1","earncoh","age",5)
med2 <- subset(med2,!(earncoh=="1980" & age>36)) # too few observations
med2 <- subset(med2,!(earncoh=="1960" & age>56)) # too few observations

gcoh6 <- grafico.varias.lineas3(med2,"age","smoo","earncoh","Age","Median hh earnings ('000)",
                                "FigureC9a.pdf",NULL,seq(25,60,5),1.2,20)


avg1 <- de.married[,mean(labtotal.pce/1000,na.rm=T),by=c("age","earncoh")]
avg1 <- subset(avg1,is.na(V1)==0 & earncoh!=0)
avg1$earncoh <- droplevels(avg1$earncoh)
setkeyv(avg1,c("earncoh","age"))
avg2 <- smooth.val3(avg1,"V1","earncoh","age",5)
avg2 <- subset(avg2,!(earncoh=="1980" & age>36))
avg2 <- subset(avg2,!(earncoh=="1960" & age>56))

gcoh6 <- grafico.varias.lineas3(avg2,"age","smoo","earncoh","Age","Average hh earnings ('000)",
                                "FigureC9b.pdf",NULL,seq(25,60,5),1.2,20)


regre.married <- lm(loglabtotal ~ factor(year) + factor(age):bigcoh,data=de.married,na.action=na.exclude)

de.married$resids <- residuals(regre.married)

sd5 <- de.married[,mean(resids^2),by=c("age","earncoh")]
sd5 <- subset(sd5,is.na(V1)==0 & earncoh!=0)
sd5$earncoh <- droplevels(sd5$earncoh)
setkeyv(sd5,c("earncoh","age"))
sd5 <- smooth.val.na(sd5,"V1","earncoh",5)
sd5 <- subset(sd5,!(age>36 & earncoh=="1980"))
sd5 <- subset(sd5,!(age>56 & earncoh=="1960"))

gcoh2 <- grafico.varias.lineas3(sd5,"age","smoo","earncoh","age","SD of the earnings dtb",
                                "FigureC9c.pdf",NULL,seq(25,60,5),1.2,20)

## Figure C.10.: Homeownership for married households + households with children

dp.married <- subset(dp,AgeSpouse>0)
homeown.coh.married <- dp.married[,mean(homeown),by=c("age","decade")]
homeown.coh.married <- subset(homeown.coh.married,is.na(decade)==0)
homeown.coh.married <- subset(homeown.coh.married,decade %in% c(1940,1960,1980))
setkeyv(homeown.coh.married,c("decade","age"))
homeown.coh.married <- smooth.val3(homeown.coh.married,"V1","decade","age",3)
#homeown.coh <- subset(homeown.coh,!(age>40 & decade==1980))
g3.homeown.coh <- grafico.varias.lineas3(homeown.coh.married,"age","smoo","decade","age","Proportion of homeowners",
                                         "FigureC10a.pdf",c(0.0,1.0),seq(25,60,5),1.2,20)


dp.child <- subset(dp,NumberChildren>0)
homeown.coh.child <- dp.child[,mean(homeown),by=c("age","decade")]
homeown.coh.child <- subset(homeown.coh.child,is.na(decade)==0)
homeown.coh.child <- subset(homeown.coh.child,decade %in% c(1940,1960,1980))
setkeyv(homeown.coh.child,c("decade","age"))
homeown.coh.child <- smooth.val3(homeown.coh.child,"V1","decade","age",3)
#homeown.coh <- subset(homeown.coh,!(age>40 & decade==1980))
g3.homeown.coh <- grafico.varias.lineas3(homeown.coh.child,"age","smoo","decade","age","Proportion of homeowners",
                                         "FigureC10b.pdf",c(0.0,1.0),seq(25,60,5),1.2,20)



# SHARE THAT EVER MOVED STATE
# FIGURE C.11.

dp$EverMoved <- 0
dp$EverMoved[dp$LivesWhereGrewUp>1] <- 1
dp$EverMoved[dp$LivesWhereGrewUp==1 & dp$year==1968] <- 1

mobility1 <- dp[,mean(EverMoved),by=c("decade","age")]
mobility1 <- subset(mobility1,mobility1$decade %in% c("1940","1960","1980"))
mobility1 <- smooth.val3(mobility1,"V1","decade","age",5)
mobility1$smoo[mobility1$age>35 & mobility1$decade==1980] <- NA
mobility1$smoo[mobility1$age>55 & mobility1$decade==1960] <- NA
mobility1 <- subset(mobility1,age<60)
mobility1$smoo <- 100*mobility1$smoo

gp1 <- grafico.varias.lineas(mobility1,"age","smoo","decade","age","% ever moved state","FigureC11.pdf",c(0,50),NULL,
                             1.20,20)


# FIGURE C.12.: Homeownership by education status

dp$college <- 0
dp$college[dp$EducationBkt>5 & dp$EducationBkt<9] <- 1
dp$college[dp$Education>12 & dp$Education<99] <- 1

dp.college <- subset(dp,college==1)

homeown.coh.college <- dp.college[,mean(homeown),by=c("age","decade")]
homeown.coh.college <- subset(homeown.coh.college,age<65)
homeown.coh.college <- subset(homeown.coh.college,decade %in% c("1940","1960","1980"))
homeown.coh.college$decade <- droplevels(homeown.coh.college$decade)
homeown.coh.college <- smooth.val3(homeown.coh.college,"V1","decade","age",3)

g3.homeown.coh.collegeren <- grafico.varias.lineas3(homeown.coh.college,"age","smoo","decade","age","Proportion of homeowners",
                                                    "FigureC12b.pdf",c(0,1),seq(25,60,5),1.2,20)


dp.nocollege <- subset(dp,college==0)

homeown.coh.nocollege <- dp.nocollege[,mean(homeown),by=c("age","decade")]
homeown.coh.nocollege <- subset(homeown.coh.nocollege,age<65)
homeown.coh.nocollege <- subset(homeown.coh.nocollege,decade %in% c("1940","1960","1980"))
homeown.coh.nocollege$decade <- droplevels(homeown.coh.nocollege$decade)
homeown.coh.nocollege <- smooth.val3(homeown.coh.nocollege,"V1","decade","age",3)

g3.homeown.coh.collegeren <- grafico.varias.lineas3(homeown.coh.nocollege,"age","smoo","decade","age","Proportion of homeowners",
                                                    "FigureC12a.pdf",c(0,1),seq(25,60,5),1.2,20)


# Equivalence scales by age and cohort
# FIGURE C.13.

n.people.age.coh <- dt[,as.list(c(people=mean(NumberPeople),children=mean(NumberChildren),
                                         adults=mean(NumberPeople-NumberChildren))),by=c("age","alldecade")]
n.people.age.coh$equiv <- 1+0.7*(n.people.age.coh$adults-1)+0.5*(n.people.age.coh$children)

n.people.age.coh.prev <- subset(n.people.age.coh,alldecade %in% c("1910","1920","1930"))
n.people.age.coh.prev <- n.people.age.coh.prev[,list(equiv=mean(equiv)),by="age"]

n.people.age.coh <- subset(n.people.age.coh,alldecade %in% c("1940","1960","1980"))
setkeyv(n.people.age.coh,c("alldecade","age"))
n.people.age.coh <- smooth.val3(n.people.age.coh,"equiv","alldecade","age",5)
gecoh <- grafico.varias.lineas3(n.people.age.coh,"age","smoo","alldecade","age","Equivalence scale","FigureC13.pdf",NULL,seq(25,60,5),1.2,20)


equivc.bi <- subset(n.people.age.coh,is.even(age)==1)
equiv40.bi <- subset(equivc.bi,alldecade=="1940")
equiv60.bi <- subset(equivc.bi,alldecade=="1960")
equiv80.bi <- subset(equivc.bi,alldecade=="1980")

# Fill in missing parts of equivalence scale with previous generations (nearest)
equivc.bi.prev <- subset(n.people.age.coh.prev,is.even(age)==1 & age>76 & age<85)
setkeyv(equivc.bi.prev,"age")

equiv40.bi <- rbind(subset(equiv40.bi,select=c("age","equiv")),equivc.bi.prev)
equiv60.bi <- rbind(subset(equiv60.bi,select=c("age","equiv")),
                    equiv40.bi[20:33,])
equiv80.bi <- rbind(subset(equiv80.bi,select=c("age","equiv")),
                    equiv60.bi[10:33,])

write(equiv40.bi$equiv,file=paste0(folder.model,"1940/equiv_coh.csv"),sep="\n")
write(equiv60.bi$equiv,file=paste0(folder.model,"1960/equiv_coh.csv"),sep="\n")
write(equiv80.bi$equiv,file=paste0(folder.model,"1980/equiv_coh.csv"),sep="\n")





# House price pti graph 
# FIGURE F.4., LEFT

medianinc <- de[,list(medianinc=median(labtotal)),by="year"]
dp <- merge(dp,medianinc,by="year",all.x=T)
dp$housepti <- dp$Houseval/dp$medianinc

housedtb <- subset(dp,Houseval>0)
housedtb$ptibin <- .bincode(housedtb$housepti,breaks=seq(0,10,0.25),right=F)
housedtb$ptibin <- plyr::mapvalues(housedtb$ptibin,from=seq(1,40),to=seq(0.25,10,0.25))

housedtb <- subset(housedtb,decade==1940 | decade==1960 | decade==1980)
housedtb <- subset(housedtb,age>30 & age<40)

dpbis <- subset(dp,is.na(Houseval)==0)
#dpbis <- subset(dpbis,Houseval>0)
dpbis <- subset(dpbis,age>30 & age<40)
dpbis <- subset(dpbis,decade==1940 | decade==1960 | decade==1980)
housedtb.bigtotal <- dpbis[,list(v3=length(Houseval)),by="decade"]

housedtb.graph <- housedtb[,length(housepti),by=c("ptibin","decade")]
housedtb.graph.t <- housedtb[,list(v2=length(housepti)),by="decade"]
housedtb.graph <- merge(housedtb.graph,housedtb.graph.t,all.x=T,by="decade")
housedtb.graph <- merge(housedtb.graph,housedtb.bigtotal,all.x=T,by="decade")
housedtb.graph$prop <- 100*housedtb.graph$V1/housedtb.graph$v3
housedtb.graph$decade <- as.factor(housedtb.graph$decade)
housedtb.graph <- smooth.val3(housedtb.graph,"prop","decade","ptibin",5)

g36 <- grafico.varias.lineas3(housedtb.graph,"ptibin","smoo","decade","PTI ratio","Percentage of homeowners, age 35","FigureF4a.pdf",
                              c(0,10),NULL,1.2,20)

housedtb.graph2 <- subset(housedtb.graph,is.na(ptibin)==0)
# Reported share:
mm <- housedtb.graph2$prop[housedtb.graph2$ptibin>0 & housedtb.graph2$ptibin<2 & 
                             housedtb.graph2$decade==1980]
sum(mm)

## FIGURE F.4., RIGHT

nroomdtb.graph <- housedtb[,length(housepti),by=c("Nrooms","decade")]
nroomdtb.graph <- merge(nroomdtb.graph,housedtb.bigtotal,all.x=T,by="decade")
nroomdtb.graph$prop <- 100*nroomdtb.graph$V1/nroomdtb.graph$v3
nroomdtb.graph$decade <- as.factor(nroomdtb.graph$decade)
nroomdtb.graph <- smooth.val3(nroomdtb.graph,"prop","decade","Nrooms",5)

g36 <- grafico.varias.lineas3(nroomdtb.graph,"Nrooms","smoo","decade","Number of rooms","Percentage of households, age 35",
                              "FigureF4b.pdf",
                              NULL,seq(0,12),1.2,20)



### AGE-EFFICIENCY PROFILES

# By cohort

regre.ageeff.log1 <- lm(loglabtotal ~ year + factor(age),data=subset(de,earncoh=="1940"))
regre.ageeff.log2 <- lm(loglabtotal ~ year + factor(age),data=subset(de,earncoh=="1960"))
regre.ageeff.log3 <- lm(loglabtotal ~ year + factor(age),data=subset(de,earncoh=="1980" & age<43))
myear1 <- mean(de$year[de$earncoh=="1940"])
myear2 <- mean(de$year[de$earncoh=="1960"])
myear3 <- mean(de$year[de$earncoh=="1980"])

age.eff.log1 <- myear1*coefficients(regre.ageeff.log1)[2]+
  coefficients(regre.ageeff.log1)[1]+c(0,coefficients(regre.ageeff.log1)[3:42])
age.eff.log1 <- age.eff.log1[is.na(age.eff.log1)==0]
write.table(age.eff.log1,file=paste0(folder.earnings.process,"1940/age_efficiency_log.csv"),row.names=F,col.names=F,sep=",")   

age.eff.log2 <- myear2*coefficients(regre.ageeff.log2)[2]+
  coefficients(regre.ageeff.log2)[1]+c(0,coefficients(regre.ageeff.log2)[3:42])
age.eff.log2 <- age.eff.log2[is.na(age.eff.log2)==0]
write.table(age.eff.log2,file=paste0(folder.earnings.process,"1960/age_efficiency_log.csv"),row.names=F,col.names=F,sep=",")   

age.eff.log3 <- myear3*coefficients(regre.ageeff.log3)[2]+
  coefficients(regre.ageeff.log3)[1]+c(0,coefficients(regre.ageeff.log3)[3:42])
age.eff.log3 <- age.eff.log3[is.na(age.eff.log3)==0]
write.table(age.eff.log3,file=paste0(folder.earnings.process,"1980/age_efficiency_log.csv"),row.names=F,col.names=F,sep=",")   

coh.mean1 <- mean(de$labtotal[de$age>27 & de$age<33 & de$earncoh=="1940"])
write.table(coh.mean1,file=paste0(folder.earnings.process,"1940/cohort_mean.csv"),row.names=F,col.names=F,sep=",")
coh.mean2 <- mean(de$labtotal[de$age>27 & de$age<33 & de$earncoh=="1960"])
write.table(coh.mean2,file=paste0(folder.earnings.process,"1960/cohort_mean.csv"),row.names=F,col.names=F,sep=",")
coh.mean3 <- mean(de$labtotal[de$age>27 & de$age<33 & de$earncoh=="1980"])
write.table(coh.mean3,file=paste0(folder.earnings.process,"1980/cohort_mean.csv"),row.names=F,col.names=F,sep=",")


values.age <- dam[,mean(labtotal),by="age"]

# For married households...

regre.ageeff.log1.m <- lm(loglabtotal ~ year + factor(age),data=subset(de,earncoh=="1940" & AgeSpouse>0))
regre.ageeff.log2.m <- lm(loglabtotal ~ year + factor(age),data=subset(de,earncoh=="1960" & AgeSpouse>0))
regre.ageeff.log3.m <- lm(loglabtotal ~ year + factor(age),data=subset(de,earncoh=="1980" & AgeSpouse>0 & age<43))
myear1.m <- mean(de$year[de$earncoh=="1940" & de$AgeSpouse>0])
myear2.m <- mean(de$year[de$earncoh=="1960" & de$AgeSpouse>0],na.rm=T)
myear3.m <- mean(de$year[de$earncoh=="1980" & de$AgeSpouse>0],na.rm=T)

age.eff.log1.m <- myear1.m*coefficients(regre.ageeff.log1.m)[2]+
  coefficients(regre.ageeff.log1.m)[1]+c(0,coefficients(regre.ageeff.log1.m)[3:42])
age.eff.log1.m <- age.eff.log1.m[is.na(age.eff.log1.m)==0]
write.table(age.eff.log1.m,file=paste0(folder.earnings.process,"1940m/age_efficiency_log.csv"),row.names=F,col.names=F,sep=",")   

age.eff.log2.m <- myear2.m*coefficients(regre.ageeff.log2.m)[2]+
  coefficients(regre.ageeff.log2.m)[1]+c(0,coefficients(regre.ageeff.log2.m)[3:42])
age.eff.log2.m <- age.eff.log2.m[is.na(age.eff.log2.m)==0]
write.table(age.eff.log2.m,file=paste0(folder.earnings.process,"1960m/age_efficiency_log.csv"),row.names=F,col.names=F,sep=",")   

age.eff.log3.m <- myear3.m*coefficients(regre.ageeff.log3.m)[2]+
  coefficients(regre.ageeff.log3.m)[1]+c(0,coefficients(regre.ageeff.log3.m)[3:42])
age.eff.log3.m <- age.eff.log3.m[is.na(age.eff.log3.m)==0]
write.table(age.eff.log3.m,file=paste0(folder.earnings.process,"1980m/age_efficiency_log.csv"),row.names=F,col.names=F,sep=",")   


coh.mean1.m <- mean(de$labtotal[de$age>27 & de$age<33 & de$earncoh=="1940" & de$AgeSpouse>0])
write.table(coh.mean1.m,file=paste0(folder.earnings.process,"1940m/cohort_mean.csv"),row.names=F,col.names=F,sep=",")
coh.mean2.m <- mean(de$labtotal[de$age>27 & de$age<33 & de$earncoh=="1960" & de$AgeSpouse>0])
write.table(coh.mean2.m,file=paste0(folder.earnings.process,"1960m/cohort_mean.csv"),row.names=F,col.names=F,sep=",")
coh.mean3.m <- mean(de$labtotal[de$age>27 & de$age<33 & de$earncoh=="1980" & de$AgeSpouse>0],na.rm=T)
write.table(coh.mean3.m,file=paste0(folder.earnings.process,"1980m/cohort_mean.csv"),row.names=F,col.names=F,sep=",")




# Variances and autocovariances for estimation of CANONICAL process

# First, generate nresy so that we can get autocovariances

earn.new4 <- earn.new2
earn.new4$obsi <- floor(earn.new4$order/100)
earn.new4[,nresy := shift(resy,type="lead"),by="obsi"]
earn.new4 <- subset(earn.new4,is.na(nresy)==0)

sd2.bc <- earn.new2[,sd(resy),by=c("byear","age","rec","earncoh")]
sd2.bc <- subset(sd2.bc,is.na(V1)==0)
setkeyv(sd2.bc,c("earncoh","age","rec"))
write.table(subset(subset(sd2.bc,earncoh=="1940"),select=c("byear","age","rec","V1")),file=paste0(folder.earnings.process,"1940/plain_sdearn_bc.csv"),sep=",",col.names=F,row.names=F)
write.table(subset(subset(sd2.bc,earncoh=="1960" & age<58),select=c("byear","age","rec","V1")),file=paste0(folder.earnings.process,"1960/plain_sdearn_bc.csv"),sep=",",col.names=F,row.names=F)
write.table(subset(subset(sd2.bc,earncoh=="1980"),select=c("byear","age","rec","V1")),file=paste0(folder.earnings.process,"1980/plain_sdearn_bc.csv"),sep=",",col.names=F,row.names=F)

ac2.bc <- earn.new4[,cov(resy,nresy),by=c("byear","age","rec","earncoh")]
ac2.bc <- subset(ac2.bc,is.na(V1)==0)
setkeyv(ac2.bc,c("earncoh","byear","age","rec"))
write.table(subset(subset(ac2.bc,earncoh=="1940"),select=c("byear","age","rec","V1")),file=paste0(folder.earnings.process,"1940/plain_acearn_bc.csv"),sep=",",col.names=F,row.names=F)
write.table(subset(subset(ac2.bc,earncoh=="1960" & age<58),select=c("byear","age","rec","V1")),file=paste0(folder.earnings.process,"1960/plain_acearn_bc.csv"),sep=",",col.names=F,row.names=F)
write.table(subset(subset(ac2.bc,earncoh=="1980"),select=c("byear","age","rec","V1")),file=paste0(folder.earnings.process,"1980/plain_acearn_bc.csv"),sep=",",col.names=F,row.names=F)


# Now save resy and nresy to compute NL persistence graphs 


matlab.nlpers <- subset(earn.new4,select=c("resy","nresy","age","earncoh"))
matlab.nlpers$earncoh <- as.numeric((matlab.nlpers$earncoh))
matlab.nlpers <- subset(matlab.nlpers,is.na(earncoh)==0 & earncoh>0)
setkeyv(matlab.nlpers,"earncoh")
write.table(matlab.nlpers,file=paste0(folder.model,"data_fornlpers.csv"),row.names=F,col.names=F,sep=",")


# source: https://www.taxpolicycenter.org/statistics/standard-deduction
# Note before 1970 it was the min of 10% of income or $1000. I am assuming it's 1000
# but could add that to the model easily

std.deduction <- c(rep(1000,7),1100,1050,1300,1300,1300,1900,2100,3200,3200,rep(3400,6),
                   3550,3670,3760,5000,5200,5450,5700,6000,6200,6350,6550,6700,6900,
                   7100,7200,7350,7600,7850,9500,9700,10000,10300,10700,10900,11400,
                   11400,11600,11900,12200,12400,12600,12600,12700)
std.deduction <- as.data.table(std.deduction)
std.deduction$year <- seq(1963,2017)

avg.income <- de[,mean(NDlabtotal),by="year"] #labtotal for everyone, not just cohort in that year (age effects)
avg.income$year <- avg.income$year-1
avg.income.2017 <- subset(dt,year==2017 & is.na(labtotal)==0)
avg.income.2017 <- avg.income.2017[,mean(labtotal)]
# Note that by using de we are computing the average of the over 1500s
# I guess most sensible given that that is our sample for earnings...

std.deduction <- merge(std.deduction,avg.income,by="year")
std.deduction$pc <- std.deduction$std.deduction/std.deduction$V1

mean(std.deduction$pc)

# std.deduction is the real data
# Note before I was doing an interpolation for the years after 1999
# Now, real data
# Assume that they are the same for the missing years

std.deduction.interp <- subset(subset(std.deduction,select=c("year","pc")),
                               year>1995)
std.deduction.interp$year <- std.deduction.interp$year+1

# 2nd, 10% before sample period

year <- seq(1963,1967)
std.deduction.pre <- as.data.table(year)
std.deduction.pre$pc <- 0.10

# and 15% afterwards

year <- seq(2018,2068)
std.deduction.extra <- as.data.table(year)
std.deduction.extra$pc <- 0.15

std.deduction.total <- rbind(std.deduction.pre,
                             subset(std.deduction,select=c("year","pc")),
                             std.deduction.extra,std.deduction.interp)

setkeyv(std.deduction.total,"year")

# ...and... estimation of tax parameters

# Standard HSV approach
# Estimate for 1940s cohort because given that FederalIncomeTax not available after
# a certain year, we get a more representative estimate

de$disposable2 <- de$labtotal - de$FederalIncomeTax + de$GovtTransfer2
de$disposable3 <- de$labtotal - de$FederalIncomeTax #post-tax, pre-benefit income

hsv.reg3 <- lm(log(disposable2) ~ loglabtotal,data=subset(de,disposable2>0 & earncoh=="1940"))
# 0.18


hsv.reg3.taxes <- lm(log(disposable3) ~ loglabtotal,data=subset(de,disposable2>0 & earncoh=="1940"))

# Use mortgage payment information

de$MortPayment <- de$MortPayment/de$cpi

fr.mortgages <- read.csv(paste0(folder.data,"MORTGAGE30US.csv"))
fr.mortgages$DATE <- as.character(fr.mortgages$DATE)
fr.mortgages$DATE <- substring(fr.mortgages$DATE,1,4)
fr.mortgages$DATE <- as.numeric(fr.mortgages$DATE)
fr.mortgages <- dplyr::rename(fr.mortgages,c("year"="DATE",
                                             "rb"="MORTGAGE30US"))

de <- merge(de,fr.mortgages,by="year",all.x=T)

# And merge standard deduction into de 

de <- merge(de,subset(std.deduction,select=c("year","std.deduction")),by="year",all.x=T)

de$std.deduction.def <- de$std.deduction/de$cpi

# Build how much they pay + how much they would
de$count.disposable.level4 <- exp(coef(hsv.reg3.taxes)[1])*(de$labtotal)^(coef(hsv.reg3.taxes)[2])
de$count.disposable.level5 <- exp(coef(hsv.reg3.taxes)[1])*(de$labtotal+pmax(de$std.deduction.def,pmin(de$MortPayment,de$rb*de$Mortrem)))^(coef(hsv.reg3.taxes)[2])
de$tax.count3 <- de$labtotal+pmax(de$std.deduction.def,pmin(de$MortPayment,de$rb*de$Mortrem))-de$count.disposable.level5 # how much would pay
de$tax.count4 <- de$labtotal - de$count.disposable.level4 # how much they do pay

# Build counterfactual post-tax income, including benefits

de$count.disposable.4 <- de$disposable2+de$tax.count4-de$tax.count3

# and reestimate

hsv.reg3.count4 <- lm(log(count.disposable.4) ~ loglabtotal,
                      data=subset(de,disposable2>0 & earncoh=="1940"))

tax.parameter <- 1-hsv.reg3.count4$coefficients[2]
# obtaining 0.085 as reported in Appendix A.1.2.



# Information on home repairs (footnote 12)

dam.hr <- subset(dp,year>2003)
dam.hr <- subset(dam.hr,TotalFamilyIncome>0)
dam.hr <- subset(dam.hr,Houseval>0)
dam.hr$repairs <- dam.hr$HomeRepairsExp
dam.hr$repairs[dam.hr$repairs>999997] <- NA
dam.hr$repairs[dam.hr$Period_HomeRepairsexp==5] <- 12*dam.hr$repairs[dam.hr$Period_HomeRepairsexp==5]
dam.hr$ratio <- dam.hr$repairs/dam.hr$TotalFamilyIncome
median(dam.hr$ratio,na.rm=T)

# How many people receive Section 8 help (footnote 15)

dp$housinghelp <- 0
dp$housinghelp[dp$WhetherGovtHelpsRent==1 | dp$WhetherOwnedLHA==1] <- 1
dp40 <- subset(dp,age>25 & age<60)
housinghelp <- dp40[,mean(housinghelp),by="year"]
mean(housinghelp$V1[housinghelp$V1>0]) # not available for some years

# Average tax rate 

avg.trate <- mean(de$FederalIncomeTax[de$earncoh=="1940" & de$age>20 & de$age<65],na.rm=T)/
  mean(de$labtotal[de$earncoh=="1940" & de$age>20 & de$age<65],na.rm=T)

# Median household income

median(de$labtotal[de$year==2015])


