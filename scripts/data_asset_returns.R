# data_asset_returns.R

# Concentrates everything related to prices, returns, and
# the aggregate state 

# It uses PSID data, but also other price data, in particular:
# - Shiller house price data
# - SP500 returns 
# - interest rate on mortgages (NOTE: maybe concentrate the loading of all this data,
# possibly in load_psid_data_2)
# - Freddie Mac FHFI


# In particular, it generates

# Figure 2 (evolution of house prices and asset returns)
# Figure E.1., left (fit of asset returns with agg. state)
# The numbers reported in Appendix F.4.1.
# Some outputs for the model, that are mostly then organized in Matlab's
# gen_agg_state_fixed, namely:
#  - Realizations of stock returns by stock state, rf_values.csv
#  - Realizations of the aggregate state hre, hre_bi_data11.csv
#  - Transition matrix of agg state Qhre, Qhre_bi_data11_new.csv
#  - Information to build transition matrix of housing Qh, h_growths12.csv
#  - Robustness checks on Qhre, Qhre_bi_data7_new.csv to Qhre_bi_data10_new.csv

dowrite <- 1

folder <- paste0(main.folder,"results/Figures/")
folder.model <- paste0(main.folder,"working/Model/")
folder.from <- paste0(main.folder,"data/")
folder.wk <- paste0(main.folder,"working/")
source(paste0(main.folder,'code/data_analysis/functions/gon_graphs.R'))

# DATA ON HOUSING PRICES...........................................................................................

load(paste0(folder.wk,'PSIDSample.RData'))

 dp <- subset(dam,age>19)
 
 dp <- subset(dp,age>19 & age<100)
 dp <- subset(dp,src==1)
 dp <- subset(dp,Seqno==1) # otherwise double-counting some heads
 
 psid.all <- dp
 psid.all$year <- psid.all$year-1 # this is important when comparing sources for asset returns
                                 # (data refers to previous PSID interview)

# Compute inflation

cpi$inflation <- c(NA,(cpi$cpi[2:nrow(cpi)]-cpi$cpi[1:(nrow(cpi)-1)])/cpi$cpi[1:(nrow(cpi)-1)])

# And get un-deflated income (some of the variables will be defined as ratios of current income)

psid.all$NDLaborEarningsHead <- psid.all$LaborEarningsHead*psid.all$cpi
psid.all$NDHouseval <- psid.all$Houseval*psid.all$cpi

aveinc.hw.nd <- psid.all[,list(inc.mean=mean(NDlabtotal,na.rm=T),inc.med=median(NDlabtotal,na.rm=T)),by="year"]


# House values and PTIs in the PSID

hv.psid <- psid.all[,as.list(c(mean=mean(NDHouseval,na.rm=T),med=median(NDHouseval,na.rm=T))),by=year]
pti.psid <- psid.all[,as.list(c(mean=mean(Houseval,na.rm=T)/mean(labtotal,na.rm=T),
                              med=median(Houseval,na.rm=T)/median(labtotal,na.rm=T))),by=year]

psid.all$homeo <- 0 
psid.all$homeo[psid.all$Houseval>0] <- 1
psid.homeo <- subset(psid.all,homeo==1)
pti.psid2 <- psid.homeo[,list(meanhouse=mean(Houseval,na.rm=T),
                                 medianhouse=median(Houseval,na.rm=T)),by=year]
pti.psid2b <- psid.all[,list(meanincome=mean(labtotal,na.rm=T),
                             medianincome=median(labtotal,na.rm=T)),by=year]
pti.psid3 <- merge(pti.psid2,pti.psid2b,by="year")
pti.psid3$mean <- pti.psid3$meanhouse/pti.psid3$meanincome
pti.psid3$med <- pti.psid3$medianhouse/pti.psid3$medianincome

pti.psid <- pti.psid3

## Older data on housing prices

h.states.pre <- as.data.table(read.csv(paste0(folder.from,"Shiller_house_prices.csv"),sep=";"))
names(h.states.pre) <- c("Year","Value")



## DATA ON FINANCIAL ASSETS .........................................................................



# Stocks and financial assets from csv data

fr.sp500 <- read.csv(paste0(folder.from,"sp500_returns.csv"))

v1 <- fr.sp500$sp500[fr.sp500$year>1966 & fr.sp500$year<2018]
v1 <- v1[1:(length(v1)-1)]
nominal.period.return.stock <- cumprod(1+v1)

v1 <- fr.sp500$tbill[fr.sp500$year>1966 & fr.sp500$year<2018]
v1 <- v1[1:(length(v1)-1)]
nominal.period.return.tbill <- cumprod(1+v1)


inflation.period <- cpi$cpi[cpi$year==2012]/cpi$cpi[cpi$year==1967]



# Interest rate on mortgages

fr.mortgages <- read.csv(paste0(folder.from,"MORTGAGE30US.csv"))
fr.mortgages$DATE <- as.character(fr.mortgages$DATE)
fr.mortgages$DATE <- substring(fr.mortgages$DATE,1,4)
fr.mortgages$DATE <- as.numeric(fr.mortgages$DATE)
fr.mortgages <- dplyr::rename(fr.mortgages,c("year"="DATE",
                                      "rb"="MORTGAGE30US"))

# net of inflation?

fr.mortgages <- merge(fr.mortgages,cpi,by="year")
fr.mortgages$realrb <- fr.mortgages$rb-100*fr.mortgages$inflation



real.period.return.tbill <- (nominal.period.return.tbill[46]-inflation.period)/inflation.period
real.pp.return.tbill <- (1+real.period.return.tbill)^(1/45)-1


### Compute returns + variables that matter for the state ..............................................


# 1. Data on % returns (rets)

hv.psid$return <- (c(NA,hv.psid$mean[2:nrow(hv.psid)])-
                     c(NA,hv.psid$mean[1:nrow(hv.psid)-1]))/(c(NA,hv.psid$mean[1:nrow(hv.psid)-1]))
hv.psid$return[hv.psid$year>1997] <- (1+hv.psid$return[hv.psid$year>1997])^(1/2)-1

# Fredie Mac FHFA house price index
# Naturally this is on top of inflation
# From origin of data: "No, the HPI is not adjusted for inflation. "
# This is to plot returns in a more consistent way (but verified no large deviations from
# PSID also in other measures) & annually

fhfa <- as.data.table(read.csv(paste0(folder.from,"HPI_AT_us_and_census.csv"),header=F))
fhfa <- rename(fhfa,c("area"="V1","year"="V2","quarter"="V3","hprice"="V4"))
fhfa <- fhfa[,mean(hprice),by=c("area","year")]
fhfa <- rename(fhfa,c("hv.mean"="V1"))
fhfa <- subset(fhfa,area=="USA")
fhfa$area <- NULL
fhfa$fhfa.return <- (c(NA,fhfa$hv.mean[2:nrow(fhfa)])-
                      c(NA,fhfa$hv.mean[1:nrow(fhfa)-1]))/(c(NA,fhfa$hv.mean[1:nrow(fhfa)-1]))

rets <- subset(hv.psid,select=c("year","return"))
rets <- rename(rets,c("psid.hprice"="return"))
rets <- merge(rets,fr.sp500,by="year",all=T)
rets <- merge(rets,subset(cpi,select=c("year","inflation")),by="year",all=T)
rets <- merge(rets,subset(fhfa,select=c("year","fhfa.return")),by="year",all=T)

real.rets <- rets
real.rets$sp500 <- real.rets$sp500-real.rets$inflation
real.rets$tbill <- real.rets$tbill-real.rets$inflation
real.rets$tbond <- real.rets$tbond-real.rets$inflation
real.rets$psid.hprice <- real.rets$psid.hprice-real.rets$inflation

real5.rets <- real.rets
real5.rets$sp500 <- (rollapply((1+real5.rets$sp500),5,prod,fill=NA))^(1/5)-1
real5.rets$tbill <- (rollapply((1+real5.rets$tbill),5,prod,fill=NA))^(1/5)-1
real5.rets$tbond <- (rollapply((1+real5.rets$tbond),5,prod,fill=NA))^(1/5)-1
real5.rets$psid.hprice <- (rollapply((1+real5.rets$psid.hprice),5,prod,na.rm=T,fill=NA))^(1/5)-1

# 2. Data on housing PTIs

psid.pti.reg <- lm(mean ~ year, data=pti.psid,na.action=na.exclude)
pti.psid$trendmean <- predict(psid.pti.reg,na.rm=F)


psid.pti.reg <- lm(med ~ year, data=pti.psid,na.action=na.exclude)
pti.psid$trendmed <- predict(psid.pti.reg,na.rm=F)

# 3. CONSTRUCT AGGREGATE STATE

# Housing grid is fixed: either above or below trend
# Stock grid can have several possibilities

# 3.1 Housing state

h.state <- data.frame(year=seq(1968,2017))
h.state <- merge(h.state,pti.psid,by="year",all.x=T)
h.state.mean <- rename(subset(h.state,select=c("year","mean")),c("PTI"="mean"))
h.state <- subset(h.state,select=c("year","med"))
h.state <- rename(h.state,c("PTI"="med"))

h.states.preg <- h.states.pre
h.states.preg$realyear <- floor(h.states.preg$Year)
h.states.preg <- subset(h.states.preg,realyear>1947 & realyear<1970) 
h.states.preg <- h.states.preg[,mean(Value),by="realyear"]

h.states.preg <- rename(h.states.preg,c("year"="realyear",
                                        "PTI"="V1"))
h.state <- rbind(h.states.preg,h.state)
h.state$PTI[1:22] <- h.state$PTI[1:22]*h.state$PTI[24]/h.state$PTI[22]
h.state <- rbind(h.state[1:20],h.state[23:72])

h.state.mean <- rbind(h.states.preg,h.state.mean)
h.state.mean$PTI[1:22] <- h.state.mean$PTI[1:22]*h.state.mean$PTI[24]/h.state.mean$PTI[22]
h.state.mean <- rbind(h.state.mean[1:20],h.state.mean[23:72])

#h.states.preg[,twopre := shift(V1,type="lag",n=2)]
#h.states.preg$growth <- (h.states.preg$V1 - h.states.preg$twopre)/h.states.preg$twopre

# Join them together
# Income information not available before PSID, so assume that PTI
# ratios evolved at the same rate as house prices before 1968
is.even <- function(x) x %% 2 == 0
h.state.g4 <- subset(h.state,year>1960 & is.even(year)==1)
if (dowrite==1) {
  write(h.state.g4$PTI,file=paste0(folder.wk,"hvalues_forstate11.csv"),sep="\n")
}


# 3.2. Stock states

f.state2 <- subset(real.rets,select=c("year","sp500"))
f.state2.q <- quantile(f.state2$sp500,c(1/3,2/3),na.rm=T)

f.state2$highstock <- NA
f.state2$highstock[f.state2$sp500<f.state2.q[1]] <- 0
f.state2$highstock[f.state2$sp500>f.state2.q[1] & f.state2$sp500<f.state2.q[2]] <- 1
f.state2$highstock[f.state2$sp500>f.state2.q[2]] <- 2

media.fstates <- f.state2[,mean(sp500),by=highstock] # median is similar
media.fstates <- rename(media.fstates,c("modelstock"="V1"))

f.state2 <- merge(f.state2,media.fstates,by="highstock")

f.state.forgraph <- f.state2

a.state2 <- f.state2

y.state <- as.data.table(seq(1950,2017))
y.state <- rename(y.state,c("year"="V1"))
y.state$recession <- c(0,0,0,1,1,0,0,1,1,0,1,0,
                        0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,
                        0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0)

a.state2 <- merge(a.state2,y.state,by="year")


# FIGURES AND GRAPHS PRODUCED HERE....


# # FIGURE 2 IN PAPER 

grafico.varias.lineas.recession <- function(df,xaxis,yaxis,groupvar,xlabel,ylabel,graphname,ylim=NULL,xticks=NULL,
                                            lwidth=0.3,fsize=15){
  
  if (is.null(xticks)){
    xticks <- waiver()
    minticks <- NULL
    maxticks <- NULL
  } else {
    minticks <- min(xticks)
    maxticks <- max(xticks)
  }
  
  y <- ggplot(df,aes(x=get(xaxis,as.environment(df)),
                     y=get(yaxis,as.environment(df)),
                     colour=get(groupvar,as.environment(df)),
                     group=get(groupvar,as.environment(df)))) +
    geom_line() +
    geom_rect(aes(xmin=1969.9,xmax=1970.9,ymin=-Inf,ymax=Inf),linetype="blank",alpha=0.2,fill="grey75") +
    geom_rect(aes(xmin=1973.9,xmax=1975.2,ymin=-Inf,ymax=Inf),linetype="blank",alpha=0.2,fill="grey75") +
    geom_rect(aes(xmin=1980.0,xmax=1980.5,ymin=-Inf,ymax=Inf),linetype="blank",alpha=0.2,fill="grey75") +
    geom_rect(aes(xmin=1981.6,xmax=1982.9,ymin=-Inf,ymax=Inf),linetype="blank",alpha=0.2,fill="grey75") +
    geom_rect(aes(xmin=1990.6,xmax=1991.2,ymin=-Inf,ymax=Inf),linetype="blank",alpha=0.2,fill="grey75") +
    geom_rect(aes(xmin=2001.2,xmax=2001.9,ymin=-Inf,ymax=Inf),linetype="blank",alpha=0.2,fill="grey75") +
    geom_rect(aes(xmin=2007.9,xmax=2009.6,ymin=-Inf,ymax=Inf),linetype="blank",alpha=0.2,fill="grey75") +
    scale_x_continuous(name=xlabel,expand=c(0,0),breaks=xticks,labels=xticks,lim=c(minticks,
                                                                                   maxticks)) +
    scale_y_continuous(name=ylabel,lim=ylim) +
    theme_light() + geom_line(size=lwidth) +
    theme(axis.text=element_text(size=fsize,face="bold"),
          axis.title=element_text(size=fsize,face="bold"),
          legend.position="bottom",legend.title=element_blank(),
          legend.text=element_text(size=fsize))+
    guides(color=guide_legend(override.aes=list(fill=NA))) +
    theme(plot.margin=unit(c(0.25,0.4,0.25,0.25),"cm"))
  
  ggsave(filename=paste(folder,graphname,sep=""),plot=y,width=8,height=6)
  
  return(y)
}

r1 <- subset(pti.psid,select=c("year","med","trendmed"))
r1 <- melt(r1,id="year")
r1$variable <- recode(r1$variable,"med"="Data","trendmed"="Trend")

datatrend2 <- grafico.varias.lineas.recession(r1,"year","value","variable","year","PTI","Figure2a.pdf",
                                              NULL,NULL,1.2,20)


# returns on PSID hprice
r3 <- subset(rets,select=c("year","fhfa.return","sp500"))
r3 <- subset(r3,year>1967)
r3 <- melt(r3,id="year")
r3$variable <- recode(r3$variable,"fhfa.return"="Housing","sp500"="Stocks")

g7 <- grafico.varias.lineas.recession(r3,"year","value","variable","year",
                                      "Annual change","Figure2b.pdf",NULL,NULL,1.2,20)


# In-text numbers in Section IV.B.

## Average return on mortgages  - 4%
mean(fr.mortgages$realrb)
## Average return on risk-free assets - 1%
mean(real.pp.return.tbill)
## Twice median household income in 2015
2*median(psid.all$NDlabtotal[psid.all$year==2016
                             & psid.all$age>24 & psid.all$age<66],na.rm=T)



### INPUTS FOR MODEL


# BUILD STATE MATRIX FOR THE COHORTS CASE

# NOTICE now I redefine stock as the two years before that one

bi.real.rets <- subset(real.rets,select=c("year","sp500"))
bi.real.rets[, splead := shift(sp500,type="lag",n=1)]
bi.real.rets$bi <- (1+bi.real.rets$sp500)*(1+bi.real.rets$splead)-1

bi.fstate <- subset(bi.real.rets,select=c("year","bi"))
bi.fstate.q <- quantile(bi.real.rets$bi,c(0.05,1/3,2/3),na.rm=T)

bi.fstate$highstock <- NA
bi.fstate$highstock[bi.fstate$bi<bi.fstate.q[1]] <- 0
bi.fstate$highstock[bi.fstate$bi>bi.fstate.q[1] & bi.fstate$bi<bi.fstate.q[2]] <- 1
bi.fstate$highstock[bi.fstate$bi>bi.fstate.q[2] & bi.fstate$bi<bi.fstate.q[3]] <- 2
bi.fstate$highstock[bi.fstate$bi>bi.fstate.q[3]] <- 3
bi.fstate <- subset(bi.fstate,is.na(bi)==0)

bi.media.fstates <- bi.fstate[,mean(bi),by=highstock] # median is similar
bi.media.fstates <- rename(bi.media.fstates,c("modelstock"="V1"))

# Save stock returns by state for model
setkeyv(bi.media.fstates,"modelstock")
if (dowrite==1) {
write.table(bi.media.fstates$modelstock,file=paste0(folder.model,"rf_values.csv"),row.names=F,col.names=F)
}


# The bi-yearly recession
# Most recession span several year, so can proceed in same way

bi.onlyrec.trm <- subset(a.state2,select=c("year","recession"))
bi.onlyrec.trm <- subset(bi.onlyrec.trm,is.na(recession)==0)
bi.onlyrec.trm[,leadrec:=shift(recession,type="lead",n=2)]
bi.onlyrec.trm <- subset(bi.onlyrec.trm,is.na(leadrec)==0)
bi.oh.y2 <- table(bi.onlyrec.trm$recession,bi.onlyrec.trm$leadrec)
bi.ohjoint.y2 <- prop.table(bi.oh.y2,margin=1)
bi.oh.yabs <- rowSums(prop.table(bi.oh.y2))

# This for two-year now

bi.astate <- merge(bi.fstate,subset(a.state2,select=c("year","recession")))


# Bi-yearly GROWTH process for house prices

h.state[, twolag := shift(PTI,type="lag",n=2)]
h.state$growth <- (h.state$PTI - h.state$twolag)/h.state$twolag
# note this works fine because h.state has all of the years, even though there are
# NAs in PSID

h.state.gn <- subset(h.state,year>1960)

h.state$highgrowth <- NA
h.state$highgrowth[h.state$growth>h.state.gn[,mean(growth,na.rm=T)]] <- 1
h.state$highgrowth[h.state$growth<=h.state.gn[,mean(growth,na.rm=T)]] <- 0

bi.astate.growth <- bi.astate
bi.astate.growth <- merge(bi.astate.growth,subset(h.state,is.even(year)==1),
                          by="year")
bi.astate.growth$state <- 8*bi.astate.growth$recession+4*bi.astate.growth$highgrowth+bi.astate.growth$highstock+1 # PLUS ONE IMPORTANT

bi.astate.growth.export <- bi.astate.growth$state

# Save realizations of hre
if (dowrite==1) {
  write(bi.astate.growth.export, file=paste0(folder.wk,"hre_bi_data11.csv"),sep="\n")
  #save(bi.astate.growth,file=paste0(folder.wk,"bi_astate_growth.RData"))
}


# Now compute the transition matrix (Qhre)

# Find joint probabilities of recession+stock mkt outcomes
bi.r1 <- bi.astate[,list(both=length(year)),by=c("recession","highstock")]
bi.r2 <- bi.astate[,list(uno=length(year)),by=c("recession")]
bi.rc1 <- merge(bi.r1,bi.r2,by="recession",all.x=T)
setkeyv(bi.rc1,c("recession","highstock"))
bi.rc1$prob1 <- bi.rc1$both/bi.rc1$uno
bi.rc1.only <- subset(bi.rc1,select=c("recession","highstock","prob1"))
bi.o1.only <- as.matrix(bi.rc1.only$prob1)
dim(bi.o1.only) <- c(4,2)
bi.o1.exp <- t(cbind(bi.o1.only[,1],bi.o1.only[,1],bi.o1.only[,1],bi.o1.only[,1]))
bi.o1.rec <- t(cbind(bi.o1.only[,2],bi.o1.only[,2],bi.o1.only[,2],bi.o1.only[,2]))


bi.onlygrowth.trm <- subset(h.state,select=c("year","highgrowth"))
bi.onlygrowth.trm <- subset(bi.onlygrowth.trm,is.na(highgrowth)==0)
bi.onlygrowth.trm[,leadgrowth:=shift(highgrowth,type="lead",n=2)]
bi.onlygrowth.trm <- subset(bi.onlygrowth.trm,is.na(leadgrowth)==0)
bi.ohg.t2 <- table(bi.onlygrowth.trm$highgrowth,bi.onlygrowth.trm$leadgrowth)
bi.ohjointg.t2 <- prop.table(bi.ohg.t2,margin=1)
bi.ohjointg.abs <- rowSums(prop.table(bi.ohg.t2))

bi.c.matrix4 <- rbind(cbind(bi.ohjoint.y2[1,1]*rbind(cbind(bi.ohjointg.t2[1,1]*bi.o1.exp,bi.ohjointg.t2[1,2]*bi.o1.exp),
                                                     cbind(bi.ohjointg.t2[2,1]*bi.o1.exp,bi.ohjointg.t2[2,2]*bi.o1.exp)),
                            bi.ohjoint.y2[1,2]*rbind(cbind(bi.ohjointg.t2[1,1]*bi.o1.rec,bi.ohjointg.t2[1,2]*bi.o1.rec),
                                                     cbind(bi.ohjointg.t2[2,1]*bi.o1.rec,bi.ohjointg.t2[2,2]*bi.o1.rec))),
                      cbind(bi.ohjoint.y2[2,1]*rbind(cbind(bi.ohjointg.t2[1,1]*bi.o1.exp,bi.ohjointg.t2[1,2]*bi.o1.exp),
                                                     cbind(bi.ohjointg.t2[2,1]*bi.o1.exp,bi.ohjointg.t2[2,2]*bi.o1.exp)),
                            bi.ohjoint.y2[2,2]*rbind(cbind(bi.ohjointg.t2[1,1]*bi.o1.rec,bi.ohjointg.t2[1,2]*bi.o1.rec),
                                                     cbind(bi.ohjointg.t2[2,1]*bi.o1.rec,bi.ohjointg.t2[2,2]*bi.o1.rec))))

if (dowrite==1) {
write(bi.c.matrix4,file=paste0(folder.wk,"Qhre_bi_data11_new.csv"),sep="\n")
}



# And we now also need Qh
# Save h_growths in each of the two housing states to build Qh in Matlab

gstates2 <- h.state[,median(growth),by="highgrowth"]
gstates2 <- subset(gstates2,is.na(highgrowth)==0)
setkeyv(gstates2,"highgrowth")
if (dowrite==1) {
 write(gstates2$V1,file=paste0(folder.wk,"h_growths12.csv"),sep="\n") 
}


hsebuilder <- h.state.g4
hsebuilder.prev <- hsebuilder$PTI[1:6]
write.table(hsebuilder.prev,file=paste0(folder.wk,"hsebuilder_prev11.csv"),row.names=F,
            col.names=F)






## ROBUSTNESS TO STOCK RETURNS
# Information in Appendix F.4.
# and inputs that relate to those robustness checks


# Probability of staying in a recession after two years

bi.ohjoint.y2[1,1]

# How much of the variance in stock returns can be explained by exp/rec

rub1 <- merge(bi.astate.growth,real.rets,by="year")
reg1 <- lm(sp500 ~ recession,data=rub1)
1-summary(reg1)$r.squared

# Reduction in probability of recession (16 percentage points)

bi.ohjoint.y2[2,2]-bi.ohjoint.y2[1,2]

# Growth of house prices

mean(real.rets$psid.hprice[is.even(real.rets$year)==1],na.rm=T)



## Robustness checks on Qhre (Appendix F.4.)


# bi.c.matrix4
# Average across possible states...
# bi.oh.yabs for expansions
# bi.ohjointg.abs for house prices

s.matrix1 <- bi.c.matrix4[,1:4]+bi.c.matrix4[,5:8]+bi.c.matrix4[,9:12]+
  bi.c.matrix4[,13:16]

# # Only expansions- recessions matter

s.unconditional <- bi.oh.yabs[1]*s.matrix1[1,]+
    bi.oh.yabs[2]*s.matrix1[9,]

# # Add a counterfactual in which stock market returns do not depend
# # on expansions-recessions

s.uncond <- t(matrix(rep(s.unconditional,4),4,4))

bi.c.matrix8 <- rbind(cbind(bi.ohjoint.y2[1,1]*rbind(cbind(bi.ohjointg.t2[1,1]*s.uncond,bi.ohjointg.t2[1,2]*s.uncond),
                                                     cbind(bi.ohjointg.t2[2,1]*s.uncond,bi.ohjointg.t2[2,2]*s.uncond)),
                            bi.ohjoint.y2[1,2]*rbind(cbind(bi.ohjointg.t2[1,1]*s.uncond,bi.ohjointg.t2[1,2]*s.uncond),
                                                     cbind(bi.ohjointg.t2[2,1]*s.uncond,bi.ohjointg.t2[2,2]*s.uncond))),
                      cbind(bi.ohjoint.y2[2,1]*rbind(cbind(bi.ohjointg.t2[1,1]*s.uncond,bi.ohjointg.t2[1,2]*s.uncond),
                                                     cbind(bi.ohjointg.t2[2,1]*s.uncond,bi.ohjointg.t2[2,2]*s.uncond)),
                            bi.ohjoint.y2[2,2]*rbind(cbind(bi.ohjointg.t2[1,1]*s.uncond,bi.ohjointg.t2[1,2]*s.uncond),
                                                     cbind(bi.ohjointg.t2[2,1]*s.uncond,bi.ohjointg.t2[2,2]*s.uncond))))

if (dowrite==1) {
  write(bi.c.matrix8,file=paste0(folder.wk,"Qhre_bi_data8_new.csv"),sep="\n")
}

# Add a counterfactual in which recessions+expansions are "iid" 

yprobs.iid <- prop.table(table(bi.astate.growth$recession))


bi.c.matrix9 <- rbind(cbind(yprobs.iid[1]*rbind(cbind(bi.ohjointg.t2[1,1]*bi.o1.exp,bi.ohjointg.t2[1,2]*bi.o1.exp),
                                                cbind(bi.ohjointg.t2[2,1]*bi.o1.exp,bi.ohjointg.t2[2,2]*bi.o1.exp)),
                            yprobs.iid[2]*rbind(cbind(bi.ohjointg.t2[1,1]*bi.o1.rec,bi.ohjointg.t2[1,2]*bi.o1.rec),
                                                cbind(bi.ohjointg.t2[2,1]*bi.o1.rec,bi.ohjointg.t2[2,2]*bi.o1.rec))),
                      cbind(yprobs.iid[1]*rbind(cbind(bi.ohjointg.t2[1,1]*bi.o1.exp,bi.ohjointg.t2[1,2]*bi.o1.exp),
                                                cbind(bi.ohjointg.t2[2,1]*bi.o1.exp,bi.ohjointg.t2[2,2]*bi.o1.exp)),
                            yprobs.iid[2]*rbind(cbind(bi.ohjointg.t2[1,1]*bi.o1.rec,bi.ohjointg.t2[1,2]*bi.o1.rec),
                                                cbind(bi.ohjointg.t2[2,1]*bi.o1.rec,bi.ohjointg.t2[2,2]*bi.o1.rec))))

if (dowrite==1) {
  write(bi.c.matrix9,file=paste0(folder.wk,"Qhre_bi_data9_new.csv"),sep="\n")
}

# 
# # Add a fully iid counterfactual

# yprobs.iid is unconditional pbbs of expansions-recessions
# s.uncond is unconditional pbbs of stock market returns
# bi.ohjointg.abs is unconditional pbbs of house price growth


bi.c.matrix10 <- rbind(cbind(yprobs.iid[1]*rbind(cbind(bi.ohjointg.abs[1]*s.uncond,bi.ohjointg.abs[2]*s.uncond),
                                                     cbind(bi.ohjointg.abs[1]*s.uncond,bi.ohjointg.abs[2]*s.uncond)),
                            yprobs.iid[2]*rbind(cbind(bi.ohjointg.abs[1]*s.uncond,bi.ohjointg.abs[2]*s.uncond),
                                                     cbind(bi.ohjointg.abs[1]*s.uncond,bi.ohjointg.abs[2]*s.uncond))),
                      cbind(yprobs.iid[1]*rbind(cbind(bi.ohjointg.abs[1]*s.uncond,bi.ohjointg.abs[2]*s.uncond),
                                                     cbind(bi.ohjointg.abs[1]*s.uncond,bi.ohjointg.abs[2]*s.uncond)),
                            yprobs.iid[2]*rbind(cbind(bi.ohjointg.abs[1]*s.uncond,bi.ohjointg.abs[2]*s.uncond),
                                                     cbind(bi.ohjointg.abs[1]*s.uncond,bi.ohjointg.abs[2]*s.uncond))))

if (dowrite==1) {
  write(bi.c.matrix10,file=paste0(folder.wk,"Qhre_bi_data10_new.csv"),sep="\n")
}


# Add a case with no autocorrelation in hprice growth 


bi.c.matrix7 <- rbind(cbind(bi.ohjoint.y2[1,1]*rbind(cbind(bi.ohjointg.abs[1]*bi.o1.exp,bi.ohjointg.abs[2]*bi.o1.exp),
                                                     cbind(bi.ohjointg.abs[1]*bi.o1.exp,bi.ohjointg.abs[2]*bi.o1.exp)),
                            bi.ohjoint.y2[1,2]*rbind(cbind(bi.ohjointg.abs[1]*bi.o1.rec,bi.ohjointg.abs[2]*bi.o1.rec),
                                                     cbind(bi.ohjointg.abs[1]*bi.o1.rec,bi.ohjointg.abs[2]*bi.o1.rec))),
                      cbind(bi.ohjoint.y2[2,1]*rbind(cbind(bi.ohjointg.abs[1]*bi.o1.exp,bi.ohjointg.abs[2]*bi.o1.exp),
                                                     cbind(bi.ohjointg.abs[1]*bi.o1.exp,bi.ohjointg.abs[2]*bi.o1.exp)),
                            bi.ohjoint.y2[2,2]*rbind(cbind(bi.ohjointg.abs[1]*bi.o1.rec,bi.ohjointg.abs[2]*bi.o1.rec),
                                                     cbind(bi.ohjointg.abs[1]*bi.o1.rec,bi.ohjointg.abs[2]*bi.o1.rec))))

if (dowrite==1) {
  write(bi.c.matrix7,file=paste0(folder.wk,"Qhre_bi_data7_new.csv"),sep="\n")
}



# Figure E.1., left 

f.state.forgraph <- subset(bi.astate.growth,select=c("year","bi","highstock"))
f.state.forgraph <- merge(f.state.forgraph,bi.media.fstates,by="highstock",all.x=T)
f.state.forgraph <- subset(f.state.forgraph,select=c("year","bi","modelstock"))
f.state.forgraph <- melt(f.state.forgraph,id="year")
f.state.forgraph$variable <- plyr::mapvalues(f.state.forgraph$variable,from=c("bi","modelstock"),to=c("Data","Model"))

g6 <- grafico.varias.lineas(f.state.forgraph,"year","value","variable","year","Stock returns",
                            "FigureE1a.pdf")
