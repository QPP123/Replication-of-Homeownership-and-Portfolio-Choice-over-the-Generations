# scf_participation_actual.R

# This code extracts statistics from the SCFs 1989 onwards 
# Produces directly some figures and saves some others
# as csv for files produced in Matlab

# Stock market participation
#   - Figure 1, bottom right
#   - Figure 4, bottom left, data
#   - Figure 8, data

# Wealth accumulation
#   - Figure 9 
#   - Figure 10, data

# Direct stock market participation
#   - Figure C.17.

rm(list=setdiff(ls(), c("main.folder","da.folder")))

source(paste0(main.folder,"code/data_analysis/functions/gon_graphs.R"))
data.path <- paste0(main.folder,"data/")
load(paste0(data.path,"SCFSample.RData"))
load(file=paste0(da.folder,'functions/scf_functions.R'))

folder <- paste0(main.folder,"results/Figures/")
folder.to <- paste0(main.folder,"results/Values/")
folder.old <- paste0(main.folder,"working/") 
folder.model <- paste0(main.folder,"working/Model/")

scf.design.1989 <- scf.design("1989")
scf.design.1992 <- scf.design("1992")
scf.design.1995 <- scf.design("1995")
scf.design.1998 <- scf.design("1998")
scf.design.2001 <- scf.design("2001")
scf.design.2004 <- scf.design("2004")
scf.design.2007 <- scf.design("2007")
scf.design.2010 <- scf.design("2010")
scf.design.2013 <- scf.design("2013")
scf.design.2016 <- scf.design("2016")


scf.design.all <- list(scf.design.1989,scf.design.1992,scf.design.1995,
                       scf.design.1998,scf.design.2001,scf.design.2004,
                       scf.design.2007,scf.design.2010,scf.design.2013,
                       scf.design.2016)

w35.1960 <- subset(scf.design.1995,age>29 & age<41)
w35.1980 <- subset(scf.design.2016,age>29 & age<41)

scf.design.all[1:5] <- lapply(scf.design.all[1:5],function(x) update(x,
                          hbroadstocks=as.numeric(stocks>0 | (x3601==1 & x3631 %in% c(2,4,5))  | 
                                        (x3819==1 & x3821==1) | (x4135==1 & x4234 %in% c(1,3)) | 
                                        (x4735==1 & x4834 %in% c(1,3)))))

scf.design.all[6:7] <- lapply(scf.design.all[6:7], function(x) update(x,hbroadstocks=as.numeric(stocks>0| (x3601==1 & (x6555==1 | x6563==1 | x6571==1 )) |
                                     (x3819==1 & x3821==1) | (x4135==1 & (x11036 %in% c(1,3) | x11136 %in% c(1,3) | x11236 %in% c(1,3)))
                                  |  (x4735==1 & (x11336 %in% c(1,3) | x11436 %in% c(1,3) | x11536 %in% c(1,3))) )))
                              
scf.design.all[8:10] <- lapply(scf.design.all[8:10], function(x) update(x,hbroadstocks=as.numeric(stocks>0| (x3601==1 & (x6555==1 | x6563==1 | x6571==1 )) |
                                       (x3819==1 & x3821==1) | (x4135==1 & (x11036 %in% c(1,3) | x11136 %in% c(1,3)))
                                       |  (x4735==1 & (x11336 %in% c(1,3) | x11436 %in% c(1,3)) ))))

scf.design.all[5:10] <- lapply(scf.design.all[5:10], function(x) update(x,everbkr=(x6772==1)))


scf.design.1989 <- scf.design.all[[1]]
scf.design.1992 <- scf.design.all[[2]]
scf.design.1995 <- scf.design.all[[3]]
scf.design.1998 <- scf.design.all[[4]]
scf.design.2001 <- scf.design.all[[5]]
scf.design.2004 <- scf.design.all[[6]]
scf.design.2007 <- scf.design.all[[7]]
scf.design.2010 <- scf.design.all[[8]]
scf.design.2013 <- scf.design.all[[9]]
scf.design.2016 <- scf.design.all[[10]]


scf.design.1989.mycoh <- subset(scf.design.1989,age>44 & age<50)
scf.design.1992.mycoh <- subset(scf.design.1992,age>47 & age<53)
scf.design.1995.mycoh <- subset(scf.design.1995,age>50 & age<56)
scf.design.1998.mycoh <- subset(scf.design.1998,age>53 & age<59)
scf.design.2001.mycoh <- subset(scf.design.2001,age>56 & age<62)
scf.design.2004.mycoh <- subset(scf.design.2004,age>59 & age<65)
scf.design.2007.mycoh <- subset(scf.design.2007,age>62 & age<68)
scf.design.2010.mycoh <- subset(scf.design.2010,age>65 & age<71)
scf.design.2013.mycoh <- subset(scf.design.2013,age>68 & age<74)
scf.design.2016.mycoh <- subset(scf.design.2016,age>71 & age<77)

scf.design.1989.mycoh2 <- subset(scf.design.1989,age>24 & age<30)
scf.design.1992.mycoh2 <- subset(scf.design.1992,age>27 & age<33)
scf.design.1995.mycoh2 <- subset(scf.design.1995,age>30 & age<36)
scf.design.1998.mycoh2 <- subset(scf.design.1998,age>33 & age<39)
scf.design.2001.mycoh2 <- subset(scf.design.2001,age>36 & age<42)
scf.design.2004.mycoh2 <- subset(scf.design.2004,age>39 & age<45)
scf.design.2007.mycoh2 <- subset(scf.design.2007,age>42 & age<48)
scf.design.2010.mycoh2 <- subset(scf.design.2010,age>45 & age<51)
scf.design.2013.mycoh2 <- subset(scf.design.2013,age>48 & age<54)
scf.design.2016.mycoh2 <- subset(scf.design.2016,age>51 & age<57)

scf.design.2004.mycoh3 <- subset(scf.design.2004,age>19 & age<25)
scf.design.2007.mycoh3 <- subset(scf.design.2007,age>22 & age<28)
scf.design.2010.mycoh3 <- subset(scf.design.2010,age>25 & age<31)
scf.design.2013.mycoh3 <- subset(scf.design.2013,age>28 & age<34)
scf.design.2016.mycoh3 <- subset(scf.design.2016,age>31 & age<37)

scf.design.mycoh <- list(scf.design.1989.mycoh,
                         scf.design.1992.mycoh,scf.design.1995.mycoh,
                         scf.design.1998.mycoh,scf.design.2001.mycoh,
                         scf.design.2004.mycoh,scf.design.2007.mycoh,
                         scf.design.2010.mycoh,scf.design.2013.mycoh,
                         scf.design.2016.mycoh)

scf.design.mycoh2 <- list(scf.design.1989.mycoh2,scf.design.1992.mycoh2,
                          scf.design.1995.mycoh2,scf.design.1998.mycoh2,
                          scf.design.2001.mycoh2,scf.design.2004.mycoh2,
                          scf.design.2007.mycoh2,scf.design.2010.mycoh2,
                          scf.design.2013.mycoh2,scf.design.2016.mycoh2)

scf.design.mycoh3 <- list(scf.design.2004.mycoh3,scf.design.2007.mycoh3,
                          scf.design.2010.mycoh3,scf.design.2013.mycoh3,
                          scf.design.2016.mycoh3)

## COMPUTE STOCK MARKET PARTICIPATION
# This is Figure 1, bottom right
# PLUS exporting data for Figures 4 and 8

stockp.mycoh <- lapply(scf.design.mycoh,function(x) scf.MIcombine(with(x,
                          svymean(~hbroadstocks))))
stockp.mycoh2 <- lapply(scf.design.mycoh2, function(x) scf.MIcombine(with(x,
                          svymean(~hbroadstocks))))
stockp.mycoh3 <- lapply(scf.design.mycoh3, function(x) scf.MIcombine(with(x,
                          svymean(~hbroadstocks))))

n1 <- sapply(stockp.mycoh,function(x) return(coefficients(x)))
n2 <- sapply(stockp.mycoh,function(x) return(x$variance))

n12 <- sapply(stockp.mycoh2,function(x) return(coefficients(x)))
n22 <- sapply(stockp.mycoh2,function(x) return(x$variance))

n13 <- sapply(stockp.mycoh3,function(x) return(coefficients(x)))
n23 <- sapply(stockp.mycoh3,function(x) return(x$variance))


stockp.export <- as.data.table(n1)
stockp.export$age <- seq(47,74,by=3)
stockp.export$sd <- sqrt(n2)

stockp.export2 <- as.data.table(n12)
stockp.export2$age <- seq(27,54,by=3)
stockp.export2$sd <- sqrt(n22)

stockp.export3 <- as.data.table(n13)
stockp.export3$age <- seq(22,34,by=3)
stockp.export3$sd <- sqrt(n23)

# Load data from scf_pre80
old.data <- read.csv(paste0(folder.model,"stockp_data_oldscf.csv"),sep=",")

stockp.exportb <- stockp.export
stockp.exportb$cohort <- 1
stockp.export2b <- stockp.export2
stockp.export2b$cohort <- 2
stockp.export3b <- stockp.export3
stockp.export3b$cohort <- 3

old.data$cohort <- 1
old.data <- rename(old.data,c("n1"="piece","sd"="std"))
stockp.exportb <- rbind(stockp.exportb,old.data)
stockp.export2b <- rename(stockp.export2b,c("n1"="n12"))
stockp.export3b <- rename(stockp.export3b,c("n1"="n13"))

stockp.exportb <- rbind(stockp.exportb,stockp.export2b)
stockp.exportb <- rbind(stockp.exportb,stockp.export3b)

stockp.graph <- subset(stockp.exportb,select=c("n1","age","cohort"))

stockp.graph$cohort <- as.factor(stockp.graph$cohort)
stockp.graph$cohort <- recode(stockp.graph$cohort,"1"="1940","2"="1960","3"="1980")
                              
m1 <- grafico.varias.lineas3(stockp.graph,"age","n1","cohort","Age","Stock market participation",
                            "Figure1f.pdf",NULL,seq(25,60,5),1.2,20)


write.table(stockp.export,file=paste0(folder.model,"newscf_sd.csv"),row.names=F,sep=",")
write.table(stockp.export2,file=paste0(folder.model,"newscf_sd2.csv"),row.names=F,sep=",")
write.table(stockp.export3,file=paste0(folder.model,"newscf_sd3.csv"),row.names=F,sep=",")


# FIGURE C.17 - DIRECT STOCK MARKET PARTICIPATION 


stockpd.mycoh <- lapply(scf.design.mycoh,function(x) scf.MIcombine(with(x,
                                                                        svymean(~hstocks))))
stockpd.mycoh2 <- lapply(scf.design.mycoh2,function(x) scf.MIcombine(with(x,
                                                                          svymean(~hstocks))))
stockpd.mycoh3 <- lapply(scf.design.mycoh3,function(x) scf.MIcombine(with(x,
                                                                          svymean(~hstocks))))

n1d <- sapply(stockpd.mycoh,function(x) return(coefficients(x)))

n12d <- sapply(stockpd.mycoh2,function(x) return(coefficients(x)))

n13d <- sapply(stockpd.mycoh3,function(x) return(coefficients(x)))

stockpd.export <- as.data.table(n1d)
stockpd.export$age <- seq(47,74,by=3)

stockpd.export2 <- as.data.table(n12d)
stockpd.export2$age <- seq(27,54,by=3)

stockpd.export3 <- as.data.table(n13d)
stockpd.export3$age <- seq(22,34,by=3)

stockpd.exportb <- stockpd.export
stockpd.exportb$cohort <- 1
stockpd.export2b <- stockpd.export2
stockpd.export2b$cohort <- 2
stockpd.export3b <- stockpd.export3
stockpd.export3b$cohort <- 3

stockpd.export2b <- rename(stockpd.export2b,c("n1d"="n12d"))
stockpd.export3b <- rename(stockpd.export3b,c("n1d"="n13d"))

stockpd.exportb <- rbind(stockpd.exportb,stockpd.export2b)
stockpd.exportb <- rbind(stockpd.exportb,stockpd.export3b)

stockpd.graph <- subset(stockpd.exportb,select=c("n1d","age","cohort"))

stockpd.graph$cohort <- as.factor(stockpd.graph$cohort)
stockpd.graph$cohort <- recode(stockpd.graph$cohort,"1"="1940","2"="1960",
                               "3"="1980")

m1 <- grafico.varias.lineas3(stockpd.graph,"age","n1d","cohort","Age","Stock market participation",
                             "FigureC17.pdf",NULL,NULL,1.2,20)




## DATA FOR FIGURE 9 
# Distribution of wealth at age 35

# WEALTH DISTRIBUTIONS at age 35
#w35.1960, w35.1980

qt.35.60 <- scf.MIcombine( 
  with(w35.1960 , svyby( ~networth , ~one , svyquantile , 
                         c(seq(0,1,0.01)) , ci = FALSE , method = 'constant' ,interval.type = 'quantile' ) ) )
w35.1960 <- update(w35.1960,wealthpc=.bincode(networth,breaks=qt.35.60$coefficients))

avg.inc.1995 <- scf.MIcombine(with(scf.design.1995,svymean(~income)))
w35.1960 <- update(w35.1960,normwealth=networth/avg.inc.1995$coefficients[1])
w35.1960 <- update(w35.1960,normhassets=(houses+oresre+nnresre)/avg.inc.1995$coefficients[1])
w35.1960 <- update(w35.1960,normhwealth=(houses+oresre+nnresre-mrthel)/avg.inc.1995$coefficients[1])
w35.1960 <- update(w35.1960,normfwealth=(normwealth-normhwealth))
w35.1960 <- update(w35.1960,normfassets=(normfwealth+(debt-mrthel)/avg.inc.1995$coefficients[1]))

cc4 <- scf.MIcombine(with(w35.1960,svyby(~normwealth,~wealthpc,svyquantile,0.5)))
cc4b <- scf.MIcombine(with(w35.1960,svyby(~normhassets,~wealthpc,svyquantile,0.5)))
cc4c <- scf.MIcombine(with(w35.1960,svyby(~normhwealth,~wealthpc,svyquantile,0.5)))
cc4d <- scf.MIcombine(with(w35.1960,svyby(~normfwealth,~wealthpc,svyquantile,0.5)))
cc4e <- scf.MIcombine(with(w35.1960,svyby(~normfassets,~wealthpc,svyquantile,0.5)))

medw.bypercentile60 <- as.data.table(cbind(seq(1,99),cc4$coefficients,cc4b$coefficients,
                                           cc4c$coefficients,cc4d$coefficients,
                                           cc4e$coefficients))
setkeyv(medw.bypercentile60,"V1")

write.table(medw.bypercentile60,file=paste0(folder.model,"wealthdtb_35_1995.csv"),row.names=F,col.names=F,sep=",")


qt.35.80 <- scf.MIcombine( 
  with(w35.1980 , svyby( ~networth , ~one , svyquantile , 
                         c(seq(0,1,0.01)) , ci = FALSE , method = 'constant' ,interval.type = 'quantile' ) ) )
w35.1980 <- update(w35.1980,wealthpc=.bincode(networth,breaks=qt.35.80$coefficients))


avg.inc.2016 <- scf.MIcombine(with(scf.design.2016,svymean(~income)))
w35.1980 <- update(w35.1980,normwealth=networth/avg.inc.2016$coefficients[1])
w35.1980 <- update(w35.1980,normhassets=(houses+oresre+nnresre)/avg.inc.2016$coefficients[1])
w35.1980 <- update(w35.1980,normhwealth=(houses+oresre+nnresre-mrthel)/avg.inc.2016$coefficients[1])
w35.1980 <- update(w35.1980,normfwealth=(normwealth-normhwealth))
w35.1980 <- update(w35.1980,normfassets=(normfwealth+(debt-mrthel)/avg.inc.2016$coefficients[1]))

bb4 <- scf.MIcombine(with(w35.1980,svyby(~normwealth,~wealthpc,svyquantile,0.5)))
bb4b <- scf.MIcombine(with(w35.1980,svyby(~normhassets,~wealthpc,svyquantile,0.5)))
bb4c <- scf.MIcombine(with(w35.1980,svyby(~normhwealth,~wealthpc,svyquantile,0.5)))
bb4d <- scf.MIcombine(with(w35.1980,svyby(~normfwealth,~wealthpc,svyquantile,0.5)))
bb4e <- scf.MIcombine(with(w35.1980,svyby(~normfassets,~wealthpc,svyquantile,0.5)))

medw.bypercentile <- as.data.table(cbind(seq(1,100),bb4$coefficients,bb4b$coefficients,
                                         bb4c$coefficients,bb4d$coefficients,
                                         bb4e$coefficients))
setkeyv(medw.bypercentile,"V1")

write.table(medw.bypercentile,file=paste0(folder.model,"wealthdtb_35_2016.csv"),row.names=F,col.names=F,sep=",")



### DATA FOR FIGURE 10 (Average and median wealth over cohorts)

scf.aveinc <- lapply(scf.design.all,function(x) scf.MIcombine( with(x, svymean(~income))))
scf.avew <- lapply(scf.design.mycoh,function(x) scf.MIcombine( with(x, svymean(~networth))))
scf.avew2 <- lapply(scf.design.mycoh2,function(x) scf.MIcombine( with(x, svymean(~networth))))
scf.avew3 <- lapply(scf.design.mycoh3,function(x) scf.MIcombine( with(x, svymean(~networth))))
scf.medw <- lapply(scf.design.mycoh,function(x) scf.MIcombine( with(x, svyquantile(~networth,0.5))))
scf.medw2 <- lapply(scf.design.mycoh2,function(x) scf.MIcombine( with(x, svyquantile(~networth,0.5))))
scf.medw3 <- lapply(scf.design.mycoh3,function(x) scf.MIcombine( with(x, svyquantile(~networth,0.5))))
scf.q <- lapply(scf.design.all,function(x) scf.MIcombine( with(x, svyquantile(~income,0.95))))

scf.restrict <- lapply(seq_along(scf.design.all),function(i) subset(scf.design.all[[i]],
                                                  income<as.numeric(coefficients(scf.q[[i]]))))

scf.aveinc.95 <- lapply(scf.restrict,function(x) scf.MIcombine( with(x,svymean(~income))))                                                

medwtoy <- sapply(scf.medw,function(x) x$coefficients)/sapply(scf.aveinc,function(x) x$coefficients)
medwtoy2 <- sapply(scf.medw2,function(x) x$coefficients)/sapply(scf.aveinc,function(x) x$coefficients)
medwtoy3 <- sapply(scf.medw3,function(x) x$coefficients)/sapply(scf.aveinc[6:10],function(x) x$coefficients)


mycoh.wq <- lapply(scf.design.mycoh,function(x) scf.MIcombine(with(x,svyquantile(~networth,0.95))))
scf.mycoh.avew.95 <- lapply(seq_along(scf.design.mycoh),
                                    function(i) { 
                                      y <- subset(scf.design.mycoh[[i]],networth<as.numeric(coefficients(mycoh.wq[[i]])))
                                      return(scf.MIcombine(with(y,svymean(~networth))))
                                    })
wtoy.95 <- sapply(scf.mycoh.avew.95,function(x) x$coefficients)/sapply(scf.aveinc.95,function(x) x$coefficients)

mycoh2.wq <- lapply(scf.design.mycoh2,function(x) scf.MIcombine(with(x,svyquantile(~networth,0.95))))
scf.mycoh2.avew.95 <- lapply(seq_along(scf.design.mycoh2),
                            function(i) { 
                              y <- subset(scf.design.mycoh2[[i]],networth<as.numeric(coefficients(mycoh2.wq[[i]])))
                              return(scf.MIcombine(with(y,svymean(~networth))))
                            })
wtoy2.95 <- sapply(scf.mycoh2.avew.95,function(x) x$coefficients)/sapply(scf.aveinc.95,function(x) x$coefficients)

mycoh3.wq <- lapply(scf.design.mycoh3,function(x) scf.MIcombine(with(x,svyquantile(~networth,0.95))))
scf.mycoh3.avew.95 <- lapply(seq_along(scf.design.mycoh3),
                             function(i) { 
                               y <- subset(scf.design.mycoh3[[i]],networth<as.numeric(coefficients(mycoh3.wq[[i]])))
                               return(scf.MIcombine(with(y,svymean(~networth))))
                             })
wtoy3.95 <- sapply(scf.mycoh3.avew.95,function(x) x$coefficients)/sapply(scf.aveinc.95[6:10],function(x) x$coefficients)


age2 <- c(47,50,53,56,59,62,65,68,71,74)

age3 <- seq(27,54,by=3)
age4 <- seq(22,34,by=3)


wtoy.data.oldscf <- as.data.table(wtoy.95)
wtoy.data.oldscf$age <- age2

write.table(wtoy.data.oldscf,file=paste0(folder.model,"wtoy_data_newscf_95.csv"),
            row.names=F,sep=",")

wtoy.data.oldscf <- as.data.table(medwtoy)
wtoy.data.oldscf$age <- age2

write.table(wtoy.data.oldscf,file=paste0(folder.model,"wtoy_data_newscf_med.csv"),
            row.names=F,sep=",")





wtoy.data.oldscf <- as.data.table(wtoy2.95)
wtoy.data.oldscf$age <- age3

write.table(wtoy.data.oldscf,file=paste0(folder.model,"wtoy_data_newscf_95_2.csv"),
            row.names=F,sep=",")

wtoy.data.oldscf <- as.data.table(medwtoy2)
wtoy.data.oldscf$age <- age3

write.table(wtoy.data.oldscf,file=paste0(folder.model,"wtoy_data_newscf_med_2.csv"),
            row.names=F,sep=",")





wtoy.data.oldscf <- as.data.table(wtoy3.95)
wtoy.data.oldscf$age <- age4

write.table(wtoy.data.oldscf,file=paste0(folder.model,"wtoy_data_newscf_95_3.csv"),
            row.names=F,sep=",")

wtoy.data.oldscf <- as.data.table(medwtoy3)
wtoy.data.oldscf$age <- age4

write.table(wtoy.data.oldscf,file=paste0(folder.model,"wtoy_data_newscf_med_3.csv"),
            row.names=F,sep=",")

## Bankruptcy measures for Table F.1. 

m1 <- lapply(scf.design.mycoh[5],function(x) scf.MIcombine(with(x,
                                                                svymean(~everbkr))))
m2 <- lapply(scf.design.mycoh2[5:10],function(x) scf.MIcombine(with(x,
                                                                    svymean(~everbkr))))
m3 <- lapply(scf.design.mycoh3,function(x) scf.MIcombine(with(x,
                                                              svymean(~everbkr))))

m1.coef <- sapply(m1,function(x) x$coefficients)
m2.coef <- sapply(m2,function(x) x$coefficients)
m3.coef <- sapply(m3,function(x) x$coefficients)

m1.var <- sapply(m1,function(x) x$variance[1])
m2.var <- sapply(m2,function(x) x$variance[1])
m3.var <- sapply(m3,function(x) x$variance[1])
