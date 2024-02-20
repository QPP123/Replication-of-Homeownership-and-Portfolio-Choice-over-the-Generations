# ipums_rev.R reproduces all of the results related to IPUMS
# Census data. In particular, these are Figures C.14, C.15, and C.16

# NOTE: To load data, you must download both the extract's data and the DDI
# to the "data" folder

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

source(paste0(main.folder,"code/data_analysis/functions/gon_graphs.R"))
folder.ipums.data <- paste0(main.folder,'data/')
folder.ipums.aux <- paste0(main.folder,'data/data_aux/')
folder <- paste0(main.folder,'results/Figures/')

if (file.exists(paste0(folder.ipums.data,'usa_00002.dat'))) {

ddi <- read_ipums_ddi(paste0(folder.ipums.aux,"usa_00002.xml"))
## SEE INFORMATION I USE FROM THE DATA in case it is needed for the download
ddi$var_info 

ddi2 <- read_ipums_ddi(paste0(folder.ipums.aux,"usa_00003.xml"))
ddi2$var_info 

data <- read_ipums_micro(ddi,data_file=paste0(folder.ipums.data,"usa_00002.dat"))
data2 <- read_ipums_micro(ddi2,data_file=paste0(folder.ipums.data,"usa_00003.dat"))


data$homeo <- NA
data$homeo[data$OWNERSHP==1] <- 1
data$homeo[data$OWNERSHP==2] <- 0

data$bdecade <- cut(data$BIRTHYR,breaks=seq(1920,1990,10),labels=seq(1920,1980,10))
data$bdecade2 <- cut(data$BIRTHYR,breaks=seq(1880,2000,20),labels=seq(1880,1990,20))

data <- as.data.table(data)

homeo.raw <- data[,wtd.mean(homeo,weights=HHWT,na.rm=T),by=c("AGE","bdecade")]

homeo.raw2 <- subset(homeo.raw,bdecade %in% c("1940","1960","1980"))
homeo.raw2$bdecade <- droplevels(homeo.raw2$bdecade)

# Figure C.14.
g1 <- grafico.varias.lineas(homeo.raw2,"AGE","V1","bdecade","Age",
                            "Homeownership rates","FigureC14.pdf",NULL,
                            seq(25,60,5))

year.check <- data[,wtd.mean(YEAR,weights=HHWT,na.rm=T),by=c("AGE","bdecade")]

# Find aggregate homeownership rates

agg.homeo <- data[,wtd.mean(homeo,weights=HHWT,na.rm=T),by=c("YEAR")]

# Number reported in main text:
mean(agg.homeo$V1)

# Figure C.15.

data.nona <- subset(data,is.na(homeo)==0)
m1 <- data.nona[,sum(HHWT,na.rm=T),by=c("YEAR","bdecade2","homeo")]
m2 <- data.nona[,list(V2=sum(HHWT,na.rm=T)),by=c("YEAR")]
m1$types <- m1$bdecade2
levels(m1$types) <- c(levels(m1$types),"Non-homeowner")
m1$types[m1$homeo==0] <- "Non-homeowner"

m3 <- m1[,sum(V1),by=c("YEAR,types")]
m3 <- merge(m3,m2,by="YEAR",all.x=T)

m3$V3 <- m3$V1/m3$V2

m4 <- expand.grid(YEAR=as.numeric(levels(as.factor(m3$YEAR))),
                  types=levels(m3$types))
m4 <- merge(m4,m3,by=c("YEAR","types"),all.x=T)
m4$V3[is.na(m4$V3)] <- 0

m4 <- subset(m4,types!="Non-homeowner")
m4$types <- droplevels(m4$types)
m4$`Birth period` <- m4$types
#m4$types <- relevel(m4$types,"Non-homeowner")

m4$`Birth period` <- plyr::mapvalues(m4$`Birth period`,
                               from=c(1880,1900,1920,1940,1960,1980),
                               to=c("1880-1900","1901-1920",
                                    "1921-1940","1941-1960",
                                    "1961-1980","1981-"))

m4 <- as.data.table(m4)
mira <- m4[,list(sumhomeo=sum(V3)),by="YEAR"]
m4 <- merge(m4,mira,by="YEAR",all.x=T)

g4 <- ggplot(m4,aes(x=YEAR,y=V3,fill=`Birth period`))  +
  geom_area(position='stack') +
  theme_light() + #scale_fill_manual(values=mira$V1) +
  scale_y_continuous(name="Share of population that own home",limits=c(0,1)) +
  scale_x_continuous(name="Year") + theme(legend.position="bottom",
                                          axis.text=element_text(size=14),
                                          axis.title=element_text(size=14),
                                          legend.text=element_text(size=14),
                                          legend.title=element_text(size=14)) + 
  scale_fill_discrete(name="Birth period") + 
  geom_line(aes(x=YEAR,y=sumhomeo,group=1))

ggsave(filename=paste0(folder,"FigureC15.pdf"),plot=g4,width=8,height=6)



# Figure C.16.



data2$homeo <- NA
data2$homeo[data2$OWNERSHP==1] <- 1
data2$homeo[data2$OWNERSHP==2] <- 0

data2$bdecade <- cut(data2$BIRTHYR,breaks=seq(1920,1990,10),labels=seq(1920,1980,10))

data2$rural <- 0
data2$rural[data2$METRO==1 | data2$METRO==3] <- 1

attributes(data2$METRO)

data2 <- as.data.table(data2)

data2.urban <- subset(data2,METRO==1)
homeo.urban <- data2.urban[,wtd.mean(homeo,weights=HHWT,na.rm=T),by=c("AGE","bdecade")]

homeo.urban <- subset(homeo.urban,bdecade %in% c("1940","1960","1980"))
homeo.urban$bdecade <- droplevels(homeo.urban$bdecade)

g1 <- grafico.varias.lineas(homeo.urban,"AGE","V1","bdecade","Age",
                            "Homeownership rates","FigureC16a.pdf",NULL,
                            seq(25,60,5))

data2.urban <- subset(data2,METRO==2)
homeo.urban <- data2.urban[,wtd.mean(homeo,weights=HHWT,na.rm=T),by=c("AGE","bdecade")]

homeo.urban <- subset(homeo.urban,bdecade %in% c("1940","1960","1980"))
homeo.urban$bdecade <- droplevels(homeo.urban$bdecade)

g1 <- grafico.varias.lineas(homeo.urban,"AGE","V1","bdecade","Age",
                            "Homeownership rates","FigureC16b.pdf",NULL,
                            seq(25,60,5))

data2.urban <- subset(data2,METRO==3)
homeo.urban <- data2.urban[,wtd.mean(homeo,weights=HHWT,na.rm=T),by=c("AGE","bdecade")]

homeo.urban <- subset(homeo.urban,bdecade %in% c("1940","1960","1980"))
homeo.urban$bdecade <- droplevels(homeo.urban$bdecade)

g1 <- grafico.varias.lineas(homeo.urban,"AGE","V1","bdecade","Age",
                            "Homeownership rates","FigureC16c.pdf",NULL,
                            seq(25,60,5))

data2.urban <- subset(data2,METRO==4)
homeo.urban <- data2.urban[,wtd.mean(homeo,weights=HHWT,na.rm=T),by=c("AGE","bdecade")]

homeo.urban <- subset(homeo.urban,bdecade %in% c("1940","1960","1980"))
homeo.urban$bdecade <- droplevels(homeo.urban$bdecade)

g1 <- grafico.varias.lineas(homeo.urban,"AGE","V1","bdecade","Age",
                            "Homeownership rates","FigureC16d.pdf",NULL,
                            seq(25,60,5))

}
