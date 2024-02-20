# load_psid_data2.R

# This code performs the data cleaning and variable construction
# from the previously saved PSID raw data

## OTHER INPUTS APART FROM PSID:
# - CPI data
# - PCE data from FRED
# - FICA data from DFP 2020
# - (psid_outputs.R) interest rate on mortgages 

## Directories 

# Where to save figures
folder <- paste0(main.folder,"results/Figures/")

# Where PSID data is stored
folder.from <- paste0(main.folder,"data/")

# Where to save output 

folder.to <- paste0(main.folder,"working/")

### With directories fixed and all data available, the below
# should run without needing adjustments


# Load PSID data

dam <- as.data.table(read.csv(paste0(folder.from,"psid_raw_data.csv"),sep=" "))

# Load CPI to deflate values
cpi <- as.data.table(read.csv(paste0(folder.from,"cpiu_2017.csv"),stringsAsFactors=FALSE))
cpi$year <- as.numeric(substr(cpi$DATE,1,4))
cpi$cpi <- as.numeric(cpi$CPIAUCSL)
cpi$cpi <- cpi$cpi/(cpi$cpi[67]) #year 2013
cpi$DATE <- NULL
cpi$CPIAUCSL <- NULL

# Load PCE to deflate values
pce <- as.data.table(read.csv(paste0(folder.from,"PCE_fred.csv"),stringsAsFactors=FALSE))
names(pce) <- c("year","pce")
pce$pce <- pce$pce/(pce$pce[67])


# Create person weights for equivalence scales

dam$personweight <- 0
dam$personweight[dam$Relhead==1 | dam$Relhead==10] <- 1
dam$personweight[dam$personweight==0 & dam$age>17] <- 0.7
dam$personweight[dam$personweight==0 & dam$age<18] <- 0.5


dam2 <- subset(dam,Seqno<51)
dam2 <- subset(dam2,Seqno>0)
dum <- dam2[,sum(personweight),by=c("year","Intno")]
dum <- subset(dum,Intno>0)
dum$EquiScale <- dum$V1
dum$V1 <- NULL
# Now build equivalence scale (this is what we are interested in)

dam <- merge(dam,dum,by=c("year","Intno"),all.x=T)

setkeyv(dam,c("personid","year"))

# Generate NEXT variables (i.e., age, year, etc. in the following period)
# In this code we are interested in TWO-year transitions

dam[,nextage := shift(age,type="lead"),by="personid"]
dam[,lastage := shift(age,type="lag"),by="personid"]
dam[,nextfamchange := shift(Famchange,type="lead"),by="personid"]
dam[,nextrelhead := shift(Relhead,type="lead"),by="personid"]
dam[,nextyear := shift(year,type="lead"),by="personid"]

dam[,nextage2 := shift(age,type="lead",n=2),by="personid"]
dam[,nextrelhead2 := shift(Relhead,type="lead",n=2),by="personid"]
dam[,nextyear2 := shift(year,type="lead",n=2),by="personid"]
dam[,nextagespouse := shift(AgeSpouse,type="lead",n=2),by="personid"]

dam$rightj <- 0
dam$rightj[dam$nextyear==(dam$year)+2] <- 1
dam$rightj[dam$nextyear2==(dam$year)+2] <- 2

dam <- subset(dam,is.na(age)==0) 
dam <- subset(dam,Relhead==1 | Relhead==10) #Select heads today

# Drop duplicates personid year - should not be any

setkeyv(dam,c("personid","year"))
dam <- unique(dam)

# Deal with variables that have top-coding or missings

dam$WifeLaborBusinessIncome[dam$WifeLaborBusinessIncome>999995] <- 0
dam$TotalFamilyIncome[dam$TotalFamilyIncome>9999995] <- 0

# Continue dealing with missings

m9 <- 9999997
mk9 <- 999999997

dam$Bonds[dam$Bonds>mk9] <- NA
dam$Business[dam$Business>999999997] <- NA
dam$BusinessDebt[dam$BusinessDebt>999999997] <- NA
dam$BusinessGross[dam$BusinessGross>999999997] <- NA
dam$Cash[dam$Cash>999999997] <- NA
dam$CashOld[dam$CashOld>999999997] <- NA
dam$CreditCard[dam$CreditCard>m9] <- NA
dam$DebtsOther[dam$DebtsOther>mk9] <- NA
dam$DebtsOtherOther[dam$DebtsOtherOther>m9] <- NA
dam$IRAs[dam$IRAs>mk9] <- NA
dam$LegalBill[dam$LegalBill>m9] <- NA
dam$LoanRelatives[dam$LoanRelatives>m9] <- NA
dam$MedicalBill[dam$MedicalBill>m9] <- NA
dam$Mortrem[dam$Mortrem>m9] <- NA
dam$OtherRE[dam$OtherRE>999999997] <- NA
dam$OtherREDebt[dam$OtherREDebt>999999997] <- NA
dam$OtherREGross[dam$OtherREGross>999999997] <- NA
dam$SecondMortrem[dam$SecondMortrem>9999997] <- NA
dam$SoldHomePrice[dam$SoldHomePrice>999999997] <- NA
dam$SoldOtherREPrice[dam$SoldOtherREPrice>999999997] <- NA
dam$Stocks[dam$Stocks>999999997] <- NA
dam$StudentLoan[dam$StudentLoan>m9] <- NA
dam$Vehicles[dam$Vehicles>999999997] <- NA


dam$Bonds[dam$Bonds>9999996 & dam$year<1999] <- NA
dam$Business[dam$Business>9999996 & dam$year<1999] <- NA
dam$BusinessDebt[dam$BusinessDebt>9999996 & dam$year<1999] <- NA
dam$BusinessGross[dam$BusinessGross>9999996 & dam$year<1999] <- NA
dam$Cash[dam$Cash>9999996 & dam$year<1999] <- NA
dam$CashOld[dam$CashOld>9999996 & dam$year<1999] <- NA
dam$DebtsOther[dam$DebtsOther>9999996 & dam$year<1999] <- NA
dam$DebtsOtherOther[dam$DebtsOtherOther>9999996 & dam$year<1999] <- NA
dam$IRAs[dam$IRAs>9999996 & dam$year<1999] <- NA
dam$LegalBill[dam$LegalBill>9999996 & dam$year<1999] <- NA
dam$LoanRelatives[dam$LoanRelatives>9999996 & dam$year<1999] <- NA
dam$MedicalBill[dam$MedicalBill>9999996 & dam$year<1999] <- NA
dam$OtherRE[dam$OtherRE>9999996 & dam$year<1999] <- NA
dam$OtherREDebt[dam$OtherREDebt>9999996 & dam$year<1999] <- NA
dam$OtherREGross[dam$OtherREGross>9999996 & dam$year<1999] <- NA
dam$SoldHomePrice[dam$SoldHomePrice>9999996 & dam$year<1999] <- NA
dam$SoldOtherREPrice[dam$SoldOtherREPrice>9999996 & dam$year<1999] <- NA
dam$Stocks[dam$Stocks>9999996 & dam$year<1999] <- NA
dam$Vehicles[dam$Vehicles>9999996 & dam$year<1999] <- NA

dam$Vehicles[dam$Vehicles>999996 & dam$year==1984] <- NA
dam$CashOld[dam$CashOld>999996 & dam$year==1984] <- NA

dam$Houseval[dam$Houseval>9999997] <- NA
dam$Mortrem[dam$Mortrem>9999997] <- NA

dam$YearLastMoved[dam$YearLastMoved==1 & dam$year==2001] <- 1999
dam$YearLastMoved[dam$YearLastMoved==2 & dam$year==2001] <- 2000
dam$YearLastMoved[dam$YearLastMoved==3 & dam$year==2001] <- 2001

dam$YearLastMoved[dam$YearLastMoved==1 & dam$year==1999] <- 1997
dam$YearLastMoved[dam$YearLastMoved==2 & dam$year==1999] <- 1998
dam$YearLastMoved[dam$YearLastMoved==3 & dam$year==1999] <- 1999

dam$YearLastMoved[dam$year<1999] <- NA

# Build measures of government transfers

dam$WifeChildSupport[is.na(dam$WifeChildSupport)]<-0
dam$WifeHelpRelatives[is.na(dam$WifeHelpRelatives)]<-0
dam$HWTRansfer[is.na(dam$HWTRansfer)] <- 0
dam$HeadAlimony[is.na(dam$HeadAlimony)] <- 0
dam$HeadHelpRelatives[is.na(dam$HeadHelpRelatives)] <- 0
dam$GovtTransfer <- dam$HWTRansfer-dam$HeadAlimony-dam$HeadHelpRelatives-dam$WifeChildSupport-dam$WifeHelpRelatives

dam$HeadUnempCompensation[is.na(dam$HeadUnempCompensation)==1] <- 0
dam$HeadUnempWksCompensation[is.na(dam$HeadUnempWksCompensation)==1] <- 0
dam$HeadWksCompensation[is.na(dam$HeadWksCompensation)==1] <- 0
dam$HeadSocialSecurity[is.na(dam$HeadSocialSecurity)==1] <- 0
dam$HeadSSI[is.na(dam$HeadSSI)==1] <- 0
dam$HeadVApension[is.na(dam$HeadVApension)==1] <- 0
dam$HdOtherWelfare[is.na(dam$HdOtherWelfare)==1] <- 0
dam$WifeOtherWelfare[is.na(dam$WifeOtherWelfare)==1] <- 0
dam$WifeSocialSecurity[is.na(dam$WifeSocialSecurity)==1] <- 0
dam$WifeSSI[is.na(dam$WifeSSI)==1] <- 0
dam$WifeTANF[is.na(dam$WifeTANF)==1] <- 0
dam$WifeVApension[is.na(dam$WifeVApension)==1] <- 0
dam$WifeUnempCompensation[is.na(dam$WifeUnempCompensation)==1] <- 0
dam$WifeVApension[is.na(dam$WifeVApension)==1] <- 0
dam$TANF[is.na(dam$TANF)==1] <- 0


dam$GovtTransfer2 <- dam$HeadUnempCompensation+dam$HeadUnempWksCompensation+dam$HeadWksCompensation+
  dam$HeadSocialSecurity+dam$HeadSSI+dam$HeadVApension+dam$HdOtherWelfare+dam$WifeOtherWelfare+dam$WifeSocialSecurity+
  dam$WifeSSI+dam$WifeTANF+dam$WifeVApension+dam$WifeUnempCompensation+dam$WifeVApension+dam$TANF


## Construct education measures

# Whether any college (colleged)

dam$college <- 0

dam$college[dam$EducationBk>5 & dam$HeadEducationBk<9] <- 1

dam$college[dam$Education>12 & dam$Education<99] <- 1

dam$colleged <- factor(dam$college)


# Years of education

dam$yearsedu <- 0
dam$yearsedu[dam$EducationBkt==0] <- 0
dam$yearsedu[dam$EducationBkt==1] <- 5
dam$yearsedu[dam$EducationBkt==2] <- 7
dam$yearsedu[dam$EducationBkt==3] <- 10
dam$yearsedu[dam$EducationBkt==4] <- 12
dam$yearsedu[dam$EducationBkt==5] <- 14
dam$yearsedu[dam$EducationBkt==6] <- 14
dam$yearsedu[dam$EducationBkt==7] <- 16
dam$yearsedu[dam$EducationBkt==8] <- 19
dam$yearsedu[dam$EducationBkt==9] <- 0
dam$yearsedu[is.na(dam$Education)==0] <- dam$Education[is.na(dam$Education)==0]
dam$yearsedu[dam$yearsedu==99] <- 0


# Generate measures related to unemployment

dam$TimeUnemployed <- 0 # let's measure in days 
dam$TimeUnemployed[is.na(dam$HoursUnemployed)==0] <- 
  dam$HoursUnemployed[is.na(dam$HoursUnemployed)==0]/8
dam$TimeUnemployed[is.na(dam$DaysUnemployed)==0 & dam$DaysUnemployed<400] <- 
  dam$TimeUnemployed[is.na(dam$DaysUnemployed)==0 & dam$DaysUnemployed<400]+
  dam$DaysUnemployed[is.na(dam$DaysUnemployed)==0 & dam$DaysUnemployed<400]
dam$TimeUnemployed[is.na(dam$WeeksUnemployed)==0 & dam$WeeksUnemployed<90] <- 
  dam$TimeUnemployed[is.na(dam$WeeksUnemployed)==0 & dam$WeeksUnemployed<90]+
  dam$WeeksUnemployed[is.na(dam$WeeksUnemployed)==0 & dam$WeeksUnemployed<90]*7
dam$TimeUnemployed[is.na(dam$MonthsUnemployed)==0 & dam$MonthsUnemployed<90] <- 
  dam$TimeUnemployed[is.na(dam$MonthsUnemployed)==0 & dam$MonthsUnemployed<90]+
  dam$MonthsUnemployed[is.na(dam$MonthsUnemployed)==0 & dam$MonthsUnemployed<90]*30

dam$SpouseTimeUnemployed <- 0
dam$SpouseTimeUnemployed[is.na(dam$SpouseHoursUnemployed)==0] <- 
  dam$SpouseHoursUnemployed[is.na(dam$SpouseHoursUnemployed)==0]/8
dam$SpouseTimeUnemployed[is.na(dam$SpouseWeeksUnemployed)==0] <- 
  dam$SpouseWeeksUnemployed[is.na(dam$SpouseWeeksUnemployed)==0]*7


dam$LaborEarningsWife <- NA
dam$LaborEarningsWife[dam$year<1993] <- dam$LaborEarningsWife1[dam$year<1993]
dam$LaborEarningsWife[dam$year>1992] <- dam$LaborEarningsWife2[dam$year>1992] +
    dam$WifeLaborBusinessIncome[dam$year>1992]

# Generate summary income measures

dam$labtotal <- dam$LaborEarningsHead+dam$LaborEarningsWife
dam$labsq <- dam$labtotal^2



dam$disposable <- dam$labtotal - dam$FederalIncomeTax + dam$GovtTransfer
dam$disposable2 <- dam$labtotal - dam$FederalIncomeTax + dam$GovtTransfer2



dam$byear <- dam$year-dam$age


# FICA adjustment for disposable income (following procedure described in
# De Nardi, Fella and Paz-Pardo, JEEA 2020)

fica.data <- read.csv(paste0(folder.from,"fica_data.csv"))
fica.data <- plyr::rename(fica.data,c("tax"="sstax","cap"="sscap"))
fica.data$year <- fica.data$year+1 #PSID income measures refer to previous year
dam <- merge(dam,fica.data,by="year",all.x=T)
dam$sstaxliab <- dam$sstax*pmin(dam$LaborEarningsHead,dam$sscap)+dam$sstax*pmin(dam$LaborEarningsWife1,dam$sscap)
dam$FederalIncomeTax <- dam$FederalIncomeTax + dam$sstaxliab
dam$statetax <- 0.1*(dam$LaborEarningsHead+dam$LaborEarningsWife1)
dam$FederalIncomeTax <- dam$FederalIncomeTax + dam$sstaxliab + dam$statetax

setkeyv(dam,"year")
setkeyv(cpi,"year")
dam <- merge(dam,cpi,all.x=TRUE)

setkeyv(dam,"year")
setkeyv(pce,"year")
dam <- merge(dam,pce,all.x=TRUE)

dam$NDlabtotal <- dam$labtotal

# Perform deflation (baseline: CPI)


vars.to.deflate <- c("Houseval","Annualrent","Bonds","Business","BusinessDebt","BusinessGross",
                     "Cash","CashOld","CreditCard","DebtsOther","DebtsOtherOther","IRAs","LegalBill",
                     "LoanRelatives","MedicalBill","Mortrem","OtherRE","OtherREDebt","OtherREGross",
                     "SoldHomePrice","SoldOtherREPrice","Stocks","StudentLoan","Vehicles",
                     "HomeRepairsExp","TotalFamilyIncome","LaborEarningsHead","LaborEarningsWife1",
                     "LaborEarningsWife2","CheckingAccountsW","HomeEquityW","OtherAssetsW",
                     "OtherDebtW","OtherREW","StocksW","ValueBusinessW","VehiclesW","WealthW",
                     "WealthnoeqW","Rent","LaborIncomeOthers","IncomeOthers","HWTaxableIncome",
                     "FederalIncomeTax","HeadHourlyEarnings","labtotal","GovtTransfer2",
                     "GovtTransfer","disposable","disposable2")

dam[,vars.to.deflate] <- dam[,vars.to.deflate,with=FALSE]/dam$cpi

dam$loglabtotal <- log(dam$labtotal)
 
 mean(dam$labtotal)

 
 # Add information on recessions from NBER to data
  
  
year <- seq(1968,2017)
nber <- as.data.table(year)
#Any recession appears in here
#a year is recessionary if it had part of a recession included in it
nber$rec <- c(0,1,1,0,0,1,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0)

#but crucially, "year" is the survey year of the PSID
# so NBER recessions in this data.table actually refer to the year AFTER in this data.frame
nber$year <- nber$year+1
nber[,tomrec := shift(rec,1,type="lead")]
nber$tomrec[nber$year == 2017] <- 0
nber[, prerec :=shift(rec,1,type="lag")]
nber$prerec[nber$year==1969] <- 0


dam <- merge(dam,nber,all.x=T,by="year")


# WEALTH MEASURES

# Note wealth supplement variables are better (as they have nice imputations)

# House value and remaining mortgage are the two wealth-related variables
# that were available since the beginning of the PSID

# Note with some limitations in remaining mortgage (missing for some years)

# The rest (most of them) only available in 84, 89, 94 and 99 onwards

# Now replace variables for their more accurate counterparts

dam$Bonds <- dam$OtherAssetsW
dam$Business <- dam$ValueBusinessW
dam$Cash <- dam$CheckingAccountsW
dam$DebtsOther <- dam$OtherDebtW
dam$OtherRE <- dam$OtherREW
dam$Stocks <- dam$StocksW
dam$Vehicles <- dam$VehiclesW
rub1 <- c("CashOld","OtherAssetsW","ValueBusinessW","CheckingAccountsW","OtherDebtW","OtherREW","StocksW","VehiclesW")
dam[, (rub1):=NULL]

# And build summary variables

# Real estate
dam$AllRE <- dam$Houseval + dam$OtherRE
dam$AllRE[dam$year>2012] <- dam$Houseval[dam$year>2012] + dam$OtherREGross[dam$year>2012]
dam$AllRENet <- dam$HomeEquityW + dam$OtherRE
dam$AllRENet[dam$year>2012] <- dam$HomeEquityW[dam$year>2012] + dam$OtherREGross[dam$year>2012] -
  dam$OtherREDebt[dam$year>2012]

# Real estate other than main residence + vehicles
dam$OtherReal[dam$year<2013] <- dam$Vehicles[dam$year<2013]+
  dam$OtherRE[dam$year<2013]
dam$OtherReal[dam$year>2012] <- dam$Vehicles[dam$year>2012] +
  dam$OtherREGross[dam$year>2012]-dam$OtherREDebt[dam$year>2012]

dam$OtherRealNV[dam$year<2013] <- dam$OtherRE[dam$year<2013]
dam$OtherRealNV[dam$year>2012] <- dam$OtherREGross[dam$year>2012] - dam$OtherREDebt[dam$year>2012]

# Financial assets

dam$Liquid <- dam$Cash + dam$Bonds
dam$Risky <- dam$Stocks
dam$Risky[dam$year>1994] <- dam$Stocks[dam$year>1994] + dam$IRAs[dam$year>1994]

dam$AllFinancial <- dam$Liquid+dam$Risky

# All assets

dam$Assets <- dam$Houseval + dam$OtherReal + dam$AllFinancial + dam$Business

# All Debt
# Before availability of DebtsOther, just Mortrem and SecondMortrem

dam$Debt[dam$year<1994 & is.na(dam$DebtsOther)] <- dam$Mortrem[dam$year<1994 & is.na(dam$DebtsOther)]
dam$Debt[dam$year>1993 & dam$year<2011 & is.na(dam$DebtsOther)] <- 
  dam$Mortrem[dam$year>1993 & dam$year<2011 & is.na(dam$DebtsOther)] +
  dam$SecondMortrem[dam$year>1993 & dam$year<2011 & is.na(dam$DebtsOther)]

dam$Debt[dam$year<1994 & !is.na(dam$DebtsOther)] <- dam$Mortrem[dam$year<1994 & !is.na(dam$DebtsOther)] +
  dam$DebtsOther[dam$year<1994 & !is.na(dam$DebtsOther)]
dam$Debt[dam$year>1993 & dam$year<2011 & !is.na(dam$DebtsOther)] <-
  dam$Mortrem[dam$year>1993 & dam$year<2011 & !is.na(dam$DebtsOther)] +
  dam$DebtsOther[dam$year>1993 & dam$year<2011 & !is.na(dam$DebtsOther)] +
  dam$SecondMortrem[dam$year>1993 & dam$year<2011 & !is.na(dam$DebtsOther)]

dam$Debt[dam$year>2010] <- dam$Mortrem[dam$year>2010] +
  dam$CreditCard[dam$year>2010] + dam$StudentLoan[dam$year>2010]+
  dam$MedicalBill[dam$year>2010] + dam$LegalBill[dam$year>2010] +
  dam$LoanRelatives[dam$year>2010] + dam$SecondMortrem[dam$year>2010]

dam$Debt[dam$year>2012] <- dam$Debt[dam$year>2012] + dam$DebtsOtherOther[dam$year>2012]

dam$Wealth <- dam$Assets-dam$Debt

# And adjust rent

dam$Rent[dam$RentPeriod==2 & is.na(dam$RentPeriod)==0] <- 365*dam$Rent[dam$RentPeriod==2 & is.na(dam$RentPeriod)==0]
dam$Rent[dam$RentPeriod==3 & is.na(dam$RentPeriod)==0] <- 52*dam$Rent[dam$RentPeriod==3 & is.na(dam$RentPeriod)==0]
dam$Rent[dam$RentPeriod==4 & is.na(dam$RentPeriod)==0] <- 26*dam$Rent[dam$RentPeriod==4 & is.na(dam$RentPeriod)==0]
dam$Rent[dam$RentPeriod==5 & is.na(dam$RentPeriod)==0] <- 12*dam$Rent[dam$RentPeriod==5 & is.na(dam$RentPeriod)==0]
dam$Rent[dam$RentPeriod==6 & is.na(dam$RentPeriod)==0] <- dam$Rent[dam$RentPeriod==6 & is.na(dam$RentPeriod)==0]

save("dam","cpi",file=paste0(folder.to,"PSIDSample.RData"))


