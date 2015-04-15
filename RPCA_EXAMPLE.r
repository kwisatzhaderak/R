DATA = read.csv('~/R/Data/PVPrinComp.csv',na.strings = "NULL")

##Unified Multiple State, Reserve Segmented Bayesian LASSO
library(lubridate)
library(penalized)
library(glmnet)
library(stringr)

#Select the run date
rundate = format(Sys.time(), "%Y-%m-%d")
#Select the training data ending date, this is the date starting the test data
date = format(ymd(format(Sys.time(), "%Y-%m-%d")) - days(30), "%Y-%m-%d")
DATA$AuctionDate = mdy(as.character(DATA$AuctionDate))

#Numericize data
NumFun = function(x){as.numeric(as.character(x))}
NumericData = which(names(DATA)=="Latitude"):ncol(DATA)
MyData = DATA
MyData[,NumericData] = apply(MyData[,NumericData],2,NumFun)
#check
apply(MyData,2,class)

#Remove Any BPOs with a BPO Surplus PCT > 5x
MyData$IsSold = as.numeric(as.character(MyData$IsSold))
MyData$SFR_Flag = as.numeric(as.character(MyData$SFR_Flag))
MyData$Townhouse_Flag = as.numeric(as.character(MyData$Townhouse_Flag))
MyData$Condo_Flag = as.numeric(as.character(MyData$Condo_Flag))
MyData$MultiResidence_Flag = as.numeric(as.character(MyData$MultiResidence_Flag))
MyData$ManufacturedHome_Flag = as.numeric(as.character(MyData$ManufacturedHome_Flag))
MyData$RunNum = as.numeric(as.character(MyData$RunNum))
MyData$HasPrevAttempts = as.numeric(as.character(MyData$HasPrevAttempts))
MyData$Reserve  = as.numeric(as.character(MyData$Reserve))
MyData$BPOSurplus = as.numeric(as.character(MyData$BPOSurplus))
MyData$BPOSurplusPct = as.numeric(as.character(MyData$BPOSurplusPct))
MyData$Condition_poor = as.numeric(as.character(MyData$Condition_poor))
MyData$Condition_fair = as.numeric(as.character(MyData$Condition_fair))
MyData$Condition_average = as.numeric(as.character(MyData$Condition_fair))
MyData$Condition_good = as.numeric(as.character(MyData$Condition_good))
MyData$Condition_unknown = as.numeric(as.character(MyData$Condition_unknown))
MyData$PropertyOccupancyStatus_Occupied = as.numeric(as.character(MyData$PropertyOccupancyStatus_Occupied))
MyData$PropertyOccupancyStatus_Unknown = as.numeric(as.character(MyData$PropertyOccupancyStatus_Unknown))
MyData$PropertyOccupancyStatus_Vacant = as.numeric(as.character(MyData$PropertyOccupancyStatus_Vacant))
MyData$FinancingAvailable = as.numeric(as.character(MyData$FinancingAvailable))
MyData$TotalActive = as.numeric(as.character(MyData$TotalActive))
MyData$Vacancy = as.numeric(as.character(MyData$Vacancy))
MyData$ValueLevel_Low = as.numeric(as.character(MyData$ValueLevel_Low))
MyData$ValueLevel_Med = as.numeric(as.character(MyData$ValueLevel_Med))
MyData$ValueLevel_High = as.numeric(as.character(MyData$ValueLevel_High))
MyData$Favorites = as.numeric(as.character(MyData$Favorites))
MyData$Appreciation = as.numeric(as.character(MyData$Appreciation))
MyData$Turnover = as.numeric(as.character(MyData$Turnover))
MyData$MedianPPSQFT = as.numeric(as.character(MyData$MedianPPSQFT))
MyData$MedianPPSQFTDeviation = as.numeric(as.character(MyData$MedianPPSQFTDeviation))
MyData$ForeclosedPer10000 = as.numeric(as.character(MyData$ForeclosedPer10000))
MyData$PrevForeclosureSoldRate = as.numeric(as.character(MyData$PrevForeclosureSoldRate))
MyData$FullWarrantyDeed = as.numeric(as.character(MyData$FullWarrantyDeed))
MyData$SpecialWarrantyDeed = as.numeric(as.character(MyData$SpecialWarrantyDeed))
MyData$QuitClaimDeed = as.numeric(as.character(MyData$QuitClaimDeed))
MyData$ADC_Historical_Execution = as.numeric(as.character(MyData$ADC_Historical_Execution))
MyData$SellerPortfolioQuality = as.numeric(as.character(MyData$SellerPortfolioQuality))
MyData$NumImage = as.numeric(as.character(MyData$NumImage))
MyData$SubjToAcceptRate = as.numeric(as.character(MyData$SubjToAcceptRate))
MyData$PrevPrice = as.numeric(as.character(MyData$PrevPrice))
MyData$TotalPageViews = as.numeric(as.character(MyData$TotalPageViews))
MyData$Day14OutPVs = as.numeric(as.character(MyData$Day14OutPVs))

ModelData = MyData[MyData$AuctionDate < ymd(date)
          & is.na(MyData$Reserve)==F
          & is.na(MyData$BPO)==F
          & is.na(MyData$BPOSurplus)==F
          & is.na(MyData$Condition_poor)==F
          & is.na(MyData$Condition_fair)==F
          & is.na(MyData$Condition_average)==F
          & is.na(MyData$Condition_good)==F
          & is.na(MyData$Condition_unknown)==F
          & is.na(MyData$PropertyOccupancyStatus_Occupied)==F
          & is.na(MyData$PropertyOccupancyStatus_Unknown)==F
          & is.na(MyData$PropertyOccupancyStatus_Vacant)==F
          & is.na(MyData$ValueLevel_Low)==F
          & is.na(MyData$ValueLevel_Med)==F
          & is.na(MyData$ValueLevel_High)==F
          & is.na(MyData$RunNum)==F
          & is.na(MyData$FinancingAvailable)==F
          & is.na(MyData$Favorites)==F
          & is.na(MyData$TotalActive)==F
          & is.na(MyData$Vacancy)==F
          & is.na(MyData$HasPrevAttempts)==F
          & is.na(MyData$BPOSurplusPct)==F
          & is.na(MyData$TotalRegistrantScore)==F
          & is.na(MyData$IntrinsicBidderQuality)==F
          & is.na(MyData$Appreciation)==F
          & is.na(MyData$Turnover)==F
          & is.na(MyData$MedianPPSQFT)==F
          & is.na(MyData$MedianPPSQFTDeviation)==F
          & is.na(MyData$NumImage)==F
          & is.na(MyData$SubjToAcceptRate)==F,]

#Create formulae for each value predicted
AuctionDaySaleFormula = {as.formula(IsSold
                                    ~SFR_Flag
                                    +Townhouse_Flag
                                    +Condo_Flag
                                    +MultiResidence_Flag
                                    +ManufacturedHome_Flag
                                    +RunNum
                                    +HasPrevAttempts
                                    +Reserve 
                                    +BPOSurplus
                                    +BPOSurplusPct
                                    +Condition_poor
                                    +Condition_fair
                                    +Condition_average
                                    +Condition_good
                                    +Condition_unknown
                                    +PropertyOccupancyStatus_Occupied
                                    +PropertyOccupancyStatus_Unknown
                                    +PropertyOccupancyStatus_Vacant
                                    +FinancingAvailable
                                    +TotalActive
                                    +Vacancy
                                    +ValueLevel_Low
                                    +ValueLevel_Med
                                    +ValueLevel_High
                                    +Favorites
                                    +Appreciation
                                    +Turnover
                                    +MedianPPSQFT
                                    +MedianPPSQFTDeviation
                                    +ForeclosedPer10000
                                    +PrevForeclosureSoldRate
                                    +FullWarrantyDeed
                                    +SpecialWarrantyDeed
                                    +QuitClaimDeed
                                    +ADC_Historical_Execution
                                    +SellerPortfolioQuality
                                    +NumImage
                                    +SubjToAcceptRate
                                    +TotalPageViews
                                    +Day14OutPVs
)}


x = ModelData
y = x$IsSold
x = model.matrix(object = AuctionDaySaleFormula, data = x)
AD = glmnet(x, y, family = "binomial", alpha = 0.00001)
cvmr = cv.glmnet(x, y)
a = coef(AD, s = cvmr$lambda.min)
b = row.names(a)[3:length(row.names(a))]
c = as.vector(coef(AD, s = cvmr$lambda.min)[3:length(row.names(a))])
d = cbind(b,c)
penalized_export = d[order(as.numeric(d[,2]),decreasing=T),]


predict(AD, newx = predx, s = cvmr$lambda.min, type = "response")


least_squares = glm(AuctionDaySaleFormula, family = binomial, data = MyData)
ls(least_squares)
a = a = coef(least_squares)
b = names(coef(least_squares))
c = as.data.frame(cbind(b,a))
leastsq_export = c[order(as.numeric(c[,2]),decreasing=T),]

PCForm = {as.formula(~IsSold +SFR_Flag
                     
                                    +Townhouse_Flag
                                    +Condo_Flag
                                    +MultiResidence_Flag
                                    +ManufacturedHome_Flag
                                    +RunNum
                                    +HasPrevAttempts
                                    +Reserve 
                                    +BPOSurplus
                                    +BPOSurplusPct
                                    +Condition_poor
                                    +Condition_fair
                                    +Condition_average
                                    +Condition_good
                                    +Condition_unknown
                                    +PropertyOccupancyStatus_Occupied
                                    +PropertyOccupancyStatus_Unknown
                                    +PropertyOccupancyStatus_Vacant
                                    +FinancingAvailable
                                    +TotalActive
                                    +Vacancy
                                    +ValueLevel_Low
                                    +ValueLevel_Med
                                    +ValueLevel_High
                                    +Favorites
                                    +Appreciation
                                    +Turnover
                                    +MedianPPSQFT
                                    +MedianPPSQFTDeviation
                                    +ForeclosedPer10000
                                    +PrevForeclosureSoldRate
                                    +FullWarrantyDeed
                                    +SpecialWarrantyDeed
                                    +QuitClaimDeed
                                    +ADC_Historical_Execution
                                    +SellerPortfolioQuality
                                    +NumImage
                                    +SubjToAcceptRate
                                    +TotalPageViews
                                    +Day14OutPVs
)}
pcdata = as.data.frame(cbind(y,x[,-1]))
names(pcdata)[1] = "IsSold"
#Keep looking at principle components until the change in explained variance is ~0
pc = prcomp(formula = PCForm, data = pcdata, cor = TRUE, center = TRUE, tol = .001, retx = F, scale.=T)
summary(pc)
ls(pc)
screeplot(pc, type ='lines', main = "Property Data Scree Plot")
#PC1
a = pc$rotation[,1]
b = row.names(pc$rotation)
c = as.data.frame(cbind(b,a))
princom1_export = c[order(abs(as.numeric(as.character(c[,2]))),decreasing=T),]
#PC2
a = pc$rotation[,2]
b = row.names(pc$rotation)
c = as.data.frame(cbind(b,a))
princom2_export = c[order(abs(as.numeric(as.character(c[,2]))),decreasing=T),]
#PC3
a = pc$rotation[,3]
b = row.names(pc$rotation)
c = as.data.frame(cbind(b,a))
princom3_export = c[order(abs(as.numeric(as.character(c[,2]))),decreasing=T),]
#PC4
a = pc$rotation[,4]
b = row.names(pc$rotation)
c = as.data.frame(cbind(b,a))
princom4_export = c[order(abs(as.numeric(as.character(c[,2]))),decreasing=T),]


