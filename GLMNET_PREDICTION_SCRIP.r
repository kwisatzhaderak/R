#--===========================================================================================================
#  R script for looped prediction 
#AUTHOR: Wolf Rendall, wrendall@auction.com, rendall.wolf@gmail.com
#DATE:  2014-09-24
#NOTES:  

##Segmented Bayesian LASSO
#Loops over State, Product Type Priors

#---=====**OUTFILE DEFINITION**=====---  				
#   Coefficient_Output.csv
#   Prediction_Output.csv


#************************CHANGE LOG
#1:	All Quiet on the western front.

#************************DEPENDENCIES
#1: SaleLikelihoodEstimator_OptimizedDataPull.sql
#--===========================================================================================================
#  CHANGE DATE 					CHANGED BY 						CHANGE DESCRIPTION		
#   2014-09-24          Wolf Rendall          Initial Release
#   2014-12-09          Wolf Rendall          Added newly acquired RealtyTrac data aggregations
#
#--===========================================================================================================

#Import
library(lubridate)
library(glmnet)
library(stringr)

#Select date parameters
daysAgoEndTraining = 20
daysAgoStartTraining = 370
rundate = format(Sys.time(), "%Y-%m-%d")

#Select the training data ending date, this is the date starting the test data
date = format(ymd(format(Sys.time(), "%Y-%m-%d")) - days(daysAgoEndTraining), "%Y-%m-%d")

#Import Auction.com Data And set the bounds on the training data
filename = paste0("~/R/R Data/NonTrusteeZoltarData_",rundate,".csv")
DATA = read.csv(file = filename)
DATA$BidEndDT = mdy(as.character(DATA$BidEndDT))
DATA = DATA[DATA$BidEndDT >= ymd(format(ymd(format(Sys.time(), "%Y-%m-%d")) - days(daysAgoStartTraining), "%Y-%m-%d")),]

#Make stuff numeric, converting SQL NULLS to NAs in the process
#especially importand are the Reserve, BPO, and Zillow Metrics
makeNum = function(x){as.numeric(as.character(x))}
DATA[,24:ncol(DATA)] = apply(X = DATA[,24:ncol(DATA)], MARGIN = 2, FUN = makeNum)

#Price sensitive, needs reserves
DATA = DATA[is.na(DATA$Reserve)==F,]
DATA$AuctionReserveSuccess = 0
DATA$AuctionReserveSuccess[DATA$HighBid >= DATA$Reserve] = 1

#Remove Commas
DATA$SellerName = gsub(x=DATA$SellerName, pattern=",", replacement="", fixed=T, )
DATA$PropertyCity = gsub(x=DATA$PropertyCity, pattern=",", replacement="", fixed=T, )

#Other Fixes
DATA$NumImage[is.na(DATA$NumImage)==T] = 1

#Try to interpolate some zillow metrics at the county level
ZillowData = DATA[,c("Turnover","Appreciation","MedianPPSQFT","ForeclosedPer10000","PrevForeclosureSoldRate")]
CountyAggZillow = aggregate(ZillowData, list(PropertyState = DATA$PropertyState, PropertyCounty = DATA$PropertyCounty), mean, na.rm=T)
StateAggZillow = aggregate(ZillowData, list(PropertyState = DATA$PropertyState), mean, na.rm=T)

replaceMissing = function(x,fieldname){
  Observation = x[ ,c(fieldname,"PropertyState","PropertyCounty")]
  MissingZillow = which(is.na(x[ ,fieldname]) | is.nan(x[ ,fieldname]))
  Iterations = 0
  for(i in MissingZillow){
      State = Observation[ i ,"PropertyState"]
      County = Observation[ i ,"PropertyCounty"]
      Match = which(CountyAggZillow$PropertyState == State & CountyAggZillow$PropertyCounty == County)
      Replacement = CountyAggZillow[ Match , fieldname]
      #If that match is null, replace with state
      if(is.na(Replacement) | is.nan(Replacement)){
        Replacement = StateAggZillow[StateAggZillow$PropertyState == State , fieldname]
      }
      DATA[i, fieldname] <<- Replacement
      Iterations = Iterations + 1
      if( Iterations %% 50 == 0 ) {
        print(paste0(Iterations, " replacements made"))
      }
    }  
  }


sum(is.na(DATA$Turnover))
replaceMissing(DATA,"Turnover")
replaceMissing(DATA,"Appreciation")
replaceMissing(DATA,"MedianPPSQFT")
replaceMissing(DATA,"ForeclosedPer10000")
replaceMissing(DATA,"PrevForeclosureSoldRate")
sum(is.na(DATA$Turnover))

#Include RealtyTrac Metrics 
#rt = read.csv(file = '~/R/R Data/RealtyTrac_County.csv')
#rt = rt[rt$Distress == 'S' & rt$Sale == 'R',]
#rt$Count = as.numeric(as.character(rt$Count))
#rt$Value = as.numeric(as.character(rt$Value))

#DATA$PropertyCounty = toupper(DATA$PropertyCounty)
#DATA = merge(DATA, rt, 
#             by.x = c('PropertyState', 'PropertyCounty'), 
#             by.y = c('State', 'County'),
#             all.x =T)


#DATA$Count[is.na(DATA$Count)==T] = 0.0
#DATA$Value[is.na(DATA$Value)==T] = 0.0

#Now that general data is cleaned and interpolated, we define a model toward defining test and training sets

#Define a model to loop over individual data sets.
#This is a kitchen sink approach and we **let the algorithm do the model selection**. Throw anything you want in.
#This code was written by an economist, so I know how dangerous that sounds due to multicolinearity. It's OK.
#We're using the same data each time, just changing the training set and the success criteria
FORMULA = {as.formula(Sold_Criteria 
                      ~ADC_Historical_Execution
                      +Age
                      +Appreciation
                      +avg_reserve_attainment
                      +Baths
                      +Bedrooms
                      +BPOSurplus
                      +BPOSurplusPct
                      +Condition_average
                      +Condition_fair
                      +Condition_good
                      +Condition_poor
                      +Condition_unknown
                      +Condo_Flag
                      #+Count #RT
                      +CountyAvgHouseholdIncome
                      +CountyHouseholds
                      +DayOfQuarter
                      +DayOfMonth
                      +DayOfWeek
                      +DayOfYear
                      +Favorites
                      +FinancingAvailable
                      +ForeclosedPer10000
                      +FullWarrantyDeed
                      +HasPrevAttempts
                      +ManufacturedHome_Flag
                      +MedianPPSQFT
                      +MedianPPSQFTDeviation
                      +min_reserve_attainment
                      +MultiResidence_Flag
                      +NumImage
                      +PrevForeclosureSoldRate
                      +PropertyOccupancyStatus_Occupied
                      +PropertyOccupancyStatus_Unknown
                      +PropertyOccupancyStatus_Vacant
                      #+Value #RT
                      +QuitClaimDeed
                      +Reserve
                      +RunNum
                      +SellerPortfolioQuality
                      +SFR_Flag
                      +SpecialWarrantyDeed
                      +Townhouse_Flag
                      +Turnover
                      +ValueLevel_High
                      +ValueLevel_Low
                      +ValueLevel_Med
                      +Total
                      +Total_DA
                      +Total_DS
                      +DA_PERCENT
                      +DS_PERCENT
                      +AvgZipGrade

      )}

#Create a vector of upper and lower limits for 53 variables, including intercept
lo_lim = c(-Inf)
hi_lim = c(Inf)


#Create Placeholders for results
FullOutputlist = numeric(0)
Coeflist = numeric(0)
CoefOutput = numeric(0)
HBlist = numeric(0)
ADlist = numeric(0)
SAlist = numeric(0)
ERlist = numeric(0)
BPOlist = numeric(0)

#Model and placeholders in hand, define groups to iterate over
#Generally, we want as many separate models as possible, then general ones to fill in the blanks

#Find the product types in each state that are large enough to be modeled individually.
ModelData = DATA[DATA$BidEndDT < ymd(date),]
DataCount = aggregate(ModelData[,"GlobalPropertyId"], list(PropertyState = ModelData$PropertyState, ProductType = ModelData$ProductType), length)
names(DataCount)[ncol(DataCount)] = "Count"
DataWinSum = aggregate(ModelData[,"IsSold"], list(PropertyState = ModelData$PropertyState, ProductType = ModelData$ProductType), sum)
names(DataWinSum)[ncol(DataWinSum)] = "Wins"
DataHBCount = aggregate(ModelData[is.na(ModelData$HighBid)==F,"HighBid"], 
            list(PropertyState = ModelData[is.na(ModelData$HighBid)==F,"PropertyState"],
                 ProductType = ModelData[is.na(ModelData$HighBid)==F,"ProductType"]),
            length)
names(DataHBCount)[ncol(DataHBCount)] = "HighBids"
DataSummary = merge(DataCount, DataWinSum, by = c("PropertyState","ProductType"))
DataSummary = merge(DataSummary, DataHBCount, by = c("PropertyState","ProductType"), all.x = T)
DataSummary[is.na(DataSummary$HighBids)==T,"HighBids"] = 0
DataSummary = DataSummary[order(DataSummary$Count, decreasing =T),]
DataSummary = DataSummary[DataSummary$Count > 40 & DataSummary$Wins > 5 & DataSummary$Wins != DataSummary$Count & DataSummary$HighBids > 5,]

#Product types left out of the above ranking will be assigned to general state models with all data.

#Create product type individual models
for( i in 1:nrow(DataSummary)){
  State = DataSummary[i, "PropertyState"]
  ProductType = DataSummary[i, "ProductType"]
  ModelData = DATA[DATA$PropertyState == State & DATA$ProductType == ProductType,]
  
  #Fix Investor and School Metrics
  ModelData$Total[is.na(ModelData$Total)==T] = mean(ModelData$Total, na.rm=T)
  ModelData$Total[is.na(ModelData$Total)==T] = mean(DATA$Total,na.rm=T)
  ModelData$Total_DA[is.na(ModelData$Total_DA)==T] = mean(ModelData$Total_DA, na.rm=T)
  ModelData$Total_DA[is.na(ModelData$Total_DA)==T] = mean(DATA$Total_DA, na.rm=T)
  ModelData$Total_DS[is.na(ModelData$Total_DS)==T] = mean(ModelData$Total_DS, na.rm=T)
  ModelData$Total_DS[is.na(ModelData$Total_DS)==T] = mean(DATA$Total_DS,na.rm=T)
  ModelData$DA_PERCENT[is.na(ModelData$DA_PERCENT)==T] = mean(ModelData$DA_PERCENT, na.rm=T)
  ModelData$DA_PERCENT[is.na(ModelData$DA_PERCENT)==T] = mean(DATA$DA_PERCENT,na.rm=T)
  ModelData$DS_PERCENT[is.na(ModelData$DS_PERCENT)==T] = mean(ModelData$DS_PERCENT, na.rm=T)
  ModelData$DS_PERCENT[is.na(ModelData$DS_PERCENT)==T] = mean(DATA$DS_PERCENT,na.rm=T)
  ModelData$AvgZipGrade[is.na(ModelData$AvgZipGrade)==T] = mean(ModelData$AvgZipGrade, na.rm=T)
  ModelData$AvgZipGrade[is.na(ModelData$AvgZipGrade)==T] = mean(DATA$AvgZipGrade,na.rm=T)
  
  Step = 'Specific Model: Separate Model Data Created. BPO Start'
  
  #Create a model for imputing BPOs where missing or invalid
  BPO = {ModelData[(is.na(ModelData$BPO)==F | is.na(ModelData$HighBid)==F),
                   c("SFR_Flag",
                     "Condo_Flag",
                     "MultiResidence_Flag",
                     "ManufacturedHome_Flag",
                     "BPO",
                     "Bedrooms",
                     "Baths",
                     "Age",
                     "LotSize",
                     "HomeSquareFootage",
                     "PropertyOccupancyStatus_Vacant",
                     "MedianPPSQFT",
                     "CountyAvgHouseholdIncome",
                     "Appreciation",
                     "Turnover",
                     "Condition_fair",
                     "Condition_average",
                     "Condition_good",
                     "Condition_poor",
                     "Condition_unknown",
                     "HighBid"                     
                   )]}
      
  #Introduce an "Unknown" County with median value for all other inputs
  BPO[,1:(ncol(BPO))] = apply(BPO[,1:(ncol(BPO))],2,makeNum)
  BPO$BPO[is.na(BPO$BPO)==T] = BPO$HighBid[is.na(BPO$BPO)==T]
  UkC = apply(BPO[,1:(ncol(BPO)-5)], 2, median, na.rm=T)
  
  UkC = c(UkC,
          Condition_fair = 0,
          Condition_average = 0,
          Condition_good = 0,
          Condition_poor = 0,
          Condition_unknown = 1)
  BPO = rbind(BPO,UkC)
  #Re-Numericize the Condition Dummies
  BPO[,2:(ncol(BPO))] = apply(BPO[,2:(ncol(BPO))],2,makeNum)
  
  #Construct BPO Estimator
  #Remove Missing Data
  BPO$Bedrooms[BPO$Bedrooms < 0] = NA
  BPO$Baths[BPO$Baths < 0] = NA
  BPO$Age[BPO$Age >= 200] = NA
  BPO$LotSize[is.na(BPO$LotSize)==T] = 0.1
  BPO$HomeSquareFootage[BPO$HomeSquareFootage < 100] = NA
  
  
  #Create Valuation Model, note the interaction terms.
  lm.BPO = lm(BPO ~ 
              HomeSquareFootage*Condition_good + 
              HomeSquareFootage*Condition_average + 
              HomeSquareFootage*Condition_fair + 
              HomeSquareFootage*Condition_poor + 
              HomeSquareFootage*Condition_unknown + 
              Bedrooms*Baths + 
              MedianPPSQFT:HomeSquareFootage +
              Age + 
              SFR_Flag +
              Condo_Flag +
              MultiResidence_Flag +
              ManufacturedHome_Flag +
              Bedrooms +
              Baths +
              Age +
              LotSize +
              PropertyOccupancyStatus_Vacant +
              CountyAvgHouseholdIncome +
              Appreciation +
              Turnover
              , data = BPO)
  
  BPOcoef = cbind(as.character(ProductType), as.character(State), coef(lm.BPO))
  BPOlist = rbind(BPOlist, BPOcoef)
  
  #For Debugging, use this to tell where you are in the loop
  Step = 'Specific Model: BPO Model Created'
  
  BPO.hat = predict(object = lm.BPO, newdata = ModelData, type= "response")
  
  #Fix a prediction when it's outside logical bounds.
  #Fix when BPO is below starting bid or when it is above maximum legitimate BPO
  if(any(BPO.hat <= 0 | is.na(BPO.hat)==T)){
    BPO.hat[BPO.hat <= 0 | is.na(BPO.hat)==T] = ifelse(is.na(ModelData$StartingBid[BPO.hat <= 0 | is.na(BPO.hat)==T])==T,
                                                       NA,
                                                       ModelData$StartingBid[BPO.hat <= 0 | is.na(BPO.hat)==T])
  }
  if(any(BPO.hat >= 3*ModelData$Reserve)){
    BPO_index = which(BPO.hat >= 3*ModelData$Reserve | is.na(BPO.hat) ==F)
    BPO.hat[BPO_index] = 3*ModelData$Reserve[BPO_index]
  }
  
  #PredictedBPOs Created
  #Check the distribution of predicted BPOs
  BPO_Comparison = data.frame(GlobalPropertyId = ModelData$GlobalPropertyId, predicted = BPO.hat, actual = ModelData$BPO)
  BPO_Comparison$deviation = (BPO_Comparison[,"predicted"] - as.numeric(BPO_Comparison[,"actual"]))/as.numeric(BPO_Comparison[,"actual"])
  title = paste0("Histogram of BPO Predicted Error Pct. in ", State)
  if(length(BPO_Comparison[is.na(BPO_Comparison[,"deviation"])==F,"deviation"]) > 20){
    hist(BPO_Comparison[,"deviation"],breaks = 10, main = title, xlab = "Pct. Deviation")
  }

  #when a BPO is missing, we want to weight it toward the median BPO of assets in its reserve X-cile
  #This way, we can draw our BPO Estimates back into normal territory
  #Calculating the appropriate replacement value should be done outside the loop
  x = .1
  #A is BPOs, B is Reserves
  A = quantile(ModelData$BPO, probs = seq(0,1,x), na.rm=T)
  B = quantile(ModelData$Reserve, probs = seq(0,1,x), na.rm=T)
  ImputedBPO = 1:(length(B)-1)
  for(k in 1:(length(ImputedBPO))){
    LowerIndex = k
    UpperIndex = k+1
    LB = as.numeric(B[LowerIndex])
    UB = as.numeric(B[UpperIndex])
    ID = which(ModelData$Reserve >= LB & ModelData$Reserve <= UB)
    ImputedBPO[k] = min(ModelData$BPO[ModelData$Reserve >= LB & ModelData$Reserve <= UB],na.rm=T)
  }
  #Bias toward the High End
  ImputedBPO[length(ImputedBPO)+1] = max(ModelData$BPO,na.rm=T)
  
  #Find the rank of the property inside the Reserve range
  Rank = rep(0,nrow(ModelData))
  Index = rep(0,nrow(ModelData))
  
  for(k in 1:nrow(ModelData)){
    C = B
    D = ModelData$Reserve[k]
    if(is.na(D)==T) D = median(ModelData$Reserve,na.rm=T)
    C[length(C)+1] = D
    C = C[order(C)]
    Index[k] = median(which(C==D))
    Rank[k]=ImputedBPO[Index[k]]
    
    #Mix it back in
    AdjustedBPOs = (4*BPO.hat + 3*Rank) / 7
  }
  
  #Now Check new Deviations
  AdjustedBPOs[is.infinite(AdjustedBPOs)==T] = NA
  BPO_Comparison$Adjusted_BPO = AdjustedBPOs
  BPO_Comparison$new_deviation = (BPO_Comparison$Adjusted_BPO - BPO_Comparison$actual)/BPO_Comparison$actual 
  BPO_Comparison$Optimal_BPO_Vector = BPO_Comparison$actual
  BPO_Comparison$Optimal_BPO_Vector[is.na(BPO_Comparison$Optimal_BPO_Vector)==T] = BPO_Comparison$Adjusted_BPO[is.na(BPO_Comparison$Optimal_BPO_Vector)==T]
  BPO_Comparison$Optimal_BPO_Vector[is.na(BPO_Comparison$Optimal_BPO_Vector)==T|is.infinite(BPO_Comparison$Optimal_BPO_Vector)==T] = BPO_Comparison$predicted[is.na(BPO_Comparison$Optimal_BPO_Vector)==T]
  title = paste0("Histogram of BPO Predicted Error Pct. in ", State, " w/ Medians")
  
  if(length(BPO_Comparison$new_deviation[is.na(BPO_Comparison$new_deviation)==F]) > 20) {
    hist(BPO_Comparison$new_deviation[is.na(BPO_Comparison$new_deviation)==F], breaks =10,main = title, xlab = "Pct. Deviation")
    summary(BPO_Comparison$new_deviation)
    summary(BPO_Comparison$deviation)
  }
  #They look OK, let's use these
  if(any(is.na(ModelData$BPO))==T){
    
    ModelData$BPO[is.na(ModelData$BPO)==T] = BPO_Comparison$Optimal_BPO_Vector[is.na(ModelData$BPO)==T]
  }
  ModelData = ModelData[ModelData$BPO != Inf,]
  ModelData$BPOSurplus = ModelData$BPO - ModelData$Reserve
  ModelData$BPOSurplusPct = ModelData$BPOSurplus / ModelData$Reserve
  
  DevIndex = which(is.na(ModelData$MedianPPSQFTDeviation)==T & ModelData$HomeSquareFootage > 0)
  ModelData$MedianPPSQFTDeviation[DevIndex] = (
    ModelData$MedianPPSQFT[DevIndex] - (
      ModelData$Reserve[DevIndex] / 
      ModelData$HomeSquareFootage[DevIndex])
  )
  Step = 'Specific Model: BPO Values Imputed'
    

  
  #Now Create the Data Sets for the individual Predictions
  #Must have all variables present in order to predict.
  CompleteData = ModelData[  is.na(ModelData$ADC_Historical_Execution)==F
                           & is.na(ModelData$Age)==F
                           & is.na(ModelData$Appreciation)==F
                           & is.na(ModelData$avg_reserve_attainment)==F
                           & is.na(ModelData$Baths)==F
                           & is.na(ModelData$Bedrooms)==F
                           & is.na(ModelData$BPOSurplus)==F
                           & is.na(ModelData$BPOSurplusPct)==F
                           & is.na(ModelData$Condition_average)==F
                           & is.na(ModelData$Condition_fair)==F
                           & is.na(ModelData$Condition_good)==F
                           & is.na(ModelData$Condition_poor)==F
                           & is.na(ModelData$Condition_unknown)==F
                           & is.na(ModelData$Condo_Flag)==F
                           & is.na(ModelData$CountyAvgHouseholdIncome)==F
                           & is.na(ModelData$CountyHouseholds)==F
                           & is.na(ModelData$DayOfQuarter)==F
                           & is.na(ModelData$DayOfMonth)==F
                           & is.na(ModelData$DayOfWeek)==F
                           & is.na(ModelData$Favorites)==F
                           & is.na(ModelData$FinancingAvailable)==F
                           & is.na(ModelData$ForeclosedPer10000)==F
                           & is.na(ModelData$FullWarrantyDeed)==F
                           & is.na(ModelData$HasPrevAttempts)==F
                           & is.na(ModelData$ManufacturedHome_Flag)==F
                           & is.na(ModelData$MedianPPSQFT)==F
                           & is.na(ModelData$MedianPPSQFTDeviation)==F
                           & is.na(ModelData$min_reserve_attainment)==F
                           & is.na(ModelData$MultiResidence_Flag)==F
                           & is.na(ModelData$NumImage)==F
                           & is.na(ModelData$PrevForeclosureSoldRate)==F
                           & is.na(ModelData$PropertyOccupancyStatus)==F
                           & is.na(ModelData$PropertyOccupancyStatus_Unknown)==F
                           & is.na(ModelData$PropertyOccupancyStatus_Vacant)==F
                           & is.na(ModelData$QuitClaimDeed)==F
                           & is.na(ModelData$Reserve)==F
                           & is.na(ModelData$RunNum)==F
                           & is.na(ModelData$SellerPortfolioQuality)==F
                           & is.na(ModelData$SFR_Flag)==F
                           & is.na(ModelData$SpecialWarrantyDeed)==F
                           & is.na(ModelData$Townhouse_Flag)==F
                           & is.na(ModelData$ValueLevel_High)==F
                           & is.na(ModelData$ValueLevel_Low)==F
                           & is.na(ModelData$ValueLevel_Med)==F
                           & is.na(ModelData$Total)==F
                           & is.na(ModelData$Total_DA)==F
                           & is.na(ModelData$Total_DS)==F
                           & is.na(ModelData$DA_PERCENT)==F
                           & is.na(ModelData$DS_PERCENT)==F
                           & is.na(ModelData$AvgZipGrade)==F
                           ,]
  
  
  if(all(CompleteData$QuitClaimDeed == CompleteData$QuitClaimDeed[1])){
    CompleteData$QuitClaimDeed[1] = abs(CompleteData$QuitClaimDeed[1]-1)
  }
  if(all(CompleteData$SpecialWarrantyDeed == CompleteData$SpecialWarrantyDeed[1])){
    CompleteData$SpecialWarrantyDeed[1] = abs(CompleteData$SpecialWarrantyDeed[1]-1)
  }
  if(all(CompleteData$FullWarrantyDeed == CompleteData$FullWarrantyDeed[1])){
    CompleteData$FullWarrantyDeed[1] = abs(CompleteData$FullWarrantyDeed[1]-1)
  }
  
  
  
  
  #Must have all values and also high bid to get optimal reserve / predicted high bid
  HIGH_BID_TRAIN = CompleteData[  CompleteData$BidEndDT < ymd(date)
                                  & is.na(CompleteData$HighBid)==F,]
  
  #Define a training set for the likleihood to sell metrics
  LTS_TRAIN = {CompleteData[CompleteData$BidEndDT < ymd(date),]}
  
  #Need Complete Data to Predict upcoming auctions and also they must be in the future
  TEST = {CompleteData[CompleteData$BidEndDT >= ymd(date),]}
  
  #Run Predictions start with AuctionDay Sale (Easiest Criteria)
  #Train a model
  x = LTS_TRAIN
  y = x$IsSold
  y = y[is.na(y)==F]
  names(x)[which(names(x)=="IsSold")] = "Sold_Criteria"
  x = model.matrix(object = FORMULA, data = x)
  AD = glmnet(x, y, family = "binomial", alpha = 0.00001,  upper.limits = hi_lim, lower.limits = lo_lim)
  cvmr = cv.glmnet(x, y)
  
  #Increment the Step for Debugging
  Step = 'Specific Model: Auction Day Sale Prediction Run on Training Data'
  
  #Predict for new data
  if(nrow(TEST)>0){
    predx = TEST
    if(any(is.na(predx$HighBid)==T)){
      predx$HighBid[is.na(predx$HighBid)==T]  = 0   
    }
    names(predx)[which(names(predx)=="IsSold")] = "Sold_Criteria"
    predx = model.matrix(object = FORMULA, predx)
    AuctionDaySalePrediction = predict(AD, newx = predx, s = cvmr$lambda.min, type = "response")
    
    #Increment the Step for Debugging
    Step = 'Specific Model: Is Sold Prediction Run on Test Data'
  }
  
  #Continue with Ultimately Accepted / NetSold (Medium Difficulty Criteria)
  x = LTS_TRAIN
  y = x$IsNetSold
  y = y[is.na(y)==F]
  if(all(unique(y))==0){
    y[which((LTS_TRAIN$Reserve - LTS_TRAIN$HighBid)/LTS_TRAIN$Reserve == 
              min((LTS_TRAIN$Reserve - LTS_TRAIN$HighBid)/LTS_TRAIN$Reserve, na.rm=T))] = 1
  }
  names(x)[which(names(x)=="IsNetSold")] = "Sold_Criteria"
  x = model.matrix(object = FORMULA, data = x)
  SA = glmnet(x, y, family = "binomial", alpha = 0.00001,  upper.limits = hi_lim, lower.limits = lo_lim)
  cvsa = cv.glmnet(x, y)
  
  #Increment the Step for Debugging
  Step = 'Specific Model: Is Net Sale Prediction Run on Training Data'
  
  #Predict NetSold for New Data
  if(nrow(TEST)>0){
    predx = TEST
    if(any(is.na(predx$HighBid)==T)){
      predx$HighBid[is.na(predx$HighBid)==T]  = 0   
    }
    predx$IsNetSold[is.na(predx$IsNetSold)==T]=0
    names(predx)[which(names(predx)=="IsNetSold")] = "Sold_Criteria"
    predx = model.matrix(object = FORMULA, predx)
    SellerAcceptedSalePrediction = predict(SA, newx = predx, s = cvsa$lambda.min, type = "response")
    
    #Increment the Step for Debugging
    Step = 'Specific Model: Is Net Sale Prediction Run on Test Data'
  }
  
  #Continue with Exceeds Seller Reserve (Hardest Criteria)
  x = LTS_TRAIN
  y = x$AuctionReserveSuccess
    y = y[is.na(y)==F]
  if(all(unique(y))==0){
    y[which((LTS_TRAIN$Reserve - LTS_TRAIN$HighBid)/LTS_TRAIN$Reserve == 
              min((LTS_TRAIN$Reserve - LTS_TRAIN$HighBid)/LTS_TRAIN$Reserve, na.rm=T))] = 1
  }
  names(x)[which(names(x)=="AuctionReserveSuccess")] = "Sold_Criteria"
  x = model.matrix(object = FORMULA, data = x)
  ER = glmnet(x, y, family = "binomial", alpha = 0.00001,  upper.limits = hi_lim, lower.limits = lo_lim)
  cver = cv.glmnet(x, y)
  
  #Increment the Step for Debugging
  Step = 'Specific Model: Exceeds Reserve Prediction Run on Training Data'
  
  #Predict AuctionReserveSuccess for New Data
  if(nrow(TEST)>0){
    predx = TEST
    if(any(is.na(predx$HighBid)==T)){
      predx$HighBid[is.na(predx$HighBid)==T]  = 0   
    }
    names(predx)[which(names(predx)=="AuctionReserveSuccess")] = "Sold_Criteria"
    predx = model.matrix(object = FORMULA, predx)
    ExceedsReservePrediction = predict(ER, newx = predx, s = cver$lambda.min, type = "response")

    #Increment the Step for Debugging
    Step = 'Specific Model: Exceeds Reserve Prediction Run on Test Data'
  }
  

  #Continue with High Bid Prediction
  #We will transform High Bid to Log and Exponentiate it after
  #Note, we tried using logs, but we did not see a performance improvement
  #For future use, if we transform inputs into standardized units, we should revisit this
  x = HIGH_BID_TRAIN
  y = x$HighBid
  y[x$issold==1 & is.na(y)==T] = .90*x$Reserve
  names(x)[which(names(x)=="HighBid")] = "Sold_Criteria"
  x = model.matrix(object = FORMULA, data = x)
  HB = glmnet(x, y, family = "gaussian", alpha = 0.005, standardize = T,  upper.limits = hi_lim, lower.limits = lo_lim)
  cvhb = cv.glmnet(x, y, lambda = HB$lambda)
  
  #Increment the Step for Debugging
  Step = 'Specific Model: High Bid Prediction Run on Training Data'
  
  #Predict High Bid for New Data
  if(nrow(TEST)>0){
    predx = TEST
    if(any(is.na(predx$HighBid)==T)){
      predx$HighBid[is.na(predx$HighBid)==T]  = 0   
    }
    names(predx)[which(names(predx)=="HighBid")] = "Sold_Criteria"
    predx = model.matrix(object = FORMULA, predx)
    HighBidPrediction = predict(HB, newx = predx, s = HB$lambda[length(HB$lambda)], type = "response") 

    #Increment the Step for Debugging
    Step = "Specific Model: High Bid Prediction Run on Test Data"
  }
  
  #Gather the model coefficients and prepare for export
  State = as.character(State)
  ProductType = as.character(ProductType)
  vector_len = length(as.vector(coef(AD, s = cvmr$lambda.min))[-2])
  row_vector = matrix(data = rep(0,times = vector_len),nrow = 1)
  
  row_vector[1,1:vector_len] = as.vector(coef(AD, s = cvmr$lambda.min))[-2]
  LAD1 = cbind(State, "AuctionDaySale", ProductType, row_vector, deparse.level = 1)
  
  row_vector[1,1:vector_len] = as.vector(coef(SA, s = cvsa$lambda.min))[-2]
  LSA1 = cbind(State, "SellerAccepted", ProductType, row_vector, deparse.level = 1)
  
  row_vector[1,1:vector_len] = as.vector(coef(ER, s = cver$lambda.min))[-2]
  LER1 = cbind(State, "ExceedsReserve", ProductType, row_vector, deparse.level = 1)
  
  row_vector[1,1:vector_len] = as.vector(coef(HB, s = HB$lambda[length(HB$lambda)]))[-2]
  LHB1 = cbind(State, "HighBid", ProductType, row_vector, deparse.level = 1) #Remove the model.matrix intercept
  #Name the columns to keep things ordered
  names(LAD1) = c("PropertyState","ModelType","ProductType", rownames(coef(AD, s = cvmr$lambda.min))[-2])
  names(LSA1) = c("PropertyState","ModelType","ProductType", rownames(coef(SA, s = cvsa$lambda.min))[-2])
  names(LER1) = c("PropertyState","ModelType","ProductType", rownames(coef(ER, s = cver$lambda.min))[-2])
  names(LHB1) = c("PropertyState","ModelType","ProductType", rownames(coef(HB, s = HB$lambda[length(HB$lambda)]))[-2])
  
  Coeflist = rbind(LAD1,LSA1,LER1,LHB1)
  CoefOutput = rbind(CoefOutput, Coeflist)
  
  #Implement Logical Fixes for predictions
  SellerAcceptedSalePrediction = ifelse( SellerAcceptedSalePrediction < ExceedsReservePrediction, 
                                         ExceedsReservePrediction, SellerAcceptedSalePrediction)
  AuctionDaySalePrediction = ifelse( AuctionDaySalePrediction < SellerAcceptedSalePrediction, 
                                     SellerAcceptedSalePrediction, AuctionDaySalePrediction)

  if(nrow(TEST)>0){
    TEST$PredictionDate = rundate

    #FullOutputlist
    FullOutput = TEST
    #Create Housing Quality Grade
    FullOutput$AuctionDaySalePrediction = AuctionDaySalePrediction
    FullOutput$SellerAcceptedSalePrediction = SellerAcceptedSalePrediction
    FullOutput$ExceedsReservePrediction = ExceedsReservePrediction
    FullOutput$HighBidPrediction = HighBidPrediction[,1] 
    #Sometimes, assets have a perfect storm of positive characteristics that make predict well above reserve.
    Unreasonable_Preds = which(FullOutput$HighBidPrediction > 1.5*FullOutput$Reserve & FullOutput$HighBidPrediction > 200000)
    FullOutput$HighBidPrediction[Unreasonable_Preds] = 1.5*FullOutput$Reserve[Unreasonable_Preds]
    
    #Bundle this version together
    FullOutputlist = rbind(FullOutputlist, FullOutput)
  }
}

#That takes care of the large group ProductTypes, now we need general models for each state for all the "Other" assets
Step = "Specific Model: Finished. Starting General Model."

#Product types left out of the above ranking will be assigned to general state models with all data.
#Find the list of product types that must be given general models in each state
ProductTypes = unique(DATA$ProductType)


#Create statewide models

#Start by defining modelable states
DataCount = aggregate(DATA[,"GlobalPropertyId"], list(PropertyState = DATA$PropertyState), length)
names(DataCount)[ncol(DataCount)] = "Count"
DataWinSum = aggregate(DATA[,"IsSold"], list(PropertyState = DATA$PropertyState), sum)
names(DataWinSum)[ncol(DataWinSum)] = "Wins"
StateSummary = merge(DataCount, DataWinSum, by = c("PropertyState"))
StateSummary = StateSummary[order(StateSummary$Count, decreasing =T),]
StateSummary = StateSummary[StateSummary$Count > 40 & StateSummary$Wins > 0 & StateSummary$Wins != StateSummary$Count,]

GenSummary = merge(StateSummary, ProductTypes, all=T)
names(GenSummary)[names(GenSummary)=="y"] = "ProductType"
GenSummary = GenSummary[! paste0(GenSummary$PropertyState, GenSummary$ProductType) %in% paste0(DataSummary$PropertyState, DataSummary$ProductType), ]

#The above states can be converted into general models, those falling out of these cannot.
for( i in 1:nrow(GenSummary)){
  ProductType = GenSummary[i,"ProductType"]
  State = GenSummary[i, "PropertyState"]
  ModelData = DATA[DATA$PropertyState == State,]
  
  #Fix Model Data PPSQFT deviations
  DevIndex = which(is.na(ModelData$MedianPPSQFTDeviation)==T & ModelData$HomeSquareFootage > 0)
  ModelData$MedianPPSQFTDeviation[DevIndex] = (
    ModelData$MedianPPSQFT[DevIndex] - (
      ModelData$Reserve[DevIndex] / 
        ModelData$HomeSquareFootage[DevIndex])
  )
  
  #Fix Investor and School Metrics
  ModelData$Total[is.na(ModelData$Total)==T] = mean(ModelData$Total, na.rm=T)
  ModelData$Total[is.na(ModelData$Total)==T] = mean(DATA$Total,na.rm=T)
  ModelData$Total_DA[is.na(ModelData$Total_DA)==T] = mean(ModelData$Total_DA, na.rm=T)
  ModelData$Total_DA[is.na(ModelData$Total_DA)==T] = mean(DATA$Total_DA, na.rm=T)
  ModelData$Total_DS[is.na(ModelData$Total_DS)==T] = mean(ModelData$Total_DS, na.rm=T)
  ModelData$Total_DS[is.na(ModelData$Total_DS)==T] = mean(DATA$Total_DS,na.rm=T)
  ModelData$DA_PERCENT[is.na(ModelData$DA_PERCENT)==T] = mean(ModelData$DA_PERCENT, na.rm=T)
  ModelData$DA_PERCENT[is.na(ModelData$DA_PERCENT)==T] = mean(DATA$DA_PERCENT,na.rm=T)
  ModelData$DS_PERCENT[is.na(ModelData$DS_PERCENT)==T] = mean(ModelData$DS_PERCENT, na.rm=T)
  ModelData$DS_PERCENT[is.na(ModelData$DS_PERCENT)==T] = mean(DATA$DS_PERCENT,na.rm=T)
  ModelData$AvgZipGrade[is.na(ModelData$AvgZipGrade)==T] = mean(ModelData$AvgZipGrade, na.rm=T)
  ModelData$AvgZipGrade[is.na(ModelData$AvgZipGrade)==T] = mean(DATA$AvgZipGrade,na.rm=T)
  
  CompleteData = ModelData[    is.na(ModelData$ADC_Historical_Execution)==F
                             & is.na(ModelData$Age)==F
                             & is.na(ModelData$Appreciation)==F
                             & is.na(ModelData$avg_reserve_attainment)==F
                             & is.na(ModelData$Baths)==F
                             & is.na(ModelData$Bedrooms)==F
                             & is.na(ModelData$BPOSurplus)==F
                             & is.na(ModelData$BPOSurplusPct)==F
                             & is.na(ModelData$Condition_average)==F
                             & is.na(ModelData$Condition_fair)==F
                             & is.na(ModelData$Condition_good)==F
                             & is.na(ModelData$Condition_poor)==F
                             & is.na(ModelData$Condition_unknown)==F
                             & is.na(ModelData$Condo_Flag)==F
                             & is.na(ModelData$CountyAvgHouseholdIncome)==F
                             & is.na(ModelData$CountyHouseholds)==F
                             & is.na(ModelData$DayOfQuarter)==F
                             & is.na(ModelData$DayOfMonth)==F
                             & is.na(ModelData$DayOfWeek)==F
                             & is.na(ModelData$Favorites)==F
                             & is.na(ModelData$FinancingAvailable)==F
                             & is.na(ModelData$ForeclosedPer10000)==F
                             & is.na(ModelData$FullWarrantyDeed)==F
                             & is.na(ModelData$HasPrevAttempts)==F
                             & is.na(ModelData$ManufacturedHome_Flag)==F
                             & is.na(ModelData$MedianPPSQFT)==F
                             & is.na(ModelData$MedianPPSQFTDeviation)==F
                             & is.na(ModelData$min_reserve_attainment)==F
                             & is.na(ModelData$MultiResidence_Flag)==F
                             & is.na(ModelData$NumImage)==F
                             & is.na(ModelData$PrevForeclosureSoldRate)==F
                             & is.na(ModelData$PropertyOccupancyStatus)==F
                             & is.na(ModelData$PropertyOccupancyStatus_Unknown)==F
                             & is.na(ModelData$PropertyOccupancyStatus_Vacant)==F
                             & is.na(ModelData$QuitClaimDeed)==F
                             & is.na(ModelData$Reserve)==F
                             & is.na(ModelData$RunNum)==F
                             & is.na(ModelData$SellerPortfolioQuality)==F
                             & is.na(ModelData$SFR_Flag)==F
                             & is.na(ModelData$SpecialWarrantyDeed)==F
                             & is.na(ModelData$Townhouse_Flag)==F
                             & is.na(ModelData$ValueLevel_High)==F
                             & is.na(ModelData$ValueLevel_Low)==F
                             & is.na(ModelData$ValueLevel_Med)==F
                             & is.na(ModelData$Total)==F
                             & is.na(ModelData$Total_DA)==F
                             & is.na(ModelData$Total_DS)==F
                             & is.na(ModelData$DA_PERCENT)==F
                             & is.na(ModelData$DS_PERCENT)==F
                             & is.na(ModelData$AvgZipGrade)==F
                             ,]
  
  #Must have all values and also high bid to get optimal reserve / predicted high bid
  HIGH_BID_TRAIN = CompleteData[  CompleteData$BidEndDT < ymd(date)
                                  & is.na(CompleteData$HighBid)==F,]
  
  #Define a training set for the likleihood to sell metrics
  LTS_TRAIN = {CompleteData[CompleteData$BidEndDT < ymd(date),]}
  
  #Need Complete Data to Predict upcoming auctions and also they must be in the future
  TEST_STATE = {CompleteData[CompleteData$BidEndDT >= ymd(date),]}
  
  #Remove assets from test group that are already in the predictive set for the producttype specific group
  TEST_STATE = TEST_STATE[ ! TEST_STATE$GlobalPropertyId %in% FullOutputlist$GlobalPropertyId,]
  
  #Run Predictions start with AuctionDay Sale (Easiest Criteria)
  #Train a model
  x = LTS_TRAIN
  y = x$IsSold
    y = y[is.na(y)==F]
  names(x)[which(names(x)=="IsSold")] = "Sold_Criteria"
  x = model.matrix(object = FORMULA, data = x)
  AD = glmnet(x, y, family = "binomial", alpha = 0.00001,  upper.limits = hi_lim, lower.limits = lo_lim)
  cvmr = cv.glmnet(x, y)
  
  #Increment the Step for Debugging
  Step = 'Auction Day Sale Prediction Run on Training Data'
  
  if(nrow(TEST_STATE)>0){
    #Predict for new data
    predx = TEST_STATE
    if(any(is.na(predx$HighBid)==T)){
      predx$HighBid[is.na(predx$HighBid)==T]  = 0   
    }
    names(predx)[which(names(predx)=="IsSold")] = "Sold_Criteria"
    predx = model.matrix(object = FORMULA, predx)
    AuctionDaySalePrediction = predict(AD, newx = predx, s = cvmr$lambda.min, type = "response")
  }
  
  #Continue with Ultimately Accepted / NetSold (Medium Difficulty Criteria)
  x = LTS_TRAIN
  y = x$IsNetSold
    y = y[is.na(y)==F]
  if(all(unique(y))==0){
    y[which((LTS_TRAIN$Reserve - LTS_TRAIN$HighBid)/LTS_TRAIN$Reserve == 
              min((LTS_TRAIN$Reserve - LTS_TRAIN$HighBid)/LTS_TRAIN$Reserve, na.rm=T))] = 1
  }
  names(x)[which(names(x)=="IsNetSold")] = "Sold_Criteria"
  x = model.matrix(object = FORMULA, data = x)
  SA = glmnet(x, y, family = "binomial", alpha = 0.00001,  upper.limits = hi_lim, lower.limits = lo_lim)
  cvsa = cv.glmnet(x, y)
  
  #Increment the Step for Debugging
  Step = 'Is Net Sale Prediction Run on Training Data'
  
  if(nrow(TEST_STATE)>0){
    #Predict NetSold for New Data
    predx = TEST_STATE
    if(any(is.na(predx$HighBid)==T)){
      predx$HighBid[is.na(predx$HighBid)==T]  = 0   
    }
    predx$IsNetSold[is.na(predx$IsNetSold)==T]=0
    names(predx)[which(names(predx)=="IsNetSold")] = "Sold_Criteria"
    predx = model.matrix(object = FORMULA, predx)
    SellerAcceptedSalePrediction = predict(SA, newx = predx, s = cvsa$lambda.min, type = "response")
  }
  
  #Increment the Step for Debugging
  Step = 'Is Net Sale Prediction Run on Test Data'
  
  #Continue with Exceeds Seller Reserve (Hardest Criteria)
  x = LTS_TRAIN
  y = x$AuctionReserveSuccess
    y = y[is.na(y)==F]
  if(all(unique(y))==0){
    y[which((LTS_TRAIN$Reserve - LTS_TRAIN$HighBid)/LTS_TRAIN$Reserve == 
              min((LTS_TRAIN$Reserve - LTS_TRAIN$HighBid)/LTS_TRAIN$Reserve, na.rm=T))] = 1
  }
  names(x)[which(names(x)=="AuctionReserveSuccess")] = "Sold_Criteria"
  x = model.matrix(object = FORMULA, data = x)
  ER = glmnet(x, y, family = "binomial", alpha = 0.00001,  upper.limits = hi_lim, lower.limits = lo_lim)
  cver = cv.glmnet(x, y)
  
  #Increment the Step for Debugging
  Step = 'Exceeds Reserve Prediction Run on Training Data'
  
  if(nrow(TEST_STATE)>0){
    #Predict AuctionReserveSuccess for New Data
    predx = TEST_STATE
    if(any(is.na(predx$HighBid)==T)){
      predx$HighBid[is.na(predx$HighBid)==T]  = 0   
    }
    names(predx)[which(names(predx)=="AuctionReserveSuccess")] = "Sold_Criteria"
    predx = model.matrix(object = FORMULA, predx)
    ExceedsReservePrediction = predict(ER, newx = predx, s = cver$lambda.min, type = "response")
  }
  
  #Increment the Step for Debugging
  Step = 'Exceeds Reserve Prediction Run on Test Data'
  
  #Continue with High Bid Prediction
  #We will transform High Bid to Log and Exponentiate it after
  #Note, we tried using logs, but we did not see a performance improvement
  #For future use, if we transform inputs into standardized units, we should revisit this
  x = HIGH_BID_TRAIN
  y = x$HighBid
  y[x$issold==1 & is.na(y)==T] = .90*x$Reserve
  names(x)[which(names(x)=="HighBid")] = "Sold_Criteria"
  x = model.matrix(object = FORMULA, data = x)
  HB = glmnet(x, y, family = "gaussian", alpha = 0.005, standardize = T,  upper.limits = hi_lim, lower.limits = lo_lim)
  cvhb = cv.glmnet(x, y, lambda = HB$lambda)
  
  #Increment the Step for Debugging
  Step = 'High Bid Prediction Run on Training Data'
  
  if(nrow(TEST_STATE)>0){
    #Predict High Bid for New Data
    predx = TEST_STATE
    if(any(is.na(predx$HighBid)==T)){
      predx$HighBid[is.na(predx$HighBid)==T]  = 0   
    }
    names(predx)[which(names(predx)=="HighBid")] = "Sold_Criteria"
    predx = model.matrix(object = FORMULA, predx)
    HighBidPrediction = predict(HB, newx = predx, s = HB$lambda[length(HB$lambda)], type = "response") 
  }
  
  #Increment the Step for Debugging
  Step = "High Bid Prediction Run on Test Data"
  
  #Gather the model coefficients and prepare for export
  State = as.character(State)
  ProductType = as.character(ProductType)
  vector_len = length(as.vector(coef(AD, s = cvmr$lambda.min))[-2])
  row_vector = matrix(data = rep(0,times = vector_len),nrow = 1)
  
  row_vector[1,1:vector_len] = as.vector(coef(AD, s = cvmr$lambda.min))[-2]
  LAD1 = cbind(State, "AuctionDaySale", ProductType, row_vector, deparse.level = 1)
  
  row_vector[1,1:vector_len] = as.vector(coef(SA, s = cvsa$lambda.min))[-2]
  LSA1 = cbind(State, "SellerAccepted", ProductType, row_vector, deparse.level = 1)
  
  row_vector[1,1:vector_len] = as.vector(coef(ER, s = cver$lambda.min))[-2]
  LER1 = cbind(State, "ExceedsReserve", ProductType, row_vector, deparse.level = 1)
  
  row_vector[1,1:vector_len] = as.vector(coef(HB, s = HB$lambda[length(HB$lambda)]))[-2]
  LHB1 = cbind(State, "HighBid", ProductType, row_vector, deparse.level = 1) #Remove the model.matrix intercept
  #Name the columns to keep things ordered
  names(LAD1) = c("PropertyState","ModelType","ProductType", rownames(coef(AD, s = cvmr$lambda.min))[-2])
  names(LSA1) = c("PropertyState","ModelType","ProductType", rownames(coef(SA, s = cvsa$lambda.min))[-2])
  names(LER1) = c("PropertyState","ModelType","ProductType", rownames(coef(ER, s = cver$lambda.min))[-2])
  names(LHB1) = c("PropertyState","ModelType","ProductType", rownames(coef(HB, s = HB$lambda[length(HB$lambda)]))[-2])
  #Bind the list to save for export
  
  
  
  Coeflist = rbind(LAD1,LSA1,LER1,LHB1)
  CoefOutput = rbind(CoefOutput, Coeflist)
  
  Step = paste0("Coefficient Vector Finished for ", State, ' ', ProductType)
  
  
}

name_vec = rownames(coef(AD, s = cvmr$lambda.min))[-2]
name_vec = str_replace_all(name_vec, '[\\(\\)]', '')
replace_names = c("PropertyState","ModelType","ProductType", name_vec)
colnames(CoefOutput) = replace_names

#create fault lookup table
NegLookup = numeric(0)
PropertyFaultName = c("NULL",
  "ADC Has Historically Poor Execution in this ZIP",
  "Age of House Out of Line With Area",
  "Price Appreciation in ZIP Suggests Poor Returns on Investment",
  "Seller Rarely Sells Below Reserve",
  "Non-Standard Number of Bathrooms for Area",
  "Non-Standard Number of Bedrooms for Area",
  "Low Valuation Vs Reserve Means Little Profit for Investors",
  "Valuation Low as a Share of Reserve",
  "Average Condition Suggests Low Investment Potential",
  "Fair Condition Suggests Expensive Rehab",
  "Good Condition Suggests Low Return on Investment",
  "Poor Condition Suggests Expensive Refurbishment",
  "Condition Information Unknown",
  "Condos Unpopular",
  "Income Trends in County Indicate Limited Resale Potential",
  "Low Population County",
  "End of Quarter Usually Lower Performing",
  "Time of Month Historically Lower Performing in Geo",
  "Time of Week Historically Lower Performing in Geo",
  "Time of Year Historically Lower Performing in Geo",
  "Poor Web Traffic",
  "Financing Incentive Suggests Expensive Rehab Or Bad Area",
  "ZIP has high incidence of foreclosure",
  "Full Warranty Deed In Distressed-Deed Preferred Market",
  "Property Has At Least One Previous Attempt at Auction",
  "Manufactured Homes Unpopular",
  "Median Price in ZIP Suggests Limited Price Appreciation From Resale",
  "Reserve Expensive for ZIP on a $/SqFt Basis",
  "Seller's Minimum Accepted Bid is Historically Very High",
  "Multiple Unit Home in Single Preferred Region",
  "Photos Suggest Poor Investment",
  "Statistically Few Foreclosures are Resold in ZIP",
  "Occupied Properties Generally Harder to Resell",
  "Occupancy Status Unknown",
  "Vacancy Suggests Potential Condition Problems",
  "Quit Claim Deed Suggests High Risk",
  "High Reserve -- Should never suggest this",
  "Property Has Many Attempts at Auction",
  "Seller Portfolio Historically Poor in This Area",
  "Single Family Residences Unpopular vs More Dense Options",
  "Special Warranty Deed Suggests Moderate Risk for Investors",
  "Townhouses Unpopular",
  "Low Turnover in ZIP Suggests Difficult Resale",
  "Property High Priced Relative to State",
  "Property Low Priced Relative to State",
  "Property Medium Priced Relative to State",
  "Total Number of Investor Sales",
  "Total Number of Local Investor Sales",
  "Total Number of Out-Of-State Investor Sales",
  "Percentage of Local Investory Sales",
  "Percentage of Out-Of-State Investor Sales",
  "School Grades in Zip Codes")

for( i in 1:nrow(CoefOutput)){
  this_row = CoefOutput[i,]
  keys = this_row[names(this_row) %in% c("PropertyState","ProductType","ModelType")]
  keys = data.frame(t(keys))
  PropertyFaultVariables = names(this_row)[-which(names(keys) %in% names(this_row))]
  PropertyFaultVariable = data.frame(PropertyFaultVariables)
  Value = this_row[PropertyFaultVariables]
  Value = data.frame(Value)
  
  this_block = merge(PropertyFaultVariable,keys, all=T )
  this_block = cbind(this_block, Value,deparse.level = 1)
  this_block = cbind(this_block, PropertyFaultName, deparse.level = 1)
  
  NegLookup = rbind(NegLookup, this_block)
}

#Categorize the results into likelihood groups
LikelihoodLevels = c(0.00, 0.05, 0.50, 0.80, 1.00)
FullOutputlist$SaleLikelihood = "Low"
FullOutputlist$SaleLikelihood[FullOutputlist$AuctionDaySalePrediction >= LikelihoodLevels[2]] = "Borderline"
FullOutputlist$SaleLikelihood[FullOutputlist$AuctionDaySalePrediction >= LikelihoodLevels[3]] = "Moderate"
FullOutputlist$SaleLikelihood[FullOutputlist$AuctionDaySalePrediction >= LikelihoodLevels[4]] = "High"

#And now to write the results to a file
filename = paste("GLMCoefficientOutput_", rundate, ".csv", sep = "")
write.csv(CoefOutput, filename, row.names = F, quote = F, col.names=T)
filename = paste("GLMPredFullOutput_", rundate, ".csv", sep = "")
write.csv(FullOutputlist, filename, row.names = F, quote = F)
filename = "BPOEstimationCoefficients.csv"
write.csv(BPOlist, filename, row.names = T, quote = F)
filename = "GLMPriceSensitiveNegLookup.csv"
write.csv(NegLookup, filename, row.names = F, quote = F)














#While we're here, we'd like to do some analytics as well
#Prediction Analytics
hist(FullOutputlist$AuctionDaySalePrediction, xlab= "Sale Likelihood Prediction", main= "Frequency of Per-Attempt Chance to Sell Predictions")
hist1 <- hist(FullOutputlist$AuctionDaySalePrediction[FullOutputlist$BidEndDT > ymd(rundate)]
              , xlab= "Sale Likelihood Prediction", main= "Upcoming 30 Day Sale Likelihood Predictions", breaks = 20)
hist2 <- hist(FullOutputlist$AuctionDaySalePrediction[FullOutputlist$BidEndDT > ymd(rundate) & FullOutputlist$BidEndDT < ymd(rundate) + days(3)]
              , xlab= "Sale Likelihood Prediction", main= "Upcoming 3 Day Sale Likelihood Predictions", breaks = 20)  
hist2 <- hist(FullOutputlist$AuctionDaySalePrediction[FullOutputlist$BidEndDT > ymd(rundate) & FullOutputlist$BidEndDT < ymd(rundate) + days(7)]
              , xlab= "Sale Likelihood Prediction", main= "Upcoming 7 Day Sale Likelihood Predictions", breaks = 20)
hist2 <- hist(FullOutputlist$AuctionDaySalePrediction[FullOutputlist$BidEndDT > ymd(rundate) & FullOutputlist$BidEndDT < ymd(rundate) + days(14)]
              , xlab= "Sale Likelihood Prediction", main= "Upcoming 14 Day Sale Likelihood Predictions", breaks = 20)

hist3 <- hist(FullOutputlist$AuctionDaySalePrediction[FullOutputlist$BidEndDT < ymd(rundate)]
              , xlab= "Sale Likelihood Prediction", main= "Trailing 30 Day Sale Likelihood Predictions")

mean(FullOutputlist$AuctionDaySalePrediction)

hist(round(FullOutputlist$AuctionDaySalePrediction[FullOutputlist$BidEndDT > ymd(rundate)], digits=0) - 
       FullOutputlist$IsSold[FullOutputlist$BidEndDT > ymd(rundate)],
     xlab = "Error Type ( 0 = Correct, 1 = False Positive, -1 = False Negative)", main = "Frequency of Errors in Test Set") 

table(round(FullOutputlist$AuctionDaySalePrediction[FullOutputlist$BidEndDT < ymd(rundate)], digits=0) - 
        FullOutputlist$IsSold[FullOutputlist$BidEndDT < ymd(rundate)])


#Pre-Auction Stats
PrePost = data.frame(rbind(
  cbind(
    mean(FullOutputlist$BPOSurplus[FullOutputlist$BidEndDT > ymd(rundate)]),
    mean(FullOutputlist$BPOSurplus[FullOutputlist$BidEndDT < ymd(rundate)]),
    mean(FullOutputlist$Reserve[FullOutputlist$BidEndDT > ymd(rundate)]),
    mean(FullOutputlist$Reserve[FullOutputlist$BidEndDT < ymd(rundate)]),
    mean(FullOutputlist$BPO[FullOutputlist$BidEndDT > ymd(rundate)]),
    mean(FullOutputlist$BPO[FullOutputlist$BidEndDT < ymd(rundate)]),
    mean(FullOutputlist$BPOSurplusPct[FullOutputlist$BidEndDT > ymd(rundate)]),
    mean(FullOutputlist$BPOSurplusPct[FullOutputlist$BidEndDT < ymd(rundate)])
  ),
  cbind(
    mean(FullOutputlist$BPOSurplus[FullOutputlist$BidEndDT > ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
    mean(FullOutputlist$BPOSurplus[FullOutputlist$BidEndDT < ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
    mean(FullOutputlist$Reserve[FullOutputlist$BidEndDT > ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
    mean(FullOutputlist$Reserve[FullOutputlist$BidEndDT < ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
    mean(FullOutputlist$BPO[FullOutputlist$BidEndDT > ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
    mean(FullOutputlist$BPO[FullOutputlist$BidEndDT < ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
    mean(FullOutputlist$BPOSurplusPct[FullOutputlist$BidEndDT > ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
    mean(FullOutputlist$BPOSurplusPct[FullOutputlist$BidEndDT < ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")])
  ),
  cbind(
    mean(FullOutputlist$BPOSurplus[FullOutputlist$BidEndDT > ymd(rundate) & FullOutputlist$SellerCode == "NST"]),
    mean(FullOutputlist$BPOSurplus[FullOutputlist$BidEndDT < ymd(rundate) & ! FullOutputlist$SellerCode == "NST"]),
    mean(FullOutputlist$Reserve[FullOutputlist$BidEndDT > ymd(rundate) & ! FullOutputlist$SellerCode == "NST"]),
    mean(FullOutputlist$Reserve[FullOutputlist$BidEndDT < ymd(rundate) & ! FullOutputlist$SellerCode == "NST"]),
    mean(FullOutputlist$BPO[FullOutputlist$BidEndDT > ymd(rundate) & ! FullOutputlist$SellerCode == "NST"]),
    mean(FullOutputlist$BPO[FullOutputlist$BidEndDT < ymd(rundate) & ! FullOutputlist$SellerCode == "NST"]),
    mean(FullOutputlist$BPOSurplusPct[FullOutputlist$BidEndDT > ymd(rundate) & ! FullOutputlist$SellerCode == "NST"]),
    mean(FullOutputlist$BPOSurplusPct[FullOutputlist$BidEndDT < ymd(rundate) & ! FullOutputlist$SellerCode == "NST"])
  ),
  cbind(
    mean(FullOutputlist$BPOSurplus[FullOutputlist$BidEndDT > ymd(rundate) & FullOutputlist$SellerCode == "FNM"]),
    mean(FullOutputlist$BPOSurplus[FullOutputlist$BidEndDT < ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"]),
    mean(FullOutputlist$Reserve[FullOutputlist$BidEndDT > ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"]),
    mean(FullOutputlist$Reserve[FullOutputlist$BidEndDT < ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"]),
    mean(FullOutputlist$BPO[FullOutputlist$BidEndDT > ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"]),
    mean(FullOutputlist$BPO[FullOutputlist$BidEndDT < ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"]),
    mean(FullOutputlist$BPOSurplusPct[FullOutputlist$BidEndDT > ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"]),
    mean(FullOutputlist$BPOSurplusPct[FullOutputlist$BidEndDT < ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"])
  )
))
names(PrePost) = c("PreBPOSurplus", "PostBPOSurplus", "PreReserve", "PostReserve", "PreBPO", "PostBPO", "PreBPOPct", "PostBPOPct")
rownames(PrePost) = c("All","NoHUD","NSTOnly","FNMOnly")

PrePost$BPODifference = PrePost$PreBPO - PrePost$PostBPO
PrePost$ReserveDifference = PrePost$PreReserve - PrePost$PostReserve


hist(round(FullOutputlist$AuctionDaySalePrediction[FullOutputlist$BidEndDT < ymd(rundate)], digits=0) -
       FullOutputlist$IsSold[FullOutputlist$BidEndDT < ymd(rundate)],
     xlab = "Error Type ( 0 = Correct, 1 = False Positive, -1 = False Negative)", main = "Frequency of Errors in Validation Set")




#Have a look at unscored Assets

TEST1 = DATA[DATA$BidEndDT > ymd(rundate),]
t = as.data.frame(table(TEST1$AuctionCode))
t = t[t$Freq >0,]
t = as.data.frame(table(TEST1$SellerCode))
t = t[t$Freq >0,]
t = t[t$Var1 %in% c("BA1","BV1","BAJ","FNM","NSO","NST","NTS","NSV"),]  
