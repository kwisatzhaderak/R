#Merge this Zillow Data! It's awesome!

#Prepare your cooking surface, have some bowls handy and import some packages
library(lubridate)
library(plyr)
rundate = format(Sys.time(), "%Y-%m-%d")

#Start with one personal package of the most recent Zillow Data
turnoverrt = read.csv(file = "~/Zip/Zip/Zip_Turnover_AllHomes.csv")
#Zillow Stopped reporting this figure 2014-03-15
#salesupply = read.csv(file = "~/Zip/Zip/Zip_ForSaleInventorySupply_AllHomes.csv")
#Zillow Stopped Reporting This Figure 2014-02-06
#soldsupply = read.csv(file = "~/Zip/Zip/Zip_NumberOfHomesSold_AllHomes.csv")  
increasing = read.csv(file = "~/Zip/Zip/Zip_PctOfHomesIncreasingInValues_AllHomes.csv")
decreasing = read.csv(file = "~/Zip/Zip/Zip_PctOfHomesDecreasingInValues_AllHomes.csv")
saletolist = read.csv(file = "~/Zip/Zip/Zip_SalePriceToListRatio_AllHomes.csv")
zhvi_1_bed = read.csv(file = "~/Zip/Zip/Zip_Zhvi_1bedroom.csv")
zhvi_2_bed = read.csv(file = "~/Zip/Zip/Zip_Zhvi_2bedroom.csv")
zhvi_3_bed = read.csv(file = "~/Zip/Zip/Zip_Zhvi_3bedroom.csv")
zhvi_4_bed = read.csv(file = "~/Zip/Zip/Zip_Zhvi_4bedroom.csv")
zhvi_5_bed = read.csv(file = "~/Zip/Zip/Zip_Zhvi_5BedroomOrMore.csv")
foreclosed = read.csv(file = "~/Zip/Zip/Zip_HomesSoldAsForeclosures-Ratio_AllHomes.csv")
medianpric = read.csv(file = "~/Zip/Zip/Zip_MedianSoldPrice_AllHomes.csv")
prevfrclsd = read.csv(file = "~/Zip/Zip/Zip_PctTransactionsThatArePreviouslyForeclosuredHomes_AllHomes.csv")
medpricepersqft = read.csv(file = "~/Zip/Zip/Zip_MedianListingPricePerSqft_AllHomes.csv")
#Zillow Stopped Reporting this figure 2014-06-12
#numhomesforsale = read.csv(file = "~/Zip/Zip/Zip_NumberOfHomesForSale_AllHomes.csv")

namespace = c("turnoverrt",#"salesupply",#"soldsupply",
              "increasing","decreasing","saletolist","zhvi_1_bed",
              "zhvi_2_bed","zhvi_3_bed","zhvi_4_bed","zhvi_5_bed",
              "foreclosed","medianpric","prevfrclsd","medpricepersqft")

#Make a function that will grab the last three entries of a row, naive to gaps, and return the average
MovAvg = function(x){
  x = x[(length(x)-59):length(x)]
  x = as.numeric(as.character(x))
  x = rev(x)      #You must reverse the vector, so the most recent element becomes the first element
  x = x[is.na(x)==F]
  y = x[1:3]
  z = mean(y,na.rm=T)
  return(z)
}



for(i in 1:length(namespace)){
data = get(namespace[i])
data = data[order(data$RegionName,decreasing = F),]

#apply some magick to get date names, ZIP Info
Dates = names(data)
lengt = length(Dates)
Dates = Dates[(lengt-44):lengt]
Dates = gsub(x=Dates,pattern="X",replacement="", fixed = T)
Dates = paste(Dates, ".01",sep="")
Dates = ymd(Dates)
Dates[length(Dates)+1] = Dates[length(Dates)] %m+% months(1)
Dates[length(Dates)+1] = Dates[length(Dates)] %m+% months(1) #Just need to repeat, recursion FTW!
Dates[length(Dates)+1] = Dates[length(Dates)] %m+% months(1) #Just need to repeat, recursion FTW!

#ZIPs
INFO = data[,1:5]
Master = INFO
for(j in 1:47){
  Master = rbind(Master,INFO)
}
Master=Master[order(Master$RegionName),]
Base = cbind(Master, Dates)

#And now some magic using the function to get the calculations we want. We employ the same recursion method above
Mov1 = apply(data, 1, MovAvg)
data[,length(data)+1] = as.numeric(as.character(Mov1))
Mov2 = apply(data, 1, MovAvg)
data[,length(data)+1] = as.numeric(as.character(Mov2))
Mov3 = apply(data, 1, MovAvg)
data[,length(data)+1] = as.numeric(as.character(Mov3))

#Turn those values into a matrix, then unfold that matrix into a vector
Values = data[,(ncol(data)-47):ncol(data)]
Values = as.matrix(Values)
Values1 = as.vector(t(Values), mode = "numeric") #Transpose, baby! Otherwise it treats the columns as the variables' dimension

#And bake! Trick out that namespace list to keep track of values
Final = cbind(Base,Values1)
names(Final)[ncol(Final)] = namespace[i]
#And overwrite the predecessors to keep our space clean
name = namespace[i]
assign(name, Final)
}

Final = get(namespace[1])
for(i in 1:(length(namespace)-1)){
  Final = merge(Final, 
                get(namespace[i+1])[,c("RegionName","Dates",paste(namespace[i+1]))], 
                by = c("RegionName","Dates"),
                all=T)
}

#Let's do some checkig for duplicates. We want 48 of each zip, see what we get from remainder division. We want no remainders
all(table(Final$RegionName)%%48 == 0)
#And Modulo division too, we want only one cycle
all(table(Final$RegionName)%/%48 == 1)
#And do the number of zips make sense? Less than 30k? Hopefully around 20-ish thousand.
nrow(Final)/48
#YAY!

#Since we're going to SQL this, make NAs into NULL
for(i in 7:ncol(Final)){
  Final[which(is.na(Final[,i])==T),i] = "NULL"
}

#And Export For Reals
if(all(table(Final$RegionName)%%48 == 0) &
     all(table(Final$RegionName)%/%48 == 1) &
     nrow(Final)/48 < 30000 ){
filename = paste0("WWR_MergedZillow_", rundate, ".csv")
write.csv(Final, filename,row.names=F, quote=F)
}
