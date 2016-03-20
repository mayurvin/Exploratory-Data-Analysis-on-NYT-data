require(gdata)
setwd("C:/Users/talem/Desktop/Courses/Spring16/DIC/R Files/RollingSales/")
bk <- read.csv("rollingsales_brooklyn.csv")
headhead(bk)
summary(bk)
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bk$SALE.PRICE))
names(bk) <- tolower(names(bk))

## clean/format the data with regular expressions
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$GROSS.SQUARE.FEET))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$LAND.SQUARE.FEET))
bk$sale.date <- as.Date(bk$SALE.DATE)
bk$year.built <- as.numeric(as.character(bk$YEAR.BUILT))

## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bk)
hist(sale.price.n)
hist(sale.price.n[sale.price.n>0], main = "Histogram of Actual Sales" , xlab = "Actual Sales Price")
hist(gross.sqft[sale.price.n==0])
detach(bk)

## keep only the actual sales
bk.sale <- bk[bk$sale.price.n!=0,]
plot(bk.sale$gross.sqft,bk.sale$sale.price.n)
plot(log(bk.sale$gross.sqft),log(bk.sale$sale.price.n), ylab = "Logarithmic Sales Price", xlab = "Logarithmic Gross Sqft", main = "Plot of Gross Sqft VS Sales Price")
plo
## for now, let's look at 1-, 2-, and 3-family homes
bk.homes <- bk.sale[which(grepl("FAMILY",bk.sale$building.class.category)),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n), ylab = "Logarithmic Sales Price", xlab = "Logarithmic Gross Sqft", main = "Gross Sqft VS Sales Price for FAMILY Homes")

## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n), ylab = "Logarithmic Sales Price", xlab = "Logarithmic Gross Sqft", main = "FAMILY HOMES without outliers")



#### Comparison Across Neibourhood
par(mfrow=c(2,2))

##For BATH BEACH neibourhood
bk.neibourBathBeach <- bk.sale[which(grepl("BATH BEACH",bk.sale$neighborhood)),]
plot(log(bk.neibourBathBeach$gross.sqft),log(bk.neibourBathBeach$sale.price.n), main = "BATH BEACH", xlab = "Log Gross Sqft", ylab = "Log Sales Price")

# ##For BAY RIDGE neibourhood
# bk.neibourBayRidge <- bk.sale[which(grepl("BATH BEACH",bk.sale$neighborhood)),]
# plot(log(bk.neibourBayRidge$gross.sqft),log(bk.neibourBayRidge$sale.price.n), xlab = "Log Gross Sqft", ylab = "Log Sales Price")

##For BEDFORD STUYVESANT neibourhood
bk.neibourBedford <- bk.sale[which(grepl("BEDFORD STUYVESANT",bk.sale$neighborhood)),]
plot(log(bk.neibourBedford$gross.sqft),log(bk.neibourBedford$sale.price.n), main = "BEDFORD STUYVESANT", xlab = "Log Gross Sqft", ylab = "Log Sales Price")

##For BOROUGH PARK neibourhood
bk.neibourBoroughPark <- bk.sale[which(grepl("BOROUGH PARK",bk.sale$neighborhood)),]
plot(log(bk.neibourBoroughPark$gross.sqft),log(bk.neibourBoroughPark$sale.price.n), main = "BOROUGH PARK", xlab = "Log Gross Sqft", ylab = "Log Sales Price")

##For BROOKLYN HEIGHTS neibourhood
bk.neibourBrooklynHeights <- bk.sale[which(grepl("BROOKLYN HEIGHTS",bk.sale$neighborhood)),]
plot(log(bk.neibourBrooklynHeights$gross.sqft),log(bk.neibourBrooklynHeights$sale.price.n), main = "BROOKLYN HEIGHTS", xlab = "Log Gross Sqft", ylab = "Log Sales Price")

# ##For CANARSIE neibourhood
# bk.neibourCanarsie <- bk.sale[which(grepl("CANARSIE",bk.sale$neighborhood)),]
# plot(log(bk.neibourCanarsie$gross.sqft),log(bk.neibourCanarsie$sale.price.n), xlab = "Log Gross Sqft", ylab = "Log Sales Price")
# #####



