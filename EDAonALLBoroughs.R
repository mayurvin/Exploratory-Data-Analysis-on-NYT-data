require(gdata)
setwd("C:/Users/talem/Desktop/Courses/Spring16/DIC/R Files/RollingSales/")
df_brooklyn <- read.csv("rollingsales_brooklyn.csv")
df_manhattan <- read.csv("rollingsales_manhattan.csv")
df_bronx <- read.csv("rollingsales_bronx.csv")
df_queens <- read.csv("rollingsales_queens.csv")
df_island <- read.csv("rollingsales_statenisland.csv")

df_brooklyn$land.sqft <- as.numeric(gsub("[^[:digit:]]","",df_brooklyn$LAND.SQUARE.FEET))
df_brooklyn$sales.price <- as.numeric(gsub("[^[:digit:]]","",df_brooklyn$SALE.PRICE))
df_brooklyn.sale <- df_brooklyn[df_brooklyn$land.sqft != 0,]
df_brooklyn.actual.sales <- df_brooklyn[df_brooklyn$sales.price != 0,]
df_brooklyn.avg <- transform(df_brooklyn.sale, per.sqft = sales.price/land.sqft)
brooklyn.sale.per.sqft <- sum(df_brooklyn.avg$per.sqft,na.rm=TRUE)/length(df_brooklyn.avg$per.sqft)


df_manhattan$land.sqft <- as.numeric(gsub("[^[:digit:]]","",df_manhattan$LAND.SQUARE.FEET))
df_manhattan$sales.price <- as.numeric(gsub("[^[:digit:]]","",df_manhattan$SALE.PRICE))
df_manhattan.sale <- df_manhattan[df_manhattan$land.sqft != 0,]
df_manhattan.actual.sales <- df_brooklyn[df_manhattan$sales.price != 0,]
df_manhattan.avg <- transform(df_manhattan.sale, per.sqft = sales.price/land.sqft)
manhattan.sale.per.sqft <- sum(df_manhattan.avg$per.sqft,na.rm=TRUE)/length(df_manhattan.avg$per.sqft)


df_bronx$land.sqft <- as.numeric(gsub("[^[:digit:]]","",df_bronx$LAND.SQUARE.FEET))
df_bronx$sales.price <- as.numeric(gsub("[^[:digit:]]","",df_bronx$SALE.PRICE))
df_bronx.sale <- df_bronx[df_bronx$land.sqft != 0,]
df_bronx.actual.sales <- df_brooklyn[df_bronx$sales.price != 0,]
df_bronx.avg <- transform(df_bronx.sale, per.sqft = sales.price/land.sqft)
bronx.sale.per.sqft <- sum(df_bronx.avg$per.sqft,na.rm=TRUE)/length(df_bronx.avg$per.sqft)


df_queens$land.sqft <- as.numeric(gsub("[^[:digit:]]","",df_queens$LAND.SQUARE.FEET))
df_queens$sales.price <- as.numeric(gsub("[^[:digit:]]","",df_queens$SALE.PRICE))
df_queens.sale <- df_queens[df_queens$land.sqft != 0,]
df_queens.actual.sales <- df_brooklyn[df_queens$sales.price != 0,]
df_queens.avg <- transform(df_queens.sale, per.sqft = sales.price/land.sqft)
queens.sale.per.sqft <- sum(df_queens.avg$per.sqft,na.rm=TRUE)/length(df_queens.avg$per.sqft)


df_island$land.sqft <- as.numeric(gsub("[^[:digit:]]","",df_island$LAND.SQUARE.FEET))
df_island$sales.price <- as.numeric(gsub("[^[:digit:]]","",df_island$SALE.PRICE))
df_island.sale <- df_island[df_island$land.sqft != 0,]
df_island.actual.sales <- df_brooklyn[df_island$sales.price != 0,]
df_island.avg <- transform(df_island.sale, per.sqft = sales.price/land.sqft)
island.sale.per.sqft <- sum(df_island.avg$per.sqft,na.rm=TRUE)/length(df_island.avg$per.sqft)

## Sales Price Comparison
ggplot(data=df_brooklyn.actual.sales, aes(x="Borough", y="Logarithmic Sales Price", fill="Price Range")) + 
  geom_boxplot(data=df_brooklyn.actual.sales, aes(x="Brooklyn", y=log(df_brooklyn.actual.sales$sales.price), fill=log(df_brooklyn.actual.sales$sales.price)), color='blue') + 
  geom_boxplot(data=df_manhattan.actual.sales, aes(x="Manhattan", y=log(df_manhattan.actual.sales$sales.price), fill = log(df_manhattan.actual.sales$sales.price)), color='red') +
  geom_boxplot(data=df_bronx.actual.sales, aes(x="Bronx", y=log(df_bronx.actual.sales$sales.price), fill=log(df_bronx.actual.sales$sales.price)), color='green') + 
  geom_boxplot(data=df_queens.actual.sales, aes(x="Queens", y=log(df_queens.actual.sales$sales.price), fill=log(df_queens.actual.sales$sales.price), solid=TRUE), color='green') + 
  geom_boxplot(data=df_island.actual.sales, aes(x="Island", y=log(df_island.actual.sales$sales.price), fill=log(df_island.actual.sales$sales.price)), color='green')  

## Sales Price per Square feet comparison
ggplot(data=df_brooklyn, aes(x="Borough", y="Sales Price Per Sqft", fill="Price Range")) + 
  geom_boxplot(data=df_brooklyn, aes(x="Brooklyn", y=brooklyn.sale.per.sqft, fill=brooklyn.sale.per.sqft), color='blue') + 
  geom_boxplot(data=df_manhattan, aes(x="Manhattan", y=manhattan.sale.per.sqft, fill=manhattan.sale.per.sqft), color='red') +
  geom_boxplot(data=df_bronx, aes(x="Bronx", y=bronx.sale.per.sqft, fill=bronx.sale.per.sqft), color='green') + 
  geom_boxplot(data=df_queens, aes(x="Queens", y=queens.sale.per.sqft, fill=queens.sale.per.sqft, solid=TRUE), color='green') + 
  geom_boxplot(data=df_island, aes(x="Island", y=island.sale.per.sqft, fill=island.sale.per.sqft), color='green')  

##
df_brooklyn.table <- table(df_brooklyn$RESIDENTIAL.UNITS, df_brooklyn$COMMERCIAL.UNITS)
df_manhattan.table <- table(df_manhattan$RESIDENTIAL.UNITS, df_manhattan$COMMERCIAL.UNITS)
df_bronx.table <- table(df_bronx$RESIDENTIAL.UNITS, df_bronx$COMMERCIAL.UNITS)
df_queens.table <- table(df_queens$RESIDENTIAL.UNITS, df_queens$COMMERCIAL.UNITS)
df_island.table <- table(df_island$RESIDENTIAL.UNITS, df_island$COMMERCIAL.UNITS)


