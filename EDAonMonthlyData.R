
urlpart1="http://stat.columbia.edu/~rachel/datasets/nyt"
urlpart2=".csv"
dataframe=read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv")) #put day1 data first
for(i in 2:31){
  wholeurl=paste(paste(urlpart1,i,sep=""),urlpart2,sep="") #construct the url for different days urlpart1+i+urlpart2
  subdataframe=read.csv(url(wholeurl))
  dataframe=rbind(dataframe,subdataframe) 
}
head(dataframe)
dataframe$agecat<-cut(dataframe$Age,c(-Inf,0,18,24,34,44,54,65,Inf))
summary(dataframe)
library(doBy)
siterange<-function(x){c(length(x),min(x),mean(x),max(x))}
summaryBy(Age~agecat,data = dataframe,FUN = siterange)

summaryBy(Gender+Signed_In+Impressions+Clicks~agecat,data = dataframe)

library(ggplot2)

dataframe.females <- subset(dataframe, Gender==0)
dataframe.females <- subset(dataframe.females, Impressions>0)
dataframe.males <- subset(dataframe, Gender==1)
dataframe.males <- subset(dataframe.males, Impressions>0)

ggplot(dataframe.males, aes(x=Impressions,fill=agecat)) +geom_histogram(binwidth = 1)
ggplot(dataframe.females, aes(x=Impressions,fill=agecat)) +geom_histogram(binwidth = 1)

dataframe.LoggedIn <- subset(dataframe, Signed_In==1)
dataframe.NotLoggedIn <- subset(dataframe, Signed_In==0)

ggplot(dataframe.LoggedIn, aes(x=Impressions, main = "Logged In users")) +geom_histogram(binwidth = 1)
ggplot(dataframe.NotLoggedIn, aes(x=Impressions), main="Not Logged In") +geom_histogram(binwidth = 1)
# plot(density(dataframe.LoggedIn$Impressions), main = "Desnsity of Logged In People") + polygon(density(dataframe.LoggedIn$Impressions), col="blue", border = "red")
# plot(density(dataframe.NotLoggedIn$Impressions), main = "Desnsity of Logged In People") + polygon(density(dataframe.NotLoggedIn$Impressions), col="red", border = "blue")
