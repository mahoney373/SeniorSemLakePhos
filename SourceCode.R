library(knitr)
library(dplyr)
library(alr4)
library(tidyverse)
library(nlme)
library(readr)
library(lubridate)

path = "/home/mahon373/Git/CarlyMSenSem/Lake Data"

multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {merge(x,y,all = TRUE)}, datalist)
}

# Used the function I created to compile the csv files from each county

full_data = multmerge(path)

PLakes=full_data[full_data$parameter=="Phosphorus as P",]
PLakes = PLakes[,-c(8:25)]
PLakes$sampleDate = as.Date(PLakes$sampleDate)
PLakes$result=as.numeric(PLakes$result)
LakesF = PLakes[PLakes$sampleDate > "2015-01-01",]

#added a year variable and a month variable

LakesF = LakesF %>% mutate(year = year(sampleDate))
LakesF = LakesF %>% mutate(year = as.character(year))
LakesF = LakesF %>% mutate(month = month(sampleDate, label=TRUE))
LakesF = LakesF %>% mutate(month = as.character(month))

#averaged duplicate measurements from the same day a created one row of data

LakesF = aggregate(LakesF$result,by=list(stationId=LakesF$stationId,sampleDate=LakesF$sampleDate,county=LakesF$county,year=LakesF$year,month=LakesF$month,stationName=LakesF$stationName),data=LakesF,FUN=mean)


average = LakesF[,7]
LakesF = LakesF[,-7]
LakesF["result"] <- average

LakesF = LakesF %>% mutate(monthN = month(sampleDate))
LakesF = LakesF %>% mutate(year_d = yday(sampleDate))

glimpse(PLakes)

summary(PLakes$result)

summary(PLakes)


head(LakesF)

glimpse(LakesF)

summary(LakesF$result)

summary(LakesF)

kable(LakesF %>% group_by(year) %>% summarise(avePhos=mean(result),n=n(),sdPhos=sd(result)))

kable(LakesF %>% group_by(county) %>% summarise(avePhos=mean(result),n=n(),sdPhos=sd(result)))

kable(LakesF %>% group_by(monthN) %>% summarise(avePhos=mean(result),n=n(),sdPhos=sd(result)))



#This code allows you to look at individual plots by year instead of all six years at once.

PlotYear = function(x,y,z){
  ggplot(data = LakesF, aes(x = sampleDate, y = log(result) , group=stationId)) +
    geom_line(size=.15, alpha=.20) +
    geom_smooth(aes(group = year),method="loess",span=.8,se=FALSE) + 
    xlab("Date") + ylab("Phosphate") +  ggtitle(z) +
    scale_x_date(limit=c(as.Date(x),as.Date(y))) 
}

PlotYear("2020-05-01","2020-10-11","2020")
PlotYear("2019-05-01","2019-10-11","2019")
PlotYear("2018-05-01","2018-10-11","2018")
PlotYear("2017-05-01","2017-10-11","2017")
PlotYear("2016-05-01","2016-10-11","2016")
PlotYear("2015-05-01","2015-10-11","2015")

ggplot(data = LakesF, aes(x = monthN, y = result, colour=year)) +
  stat_summary(aes(group=year),geom="point",fun.y=mean,size=2) + 
  geom_smooth(method="loess",span=.95,se=FALSE) +
  xlab("Month") + ylab("Mean Phosphorous")

ggplot(data = LakesF, aes(x = monthN, y = result, colour=county)) +
  stat_summary(aes(group=county),geom="point",fun.y=mean,size=2) + 
  geom_smooth(method="loess",span=.95,se=FALSE) +
  xlab("Month") + ylab("Mean Phosphorous")

ggplot(data = LakesF, aes(x = monthN, y = log(result) , group=stationId)) +
  geom_line(size=.15, alpha=.20) + 
  geom_smooth(aes(group = year), method="loess",span=.8,se=FALSE) +
  facet_wrap(~ year) + 
  xlab("Date") + ylab("Phosphate (log)") 

ggplot(data = LakesF, aes(x = monthN, y = log(result) , group=stationId)) +
  geom_line(size=.15, alpha=.20) + 
  geom_smooth(aes(group = year), method="loess",span=.8,se=FALSE) + 
  facet_wrap(~ county) + xlab("Date") + ylab("Phosphate (log)") 

ggplot(data = LakesF[LakesF$county=="Pope",], aes(x = monthN, y = log(result) , group=stationId)) + geom_line(size=.15, alpha=.20) + 
  geom_smooth(aes(group = year), method="loess",span=.8,se=FALSE) + 
  xlab("Date") + ylab("Phosphate (log)") 

ggplot(data = LakesF[LakesF$year==2020,], aes(x = sampleDate, y = log(result) , group=stationId)) + geom_line(size=.15, alpha=.20) + 
  geom_smooth(aes(group = county), method="loess",span=.8,se=FALSE) + 
  xlab("Date") + ylab("Phosphate (log)") 


## Methods

### Model Building

#Determining the fixed effects structure:
  
 # ```{r,echo=FALSE}
initialmod <- lme(result ~ year + monthN + county, data=LakesF, random=~ 1 | stationId, method="ML")

mod2 <- lme(result ~ year + year_d*county, data=LakesF, random=~ 1 | stationId, method="ML" )

mod3 <- lme(result ~ year_d*county, data=LakesF, random=~ 1 | stationId, method="ML" )

mod4 <- lme(result ~ poly(year_d,deg=2,raw=TRUE)*county + year, data=LakesF, random=~ 1 | stationId, method="ML" )

mod5 <- lme(result ~ poly(year_d,deg=2,raw=TRUE) + county + year, data=LakesF, random=~ 1 | stationId, method="ML" )

mod6 <- lme(result ~ county*(splines::ns(year_d,df=2)) + year , data=LakesF, random=~1 | stationId, method = "ML")


kable(AIC(initialmod,mod2,mod3,mod4,mod5))
anova(mod3,mod4)



mod4a <- lme(result ~ poly(year_d,deg=2,raw=TRUE)*county + year, data=LakesF, random=~ 1 | stationId, method="REML" )

mod4b <- lme(result ~ poly(year_d,deg=2,raw=TRUE)*county + year, data=LakesF, random=~ year_d | stationId, method="REML" )

mod4c <- lme(result ~ poly(year_d,deg=2,raw=TRUE)*county + year, data=LakesF, random=list(stationId=pdDiag(~1 + monthN + I(monthN^2))), method="REML" )

kable(AIC(mod4a,mod4b,mod4c))

mod4b.car1 <- update(mod4b, correlation = corCAR1(form=~1))
mod4b.ar0ma1 <- update(mod4b, correlation = corARMA(form=~1,p=1,q=0))
mod4b.cs <- update(mod4b, correlation = corCompSymm(0.1, form=~1))
mod4b.Lin <- update(mod4b, correlation = corLin(form=~ 1))
mod4b.exp <- update(mod4b, correlation = corExp(form=~ 1 ))
mod4b.gaus <- update(mod4b, correlation = corGaus(form=~ 1))

kable(AIC(mod4b.car1,mod4b.ar0ma1))