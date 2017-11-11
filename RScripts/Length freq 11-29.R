library(FSA)
library(car)
library(multcomp)
library(plotrix)
library(base)
library(plyr)
setwd("C:/Users/Peuclide/Dropbox/Mysis metapopulations/text data")
dat <- read.csv("Rlengthdata.csv",header=TRUE)
str(dat)
dat1 <- (dat[,(1:9)])
str(dat1)
dat <- Subset(dat1, dat1$Date!= "11/4/2013") # subset data to not iclude 11/4/13 samples
dat <- dat1
#range of lengths
max(dat$Length)
min(dat$Length)
#mean and SD
mean(dat$Length)
sd(dat$Length)

#---------------------------------------------------------
#summary stats by habitat
#---------------------------------------------------------

mytable <- xtabs(~Fecund+Location+Habitat+Date, data=dat)
freq <- data.frame(mytable)
freq
benthic <- Subset(dat1, Habitat=="Benthic")
str(benthic)

tbl <- table(benthic$Length,benthic$Fecund)
tbl


pelagic <- Subset(dat1, Habitat=="Pelagic")
str(pelagic)

tbl <- table(pelagic$Length,pelagic$Fecund)
tbl
mean(benthic$Length)
mean(pelagic$Length)
(dif <- mean(benthic$Length)-mean(pelagic$Length))


#---------------------------------------------------------
#summary stats by Site
#---------------------------------------------------------
summaryd <- ddply(dat, c('Location','Date','Habitat'), function(x) c(len=mean(x$Length), sdlen=sd(x$Length), 
              fecund=sum(x$Fecund=="y"),nfecund=sum(x$Fecund=="n"), nrow(x)))
                                          
#---------------------------------------------------------
# Summary data for figure 2
#---------------------------------------------------------

midnov <- Subset(dat, Date=="11/14/2013")
latenov <- Subset(dat, Date=="11/19/2013")

Summarize(Length~Location+Habitat, data=midnov)
Summarize(Length~Location+Habitat, data=latenov)

#---------------------------------------------------------
###ANOVA Length
#---------------------------------------------------------
L <- lm(data=dat, Length~Habitat*Location+Date)
(aov <- anova(L))
plot(L)
outlierTest(fecund)
leveneTest(fecund)
plot(Length~Date+Habitat+Location, data=dat, main="Length" )

#---------------------------------------------------------
###Chi Square test
#---------------------------------------------------------

Ch <- Subset(dat, dat$Location=="Cumberland Head")
ChNov14 <- Subset(Ch, Ch$Date=="11/14/2013")
ChNov19 <- Subset(Ch, Ch$Date=="11/19/2013")
Ms <- Subset(dat, dat$Location=="Main Site")
MsNov4 <- Subset(Ms, Ms$Date=="11/4/2013")
MsNov14 <- Subset(Ms, Ms$Date=="11/14/2013")
MsNov19 <- Subset(Ms, Ms$Date=="11/19/2013")

Sr <- Subset(dat, dat$Location=="Split Rock")
Sr <- Subset(Sr, Sr$Date!="11/4/2013")
SrNov14 <- Subset(Sr, Sr$Date=="11/14/2013")
SrNov19 <- Subset(Sr, Sr$Date=="11/19/2013")

tbl <- table(ChNov19$Fecund, c(ChNov19$Habitat)) #1 = benthic, 2= pelagic
           
             
(X <- chisq.test(tbl)) #1 = CH, 2 = MS, 3 = SR, 

tbl
(obs <- as.matrix(X$observed, header=FALSE))
exp <- as.matrix(X$expected)


oe <- matrix(nrow=2, ncol=3)
             oe[1,1] <- obs[1,1]-exp[1,1]
             oe[1,2] <- obs[1,2]-exp[1,2]
             oe[1,3] <- obs[1,3]-exp[1,3]
             oe[2,1] <- obs[2,1]-exp[2,1]
             oe[2,2] <- obs[2,2]-exp[2,2]
             oe[2,3] <- obs[2,3]-exp[2,3]

oe
nrow(dat)
##### Effect size Cramer's V test
n <- nrow(dat1)
df <- min(c((nrow(obs)-1),(ncol(obs)-1)))
(v <- sqrt(X$statistic/(n*df)))

#---------------------------------------------------------
#### Odds Ratio
#---------------------------------------------------------

x <- table(Ms$Date, Ms$Fecund=="N")
x <- table(dat$Location, dat$Fecund=="N")
x <- table(dat$Habitat, dat$Fecund=="Y")
x <- table(Sr$Habitat, Sr$Fecund=="Y")

x
(or.fecund.lateNov <- (x[3,1]*x[1,2])/(x[1,1]*x[3,2]))
or.fecund.pelagic <- (x[2,1]*x[1,2])/(x[1,1]*x[2,2])
or.fecund.lateNovb <- (x[2,1]*x[1,2])/(x[1,1]*x[2,2])


or.fecund.CH <- (x[3,1]*x[1,2])/(x[1,1]*x[3,2])
or.fecund.MS <- (x[2,1]*x[1,2])/(x[1,1]*x[2,2])
or.fecund.CH ## 2.89 times more likely to be fecund at SR than at CH
or.fecund.MS ## 3.06 times more likely to be fecund at MS and CH
or.fecund.pelagic 





## late nov 1.667 times as likely than middle nov
### 3.62 times more likely to be fecund and benthic than fecund and pelagic.

####3-way anova


dat1$fdate <- as.factor(dat1$Date) #adds column with date as a factor
str(dat1)
#d2 <-Subset(d1, fdate=="11.14"|date=="11.19")
results3 <- lm(data=dat,Length ~ Habitat * Location * Date ) 
(Aresults <- anova(results3))

### box plots
par(mfrow=c(1,2)) 
plot(Length ~ Habitat+Location, data=dat1, main="length" )
Data.L=Data$V8
span=seq(5,25,by=1)
hist(Data.L,span,right=FALSE)

## Idicator variable (ANCOVA)

lm3 <- lm(Length~Location*Habitat, data=dat1) ## testing for difference in slope, sig interaction means sig dif in slope
(results <- anova(lm3))

#summary(lm3)
#confint(lm3)
#fitPlot(lm3)
#
lm4 <- lm(Length~Date*Habitat, data=Ch) ## testing for a difference in intercepts, looking at p value for main effects, ANCOVA
anova(lm4)
#summary(lm3)
#confint(lm3)
#fitPlot(lm3, pch=c(10), ylab="‰ C", xlab="", xaxt='n', lwd=1.5)

fitPlot(lm3, legend="bottomright")




###Sumary statistics

summaryd <- ddply(dat1, c("Location",'Habitat'), function(x) c(N=nrow(x),mean=mean(x$Length),
            sd=sd(x$Length)))
summaryd
#se=(sd(x$Length)/sqrt(length(x$Length)))
nrow



## editing graph titles

hist(Data.L,span,right=FALSE,col="light blue",main="Length Freq of Pelagic Samples",xlab="Length (mm)")


## building a table of freq

range(Data.L)
breaks=seq(5.5,25.5,by=1)
breaks
Data.t=cut(Data.L,breaks,right=FALSE)
Data.freq=table(SR.t)
Data.freq
cbind(Data.freq)

##summary statistics
mean(Data.L)
median(Data.L)

## finding mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(Data.L)

## histogram with two treatments
###data
DataP=CHP11.14
DataP
Data.LP=DataP$V8

DataB=CHB11.14
DataB
Data.LB=DataB$V8

###freq tables

range(Data.LP)
breaks=seq(7.5,25.5,by=1)
#breaks
Data.tP=cut(Data.LP,breaks,right=FALSE)
Data.freqP=table(Data.tP)
cbind(Data.freqP)
mean(Data.LP)
median(Data.LP)
Mode(Data.LP)

range(Data.LB)
breaks=seq(7.5,25.5,by=1)
#breaks
Data.tB=cut(Data.LB,breaks,right=FALSE)
Data.tB
Data.freqB=table(Data.tB)
#table(Data.tB)
cbind(Data.freqB)
mean(Data.LB)
median(Data.LB)
Mode(Data.LB)


x <- 8:25           #range of x axis
y1 <- Data.freqP     #data for first variable
y2 <- Data.freqB    #data fo second variable

barplot( rbind(y1,y2), col=c("light blue","grey"),names=x, beside=TRUE,main="Cumberland Head 14/11",xlab="Length (mm)")

legend("topright",  cex=0.75, pch=16,col=c("light blue", "grey"), legend=c("Pelagic", "Benthic"), ncol=2)

#---------------------------------------------------------
###paitwise comparisons
#---------------------------------------------------------

Ch <- subset(dat, Location=="Cumberland Head")
Ch_MidNov <- Subset(Ch, Date=="11/14/2013")
Ch_LateNov <- Subset(Ch, Date=="11/19/2013")
Sr <- subset(dat, Location=="Split Rock")
Sr <- Subset(Sr, Date!= "11/4/2013")
Sr_MidNov<- Subset(Sr, Date=="11/14/2013")
Sr_LateNov<- Subset(Sr, Date=="11/19/2013")
Ms <- subset(dat, Location=="Main Site")
Ms_MidNov<- Subset(Ms, Date=="11/14/2013")
Ms_LateNov<- Subset(Ms, Date=="11/19/2013")

lm_withinsitelate <- lm(Length~Habitat, data=Ch_LateNov) 
lm_withinsitemid <- lm(Length~Habitat, data=Ch_MidNov) 


anova(lm_withinsitelate)
anova(lm_withinsitemid)


##########3 Length Difference graph

Lengthdifference <- c(-0.95,-.22,1.363,.22,.53,2.958,3.38)
depth <- c(70,70,100,100,100,120,120)

plot(Lengthdifference~depth)

abline(lm(Lengthdifference~depth))

