library(siar)
library(FSA)
library(plyr)
library(car)
library(multcomp)
library(plotrix)
library(base)

setwd("C:/Users/Peuclide/Dropbox/Mysis metapopulations/Model")

dat <- read.csv("ChelMysis.csv")
str(dat)
sunlight <- read.csv("daylight.csv")
str(sunlight)
###NOVEMBER###
sunlight$NovDH <- sunlight$NovN[]-sunlight$NovD[]
sunlight$perDayN <- sunlight$NovDH/24
sunlight$perNightN <- 100-sunlight$perDayN
###OCTOBER###
sunlight$OctDH <- sunlight$OctN[]-sunlight$OctD[]
sunlight$perDayO <- sunlight$OctDH/24
sunlight$perNightO <- 100-sunlight$perDayO
####30DAYS PRIOR TO NOV 4####
x <- (sum(sunlight$perDayO[3:31]))+(sum(sunlight$perDayN[1:3]))
MDL <- x/30
MDL
MNL <- 100-MDL

################
July <- Subset(dat,Month=="July")
Mysis <- Subset(July, sampletype=="Mysis" )
source <- Subset(dat, sampletype %in% c("sediment","Zoop."))
zoop <- Subset(dat, sampletype=="Zoop.")


summaryd <- ddply(source, 'sampletype', function(x) c(C=mean(x$C),sdC=sd(x$C),N=mean(x$N),sdN=sd(x$N)))
summaryd




#original

mPC <- rnorm(10, mean=-28.54, sd=0.76) #data from ms 4-Nov
mBC <- rnorm(10, mean=-29.39, sd=0.67)#data from ms 4-Nov
mPN <- rnorm(10, mean=13.38, sd=0.73) #data from ms 4-Nov
mBN <- rnorm(10, mean=13.08, sd=0.39)#data from ms 4-Nov
#round1
mPC1 <- rnorm(10, mean=-30.24, sd=0.76)
mBC1 <- rnorm(10, mean=-28.21, sd=0.67)
mPN1 <- rnorm(10, mean=10.75, sd=0.73)
mBN1 <- rnorm(10, mean=6.69, sd=0.39)
#round2
mPC2 <- rnorm(10, mean=-30.24, sd=0.76)
mBC2 <- rnorm(10, mean=-27.21, sd=0.67)
mPN2 <- rnorm(10, mean=13.75, sd=0.73)
mBN2 <- rnorm(10, mean=8.69, sd=0.39)

#round3 - shift PC values 1 delta C up and 1 delta N down
mPC3 <- rnorm(10, mean=-29.24, sd=0.76)

mPN3 <- rnorm(10, mean=12.75, sd=0.73)

#round4 - shift PC values 1 delta C up and 1 delta N down
mPC4 <- rnorm(10, mean=-28.24, sd=0.76)

mPN4 <- rnorm(10, mean=11.75, sd=0.73)

#round5 - shift PC values 1 delta C up and 1 delta N down
mPC5 <- rnorm(10, mean=-27.24, sd=0.76)

mPN5 <- rnorm(10, mean=10.75, sd=0.73)

#round6 - shift PC values 0 delta C up and 1 delta N down
mPC6 <- rnorm(10, mean=-27.24, sd=0.76)

mPN6 <- rnorm(10, mean=9.75, sd=0.73)

#round7 - shift PC values 0 delta C up and 1 delta N down
mPC7 <- rnorm(10, mean=-27.24, sd=0.76)

mPN7 <- rnorm(10, mean=8.75, sd=0.73)

x1 <- c(1,1,1,1,1,1,1,1,1,1) #group number
x2 <- c(2,2,2,2,2,2,2,2,2,2)
x3 <- c(3,3,3,3,3,3,3,3,3,3)
x4 <- c(4,4,4,4,4,4,4,4,4,4)
x5 <- c(5,5,5,5,5,5,5,5,5,5)
x6 <- c(6,6,6,6,6,6,6,6,6,6)
x7 <- c(7,7,7,7,7,7,7,7,7,7)
con1 <- cbind(x1,mPC,mPN)
con2 <- cbind(x2,mBC,mBN)
con3 <- cbind(x3,mPC1,mPN1)
con4 <- cbind(x4,mBC1,mBN1)
con5 <- cbind(x1,mPC2,mPN2)
con6 <- cbind(x2,mBC2,mBN2)
con7 <- cbind(x3,mPC3,mPN3)
con8 <- cbind(x4,mPC4,mPN4)
con9 <- cbind(x5,mPC5,mPN5)
con10 <- cbind(x6,mPC6,mPN6)
con11 <- cbind(x7,mPC7,mPN7)

##### Seasonal predictions ####
#November 4 round1, goal: 41.79% benthic, 58.2% zoop
#mPC8 <- rnorm(10, mean=-28.5, sd=0.76)
mBC2 <- rnorm(10, mean=-27.21, sd=0.67)
#mPN8 <- rnorm(10, mean=11.9, sd=0.73)
mBN2 <- rnorm(10, mean=8.69, sd=0.39)

##ROUND 2:  goal: 41.79% benthic, 58.2% zoop
ncon1<- cbind(x1,mPC8,mPN8)
ncon2<- cbind(x2,mBC2,mBN2)

###ROUND 3: GOAL MEAN DL OVER 30 DAYS PRIOR TO 4 NOV -- 47.9% BENTHIC, 52.1% NIGHT
mPC9 <- rnorm(10, mean=-28.0, sd=0.76)

mPN9 <- rnorm(10, mean=11.4, sd=0.73)
ncon3 <-cbind(x2,mPC9,mPN9)

## consumer1 <-read.csv("consumer Model.csv")
consumer <- rbind(ncon2,ncon3)
sources <- read.csv("sourcesiar Model.csv")
tef <- read.csv("tef Model.csv")
#sept <- read.csv("sep2012isotope.csv")

#mean(sept$sampletype)
                 
                 
model1 <- siarmcmcdirichletv4(consumer,sources,tef,concdep=0,500000,50000)



siarplotdata(model1, leg=1)

siarproportionbygroupplot(model1) #plot by Group

siarproportionbysourceplot(model1, prn=TRUE, probs=c(5,25,75,95), xspc=.5) # plot by Source


siarproportionbysourceplot(model1,prn=TRUE,grp=2,probs=c(5,25,75,95))

siarproportionbygroupplot(model1, siarversion=0,probs=c(5),xlabels=NULL, grp=1, type="boxes",clr=gray((9:1)/10),scl=1,xspc=0.5,prn=FALSE,leg=FALSE)

out<Dmodel1$output
fix(out)


#### ANOVA
df <- data.frame(consumer)
df$group <-factor(df$x1) 
results <- lm(data=df,mPN2 ~ group) 
(Aresults <- anova(results))
#outlierTest(results)
#leveneTest(results)
plot(mPC2 ~ group,data=df)


###Tukey test
Group <- glht(results,mcp(group="Tukey"))
summary(Group)

cld(Group)

confint(Group)

(sum <- Summarize(mPN2~group,data=df, digits=1)) #summarizes data

sum <- within(sum,{ 
  LCI <- mean-1.96*sd/sqrt(n)
  UCI <- mean+1.96*sd/sqrt(n)
})

sum


with(sum, plotCI(1:7, mean, ui=UCI, li=LCI, pch=16, xlim=c(.5,7.5), xaxt="n",
                 ylim=c(7.5,14.0), xlab="group", ylab="N"))
axis(1,1:7,c("1","2","3","4","5","6","7"))
text(1:7,sum$mean,c("e","ab","d","cd","c","b","a"), pos=c(4,4))
text(1:7,-31,paste("n=",sum$n,sep=""))

