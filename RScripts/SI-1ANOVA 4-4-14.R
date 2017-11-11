#library(FSA)
library(car)
library(multcomp)
#library(plotrix)
library(base)
library(plyr)

setwd("~/Dropbox/Mysis metapopulations")
dat <- read.csv("Metadata.csv",header=TRUE)
str(dat)
dup <- dat[dat$ID%in% c(273,1601,1522,1602,1441,1603,925,1604,198,1605),] ##view duplicates
subset(dup, ID==273|ID==1601)
subset(dup, ID==1522|ID==1602)
subset(dup, ID==1441|ID==1603)
subset(dup, ID==925|ID==1604)
subset(dup, ID==198|ID==1605)

#### create data set
#str(dat)
d2 <- subset(dat,ID!=c(1601:1605)) #remove duplicates
#str(d2)
#xtabs(~ID,data=d2)
d1 <-subset(d2,comments=="") #remove comments
#str(d1)
#d <- subset(d1,!ID %in% c(139,164,1543)) ## removes outliers
#xtabs(~ID,data=d)
#str(d)
d1$fdate <- as.factor(d1$date) #adds column with date as a factor
d1 <- subset(d1, fdate!= 11.04)

#------------------------------------------------------------------------
# Enrichments summaries
#------------------------------------------------------------------------
midnov <- subset(d1, fdate=="11.14")
latenov <- subset(d1, fdate=="11.19")

Summarize(C~site+habitat, data=latenov) # summary data used in graphpad plots (figure 3)
Summarize(N~site+habitat, data=latenov)
Summarize(C.N~site+habitat, data=latenov)

Summarize(C~site+habitat, data=d1) # summary data used in graphpad plots (figure 3 dates combined
Summarize(N~site+habitat, data=d1)
Summarize(C.N~site+habitat, data=d1)

Summarize(C~habitat, data=d1)
Summarize(N~habitat, data=d1)
Summarize(C.N~habitat, data=d1)

Summarize(C.N~fdate, data=d1)
#------------------------------------------------------------------------
#########   1 way anova ####################
#------------------------------------------------------------------------

results <- lm(data=Sr,C.N ~ habitat*fdate) 
(Aresults <- anova(results))
outlierTest(results)
leveneTest(results)

plot(C.N ~ fdate,data=d1)
var(data=Sr,C.N ~ habitat*fdate)
sd(Sr$C.N)
x <- subset(Sr, Sr$habitat=="B")
x1 <- subset(x, x$fdate=="11.14")
x2 <- subset(x, x$fdate=="11.19")
y <- subset(Sr, Sr$habitat=="P")
y1 <- subset(y, y$fdate=="11.14")
y2 <- subset(y, y$fdate=="11.19")

(sd(x1$C.N))^2 #benthic 11.14
(sd(x2$C.N))^2#benthic 11.19
(sd(y1$C.N))^2#pelagic 11.14
(sd(y2$C.N))^2#pelagic 11.19

## residual plot

residPlot(results)
hist(~residuals(results))

## Tukeys test and plot ####
results <- lm
Hab <- glht(results,mcp(site ="Tukey"))
summary(Hab)

cld(Hab)

confint(Hab)

(sum <- Summarize(N~fdate,data=d1, digits=1)) #summarizes data

sum <- within(sum,{ 
  LCI <- mean-1.96*sd/sqrt(n)
  UCI <- mean+1.96*sd/sqrt(n)
  })

sum


with(sum, plotCI(1:3, mean, ui=UCI, li=LCI, pch=16, xlim=c(.5,3.5), xaxt="n",
                      ylim=c(13.0,14.0), xlab="fdate", ylab="C.N"))
axis(1,1:3,c("11-4","11-14","11-19"))
text(1:3,sum$mean,c("a","b", "b"), pos=c(4,4))
text(1:3,3.8,paste("n=",sum$n,sep=""))



#------------------------------------------------------------------------
########### 2- way ANOVA ############
#------------------------------------------------------------------------

d1$logN <- log(d1$N)
d1$logC <- log(d1$C)
d1$logC.N <- log(d1$C.N)


results2 <- lm(data=d1,C ~ habitat + site + habitat*site) 
(Aresults2 <- anova(results2))
outlierTest(results2)
leveneTest(lm(data=Sr,C.N~site))


### box plots
par(mfrow=c(1,2)) 
plot(C.N ~ habitat+fdate, data=d1, main="metadata C.N" )

#------------------------------------------------------------------------
## Idicator variable (ANCOVA)
#------------------------------------------------------------------------

var <- d1$C
lm3 <- lm(var~habitat*site, data=d1) ## testing for difference in slope, sig interaction means sig dif in slope
(results <- anova(lm3))

#summary(lm3)
#confint(lm3)
#fitPlot(lm3)
#
lm4 <- lm(var~site+fdate+habitat, data=d1) ## testing for a difference in intercepts, looking at p value for main effects, ANCOVA
anova(lm4)
#summary(lm3)
#confint(lm3)
#fitPlot(lm3, pch=c(10), ylab="‰ C", xlab="", xaxt='n', lwd=1.5)

fitPlot(lm3, legend="bottomright")

#------------------------------------------------------------------------
##########3 way anova########
#------------------------------------------------------------------------


str(d1)
d2 <-subset(d1, fdate=="11.14"|date=="11.19")
results3 <- lm(data=d1,length ~ habitat + site + fdate + habitat*site + habitat*fdate + site*fdate+habitat*site*date) 
(Aresults <- anova(results3))
outlierTest(results3)
residPlot(results3)


### box plots
par(mfrow=c(1,2))
plot(C ~ fdate, data=d1, main="metadata C:N" )

#------------------------------------------------------------------------
############## kruskal.test ################
#------------------------------------------------------------------------

(results4 <- kruskal.test(N ~ habitat + site, data = d1))

(r <- kruskal.test(N~habitat, data=d1))


(x <- aggregate(d1$C,list(d1$site, d1$habitat),mean))
(y <- ggregate(d1$C,list(d1$site, d1$habitat),sd))


#--------------------------------------------------------------------
## Idicator variable (ANCOVA)
#------------------------------------------------------------------------

lm3 <- lm(N~C.N+length+habitat*site*fdate, data=d1) ## testing for difference in slope, sig interaction means sig dif in slope
(results <- anova(lm3))

plot(residuals(lm3)~fitted(lm3), main="C")
summary(lm3)
confint(lm3)
fitPlot(lm3, legend="bottomleft")


#---------------------------------------------------------------------
#d1 <- subset(d1, date!= "11.04")
leveneTest(lm(data=Ch,C.N~habitat))

head(d1)
lm4<- lm(N~habitat*fdate, data=Sr) ## testing for a difference in intercepts, looking at p value for main effects, ANCOVA
anova(lm4)
summary(lm4)
confint(lm4)
fitPlot(lm4, ylab="C", xlab="", pch=1, lwd=1.5, col = c("red","blue","black"), legend="bottomright")
#---------------------------------------------------------------------
## Tukeys test and pairwise comparison ####

Hab <- glht(lm_withinsite,mcp(habitat ="Tukey"))
summary(Hab)


summaryd <- ddply(d1, c('site',"date","habitat"), function(x) c(cn=mean(x$C.N), sdcn=sd(x$C.N), 
                          nrow(x)))
cndifference <- c(.16444,.08,.57,.097143,.318889,.77,.52)
depth <- c(70,70,100,100,100,120,120)

plot(cndifference~depth)

abline(lm(cndifference~depth))

results <- lm(cndifference~depth)
anova(results)
plot(results)

cld(Hab)

confint(Hab)
Ch <- subset(d1, site=="CH")
Ch_MidNov <- subset(Ch, fdate=="11.14")
Ch_LateNov <- subset(Ch, fdate=="11.19")
Sr <- subset(d1, site=="SR")
Sr <- subset(Sr, fdate!= "11.04")
Sr_MidNov<- subset(Sr, fdate=="11.14")
Sr_LateNov<- subset(Sr, fdate=="11.19")
Ms <- subset(d1, site=="MS")
Ms_MidNov<- subset(Ms, fdate=="11.14")
Ms_LateNov<- subset(Ms, fdate=="11.19")
Ms_EarlyNov <- subset(Ms, fdate=="11.04")

lm_withinsitelate <- lm(C.N~habitat*fdate, data=Ms) 
lm_withinsitemid <- lm(C.N~length+habitat*fdate, data=Sr) 
lm_winthinsiteearly <- lm(C.N~habitat*fdate, data=Ch)

anova(lm_withinsitelate)
anova(lm_withinsitemid)
anova(lm_winthinsiteearly)

summaryd <- ddply(d1, c('site','fdate','habitat'), function(x) c(C=mean(x$C), sdC=sd(x$C),N=mean(x$N), sdN=sd(x$N)))


