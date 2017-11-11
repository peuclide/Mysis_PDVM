library(FSA)
library(car)
library(multcomp)
library(plotrix)
library(siar)
library(ggplot2)
library(plyr)
setwd("C:/Users/Peuclide/Dropbox/Mysis metapopulations/text data")
dat <- read.csv("SI len and dw.csv")
str(dat)
(summaryd <- ddply(dat, 'date', function(x) c(ave=mean(x$length),sd=sd(x$length))))



### ANOVA
results <- lm(data=dat,length ~ date) 
(Aresults <- anova(results))
outlierTest(results)
leveneTest(results)

residPlot(results)
hist(~residuals(results))

Hab <- glht(results,mcp(habitat="Tukey"))
summary(Hab)

cld(Hab)

(sum <- Summarize(length~habitat,data=dat, digits=1)) #summarizes data

sum <- within(sum,{ 
  LCI <- mean-1.96*sd/sqrt(n)
  UCI <- mean+1.96*sd/sqrt(n)
})

sum


with(sum, plotCI(1:2, mean, ui=UCI, li=LCI, pch=16, xlim=c(.5,2.5), xaxt="n",
                 ylim=c(13.0,15.0), xlab="Habitat", ylab="Length (mm)"))
axis(1,1:2,c("Benthic","Pelagic"))
text(1:2,sum$mean,c("a","a"), pos=c(4,4))
text(1:2,13,paste("n=",sum$n,sep=""))


######3 2 way anova

results2 <- lm(data=dat,length ~ site + habitat + site*habitat) 
(Aresults2 <- anova(results2))
outlierTest(results2)