library(FSA)
library(car)
library(multcomp)
library(plotrix)
library(siar)
library(ggplot2)
library(plyr)
setwd("C:/Users/Peuclide/Dropbox/Mysis metapopulations")
dat <- read.csv("metadata.csv",header=TRUE)

dup <- dat[c(11,71,68,72),] #duplicate comparisons for tray 1



ddup <- Subset(dat[-c(71,72,153,154,155),]) # removes duplicates for tray 1
dcom <- Subset(ddup,comments=="")
str(dcom)
d <- dcom
str(d)

####finds mean(mx) standard error (sx) and standard deviation(sdx) for each group.
#summaryd <- ddply(d, 'idc', function(x) c(n=nrow(x), n=mean(x$N),c=mean(x$C),mwN=mean(x$wtN),mwC=mean(x$wtC),
                              mCN=mean(x$C.N), sn=(sd(x$N)/sqrt(length(x$N))),sc=(sd(x$C)/sqrt(length(x$C))),
                              swN=(sd(x$wtN)/sqrt(length(x$wtN))),swC=(sd(x$wtC)/sqrt(length(x$wtC))),
                              sCN=(sd(x$C.N)/sqrt(length(x$C.N))), sdN=sd(x$N),sdC=sd(x$C),
                              sdwN=sd(x$wtN),sdwC=sd(x$wtC),sdCN=sd(x$C.N)))
#summaryd <- ddply(d, 'idc', function(x) c(n=var(x$N),c=var(x$C),c.n=var(x$C.N),mn=mean(x$N),c=mean(x$C),mCN=mean(x$C.N)))
summaryd <- ddply(d, 'idc', function(x) c(len=mean(x$length),n=mean(x$N), sdN=sd(x$N), N=nrow(x),C=mean(x$C), sdC=sd(x$C), N=nrow(x), c.n=mean(x$C.N), sdC.N=sd(x$C.N), N=nrow(x)))
summaryd

# bi plot
sr4b <- Subset(summaryd,idc=="sr4b")
ms4b <- Subset(summaryd,idc=="ms4b")
ms4p <- Subset(summaryd,idc=="ms4p")
ms14b <- Subset(summaryd,idc=="ms14b")
ms14p <- Subset(summaryd,idc=="ms14p")
ch14b <- Subset(summaryd,idc=="ch14b")
ch14p <- Subset(summaryd,idc=="ch14p")
sr13b <- Subset(summaryd,idc=="sr13b")
sr13p <- Subset(summaryd,idc=="sr13p")
ch19b <- Subset(summaryd,idc=="ch19b")
ch19p <- Subset(summaryd,idc=="ch19p")
sr20b <- Subset(summaryd,idc=="sr20b")
sr20p <- Subset(summaryd,idc=="sr20p")
ms19b <- Subset(summaryd,idc=="ms19b")
ms19p <- Subset(summaryd,idc=="ms19p")

########
sr4b
plotCI(sr4b$c,sr4b$n, ui=(sr4b$n+sr4b$sn),li=(sr4b$n-sr4b$sn),pch=17,xlim=c(-31,-28),ylim=c(12.5,14.5),xlab="C",ylab="N")
plotCI(sr4b$c,sr4b$n, ui=(sr4b$c+sr4b$sc),li=(sr4b$c-sr4b$sc),err="x",add=TRUE,pch=17)
plotCI(ms4b$c,ms4b$n, ui=(ms4b$n+ms4b$sn),li=(ms4b$n-ms4b$sn),add=TRUE,pch=16)
plotCI(ms4b$c,ms4b$n, ui=(ms4b$c+ms4b$sc),li=(ms4b$c-ms4b$sc),err="x",add=TRUE,pch=16)
plotCI(ms4p$c,ms4p$n, ui=(ms4p$n+ms4p$sn),li=(ms4p$n-ms4p$sn),add=TRUE,pch=1)
plotCI(ms4p$c,ms4p$n, ui=(ms4p$c+ms4p$sc),li=(ms4p$c-ms4p$sc),err="x",add=TRUE,pch=1)
plotCI(sr13b$c,sr13b$n, ui=(sr13b$n+sr13b$sn),li=(sr13b$n-sr13b$sn),add=TRUE,pch=17)
plotCI(sr13b$c,sr13b$n, ui=(sr13b$c+sr13b$sc),li=(sr13b$c-sr13b$sc),err="x",add=TRUE,pch=17)
plotCI(sr13p$c,sr13p$n, ui=(sr13p$n+sr13p$sn),li=(sr13p$n-sr13p$sn),add=TRUE,pch=2)
plotCI(sr13p$c,sr13p$n, ui=(sr13p$c+sr13p$sc),li=(sr13p$c-sr13p$sc),err="x",add=TRUE,pch=2)
plotCI(ms14b$c,ms14b$n, ui=(ms14b$n+ms14b$sn),li=(ms14b$n-ms14b$sn),add=TRUE,pch=16)
plotCI(ms14b$c,ms14b$n, ui=(ms14b$c+ms14b$sc),li=(ms14b$c-ms14b$sc),err="x",add=TRUE,pch=16)
plotCI(ms14p$c,ms14p$n, ui=(ms14p$n+ms14p$sn),li=(ms14p$n-ms14p$sn),add=TRUE,pch=1)
plotCI(ms14p$c,ms14p$n, ui=(ms14p$c+ms14p$sc),li=(ms14p$c-ms14p$sc),err="x",add=TRUE,pch=1)
plotCI(ch14b$c,ch14b$n, ui=(ch14b$n+ch14b$sn),li=(ch14b$n-ch14b$sn),add=TRUE,pch=0)
plotCI(ch14b$c,ch14b$n, ui=(ch14b$c+ch14b$sc),li=(ch14b$c-ch14b$sc),err="x",add=TRUE,pch=0)
plotCI(ch14p$c,ch14p$n, ui=(ch14p$n+ch14p$sn),li=(ch14p$n-ch14p$sn),add=TRUE,pch=15)
plotCI(ch14p$c,ch14p$n, ui=(ch14p$c+ch14p$sc),li=(ch14p$c-ch14p$sc),err="x",add=TRUE,pch=15)
plotCI(sr20b$c,sr20b$n, ui=(sr20b$n+sr20b$sn),li=(sr20b$n-sr20b$sn),add=TRUE,pch=17)
plotCI(sr20b$c,sr20b$n, ui=(sr20b$c+sr20b$sc),li=(sr20b$c-sr20b$sc),err="x",add=TRUE,pch=17)
plotCI(sr20p$c,sr20p$n, ui=(sr20p$n+sr20p$sn),li=(sr20p$n-sr20p$sn),add=TRUE,pch=2)
plotCI(sr20p$c,sr20p$n, ui=(sr20p$c+sr20p$sc),li=(sr20p$c-sr20p$sc),err="x",add=TRUE,pch=2)
plotCI(ch19b$c,ch19b$n, ui=(ch19b$n+ch19b$sn),li=(ch19b$n-ch19b$sn),add=TRUE,pch=15)
plotCI(ch19b$c,ch19b$n, ui=(ch19b$c+ch19b$sc),li=(ch19b$c-ch19b$sc),err="x",add=TRUE,pch=15)
plotCI(ch19p$c,ch19p$n, ui=(ch19p$n+ch19p$sn),li=(ch19p$n-ch19p$sn),add=TRUE,pch=0)
plotCI(ch19p$c,ch19p$n, ui=(ch19p$c+ch19p$sc),li=(ch19p$c-ch19p$sc),err="x",add=TRUE,pch=0)
plotCI(ms19b$c,ms19b$n, ui=(ms19b$n+ms19b$sn),li=(ms19b$n-ms19b$sn),add=TRUE,pch=16)
plotCI(ms19b$c,ms19b$n, ui=(ms19b$c+ms19b$sc),li=(ms19b$c-ms19b$sc),err="x",add=TRUE,pch=16)
plotCI(ms19p$c,ms19p$n, ui=(ms19p$n+ms19p$sn),li=(ms19p$n-ms19p$sn),add=TRUE,pch=1)
plotCI(ms19p$c,ms19p$n, ui=(ms19p$c+ms19p$sc),li=(ms19p$c-ms19p$sc),err="x",add=TRUE,pch=1)

legend("topleft",c("SR","MS","CH"),pch=c(17,16,15))









###long way to summarize
sr4b <- Subset(d,idc=="sr4b")
ms4b <- Subset(d,idc=="ms4b")
ms4p <- Subset(d,idc=="ms4p")
ms14b <- Subset(d,idc=="ms14b")
ms14p <- Subset(d,idc=="ms14p")
ch14b <- Subset(d,idc=="ch14b")
ch14p <- Subset(d,idc=="ch14p")
ms14b <- Subset(d,idc=="sr13b")
sr13p <- Subset(d,idc=="sr13p")
ch19b <- Subset(d,idc=="ch19b")
ch19p <- Subset(d,idc=="ch19p")
sr20b <- Subset(d,idc=="sr20b")
sr20p <- Subset(d,idc=="sr20p")
ms19b <- Subset(d,idc=="ms19b")
ms19p <- Subset(d,idc=="ms19p")
str(ms19p)





#### subsetsdata and adds to data.frame
idc <-c("sr4b","ms4b","ms4p","ms14b","ms14p","ch14p","ch14b","sr13b","sr13p","ch19b","ch19p","sr20b","sr20p","ms19b","ms19p") 
# mean data data frame
n <- c(mean(sr4b$N),mean(ms4b$N),mean(ms4p$N),mean(ms14b$N),mean(ms14p$N),mean(ch14p$N),mean(ch14b$N)
            ,mean(sr13b$N),mean(sr13p$N), mean(ch19b$N), mean(ch19p$N), mean(sr20b$N), mean(sr20p$N), 
            mean(ms19b$N),mean(ms19p$N))
c <- c(mean(sr4b$C),mean(ms4b$C),mean(ms4p$C),mean(ms14b$C),mean(ms14p$C),mean(ch14p$C),mean(ch14b$C)
            ,mean(sr13b$C),mean(sr13p$C), mean(ch19b$C), mean(ch19p$C), mean(sr20b$C), mean(sr20p$C), 
            mean(ms19b$C),mean(ms19p$C))
wn <- c(mean(sr4b$wtN),mean(ms4b$wtN),mean(ms4p$wtN),mean(ms14b$wtN),mean(ms14p$wtN),mean(ch14p$wtN),mean(ch14b$wtN)
             ,mean(sr13b$wtN),mean(sr13p$wtN), mean(ch19b$wtN), mean(ch19p$wtN), mean(sr20b$wtN), mean(sr20p$wtN), 
             mean(ms19b$wtN),mean(ms19p$wtN))
wc <- c(mean(sr4b$wtC),mean(ms4b$wtC),mean(ms4p$wtC),mean(ms14b$wtC),mean(ms14p$wtC),mean(ch14p$wtC),mean(ch14b$wtC)
             ,mean(sr13b$wtC),mean(sr13p$wtC), mean(ch19b$wtC), mean(ch19p$wtC), mean(sr20b$wtC), mean(sr20p$wtC), 
             mean(ms19b$wtC),mean(ms19p$wtC))
cn <- c(mean(sr4b$C.N),mean(ms4b$C.N),mean(ms4p$C.N),mean(ms14b$C.N),mean(ms14p$C.N),mean(ch14p$C.N),mean(ch14b$C.N)
             ,mean(sr13b$C.N),mean(sr13p$C.N), mean(ch19b$C.N), mean(ch19p$C.N), mean(sr20b$C.N), mean(sr20p$C.N), 
             mean(ms19b$C.N),mean(ms19p$C.N))
#SD
#stdError <- sd(x)/sqrt(length(x))

sn <- c(sd(sr4b$N),sd(ms4b$N),sd(ms4p$N),sd(ms14b$N),sd(ms14p$N),sd(ch14p$N),sd(ch14b$N)
       ,sd(sr13b$N),sd(sr13p$N), sd(ch19b$N), sd(ch19p$N), sd(sr20b$N), sd(sr20p$N), 
       sd(ms19b$N),sd(ms19p$N))
sc <- c(sd(sr4b$C),sd(ms4b$C),sd(ms4p$C),sd(ms14b$C),sd(ms14p$C),sd(ch14p$C),sd(ch14b$C)
       ,sd(sr13b$C),sd(sr13p$C), sd(ch19b$C), sd(ch19p$C), sd(sr20b$C), sd(sr20p$C), 
       sd(ms19b$C),sd(ms19p$C))
swn <- c(sd(sr4b$wtN),sd(ms4b$wtN),sd(ms4p$wtN),sd(ms14b$wtN),sd(ms14p$wtN),sd(ch14p$wtN),sd(ch14b$wtN)
        ,sd(sr13b$wtN),sd(sr13p$wtN), sd(ch19b$wtN), sd(ch19p$wtN), sd(sr20b$wtN), sd(sr20p$wtN), 
        sd(ms19b$wtN),sd(ms19p$wtN))
swc <- c(sd(sr4b$wtC),sd(ms4b$wtC),sd(ms4p$wtC),sd(ms14b$wtC),sd(ms14p$wtC),sd(ch14p$wtC),sd(ch14b$wtC)
        ,sd(sr13b$wtC),sd(sr13p$wtC), sd(ch19b$wtC), sd(ch19p$wtC), sd(sr20b$wtC), sd(sr20p$wtC), 
        sd(ms19b$wtC),sd(ms19p$wtC))
scn <- c(sd(sr4b$C.N),sd(ms4b$C.N),sd(ms4p$C.N),sd(ms14b$C.N),sd(ms14p$C.N),sd(ch14p$C.N),sd(ch14b$C.N)
        ,sd(sr13b$C.N),sd(sr13p$C.N), sd(ch19b$C.N), sd(ch19p$C.N), sd(sr20b$C.N), sd(sr20p$C.N), 
        sd(ms19b$C.N),sd(ms19p$C.N))

mean.dat <- data.frame(idc,n,sn,c,sc,wn,swn,wc,swc,cn,scn)
mean.dat



# bi plot
sr4b <- Subset(mean.dat,idc=="sr4b")
ms4b <- Subset(mean.dat,idc=="ms4b")
ms4p <- Subset(mean.dat,idc=="ms4p")
ms14b <- Subset(mean.dat,idc=="ms14b")
ms14p <- Subset(mean.dat,idc=="ms14p")
ch14b <- Subset(mean.dat,idc=="ch14b")
ch14p <- Subset(mean.dat,idc=="ch14p")
sr13b <- Subset(mean.dat,idc=="sr13b")
sr13p <- Subset(mean.dat,idc=="sr13p")
ch19b <- Subset(mean.dat,idc=="ch19b")
ch19p <- Subset(mean.dat,idc=="ch19p")
sr20b <- Subset(mean.dat,idc=="sr20b")
sr20p <- Subset(mean.dat,idc=="sr20p")
ms19b <- Subset(mean.dat,idc=="ms19b")
ms19p <- Subset(mean.dat,idc=="ms19p")

########
sr4b
plotCI(sr4b$c,sr4b$n, ui=(sr4b$n+sr4b$sn),li=(sr4b$n-sr4b$sn),pch=17,xlim=c(-32,-27),ylim=c(12,15),xlab="C",ylab="N")
plotCI(sr4b$c,sr4b$n, ui=(sr4b$c+sr4b$sc),li=(sr4b$c-sr4b$sc),err="x",add=TRUE,pch=17)
plotCI(ms4b$c,ms4b$n, ui=(ms4b$n+ms4b$sn),li=(ms4b$n-ms4b$sn),add=TRUE,pch=16)
plotCI(ms4b$c,ms4b$n, ui=(ms4b$c+ms4b$sc),li=(ms4b$c-ms4b$sc),err="x",add=TRUE,pch=16)
plotCI(ms4p$c,ms4p$n, ui=(ms4p$n+ms4p$sn),li=(ms4p$n-ms4p$sn),add=TRUE,pch=1)
plotCI(ms4p$c,ms4p$n, ui=(ms4p$c+ms4p$sc),li=(ms4p$c-ms4p$sc),err="x",add=TRUE,pch=1)
plotCI(sr13b$c,sr13b$n, ui=(sr13b$n+sr13b$sn),li=(sr13b$n-sr13b$sn),add=TRUE,pch=17,col="red")
plotCI(sr13b$c,sr13b$n, ui=(sr13b$c+sr13b$sc),li=(sr13b$c-sr13b$sc),err="x",add=TRUE,pch=17,col="red")
plotCI(sr13p$c,sr13p$n, ui=(sr13p$n+sr13p$sn),li=(sr13p$n-sr13p$sn),add=TRUE,pch=2,col="red")
plotCI(sr13p$c,sr13p$n, ui=(sr13p$c+sr13p$sc),li=(sr13p$c-sr13p$sc),err="x",add=TRUE,pch=2,col="red")
plotCI(ms14b$c,ms14b$n, ui=(ms14b$n+ms14b$sn),li=(ms14b$n-ms14b$sn),add=TRUE,pch=16,col="red")
plotCI(ms14b$c,ms14b$n, ui=(ms14b$c+ms14b$sc),li=(ms14b$c-ms14b$sc),err="x",add=TRUE,pch=16,col="red")
plotCI(ms14p$c,ms14p$n, ui=(ms14p$n+ms14p$sn),li=(ms14p$n-ms14p$sn),add=TRUE,pch=1,col="red")
plotCI(ms14p$c,ms14p$n, ui=(ms14p$c+ms14p$sc),li=(ms14p$c-ms14p$sc),err="x",add=TRUE,pch=1,col="red")
plotCI(ch14b$c,ch14b$n, ui=(ch14b$n+ch14b$sn),li=(ch14b$n-ch14b$sn),add=TRUE,pch=0,col="red")
plotCI(ch14b$c,ch14b$n, ui=(ch14b$c+ch14b$sc),li=(ch14b$c-ch14b$sc),err="x",add=TRUE,pch=0,col="red")
plotCI(ch14p$c,ch14p$n, ui=(ch14p$n+ch14p$sn),li=(ch14p$n-ch14p$sn),add=TRUE,pch=15,col="red")
plotCI(ch14p$c,ch14p$n, ui=(ch14p$c+ch14p$sc),li=(ch14p$c-ch14p$sc),err="x",add=TRUE,pch=15,col="red")
plotCI(sr20b$c,sr20b$n, ui=(sr20b$n+sr20b$sn),li=(sr20b$n-sr20b$sn),add=TRUE,pch=17,col="blue")
plotCI(sr20b$c,sr20b$n, ui=(sr20b$c+sr20b$sc),li=(sr20b$c-sr20b$sc),err="x",add=TRUE,pch=17,col="blue")
plotCI(sr20p$c,sr20p$n, ui=(sr20p$n+sr20p$sn),li=(sr20p$n-sr20p$sn),add=TRUE,pch=2,col="blue")
plotCI(sr20p$c,sr20p$n, ui=(sr20p$c+sr20p$sc),li=(sr20p$c-sr20p$sc),err="x",add=TRUE,pch=2,col="blue")
plotCI(ch19b$c,ch19b$n, ui=(ch19b$n+ch19b$sn),li=(ch19b$n-ch19b$sn),add=TRUE,pch=15,col="blue")
plotCI(ch19b$c,ch19b$n, ui=(ch19b$c+ch19b$sc),li=(ch19b$c-ch19b$sc),err="x",add=TRUE,pch=15,col="blue")
plotCI(ch19p$c,ch19p$n, ui=(ch19p$n+ch19p$sn),li=(ch19p$n-ch19p$sn),add=TRUE,pch=0,col="blue")
plotCI(ch19p$c,ch19p$n, ui=(ch19p$c+ch19p$sc),li=(ch19p$c-ch19p$sc),err="x",add=TRUE,pch=0,col="blue")
plotCI(ms19b$c,ms19b$n, ui=(ms19b$n+ms19b$sn),li=(ms19b$n-ms19b$sn),add=TRUE,pch=16,col="blue")
plotCI(ms19b$c,ms19b$n, ui=(ms19b$c+ms19b$sc),li=(ms19b$c-ms19b$sc),err="x",add=TRUE,pch=16,col="blue")
plotCI(ms19p$c,ms19p$n, ui=(ms19p$n+ms19p$sn),li=(ms19p$n-ms19p$sn),add=TRUE,pch=1,col="blue")
plotCI(ms19p$c,ms19p$n, ui=(ms19p$c+ms19p$sc),li=(ms19p$c-ms19p$sc),err="x",add=TRUE,pch=1,col="blue")

legend("topleft",c("SR","MS","CH"),pch=c(17,16,15))

## Hab, site grouping
sr <- Subset(d,site=="SR")
sr
summarysr <- ddply(sr, 'habitat', function(x) c(count=nrow(x),mn=mean(x$N),mc=mean(x$C),mCN=mean(x$C.N), n=sd(x$N),c=sd(x$C),c.n=sd(x$C.N),nrow=nrow(x$ID)))
summarysr

ch <- Subset(d,site=="CH")
ch
summarych <- ddply(ch, 'habitat', function(x) c(count=nrow(x),mn=mean(x$N),mc=mean(x$C),mCN=mean(x$C.N), n=sd(x$N),c=sd(x$C),c.n=sd(x$C.N),nrow=nrow(x$ID)))
summarych

ms <- Subset(d,site=="MS")
ms
summaryms <- ddply(ms, 'habitat', function(x) c(count=nrow(x),mn=mean(x$N),mc=mean(x$C),mCN=mean(x$C.N), n=sd(x$N),c=sd(x$C),c.n=sd(x$C.N),nrow=nrow(x$ID)))
summaryms


results <- lm(data=ms,C ~ habitat) 
(Aresults <- anova(results))
