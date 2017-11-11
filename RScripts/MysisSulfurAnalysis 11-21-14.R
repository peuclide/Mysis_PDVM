library(FSA)
setwd("C:/Users/Peuclide/Dropbox/Mysis metapopulations/text data")
dat <- read.csv("Rlengthdata.csv",header=TRUE)
sdat <- read.csv("sulfur.csv",header=TRUE)
head(sdat)
sdat$ID
descrip.dat <- Subset(dat, ID %in% c(sdat$ID))

sdat$date <- descrip.dat$Date
sdat$length <- descrip.dat$Length
sdat$site <- descrip.dat$Location
sdat$habitat <- descrip.dat$Habitat

write.csv(sdat, "Mysis sulfur data 11-21-14.csv", row.names=FALSE)
midNov <- Subset(sdat, date=="11/14/2013")
lateNov <- Subset(sdat, date=="11/19/2013")

Summarize(mg.S~site+habitat, data=sdat)

##----------- Idicator variable (ANCOVA)------------------------------------------------------
var <- sdat$d34S.vs..VCDT
var <- sdat$mg.S
lm3 <- lm(d34S.vs..VCDT~habitat*date, data=ch) ## testing for difference in slope, sig interaction means sig dif in slope
(results <- anova(lm3))
fitPlot(lm3, legend="top")
plot(sdat$d34S.vs..VCDT~sdat$length)

plot(lm3$residuals)

#------------- plotting raw data-------------------------------------------------------------------
nov14 <- Subset(sdat, date=="11/14/2013")
nov19 <- Subset(sdat, date=="11/19/2013")
ben <- Subset(sdat, habitat=="Benthic")
pel <- Subset(sdat, habitat=="Pelagic")
sr <- Subset(sdat, site=="Split Rock")
ms <- Subset(sdat, site =="Main Site")
ch <- Subset(sdat, site=="Cumberland Head")

## date- d34S.vs..VCDT
plot(nov14$d34S.vs..VCDT~nov14$length)
points(nov19$d34S.vs..VCDT~nov19$length, pch=18)

## date - mg.S
plot(ben$mg.S~ben$length)
points(pel$mg.S~ben$length, pch=18)




## site - d34S.vs..VCDT
plot(sr$d34S.vs..VCDT~sr$length)
  points(ms$d34S.vs..VCDT~ms$length, pch=18)
  points(ch$d34S.vs..VCDT~ch$length, pch=12)
## site - mg.s
plot(sr$mg.S~sr$length)
points(ms$mg.S~ms$length, pch=18)
points(ch$mg.S~ch$length, pch=12)

## habitat- d34S.vs..VCDT
plot(ben$d34S.vs..VCDT~ben$length)
points(pel$d34S.vs..VCDT~pel$length, pch=18)

## habitat - mg.S
plot(ben$mg.S~ben$length)
points(pel$mg.S~ben$length, pch=18)
