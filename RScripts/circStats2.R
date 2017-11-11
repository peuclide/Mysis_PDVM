library(FSA)
library(CircStats)
library(circular)

setwd("C:/Users/Peuclide/Dropbox/Mysis metapopulations")
d <- read.csv("circdat.csv",header=TRUE)
d

tc<- x <- circular(d$a2d,type="angles", units="degrees",template="geographics")

b <- d[c(1,3,5),]
p <- d[c(2,4,6),]
b
bc <- x <- circular(b$a2d,type="angles", units="degrees",template="geographics")
pc <- x <- circular(p$a2d,type="angles", units="degrees",template="geographics")

rayleigh.test(msc) ### tests for non-random directionality 



d
ch <- d[c(1:2),]
ms <- d[c(3:4),]
sr <- d[c(5:6),]
chc <- x <- circular(ch$a2d,type="angles", units="degrees",template="geographics")
msc <- x <- circular(ms$a2d,type="angles", units="degrees",template="geographics")
src <- x <- circular(sr$a2d,type="angles", units="degrees",template="geographics")
