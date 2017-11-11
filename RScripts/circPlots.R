library(FSA)
library(CircStats)
library(circular)

setwd("C:/Users/Peuclide/Dropbox/Mysis metapopulations")
d <- read.csv("circdat.csv",header=TRUE)
d
d3 <- d[,"a2d"]
d4 <- d[,"a1r"]
d5 <- d[,"a3r"]

str(d3)
##summary data
x <- circular(d3,type="angles", units="degrees",template="clock12")
mean.circular(x)



## divided by date, hab, and site
msb1 <- d[d$idc=="msb",c("a1d")]
msp1 <- d[d$idc=="msp",c("a1d")]
srb1 <- d[d$idc=="srb",c("a1d")]
msb2 <- d[d$idc=="msb",c("a2d")]
msp2 <- d[d$idc=="msp",c("a2d")]
srb2 <- d[d$idc=="srb",c("a2d")]
srp2 <- d[d$idc=="srp",c("a2d")]
chb2 <- d[d$idc=="chb",c("a2d")]
chp2 <- d[d$idc=="chp",c("a2d")]
msb3 <- d[d$idc=="msb",c("a3d")]
msp3 <- d[d$idc=="msp",c("a3d")]
srb3 <- d[d$idc=="srb",c("a3d")]

##############plot
par(mfrow=c(2,2))
r1 <- circular(msb1,type="angles", units="degrees",template="geographics")
plot(r1, cex=.00, bin=720, stack=TRUE, sep=0.035, shrink=.8, axes=FALSE, xlim=c(-1.5,2.3), ylim=c(-1.5,1.5))
x

# annotate north
text(0, 1.2, 'Increasing N', cex=1.0, font=1)
# annotate west
text(1.38,0, 'Increasing C', cex=1.0, font=1)
# annotate south
#text(0,-1.125, 'North', cex=0.85, font=1)
# annotate East
#text(-1.35,0, 'North', cex=0.85, font=1)

r2 <- circular(msp1,type="angles", units="degrees",template="geographics")
points(r2,cex=1.5, bin=720, stack=TRUE, sep=0.035, col="black",pch=1)
r3 <- circular(srb1,type="angles", units="degrees",template="geographics")
points(r3,cex=1.5, bin=720, stack=TRUE, sep=0.035, col="black", pch=17)
r4 <- circular(msb2,type="angles", units="degrees",template="geographics")
points(r4, cex=1.5, bin=720, stack=TRUE, sep=0.035, col="red")
r5 <- circular(msp2,type="angles", units="degrees",template="geographics")
points(r5,cex=1.5, bin=720, stack=TRUE, sep=0.035, col="red",pch=1)
r6 <- circular(srb2,type="angles", units="degrees",template="geographics")
points(r6, cex=1.5, bin=720, stack=TRUE, sep=0.035, col="red", pch=17)
r7 <- circular(srp2,type="angles", units="degrees",template="geographics")
points(r7,cex=1.5, bin=720, stack=TRUE, sep=0.035, col="red",pch=2)
r8 <- circular(chb2,type="angles", units="degrees",template="geographics")
points(r8, cex=1.5, bin=720, stack=TRUE, sep=0.035, col="red", pch=15)
r9 <- circular(chp2,type="angles", units="degrees",template="geographics")
points(r9,cex=1.5, bin=720, stack=TRUE, sep=0.035, col="red",pch=0)
r10 <- circular(msb3,type="angles", units="degrees",template="geographics")
points(r10, cex=1.5, bin=720, stack=TRUE, sep=0.035, col="blue")
r11 <- circular(msp3,type="angles", units="degrees",template="geographics")
points(r11,cex=1.5, bin=720, stack=TRUE, sep=0.035, col="blue",pch=1)
r12 <- circular(srb3,type="angles", units="degrees",template="geographics")
points(r12,cex=1.5, bin=720, stack=TRUE, sep=0.035, col="blue", pch=17)
#################### 3 graph figure
par(mfrow=c(2,2))
##### Shift from 4-Nov to 14-Nov

plot(r1, cex=.00, bin=720, stack=TRUE, sep=0.035, shrink=.8, axes=FALSE, xlim=c(-1.5,2), ylim=c(-1.5,1.5))
# annotate north
text(0, 1.2, 'Increasing N', cex=1.0, font=1)
# annotate west
text(1.38,0, 'Increasing C', cex=1.0, font=1)
arrows.circular(r1, y = d[d$idc=="msb","r1"], x0 = 0, y0 = 0, lty=1)
arrows.circular(r2, y = d[d$idc=="msp","r1"], x0 = 0, y0 = 0, col="black",lty=2)
arrows.circular(r3, y = d[d$idc=="srb","r1"], x0 = 0, y0 = 0, col="black",lty=3)
legend(1.0,1.2,c("MS Benthic", "MS Pelagic","SR Benthic"), 
       lty = c(1,2,3), col = c("black","black","black"))
### shift from 14-Nov to 20-Nov
plot(r1, cex=.00, bin=720, stack=TRUE, sep=0.035, shrink=.8, axes=FALSE, xlim=c(-1,1.8), ylim=c(-1.5,1.5))
# annotate north
text(0, 1.2, 'Increasing N', cex=1.0, font=1)
# annotate west
text(1.38,0, 'Increasing C', cex=1.0, font=1)
arrows.circular(r4, y = d[d$idc=="msb","r2"], x0 = 0, y0 = 0, col="black", lty=1)
arrows.circular(r5, y = d[d$idc=="msp","r2"], x0 = 0, y0 = 0, col="black", lty=2)
arrows.circular(r6, y = d[d$idc=="srb","r2"], x0 = 0, y0 = 0, col="black", lty=3)
arrows.circular(r7, y = d[d$idc=="srp","r2"], x0 = 0, y0 = 0, col="black", lty=4)
arrows.circular(r8, y = d[d$idc=="chb","r2"], x0 = 0, y0 = 0, col="black", lty=5)
arrows.circular(r9, y = d[d$idc=="chp","r2"], x0 = 0, y0 = 0, col="black", lty=6)
legend(1.1,1.2,c("MS Benthic", "MS Pelagic","SR Benthic", "SR Pelagic", "CH Benthic", "CH Pelagic"), 
       lty = c(1,2,3,4,5,6), col = c("black","black","black","black","black","black"))

#### shift from 4-Nov to 20-Nov
plot(r1, cex=.00, bin=720, stack=TRUE, sep=0.035, shrink=1.3, axes=FALSE, xlim=c(-1,1.8), ylim=c(-1.5,1.5))
# annotate north
text(0, 1.2, 'Increasing N', cex=1.0, font=1)
# annotate west
text(1.42,0, 'Increasing C', cex=1.0, font=1)
arrows.circular(r10, y = d[d$idc=="msb","r3"], x0 = 0, y0 = 0, col="Black", lty=1)
arrows.circular(r11, y = d[d$idc=="msp","r3"], x0 = 0, y0 = 0, col="Black", lty=2)
arrows.circular(r12, y = d[d$idc=="srb","r3"], x0 = 0, y0 = 0, col="Black",lty=3)

legend(1.0,1.2,c("MS Benthic", "MS Pelagic","SR Benthic"), 
       lty = c(1,2,3), col = c("black","black","black"))

## benthic=closed, pelagic=open   SR=tiangle(open=2,closed=17),
#MS=circle, CH=square (open pch=0 closed=pch=15)   11-4=black 11-14=red 11-19=blue
##benthic vs. pelagic

######## Cic plots by site

plot(r3, cex=.00, bin=720, stack=TRUE, sep=0.035, shrink=.8, axes=FALSE, xlim=c(-1.5,2), ylim=c(-1.5,1.5), main="Split Rock")
# annotate north
text(0, 1.2, 'Increasing N', cex=.8, font=1)
# annotate west
text(1.5,0, 'Increasing C', cex=.8, font=1)
arrows.circular(r3, y = d[d$idc=="srb","r1"], x0 = 0, y0 = 0, lty=1)
arrows.circular(r6, y = d[d$idc=="srp","r2"], x0 = 0, y0 = 0, col="blue",lty=2)
arrows.circular(r7, y = d[d$idc=="srb","r2"], x0 = 0, y0 = 0, col="black",lty=2)
arrows.circular(r12, y = d[d$idc=="srb","r3"], x0 = 0, y0 = 0, col="black",lty=3)

legend(1.0,1.2,c("MS Benthic", "MS Pelagic","SR Benthic"), 
       lty = c(1,2,3), col = c("black","black","black"))
### MS
plot(r1, cex=.00, bin=720, stack=TRUE, sep=0.035, shrink=.8, axes=FALSE, xlim=c(-1,1.8), ylim=c(-1.5,1.5))
# annotate north
text(0, 1.2, 'Increasing N', cex=1.0, font=1)
# annotate west
text(1.52,0, 'Increasing C', cex=1.0, font=1)
arrows.circular(r1, y = d[d$idc=="msb","r1"], x0 = 0, y0 = 0, col="black", lty=1)
arrows.circular(r2, y = d[d$idc=="msp","r1"], x0 = 0, y0 = 0, col="blue", lty=1)
arrows.circular(r4, y = d[d$idc=="msb","r2"], x0 = 0, y0 = 0, col="black", lty=2)
arrows.circular(r5, y = d[d$idc=="msp","r2"], x0 = 0, y0 = 0, col="blue", lty=2)
arrows.circular(r10, y = d[d$idc=="msb","r3"], x0 = 0, y0 = 0, col="black", lty=3)
arrows.circular(r11, y = d[d$idc=="msp","r3"], x0 = 0, y0 = 0, col="blue", lty=3)

legend(1.1,1.2,c("MS Benthic", "MS Pelagic","SR Benthic", "SR Pelagic", "CH Benthic", "CH Pelagic"), 
       lty = c(1,2,3,4,5,6), col = c("black","black","black","black","black","black"))

#### shift from 4-Nov to 20-Nov
plot(r1, cex=.00, bin=720, stack=TRUE, sep=0.035, shrink=1.3, axes=FALSE, xlim=c(-1,1.8), ylim=c(-1.5,1.5))
# annotate north
text(0, 1.2, 'Increasing N', cex=1.0, font=1)
# annotate west
text(1.42,0, 'Increasing C', cex=1.0, font=1)
arrows.circular(r10, y = d[d$idc=="msb","r3"], x0 = 0, y0 = 0, col="Black", lty=1)
arrows.circular(r11, y = d[d$idc=="msp","r3"], x0 = 0, y0 = 0, col="Black", lty=2)
arrows.circular(r12, y = d[d$idc=="srb","r3"], x0 = 0, y0 = 0, col="Black",lty=3)

legend(1.0,1.2,c("MS Benthic", "MS Pelagic","SR Benthic"), 
       lty = c(1,2,3), col = c("black","black","black"))
##### atempt to code finding angles
#d$r1 <- sqrt(((d$y2-d$y1)^2)+((d$x2-d$x1)^2))
#d$r2 <- sqrt(((d$y3-d$y2)^2)+((d$x3-d$x2)^2))
#d$r3 <- sqrt(((d$y3-d$y1)^2)+((d$x3-d$x1)^2))
#d$a1 <- (d$y2-d$y1)/d$r1
#d$a2 <- (d$y3-d$y2)/d$r2
#d$a3 <- asin((d$y3-d$y1)/d$r3)
#d
#d1 <- d[,c(1,12)]
##d1$num <- as.numeric(d1[,1])
#A <- d1[,c(2)]
#B <- d1[,c(3)]
#d2 <- cbind(B,A)
#d2
#str(d2)
#### plot#####



