rm(list=ls())	
# Set working directory
setwd("C:/Users/jnye/Dropbox (Nye lab)/Manuscripts/OA Nature response")        #For desktop

## Read in data

allfish<- read.csv("allfish_bootresults.csv")
smallfish<- read.csv("smallfish boot_results.csv")
largefish<-read.csv("bigfish_bootresults.csv")

#To make high resolution plots uncomment the tiff command below
#Otherwise keep it off as you tweak the plots
tiff("all panels.tiff", width = 4, height = 4, units = 'in', res = 300)
par(mfrow=c(3,1),oma=c(2,2,1,0),mar=c(2.5,2.5,0.2,1))

plot(density(allfish$mean.control),xlab="",main="",ylim=c(0,0.20),xlim=c(20,80),col="black",ylab="",xaxt='n',yaxt='n')
axis(side=1,seq(20,80,10),labels=F)
axis(side=2,at=c(0,0.10,0.20),labels=c(0,0.10,0.20),las=2)
lines(density(allfish$mean.treatment), col="blue")
abline(v=50,lty=2)
mtext("a) All fish     ",side=3,at=24,cex=.8)

plot(density(largefish$mean.control),xlab="",main="",ylim=c(0,0.21),xlim=c(20,80),col="black",ylab="",xaxt='n',yaxt='n')
lines(density(largefish$mean.treatment), col="blue")
axis(side=1,seq(20,80,10),labels=F)
axis(side=2,at=c(0,0.10,0.20),labels=c(0,0.10,0.20),las=2)
abline(v=50,lty=2)
mtext("b) Big fish     ",side=3,at=24,cex=.8)

plot(density(smallfish$mean.control), col="black", ylim=c(0,0.20),xlim=c(20,80),main="",ylab="",xlab="",xaxt='n',yaxt='n')
lines(density(smallfish$mean.treatment), col="blue")
abline(v=50,lty=2)
axis(side=1,seq(20,80,10),labels=T)
axis(side=2,at=c(0,0.10,0.20),labels=c(0,0.10,0.20),las=2)
mtext("c) Small fish",side=3,at=24,cex=.8)


mtext("Percentage of time in predator cue (%)",side=1,outer=TRUE)
mtext("Frequency of bootstrap outcomes",side=2,outer=TRUE)
#If you use the tiff function also make sure to turn the device off afterwards
dev.off()

