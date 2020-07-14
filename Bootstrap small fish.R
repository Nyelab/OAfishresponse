#########################################################################################
#########################################################################################
### R Code for Timothy Clark Written By Kate Stark
### Modified by Janet Nye
### Completed March 2020
#########################################################################################

# 'clean-up'
rm(list=ls())	
# Set working directory
setwd("C:/Users/jnye/Dropbox (Nye lab)/Manuscripts/OA Nature response")        

## Read in data

data <- read.csv("Time_in_cue_2+2.csv")
head(data) #Have a look over the data to ensure has been read in correctly

## In order to make this code run on all data files (all response measures), we need to code it generically as 'Response'
names(data)[7] <- "Response"


{
  data$comment <- as.character(data$comment)
  data$animal_id <- as.character(data$animal_id)
  data$animal_id <- as.integer(data$animal_id)
  data$loc <- as.character(data$loc)
  data$species <- as.character(data$species)
  data$species <- tolower(data$species)
  data$treatment <- relevel(data$treatment, 'control')
  data$size <- as.character(data$size)
  data$size <- ifelse(data$size=="", NA, data$size)
}

str(data)


with(data, table(Species, Location))
## Not sure why Location header name is reading in weird, but this fixes it!
names(data)[1]<-"Location" 

names(data)

data$Fish_ID <- factor(data$Fish_ID) # Ensures Fish_ID a factor rather than numeric (probably not necessary)

## Have a look at summary table
summary(data)
nrow(data) # How many records in data


## If you want to analyse the full dataset, then just define data1 as data:
data1 <- data	# This is necessary as all further steps use data1

## otherwise....
#########################################################################################
## SUBSETTING YOUR DATA (by Location/Species/Size) FOR ANALYSIS:
## Select the species you want to analyse, e.g:
 #data1 <- data[data$Species=="Acantho",]
## If you want to select two species (or more) use the OR operator "|", e.g:
# data1 <- data[data$Species=="Acantho"|data$Species=="Chromis",] 
## Or select data By Location, e.g:
# data1 <- data[data$Location=="AIMS 2015",]
# data1 <- data[data$Location=="LIRS 2016",]
## If you just want the LIRS locations you can use the OR operator as above, or just exclude the AIMS location using "!=", e.g:
# data1 <- data[data$Location!="AIMS 2015",]
## If you want to select data by species AND Location..., e.g:
# data1 <- data[data$Location!="AIMS 2015" & data$Species=="Acantho",]
## If you want to select data by species AND Location AND Size, e.g:
 #data1 <- data[data$Location!="AIMS 2015" & data$Species=="Acantho" & data$Size=="small",]
# data1 <- data[data$Location=="LIRS 2016" & data$Species=="Acantho" & data$Size=="small",]
# data1 <- data[data$Species=="Acantho" & data$Size=="small",]
# data1 <- data[data$Species=="Whitedams" & data$Size=="small",]
# data1 <- data[data$Species=="Lemon",]
# data1 <- data[data$Species=="Whitedams",]

#Nye et al looked at small fish and subsampled by species to correct for uneven 
data1<-data[data$Size_group=="small",]

data.acanth <- data[data$Species=="Acantho" & data$Size_group=="small",]
data.other <- data[data$Species!="Acantho" & data$Size_group=="small",]


#########################################################################################
#nrow(data1) # always check you have the correct number of records

## Define the control and treatment data (c.data and t.data)
#from original code. we do this later now
#c.data <- data1[data1$Treatment=="control",]
#t.data <- data1[data1$Treatment=="CO2",]

#check to see how many observations

with(data.acanth, table(Species,Treatment))
with(data.other, table(Species,Treatment))

#with(data1,table(Species,Treatment))

#########################################################################################
## Looking at the actual data for small fish only :
## data1 is small fish only but will use subsets of species acantho and other later

c.data <- data1[data1$Treatment=="control",]
t.data <- data1[data1$Treatment=="CO2",]
 par(mfrow=c(2,2))
# 
# ## The Absolute Latitude data is highly skewed (as you would expect), so you may wish to try log-transforming the response if you wish to use parametric tests. 
# ##	As you cant take a log of zero, you will need to add an integer. I suggest 20 (based on units of measurement). If so:
# # c.data$Response <- log(c.data$Response + 20)
# # t.data$Response <- log(t.data$Response + 20)
# 
nrow(c.data)
summary(c.data)
mean(c.data$Response)
var(c.data$Response)
median(c.data$Response)
hist(c.data$Response, main="Control",prob=TRUE)
lines(density(c.data$Response),col="red")

qqnorm(c.data$Response, main="Control")
qqline(c.data$Response,lty=2)
shapiro.test(c.data$Response)
# The Shapiro-Wilk test is very powerful and detects small departures from normality. However, t-tests (among others) are in practise VERY robust to departures from its
#		theoretical assumptions, especially where sample sizes are equal (or nearly equal) and when 2-tailed hypotheses are considered.
#		SO personally I wouldnt put much emphasis on the Shapiro-Wilk test results.

#need to change this to work with new code
nrow(t.data)
summary(t.data)
mean(t.data$Response)
var(t.data$Response)
median(t.data$Response)
hist(t.data$Response, main="Treatment",prob=TRUE)
lines(density(t.data$Response),col="red")

qqnorm(t.data$Response, main="Treatment")
qqline(t.data$Response,lty=2)
shapiro.test(t.data$Response)

par(mfrow=c(1,1))
plot.data <- data.frame(rep(c("control","treat"),c(nrow(c.data),nrow(t.data))),c(c.data$Response, t.data$Response))
names(plot.data) <- c("Group", "Response")
with(plot.data,boxplot(Response~Group))



#########################################################################################
#########################################################################################
## Define the control and treatment data (c.data and t.data)

#First separate all control fish for Acantho and other spp separately
acanth.c.data<-data.acanth[data.acanth$Treatment=="control",]
acanth.t.data<-data.acanth[data.acanth$Treatment=="CO2",]

nrow(acanth.t.data)
nrow(acanth.c.data)

other.c.data<-data.other[data.other$Treatment=="control",]
other.t.data<-data.other[data.other$Treatment=="CO2",]

nrow(other.t.data)
nrow(other.c.data)


## BOOTSTRAP

# Define the number of bootstraps you wish to run
Nboot <- 10000
# Define the sample size (to be sampled from EACH group- Control AND Treatment):
Nsamp <- 60
# Define a matrix for all bootstrap results
boot.results <- matrix(NA, Nboot, 18) 
 #First sample 60 per species
#cdata1<-c.data[sapply(rownames(c.data),c.data$Species,FUN=sample,size=10),]

set.seed(462020)  #set the seed to reproduce results

for(i in 1:Nboot){

#subsample only 33 random Acantho for small fish since only 33 Whitedams in CO2 treatment 
 acanth.c.sample<-sample(acanth.c.data$Response,33,replace=F)
 acanth.t.sample<-sample(acanth.t.data$Response,33,replace=F)
 
#Combine the acantho subsample and the other species
 c.data<-c(acanth.c.sample,other.c.data$Response)
 t.data<-c(acanth.t.sample,other.t.data$Response) 
 
  c.boot <- sample(c.data,Nsamp,replace=T)
  boot.results[i,1] <- mean(c.boot)
  boot.results[i,2] <- median(c.boot)
  boot.results[i,3] <- var(c.boot)
  st.c <- shapiro.test(c.boot)
  boot.results[i,4] <- as.numeric(st.c$statistic)
  boot.results[i,5] <- as.numeric(st.c$p.value)
  
  t.boot <- sample(t.data,Nsamp,replace=T)
  boot.results[i,6] <- mean(t.boot) 
  boot.results[i,7] <- median(t.boot)
  boot.results[i,8] <- var(t.boot)
  st.t <- shapiro.test(t.boot)
  boot.results[i,9] <- as.numeric(st.t$statistic)
  boot.results[i,10] <- as.numeric(st.t$p.value)
  
  vt <- var.test(t.boot,c.boot)
  boot.results[i,11] <- as.numeric(vt$statistic)
  boot.results[i,12] <- as.numeric(vt$p.value)
  tt <- t.test(t.boot,c.boot)
  boot.results[i,13] <- as.numeric(tt$statistic)
  boot.results[i,14] <- as.numeric(tt$p.value)
  wt <- wilcox.test(t.boot,c.boot)
  boot.results[i,15] <- as.numeric(wt$statistic)
  boot.results[i,16] <- as.numeric(wt$p.value)
  kst <- ks.test(t.boot,c.boot)
  boot.results[i,17] <- as.numeric(kst$statistic)
  boot.results[i,18] <- as.numeric(kst$p.value)

}

colnames(boot.results) <- c("mean.control", "median.control", "var.control", "shapiro.stat.control", "shapiro.p.control","mean.treatment", "median.treatment", "var.treatment", 
                            "shapiro.stat.treatment", "shapiro.p.treatment", "F.stat", "F.p", "t.stat", "t.p", "wilcox.stat", "wilcox.p", "ks.stat", "ks.p")
boot.results <- as.data.frame(boot.results)		

#Compare the distributions of means calculated from the bootstrapping
ks.test(boot.results$mean.control,boot.results$mean.treatment)
t.test(boot.results$mean.control,boot.results$mean.treatment)

# You can use summary function to extract basic statistics on the bootstrap results:
summary(boot.results)

# or import into excel or elsewhere to examine further.



## GRAPHICAL RESULTS of bootstrap
## As the specifications of each plot depends on the data set being investigated (what subset and response variable), generic coding for all plots will is not appropriate.
## If you are familiar with plotting in Excel (or other software), the easiest option would be to export the bootstrap dataand build your plots there.
## However, I have included some rough/basic examples of histograms of the bootstrap results below.

# To export data.frame as csv:
#write.table(boot.results,"smallfish boot_results.csv",row.names=F,sep=",",quote=F) 

# To plot histograms:

par(mfrow=c(2,2)) # 2x2 plots

## Top row: two separate histograms for control and treatment means:
hist(boot.results$mean.control, main="Control Histogram", xlab="Boot Mean")
hist(boot.results$mean.treatment, main="Treatment Histogram", xlab="Boot Mean")

## Bottom left: Overlapping histograms, in grey-scale
hist(boot.results$mean.control, col=rgb(0.1,0.1,0.1,0.5), xlim=c(30,60),main="Overlapping Histograms", xlab="Boot Mean")	#DARK GREY
hist(boot.results$mean.treatment, col=rgb(0.8,0.8,0.8,0.5), add=T)	#LIGHT GREY
box()
## as above in colour (red/blue):
#hist(boot.results$mean.control, col=rgb(1,0,0,0.5))	#RED
#hist(boot.results$mean.treatment, col=rgb(0,0,1,0.5), add=T)	#BLUE
#box()

## Bottom right: Density plots
plot(density(boot.results$mean.control), col="blue", main="Density Plot", xlab="Boot Mean",xlim=c(30,60))
lines(density(boot.results$mean.treatment), col="red")



