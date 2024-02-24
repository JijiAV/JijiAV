#TASK 1

#clear screen
cat("\014")

#importing library
library(gamlss)

##Section 1.1 (a)

data('dbbmi')

#print the first 6 values of the data set
head(dbbmi)

#To find the number of rows in the dataset
nrow(dbbmi)

sd(dbbmi$age)

#Plotting bmi against age
plot(bmi~age,data=subset(dbbmi,age<0.10))

#assigning the age for which bmi is to be calculated
age_old <- 12

#take the subset of the dataset satisfying the given condition(age b/w 12 and 13)
new_data <- with(dbbmi,subset(dbbmi,age>age_old  & age<age_old +1))

#print the first 6 values in new_data
head(new_data)

#assign the values in the column bmi in new_data
pred_bmi <- new_data$bmi
pred_bmi

#plot the histogram for bmi values corresponding to ages 12 to 13
hist(pred_bmi,breaks=30,main="Histogram for bmi",xlab="bmi")

##Section 1.1 (b): Fit different parametric distributions to the data

model1<-fitDist(new_data$bmi)
model1
model1$fits
model1$fails

hist1<-histDist(new_data$bmi, "SHASHo2" , density=TRUE,ylim = c(0,0.22))
plot(hist1)
wp(hist1)


##Section 1.1 (c):
summary(model1)

#The Sinh-Arcsinh (SHASH) distribution is a four parameter distribution
#mu : vector of location parameter values
#sigma: vector of scale parameter values
#nu : vector of skewness parameter values 
#tau : vector of kurtosis parameter values

#TASK 2

##Section 1.2 (a):

data('grip')

#print the first 6 values of the data set
head(grip)

##Section 1.2 (b):

set.seed(360)
index<-sample(3766,1000)
mydata<-grip[index,]
dim(mydata)

hist(mydata$age,prob=TRUE)
curve(dnorm(x, mean=mean(mydata$age), sd=sd(mydata$age)), add=TRUE,col="blue")
sd(mydata$age)

##Section 1.2 (c):

plot(mydata$age,mydata$grip)
z.scoresQS

##Section 1.2 (d):
gbccg_model <- gamlss(grip~pb(age),sigma.fo=~pb(age),nu.fo=~pb(age), family=BCCG, data=mydata)
edfAll(gbccg_model)

?edfAll

##Section 1.2 (e):

gbct_model <- gamlss(grip~pb(age),sigma.fo=~pb(age),nu.fo=~pb(age), tau.fo=~pb(age), family=BCT, data=mydata,
                     start.from = gbccg_model)
edfAll(gbct_model)

gbcpe_model <- gamlss(grip~pb(age),sigma.fo=~pb(age),nu.fo=~pb(age), tau.fo=~pb(age), family=BCPE, data=mydata,
                     start.from = gbccg_model)
edfAll(gbcpe_model)

##Section 1.2 (f):

GAIC(gbccg_model,gbct_model,gbcpe_model)

##Section 1.2 (g):

# to plot the fitted values for all the parameters of a GAMLSS model against the explanatory variable
fittedPlot(gbct_model,gbcpe_model,x=mydata$age)

##Section 1.2 (h):

centiles(gbccg_model,xvar=mydata$age,cent=c(3,10,25,50,75,90,97),xlab="age",ylab="grip")

centiles(gbct_model,xvar=mydata$age,cent=c(3,10,25,50,75,90,97),xlab="age",ylab="grip")

centiles(gbcpe_model,xvar=mydata$age,cent=c(3,10,25,50,75,90,97),xlab="age",ylab="grip")

##Section 1.2 (i):

plot(gbccg_model)
plot(gbct_model)
plot(gbcpe_model)

wp(gbccg_model,ylim.all=3)

wp(gbct_model)

wp(gbcpe_model)

Q.stats(gbcpe_model,xvar=mydata$age)
Q.stats(gbct_model,xvar=mydata$age)
Q.stats(gbccg_model,xvar=mydata$age)


##Section 1.2 (j):

#best model - gcpe (since AIC is lowest)
