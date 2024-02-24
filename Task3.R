#TASK 3

#clear screen
cat("\014")

#importing library
library(gamlss)

##Section 1.3 (a):

#importing the dataset
Data=read.csv("Life Expectancy Data.csv",header=TRUE)
head(Data)

#explanatory -> population, GDP, Adult.Martality, expenditure , BMI
#target -> life.expectancy

##Section 1.3 (b):

# data source: https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who
#To analyse what factors affect life expectancy

##Section 1.3 (c):

Data1 <- Data[c("Population", "GDP", "Adult.Mortality","Total.expenditure","BMI", "Life.expectancy")]
#Remove NA's
Rel_data<-na.omit(Data1)
summary(Rel_data)
head(Rel_data)

#Histogram for life expectancy
hist(Rel_data$Life.expectancy,main="Histogram for Life Expectancy")

#Histogram for Population
hist(Rel_data$Population, main="Histogram for Population")

#Histogram for GDP
hist(Rel_data$GDP,main="Histogram for GDP")

#Histogram for Adult Mortality
hist(Rel_data$Adult.Mortality,main="Histogram for Mortality")

#Histogram for Total.expenditure
hist(Rel_data$Total.expenditure, main="Histogram for Expenditure")

#Histogram for BMI
hist(Rel_data$Life.expectancy,main="Histogram for BMI")

#Relevance of data & possible pitfalls 
# The data was from a secondary source rather than primary one. 
# Data from 2010-2015 -> not uptodate
# possible outliers in Population and GDP

#Remove outliers from population
Q1 <- quantile(Rel_data$Population, .25)
Q3 <- quantile(Rel_data$Population, .75)
IQR <- IQR(Rel_data$Population)

Rel_data <- subset(Rel_data, Rel_data$Population > (Q1 - 1.5*IQR) & Rel_data$Population < (Q3 + 1.5*IQR))

#Histogram for Population
hist(Rel_data$Population, main="Histogram for Population")

#Checked for outliers in GDP using the same method above but as there was no outliers removed,
#the section was removed from here.

##Section 1.3 (d):

plot(Life.expectancy~Population, data=Rel_data, col=gray(0.6), pch=15, cex=0.5)
plot(Life.expectancy~GDP, data=Rel_data, col=gray(0.7), pch=15, cex=0.5)
plot(Life.expectancy~Adult.Mortality, data=Rel_data, col=gray(0.7), pch=15, cex=0.5)
plot(Life.expectancy~Total.expenditure, data=Rel_data, col=gray(0.7), pch=15, cex=0.5)
plot(Life.expectancy~BMI, data=Rel_data, col=gray(0.7), pch=15, cex=0.5)

#Changing to training and testing data
indx = sort(sample(nrow(Rel_data), nrow(Rel_data)*.75))

train <- Rel_data[indx,]
test <- Rel_data[-indx,]

fit_life=fitDist(train$Life.expectancy,type="realline")
fit_life
fit_life$fits
fit_life$failed

#SHASH - Sinh-ArcSinh

histDist(Rel_data$Life.expectancy, family=SHASH, nbins=30, line.col="black")

#model with Life.expectancy~Population
lm_model1 <- gamlss(Life.expectancy~pb(Population), family=SHASH, data=train, control = gamlss.control(n.cyc = 200,trace=FALSE))
summary(lm_model1)
#chooseDist(lm_model1) -> min GAIC for SHASHo
#Life.expectancy~Population -> 10083 -> same

#model with Life.expectancy~Adult.Mortality
lm_model2 <- gamlss(Life.expectancy~pb(Adult.Mortality), family=SHASH, data=train, control = gamlss.control(n.cyc = 200,trace=FALSE))
summary(lm_model2)

#chooseDist(lm_model2) 
#-> min GAIC for BCTo

#model with Life.expectancy~Total.expenditure
lm_model3 <- gamlss(Life.expectancy~pb(Total.expenditure), family=SHASH, data=train, control = gamlss.control(n.cyc = 200,trace=FALSE))
summary(lm_model3)
#trace=FALSE -> AIC: 10001.16 and give the below error:
#Algorithm RS not converged -> change trace=FALSE into control = gamlss.control(n.cyc = 200,trace=FALSE)

#model with Life.expectancy~GDP
lm_model4 <- gamlss(Life.expectancy~pb(GDP), family=SHASH, data=train, control = gamlss.control(n.cyc = 200,trace=FALSE))
summary(lm_model4)

#model with Life.expectancy~BMI
lm_model5 <- gamlss(Life.expectancy~pb(BMI), family=SHASH, data=train, control = gamlss.control(n.cyc = 200,trace=FALSE))
summary(lm_model5)

AIC(lm_model1, lm_model2, lm_model3,lm_model4,lm_model5)

#lm_model2 and lm_model5 has lowest AIC

#model with Life.expectancy~BMI+Adult.Mortality
lm_model6 <- gamlss(Life.expectancy~pb(BMI)+pb(Adult.Mortality), family=SHASH, data=train,control = gamlss.control(n.cyc = 200,trace=FALSE))
summary(lm_model6)

#for summary, qr is used instead of vcov -> use individual fits of the parameters
# donot take into account the correlation between the estimates of the distribution parameters

#model with Life.expectancy~BMI+Adult.Mortality+GDP
lm_model7 <- gamlss(Life.expectancy~pb(BMI)+pb(Adult.Mortality)+pb(GDP), family=SHASH, data=train, control = gamlss.control(n.cyc = 200,trace=FALSE))
summary(lm_model7)

#model with Life.expectancy~BMI+Adult.Mortality+GDP+Expenditure
lm_model8 <- gamlss(Life.expectancy~pb(BMI)+pb(Adult.Mortality)+pb(GDP)+pb(Total.expenditure), family=SHASH, data=train, control = gamlss.control(n.cyc = 200,trace=FALSE))
summary(lm_model8)


#model with Life.expectancy~BMI+Adult.Mortality+GDP+Total.expenditure+Population
lm_model9 <- gamlss(Life.expectancy~pb(BMI)+pb(Adult.Mortality)+pb(GDP)+pb(Total.expenditure)+pb(Population), family=SHASH, data=train, control = gamlss.control(n.cyc = 200,trace=FALSE))
summary(lm_model9)

AIC(lm_model6,lm_model7,lm_model8,lm_model9)

#The effective degrees of freedom:
edfAll(lm_model9,"mu")

##Section 1.3 (e):

#For Residual plots
plot(lm_model7)

plot(lm_model9)

#wormplot
wp(lm_model7,ylim.all=1)

wp(lm_model9,ylim.all=1)

#Fitted smooth functions in the best model
term.plot(lm_model9, pages=1, ask=FALSE)

##Section 1.3 (f):

#Use the chosen mode to predict all the parameters μ, σ, ν and τ for the values in newdata.
x_test <-test[c("Population", "GDP", "Adult.Mortality","Total.expenditure","BMI")]

y_test <- predict(lm_model9,newdata=x_test,type="response")
test['res']=y_test-test['Life.expectancy']

plot(y_test,test$res,col="blue")
abline(0,0)

