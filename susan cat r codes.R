rm(list = ls())
dt<-read.csv("C:\\stata assignments\\almost complete\\masters for march\\msc finance susan\\tanzania data\\Finalized TZ data1.csv")

summary(dt)
names(dt)
attach(dt)
detach(dt)
dt1<-dt[complete.cases(dt),]
View(dt1)
attach(dt1)
summary(dt1)
mean(dt1$GDP)
sd(dt1$GDP)
hist(kurtosi(dt1))
skewness(dt1)
names(dt1)

model1<-lm(dt1$GDP~dt1$CPI+dt1$Exchange.Rates)
model1
plot(model)
predict(model1,)

summary(model)
m <- lm(GDP~Gross.Investments)
par(mfrow = c(2, 2))
plot(m)

w <- lm(GDP~Consumer.price.index)
par(wfrow = c(2, 2))
plot(w)




plot(year,GDP,type="l",main="Graph of GDP of Ghana from 1960 to 2017 \n Group 5")
plot(year,Gross.Investments ,type="l",main="Graph of Gross investments of Ghana from 1960 to 2017 \n Group 5")
plot(year,Consumer.price.index ,type="l",main="Graph of Consumer price index of Ghana from 1960 to 2017 \n Group 5")

###########################
###gauss assumptions    ###
###########################

###(Generalized) Linear models make some strong assumptions 
###concerning the data structure:
##  1.)Independance of each data points
##  2.)Correct distribution of the residuals
##  3.)Correct specification of the variance structure
##  4.)Linear relationship between the response and the linear predictor
##  For simple lm 2-4) means that the residuals should be normally distributed,
 ##the variance should be homogenous across the fitted values of the model and 
 ##for each predictors separately, and the y?s should be linearly related to
 ##the predictors. # testing model assumptions some simulated data

x <- runif(100, 0, 10)
y <- 1 + 2 * x + rnorm(100, 0, 1)
m <- lm(y ~ x)
par(mfrow = c(2, 2))
plot(m)


###interpetations of the graphs
################################

#The top-left and top-right graphs are the most important one,
 #the top-left graph check for the homogeneity of the variance and
 #the linear relation, if you see no pattern in this graph 
#(ie if this graph looks like stars in the sky), 
#then your assumptions are met. 
#The second graphs check for the normal distribution of the residuals,
# the points should fall on a line

