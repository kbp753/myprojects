rm(list=ls())
library(rio)
library(moments)
library(car)

# Loading the data
avocado.data <- import("avocado.csv")
colnames(avocado.data) <- tolower(make.names(colnames(avocado.data)))
attach(avocado.data)

# Factorizing the columns in the dataset
avocado.data$total.volume <- as.factor(avocado.data$total.volume)
avocado.data$x4046 <- as.factor(avocado.data$x4046)
avocado.data$x4225 <- as.factor(avocado.data$x4225)
avocado.data$x4770 <- as.factor(avocado.data$x4770)
avocado.data$total.bags <- as.factor(avocado.data$total.bags)
avocado.data$small.bags <- as.factor(avocado.data$small.bags)
avocado.data$large.bags <- as.factor(avocado.data$large.bags)
avocado.data$xlarge.bags <- as.factor(avocado.data$xlarge.bags)
avocado.data$type <- as.factor(avocado.data$type)
avocado.data$year <- as.factor(avocado.data$year)
avocado.data$region <- as.factor(avocado.data$region)
avocado.data$averageprice <- as.numeric(avocado.data$averageprice)
avocado.data$date <- as.factor(avocado.data$date)

# Random subset sample of dataset
set.seed(32881075)
sample.data.1 <- avocado.data[sample(1:nrow(avocado.data), 100),]

# 1) Three simple regression models of the form (y, X1), (y, X2), and (y, X3).
lm1 <- lm(averageprice ~ type, sample.data.1)
summary(lm1)

lm2 <- lm(averageprice ~ large.bags, sample.data.1)
summary(lm2)

lm3 <- lm(averageprice ~ xlarge.bags, sample.data.1)
summary(lm3)

# 2) Three multiple regression models using two independent variables of the form (y, X1, X2), (y, X1, X3), and (y, X2, X3).
multi.reg.1 <- lm(averageprice ~ type + large.bags, sample.data.1)
summary(multi.reg.1)

multi.reg.2 <- lm(averageprice ~ large.bags + xlarge.bags, sample.data.1)
summary(multi.reg.2)

multi.reg.3 <- lm(averageprice ~ type + xlarge.bags, sample.data.1)
summary(multi.reg.3)

# 3) One full main-effects multiple regression model of the form (y, X1, X2, X3)
fullregm <- lm(averageprice ~ type + large.bags + xlarge.bags, sample.data.1)
summary(fullregm)

# 4) One multiple regression model using an interaction term of the form (y, X1, X2, X1X2)
multireg <- lm(averageprice ~ type + xlarge.bags + (type * xlarge.bags), sample.data.1)
summary(multireg)

# 5) Two simple regression models using squared terms to investigate the presence of non-linear relationships
squared.reg.1 <- lm(averageprice ~ (type + type^2), sample.data.1)
summary(squared.reg.1)

squared.reg.2 <- lm(averageprice ~ (xlarge.bags + xlarge.bags^2), sample.data.1)
summary(squared.reg.2)

# LINE Assumptions - Plots
par(mfrow=c(2,2))

# Residuals vs Fitted plot
plot(lm1, which=1, main="Residuals vs Fitted (lm1)")

# Normal Q-Q plot
plot(lm1, which=2, main="Normal Q-Q (lm1)")

# Scale-Location plot
plot(lm1, which=3, main="Scale-Location (lm1)")

# Residuals vs Leverage plot
plot(lm1, which=5, main="Residuals vs Leverage (lm1)")

# Cook's Distance plot
plot(lm1, which=4, main="Cook's Distance (lm1)")

# Checking multicollinearity using Variance Inflation Factor (VIF)
vif(multi.reg.1)
vif(multi.reg.2)
vif(multi.reg.3)
vif(fullregm)
vif(multireg)
vif(squared.reg.1)
vif(squared.reg.2)

# Additional analysis
# Correlation matrix
cor(avocado.data[, c("averageprice", "type", "large.bags", "xlarge.bags")])

# ANOVA
anova(fullregm)

# Effect size measures
etaSquared(fullregm)
omegaSquared(fullregm)

# Checking normality of residuals
shapiro.test(residuals(fullregm))

# Checking homoscedasticity
ncvTest(fullregm)

# Durbin-Watson test for autocorrelation
dwtest(fullregm)

# Resetting the graphical parameters
par(mfrow=c(1,1))
