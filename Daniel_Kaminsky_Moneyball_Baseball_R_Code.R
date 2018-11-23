### Daniel Kaminsky - Moneyball Baseball ###
### Kaggle competition name: @DanielK ###

## Creating the TRAINING Dataset ###
# Reading the file into R
mydata <- read.csv("D:/moneyball.csv", sep = ",")

# Check mydata using str()
str(mydata) # 'data.frame': 2276 obs. of 17 variables
head(mydata)
tail(mydata)

# Use summary() to obtain and present descriptive statistics from mydata.
summary(mydata)

# In case they are not installed, install the following packages
install.packages(c("rpart", "MASS", "moments", "car", "gvlma", "plyr", 
                   "gridExtra", "flux", "scales"))
# Load the packages 
library(moments)
library(grid)
library(gridExtra)
library(flux)
library(scales)
library(rpart)
library(rpart.plot)
library(MASS)
library(car)
library(gvlma)
library(plyr)


# Histogram and Q-Q Plots
par(mfrow = c(2, 2), mar = c(5.1, 6.1, 4.1, 2.1))
hist(mydata$TEAM_FIELDING_E, col = "deepskyblue3", main = "Histogram of TEAM_FIELDING_E", xlab = "TEAM_FIELDING_E",
     cex = 2, cex.axis = 1.5, cex.lab = 2.0, cex.main = 2, cex.sub = 1.5)
qqnorm(mydata$TEAM_FIELDING_E, col = "deepskyblue3", pch = 'o', main = "Normal Q-Q Plot",
       cex = 2, cex.axis = 1.5, cex.lab = 2.0, cex.main = 2, cex.sub = 1.5)
qqline(mydata$TEAM_FIELDING_E, col = "darkred", lty = 2, lwd = 3)
boxplot(mydata$TEAM_FIELDING_E[mydata$TEAM_FIELDING_E], col = "red", ylim = c(0.00, 2000), pch = 16,
        main = "TEAM_FIELDING_E", cex = 2.0, cex.axis = 1.65, cex.lab = 1.75, cex.main = 2.0)
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))

# Checking skewness and kurtosis helps to reveal more about distribution shape.  
# A normal distribution has a skewness of zero and kurtosis of 3.0.
moments::skewness(mydata$TEAM_FIELDING_E)
moments::kurtosis(mydata$TEAM_FIELDING_E)

# ANOVA of TARGET_WINS with Predictor Variable TEAM_FIELDING_E
ANOVA1 <- aov(TARGET_WINS ~ TEAM_FIELDING_E, data = mydata)
summary(ANOVA1)

# Missing Values - Count per Variable
sapply(mydata, function(mydata) sum(is.na(mydata)))

# list rows of data that have missing values 
mydata[!complete.cases(mydata),]

# Fixing Missing Values with MEDIANs. Select rows where the Variable Observation is NA and replace it with MEDIAN
mydata$TEAM_BATTING_SO[is.na(mydata$TEAM_BATTING_SO)==T] <- median(mydata$TEAM_BATTING_SO, na.rm = TRUE)
median(mydata$TEAM_BATTING_SO, na.rm = F) # Testing for NA values
mydata$TEAM_BASERUN_SB[is.na(mydata$TEAM_BASERUN_SB)==T] <- median(mydata$TEAM_BASERUN_SB, na.rm = TRUE)
median(mydata$TEAM_BASERUN_SB, na.rm = F) # Testing for NA values
mydata$TEAM_BASERUN_CS[is.na(mydata$TEAM_BASERUN_CS)==T] <- median(mydata$TEAM_BASERUN_CS, na.rm = TRUE)
median(mydata$TEAM_BASERUN_CS, na.rm = F) # Testing for NA values
mydata$TEAM_BATTING_HBP[is.na(mydata$TEAM_BATTING_HBP)==T] <- median(mydata$TEAM_BATTING_HBP, na.rm = TRUE)
median(mydata$TEAM_BATTING_HBP, na.rm = F) # Testing for NA values
mydata$TEAM_PITCHING_SO[is.na(mydata$TEAM_PITCHING_SO)==T] <- median(mydata$TEAM_PITCHING_SO, na.rm = TRUE)
median(mydata$TEAM_PITCHING_SO, na.rm = F) # Testing for NA values
mydata$TEAM_FIELDING_DP[is.na(mydata$TEAM_FIELDING_DP)==T] <- median(mydata$TEAM_FIELDING_DP, na.rm = TRUE)
median(mydata$TEAM_FIELDING_DP, na.rm = F) # Testing for NA values

# Missing Values - Count per Variable
sapply(mydata, function(mydata) sum(is.na(mydata)))

# Use summary() to obtain and present descriptive statistics from mydata.
summary(mydata)

# Regressing TARGET_WINS as the dependent variable on All Predictor Variables as a BASELINE Model
MLR_Model <- lm(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR
                   + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS
                   + TEAM_BATTING_HBP + TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_BB
                   + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP, data = mydata)
summary(MLR_Model)

# MLR_Model Metrics
coefficients(MLR_Model) # Model coefficients
confint(MLR_Model, level=0.95) # Confidence Intervals for model parameters
anova(MLR_Model) # anova table 
# fitted(MLR_Model) # predicted values
# residuals(MLR_Model) # residuals
# vcov(MLR_Model) # covariance matrix for model parameters 
# influence(MLR_Model) # regression diagnostics

# Diagnostic plots for the MLR_Model
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(MLR_Model)

# Assessing Outliers
outlierTest(MLR_Model) # Bonferonni p-value for most extreme obs
qqPlot(MLR_Model, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(MLR_Model) # leverage plots

# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(MLR_Model)-length(MLR_Model$coefficients)-2)) 
plot(MLR_Model, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(MLR_Model, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(MLR_Model, main="QQ Plot")
# distribution of studentized residuals
# library(MASS)
sresid <- studres(MLR_Model) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xMLR_Model<-seq(min(sresid),max(sresid),length=40) 
yMLR_Model<-dnorm(xMLR_Model) 
lines(xMLR_Model, yMLR_Model)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(MLR_Model)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(MLR_Model)

# Evaluate Collinearity
vif(MLR_Model) # variance inflation factors 
sqrt(vif(MLR_Model)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(MLR_Model)
# Ceres plots 
ceresPlots(MLR_Model)

# Test for Autocorrelated Errors
durbinWatsonTest(MLR_Model)

# Global test of model assumptions
# library(gvlma)
gvmodel <- gvlma(MLR_Model) 
summary(gvmodel)

# Transforming by capping the number to 439 and 1474 to get rid of outliers
# Testing the Frequency for the value 439
count(mydata$TEAM_BASERUN_SB > 439)
count(mydata$TEAM_BASERUN_SB == 439)
mydata$TEAM_BASERUN_SB[mydata$TEAM_BASERUN_SB > 439] <- 439
# Re-testing the frequency for the value 439
count(mydata$TEAM_BASERUN_SB > 439)
count(mydata$TEAM_BASERUN_SB == 439)

# Testing the Frequency for the value 1474
count(mydata$TEAM_PITCHING_SO > 1474)
count(mydata$TEAM_PITCHING_SO == 1474)
mydata$TEAM_PITCHING_SO[mydata$TEAM_PITCHING_SO > 1474] <- 1474
# Re-testing the frequency for the value 1474
count(mydata$TEAM_PITCHING_SO > 1474)
count(mydata$TEAM_PITCHING_SO == 1474)

# Stepwise Regression
# library(MASS) Loaded at the biginning
MLR_Stepwise_Model <- lm(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR
                         + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS
                         + TEAM_BATTING_HBP + TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_BB
                         + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP,data=mydata)
Step <- stepAIC(MLR_Stepwise_Model, direction="both")
Step$anova # display results
coefficients(Step) # Model coefficients

# Comparing Models
anova(MLR_Model, Step) 

# Decision Tree for the full model MLR_Model
DTreeFull <- rpart(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR
                   + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS
                   + TEAM_BATTING_HBP + TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_BB
                   + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP,data=mydata)
summary(DTreeFull)
plot(DTreeFull)
text(DTreeFull)

# Decision Tree for the Stepwise model MLR_Stepwise_Model
DTreeStep <- rpart(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + 
                     TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + 
                     TEAM_PITCHING_H + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP,data=mydata)
summary(DTreeStep)
plot(DTreeStep)
text(DTreeStep)

# Decision Tree for the TEAM_PITCHING_SO variable used as Response variable
DTree_PitchSO = rpart(TEAM_PITCHING_SO ~ TEAM_BATTING_SO + TEAM_BATTING_H,data=mydata)
summary(DTree_PitchSO)
plot(DTree_PitchSO)
text(DTree_PitchSO)

#### CREATING A TEST DATASET ###
# Reading the file into R
TESTdata <- read.csv("D:/moneyball_test.csv", sep = ",")

# Check mydata using str()
str(TESTdata) # 'data.frame': 259 obs. of 17 variables
head(TESTdata)
tail(TESTdata)

# Use summary() to obtain and present descriptive statistics from mydata.
summary(TESTdata)

# Missing Values - Count per Variable
sapply(TESTdata, function(TESTdata) sum(is.na(TESTdata)))

# Fixing Missing Values with MEDIANs. Select rows where the Variable Observation is NA and replace it with MEDIAN
TESTdata$TEAM_BATTING_SO[is.na(TESTdata$TEAM_BATTING_SO)==T] <- median(TESTdata$TEAM_BATTING_SO, na.rm = TRUE)
median(TESTdata$TEAM_BATTING_SO, na.rm = F) # Testing for NA values
TESTdata$TEAM_BASERUN_SB[is.na(TESTdata$TEAM_BASERUN_SB)==T] <- median(TESTdata$TEAM_BASERUN_SB, na.rm = TRUE)
median(TESTdata$TEAM_BASERUN_SB, na.rm = F) # Testing for NA values
TESTdata$TEAM_BASERUN_CS[is.na(TESTdata$TEAM_BASERUN_CS)==T] <- median(TESTdata$TEAM_BASERUN_CS, na.rm = TRUE)
median(TESTdata$TEAM_BASERUN_CS, na.rm = F) # Testing for NA values
TESTdata$TEAM_BATTING_HBP[is.na(TESTdata$TEAM_BATTING_HBP)==T] <- median(TESTdata$TEAM_BATTING_HBP, na.rm = TRUE)
median(TESTdata$TEAM_BATTING_HBP, na.rm = F) # Testing for NA values
TESTdata$TEAM_PITCHING_SO[is.na(TESTdata$TEAM_PITCHING_SO)==T] <- median(TESTdata$TEAM_PITCHING_SO, na.rm = TRUE)
median(TESTdata$TEAM_PITCHING_SO, na.rm = F) # Testing for NA values
TESTdata$TEAM_FIELDING_DP[is.na(TESTdata$TEAM_FIELDING_DP)==T] <- median(TESTdata$TEAM_FIELDING_DP, na.rm = TRUE)
median(TESTdata$TEAM_FIELDING_DP, na.rm = F) # Testing for NA values

# Missing Values - Count per Variable
sapply(TESTdata, function(TESTdata) sum(is.na(TESTdata)))

# Use summary() to obtain and present descriptive statistics from mydata.
summary(TESTdata)

# Transforming by capping the number to 439 and 1474 to get rid of outliers
# Testing the Frequency for the value 439
count(TESTdata$TEAM_BASERUN_SB > 439)
count(TESTdata$TEAM_BASERUN_SB == 439)
TESTdata$TEAM_BASERUN_SB[TESTdata$TEAM_BASERUN_SB > 439] <- 439
# Re-testing the frequency for the value 439
count(TESTdata$TEAM_BASERUN_SB > 439)
count(TESTdata$TEAM_BASERUN_SB == 439)

# Testing the Frequency for the value 1474
count(TESTdata$TEAM_PITCHING_SO > 1474)
count(TESTdata$TEAM_PITCHING_SO == 1474)
TESTdata$TEAM_PITCHING_SO[TESTdata$TEAM_PITCHING_SO > 1474] <- 1474
# Re-testing the frequency for the value 1474
count(TESTdata$TEAM_PITCHING_SO > 1474)
count(TESTdata$TEAM_PITCHING_SO == 1474)

# Regression Model
TESTdata$P_TARGET_WINS <-	(24.0240948217 + TESTdata$TEAM_BATTING_H * 0.0485105854
                           + TESTdata$TEAM_BATTING_2B * -0.0205710078 + TESTdata$TEAM_BATTING_3B * 0.0668666914
                           + TESTdata$TEAM_BATTING_HR * 0.1245180990 + TESTdata$TEAM_BATTING_SO * -0.0162703578
                           + TESTdata$TEAM_BASERUN_SB * 0.0268808689 + TESTdata$TEAM_PITCHING_H * -0.0008896633
                           + TESTdata$TEAM_PITCHING_HR * -0.0522748852 + TESTdata$TEAM_PITCHING_BB * 0.0088449965
                           + TESTdata$TEAM_PITCHING_SO * 0.0099451602 + TESTdata$TEAM_FIELDING_E * -0.0225632498
                           + TESTdata$TEAM_FIELDING_DP * -0.1167093486)

# Use summary() to obtain and present descriptive statistics from mydata.
summary(TESTdata)

# Creating the Output file with the INDEX and P_TARGET_WINS (Rounded to whole numbers)
Daniel_Kaminsky_Sec60_R_Output <- data.frame(TESTdata$INDEX, round(TESTdata$P_TARGET_WINS, 0))
names(Daniel_Kaminsky_Sec60_R_Output) <-c("INDEX", "P_TARGET_WINS")

# Checking the Output Dataset
str(Daniel_Kaminsky_Sec60_R_Output) # 'data.frame': 259 obs. of 2 variables
head(Daniel_Kaminsky_Sec60_R_Output)

### Write TESTdata to CSV ###
write.csv(Daniel_Kaminsky_Sec60_R_Output, 
          file = "D:/Moneyball_R_Output.csv", 
          row.names = FALSE)


