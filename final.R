#Reading in the dataset
nbaDataSet = read.csv('RegressionSalaryDatasetFinal.csv', sep= ",")
View(nbaDataSet)

#Importing the required libraries
library(faraway)
library(ggplot2)
library(corrplot)
library(lmtest)
library(MASS)
library(fastDummies)
library(stringr)
library(tibble)
library(glmnet)
library(randomForest)


#Heat map for correlation between predictors
library(corrplot)
salData = nbaDataSet[, -c(1,4)]
View(salData)
#Adding a new column which classiies athletes as those with rookie contracts and those without
#because atheltes with rookie contracts are often paid less than those without
#average age of rookies is roughly around 22
#rookie contracts can last up to 4 years, so anyone
#under 26 will probably be on a rookie contract
rookieContracts = which(salData$AGE < 25)
salData[ , 23] = rep("nonRookie", nrow(salData))
salData[rookieContracts, 23] = "Rookie"
colnames(salData)[23] = "RookieContract"

View(salData)

#Mean salary of altheletes with rookie Contracts is around 4.1 million
mean(salData[rookieContracts, 'AdjustedSalary'])
#Mean salary of altheletes without rookie Contracts is around 9.4 million
mean(salData[-rookieContracts, 'AdjustedSalary'])


#Making column RookieContracts a factor
salData$RookieContract = as.factor(salData$RookieContract)
class(salData$RookieContract)
levels(salData$RookieContract)

cor(salData[,-c(1,23)])
corrplot(cor(salData[,-c(1,23)]), method="square",type = "lower", order="hclust", addCoef.col = "black",number.cex = 0.5)

par(mfrow=c(1,3))
#Plot btween MinutesPG and MP
plot(MP~MinutesPG,data=salData)

#Plot btween OBPM and PER
plot(OBPM ~ PER, data=salData)

#Plot btween OBPM and VORP
plot(OBPM ~ VORP, data=salData)

par(mfrow=c(1,2))
#Plot btween OBPM and VORP
plot(PER ~ VORP, data=salData)

#Plot btween AdjustedSalaryCap and SEASON
plot(AdjustedSalaryCap ~ SEASON, data=salData) 

#Scatter Plot matrices
par(mfrow=c(2,2))
pairs(~.,data=salData[,c(2,3,4,5,6,18)],main="Scatterplot Matrix") 
pairs(~.,data=salData[,c(7,8,9,10,11,18)],main="Scatterplot Matrix")
pairs(~.,data=salData[,c(12:16,19,20,18)],main="Scatterplot Matrix")


library(ggplot2)
library(cowplot)

#for each of these variables, let's see if quadratic or cubic terms can be added

#can log the response variable because it has a large Right skew
agePlot = ggplot(standardizedData, aes(x=AGE, y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,4)) + ggtitle("Age^4 Fit") + theme(plot.title = element_text(hjust = 0.5))

#looks like age^4 is appropriate to use
age= lm(log(AdjustedSalary)~ poly(AGE, 4, raw = TRUE), data = standardizedData)
summary(age)


#now see if we can add quadratic term for minutes played
minutesPlayedPlot = ggplot(standardizedData, aes(x=MP, y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,2)) +ggtitle("MinutesPlayed^2 Fit") + theme(plot.title = element_text(hjust = 0.5))


#combine age plot with minutes played plot
plot_grid(agePlot, minutesPlayedPlot, labels = "AUTO")



mp = lm(log(AdjustedSalary)~ poly(MP, 2, raw = TRUE), data = standardizedData)
summary(mp) #it appears as though we can square MP


#x3par quadratic terms?
ggplot(standardizedData, aes(x=X3PAr, y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,1))
x3paR = lm(log(AdjustedSalary)~ poly(X3PAr, 2, raw = TRUE), data = standardizedData)
summary(x3paR) #it appears as though just no polynomial terms should be used
vif(x3paR)


#Free throw rate quadratic terms?
ggplot(standardizedData, aes(x=FTr, y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,3))
FTr = lm(log(AdjustedSalary)~ poly(FTr, 3, raw = TRUE), data = standardizedData)
summary(FTr) #it appears as though a cubic term is the best fit


#stl terms?
ggplot(standardizedData, aes(x=STL., y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,1))
stlpercentage = lm(AdjustedSalary~ poly(STL., 1, raw = TRUE), data = standardizedData)
summary(stlpercentage) #it looks like just including stl is fine


#TOV % terms
ggplot(standardizedData, aes(x=TOV., y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,2))
turnoverPercentage = lm(AdjustedSalary~ poly(TOV.,2, raw = TRUE), data = standardizedData)
summary(turnoverPercentage) #it looks like including turnover squared is best



#usage percentage terms? 
usagePlot = ggplot(standardizedData, aes(x=USG., y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,3)) +ggtitle("Usage Cubed Fit") + theme(plot.title = element_text(hjust = 0.5))
usagePlot
usgpct = lm(AdjustedSalary ~poly(USG., 3, raw = TRUE), data = standardizedData)
summary(usgpct) #usage cubed is best

#vorp terms?
vorpPlot = ggplot(standardizedData, aes(x=VORP, y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,3)) +ggtitle("Vorp Cubed Fit") + theme(plot.title = element_text(hjust = 0.5))
vorp = lm(AdjustedSalary ~poly(VORP, 4, raw = TRUE), data = standardizedData)
summary(vorp) #vorp cubed is best

#make 4 plots to include in the report 
plot_grid(agePlot, minutesPlayedPlot, usagePlot, vorpPlot, nrow = 2, ncol = 2)


#AdjustedPayroll
ggplot(standardizedData, aes(x=(AdjustedPayroll), y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,2))
adjPayroll = lm(AdjustedSalary ~poly(AdjustedPayroll, 2, raw = TRUE), data = standardizedData)
summary(adjPayroll) #payroll is fine with no quadratic terms

#minutesPG
ggplot(standardizedData, aes(x=(MinutesPG), y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,4))
minutesPG = lm(AdjustedSalary ~poly(MinutesPG, 3, raw = TRUE), data = standardizedData)
summary(minutesPG) #minutesPG should be squared


#true shooting percentage
ggplot(standardizedData, aes(x=(TS.), y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,5))
TS = lm(AdjustedSalary ~poly(TS., 3, raw = TRUE), data = standardizedData)
summary(TS) #true shooting should be cubed

#adjusted salary cap
ggplot(standardizedData, aes(x=AdjustedSalaryCap, y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,2))
AdjSal = lm(AdjustedSalary ~poly(AdjustedSalaryCap, 2, raw = TRUE), data = standardizedData)
summary(AdjSal) #adjusted salary is fine with no quadratic terms


#season
ggplot(standardizedData, aes(x=SEASON, y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,1))
season = lm(AdjustedSalary ~poly(SEASON, 4, raw = TRUE), data = standardizedData)
summary(season) #season cubed is best



#Games
ggplot(standardizedData, aes(x=G, y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,2))
games = lm(AdjustedSalary ~poly(G, 1, raw = TRUE), data = standardizedData)
summary(games) #games is fine no quadratic terms

#TRB percentage
ggplot(standardizedData, aes(x=TRB., y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,3))
TRB = lm(AdjustedSalary ~poly(TRB., 3, raw = TRUE), data = standardizedData)
summary(TRB) #trb. can be cubed


#assist percentage
ggplot(standardizedData, aes(x=AST., y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,3))
ast = lm(AdjustedSalary ~poly(AST., 3, raw = TRUE), data = standardizedData)
summary(ast) #ast. can be cubed

#OBPM
ggplot(standardizedData, aes(x=OBPM, y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,4))
obpm = lm(log(AdjustedSalary) ~poly(OBPM, 3, raw = TRUE), data = standardizedData)
summary(obpm) #OBPM can be cubed


#DBPM
ggplot(standardizedData, aes(x=DBPM, y=log(AdjustedSalary))) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,3))
dbpm = lm(log(AdjustedSalary) ~poly(DBPM, 2, raw = TRUE), data = standardizedData)
summary(dbpm) #DBPM is fine by itself



#Boxplots to identify outliers

#PER #outliers present 50 rows and 28 unique
boxplot(salData$PER, ylab = "PER")
out_PER = boxplot.stats(salData$PER)$out
#out_PER = unique(out_PER)
out_PER
length(out_PER)
uni_out_PER = unique(out_PER)
length(uni_out_PER)

#Age #outliers present 16 rows and 5 unique
boxplot(salData$AGE, ylab = "age")
out_age = boxplot.stats(salData$AGE)$out
out_age
length(out_age)
uni_out_age = unique(out_age)
uni_out_age
length(uni_out_age)


#Making factors of position
position_levels = c("PG", "SG","SF", "PF", "C")
factor(salData$POS, levels = position_levels)
class(salData$POS)
salData$POS = as.factor(salData$POS)
class(salData$POS)

#Checking relationship between AdjustedSalary and Age
lp <- ggplot(salData, aes(x = AGE, y = AdjustedSalary)) + geom_point(shape=23) + geom_smooth(method = lm)
lp



#Boxplots for rookies and Non rookies
ggplot(salData, aes(x=RookieContract, y = AdjustedSalary)) +geom_boxplot()



#create the new dataset, this time all continuous variables are standardized to help
#deal with multicollinearity of interaction terms
standardizedData = data.frame(salData)
tracemem(standardizedData) == tracemem(salData)
#now standardize the standardizedData
#standardize all continuous variables
#column 18 is the response, column 1 and 23 are categorical variables
scale(standardizedData[-c(1,18,23)])
standardizedData[,-c(1,18,23)] = scale(standardizedData[,-c(1,18,23)])
View(standardizedData)

#Splitting the data into train and test
set.seed(11)
idx = sample(nrow(standardizedData), nrow(standardizedData)*0.8)
train = standardizedData[idx, ] #Use 80% of observations as traing data
test = standardizedData[-idx, ] # Use 20% observations as testing data

#Making a model with only interaction terms
simple_mod = lm(AdjustedSalary ~ (AGE + G + MP + PER + X3PAr + FTr + TRB. + AST. + STL. + BLK. + TOV. + USG. + OBPM + DBPM + VORP + SEASON + AdjustedPayroll + MinutesPG + TS. + AdjustedSalaryCap + POS + RookieContract)^2, data = train)
summary(simple_mod)

#Model with all polynomial and interaction terms
fit_full = lm(AdjustedSalary ~ (AGE + G + MP + PER + X3PAr + FTr + TRB. + AST. + STL. + BLK. + TOV. + USG. + OBPM + DBPM + VORP + SEASON + AdjustedPayroll + MinutesPG + TS. + AdjustedSalaryCap + POS + RookieContract)^2 + poly(OBPM, 3, raw = TRUE) + poly(AST.,3, raw = TRUE)
              + poly(TRB.,3, raw = FALSE) + G + poly(SEASON, 3, raw = TRUE)
              
              + AdjustedSalaryCap + poly(TS., 3, raw = TRUE) + poly(MinutesPG, 3, raw = TRUE)
              + poly(VORP, 4, raw = TRUE) + AdjustedPayroll + poly(USG., 3, raw = TRUE) 
              + poly(TOV.,2, raw = TRUE) + STL. + poly(FTr, 3, raw = TRUE) + X3PAr
              + poly(MP, 2, raw = TRUE) + poly(AGE, 4, raw = TRUE), data = train)
anova(simple_mod,fit_full)
#polynomial and interaction terms are significant

#Doing stepwise selection with BIC
#Stepwise selection with BIC
n = nrow(train)

#Null model
fit_null = lm(AdjustedSalary ~ 1,data = train)
summary(fit_null)


#Stepwise for all interaction and polynomial terms.
#first use BIC as the step criteria
fit_all_BIC = step(fit_null,scope = AdjustedSalary ~ (AGE + G + MP + PER + X3PAr + FTr + TRB. + AST. + STL. + BLK. + TOV. + USG. + OBPM + DBPM + VORP + SEASON + AdjustedPayroll + MinutesPG + TS. + AdjustedSalaryCap + POS + RookieContract)^2 + poly(OBPM, 3, raw = TRUE) + poly(AST.,3, raw = TRUE)
                         + poly(TRB.,3, raw = FALSE) + G + poly(SEASON, 3, raw = TRUE)
                         
                         + AdjustedSalaryCap + poly(TS., 3, raw = TRUE) + poly(MinutesPG, 3, raw = TRUE)
                         + poly(VORP, 4, raw = TRUE) + AdjustedPayroll + poly(USG., 3, raw = TRUE) 
                         + poly(TOV.,2, raw = TRUE) + STL. + poly(FTr, 3, raw = TRUE) + X3PAr
                         + poly(MP, 2, raw = TRUE) + poly(AGE, 4, raw = TRUE), 
                         direction = "both", k = log(n))
summary(fit_all_BIC)


#Model Diagnostic tests
#Residual Plots- Seems like linearity and equal variance do not hold
plot(resid(fit_all_BIC)~fitted(fit_all_BIC), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",
     main="Residual plot")
abline(h=0, col = "darkorange", lwd = 2)

#Small p-value equal variance does not hold
bptest(fit_all_BIC)

#QQ plots- Normality does not seem like it holds
qqnorm(resid(fit_all_BIC), main = "Normal Q-Q Plot", col = "darkgrey",pch=20)
qqline(resid(fit_all_BIC), col = "dodgerblue", lwd = 2)

#Shapiro test- p-value is very small, normality does not hold
shapiro.test(resid(fit_all_BIC))


#The Adjusted Salary (respose variable) is highly left skewed
#So, some type of transform seems necessary 
graphics.off()
hist(salData$AdjustedSalary)


#Boxcox transform on the model
boxcox(fit_all_BIC,lambda = seq(0, 0.2, by = 0.01))
lambda=0.1
lm_cox <- lm(((AdjustedSalary^(lambda)-1)/(lambda)) ~ poly(MinutesPG, 3, raw = TRUE) + 
               poly(AGE, 4, raw = TRUE) + AdjustedPayroll + PER + TS. + 
               STL. + poly(OBPM, 3, raw = TRUE) + TRB. + USG. + AdjustedPayroll:PER, 
             data = train)
summary(lm_cox)

#Model Diagnostics of ox cox model
#Residual plot- The plot looks better than the previous one. It looks like linearity holds
#but Equal Variance does not seem to hold

par(mfrow=c(1,2))
plot(resid(lm_cox)~fitted(lm_cox), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",
     main="Residual plot")
abline(h=0, col = "darkorange", lwd = 2)

#BP test for Equal Variance- Equal variance does not hold as the p value is small
bptest(lm_cox)

#QQ plots- Normality seems like it holds
qqnorm(resid(lm_cox), main = "Normal Q-Q Plot", col = "darkgrey",pch=20)
qqline(resid(lm_cox), col = "dodgerblue", lwd = 2)


#Shapiro test- p-value is still small, normality does not hold but it is better than before
shapiro.test(resid(lm_cox))


#Since BIC underfits, we have decided to do a stepwise selction using AIC also and see 
#what the results are and whether it gives a better model
fit_all_aic = step(fit_null,scope = AdjustedSalary ~ (AGE + G + MP + PER + X3PAr + FTr + TRB. + AST. + STL. + BLK. + TOV. + USG. + OBPM + DBPM + VORP + SEASON + AdjustedPayroll + MinutesPG + TS. + AdjustedSalaryCap + POS + RookieContract)^2 + poly(OBPM, 3, raw = TRUE) + poly(AST.,3, raw = TRUE)
               + poly(TRB.,3, raw = FALSE) + G + poly(SEASON, 3, raw = TRUE)
               
               + AdjustedSalaryCap + poly(TS., 3, raw = TRUE) + poly(MinutesPG, 3, raw = TRUE)
               + poly(VORP, 4, raw = TRUE) + AdjustedPayroll + poly(USG., 3, raw = TRUE) 
               + poly(TOV.,2, raw = TRUE) + STL. + poly(FTr, 3, raw = TRUE) + X3PAr
               + poly(MP, 2, raw = TRUE) + poly(AGE, 4, raw = TRUE), 
               direction = "both", k = 2)
summary(fit_all_aic)

#Model Diagnostics for the model obtained using AIC
#Model Diagnostic tests
#Residual Plots- Seems like linearity and equal variance do not hold
plot(resid(fit_all_aic)~fitted(fit_all_aic), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",
     main="Residual plot")
abline(h=0, col = "darkorange", lwd = 2)

#Small p-value equal variance does not hold
bptest(fit_all_aic)

#QQ plots- Normality does not seem like it holds
qqnorm(resid(fit_all_aic), main = "Normal Q-Q Plot", col = "darkgrey",pch=20)
qqline(resid(fit_all_aic), col = "dodgerblue", lwd = 2)

#Shapiro test- p-value is very small, normality does not hold
shapiro.test(resid(fit_all_aic))


#Transforming using boxcox
boxcox(fit_all_aic,lambda = seq(0, 0.2, by = 0.01))
lambda=0.1
lm_cox_aic <- lm(((AdjustedSalary^(lambda)-1)/(lambda)) ~  poly(MinutesPG, 3, raw = TRUE) + 
                   poly(AGE, 4, raw = TRUE) + AdjustedPayroll + PER + TS. + 
                   STL. + poly(OBPM, 3, raw = TRUE) + poly(TRB., 3, raw = FALSE) + 
                   USG. + FTr + POS + AST. + AdjustedSalaryCap + TOV. + G + 
                   AdjustedPayroll:PER + AdjustedPayroll:TS. + AdjustedPayroll:AST. + 
                   STL.:AST. + POS:AST. + TS.:FTr + PER:TS. + PER:AdjustedSalaryCap + 
                   AdjustedPayroll:AdjustedSalaryCap + TS.:AST. + AdjustedSalaryCap:TOV. + 
                   USG.:AdjustedSalaryCap + FTr:G + STL.:TOV., data = train) 
             
summary(lm_cox_aic)


#Model diagnostics after boxcox transform
#Residual plot- The plot looks better than the previous one. It looks like linearity holds
#but Equal Variance does not seem to hold
plot(resid(lm_cox_aic)~fitted(lm_cox_aic), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",
     main="Residual plot")
abline(h=0, col = "darkorange", lwd = 2)

#BP test for Equal Variance- Equal variance does not hold as the p value is small
bptest(lm_cox_aic)

#QQ plots- Normality seems like it holds
qqnorm(resid(lm_cox_aic), main = "Normal Q-Q Plot", col = "darkgrey",pch=20)
qqline(resid(lm_cox_aic), col = "dodgerblue", lwd = 2)


#Shapiro test- p-value is still small, normality does not hold but it is better than before
shapiro.test(resid(lm_cox_aic))



#LASSO

Xtrain = model.matrix(AdjustedSalary~ (AGE + G + MP + PER + X3PAr + FTr + TRB. + AST. + STL. + BLK. + TOV. + USG. + OBPM + DBPM + VORP + SEASON + AdjustedPayroll + MinutesPG + TS. + AdjustedSalaryCap + POS + RookieContract)^2 + poly(OBPM, 3, raw = TRUE) + poly(AST.,3, raw = TRUE)
                 + poly(TRB.,3, raw = FALSE) + G + poly(SEASON, 3, raw = TRUE)
                 
                 + AdjustedSalaryCap + poly(TS., 3, raw = TRUE) + poly(MinutesPG, 3, raw = TRUE)
                 + poly(VORP, 4, raw = TRUE) + AdjustedPayroll + poly(USG., 3, raw = TRUE) 
                 + poly(TOV.,2, raw = TRUE) + STL. + poly(FTr, 3, raw = TRUE) + X3PAr
                 + poly(MP, 2, raw = TRUE) + poly(AGE, 4, raw = TRUE),train)[, -1] #the first column (for intercept) is eliminated

ytrain = train$AdjustedSalary


Xtest = model.matrix(AdjustedSalary~ (AGE + G + MP + PER + X3PAr + FTr + TRB. + AST. + STL. + BLK. + TOV. + USG. + OBPM + DBPM + VORP + SEASON + AdjustedPayroll + MinutesPG + TS. + AdjustedSalaryCap + POS + RookieContract)^2 + poly(OBPM, 3, raw = TRUE) + poly(AST.,3, raw = TRUE)
                      + poly(TRB.,3, raw = FALSE) + G + poly(SEASON, 3, raw = TRUE)
                      + AdjustedSalaryCap + poly(TS., 3, raw = TRUE) + poly(MinutesPG, 3, raw = TRUE)
                      + poly(VORP, 4, raw = TRUE) + AdjustedPayroll + poly(USG., 3, raw = TRUE) 
                      + poly(TOV.,2, raw = TRUE) + STL. + poly(FTr, 3, raw = TRUE) + X3PAr
                      + poly(MP, 2, raw = TRUE) + poly(AGE, 4, raw = TRUE),test)[, -1] #the first column (for intercept) is eliminated




graphics.off()
#five way cross validation to find best lambda
fit_lasso_cv = cv.glmnet(Xtrain, ytrain, alpha = 1, nfolds = 5)
plot(fit_lasso_cv)

bestlam = fit_lasso_cv$lambda.min
bestlam
log(bestlam)

#find the training error on the best fold
mseTrain = fit_lasso_cv$cvm[fit_lasso_cv$lambda == bestlam]
mseTrain
RMSETRAINLASSO = sqrt(mseTrain)
RMSETRAINLASSO

lassoPred = predict(fit_lasso_cv,s = bestlam,newx = Xtest)
#calculate testing RMSE
sumSquaresLasso =sum( (test$AdjustedSalary -lassoPred)**2)
RMSELASSOTEST = sqrt(sumSquaresLasso / nrow(test))
RMSELASSOTEST


#CODE USED TO GENERATE RANDOM FOREST
fullModel = lm(AdjustedSalary~ ., data = standardizedData)
stepBIC = stepAIC(fullModel, direction = "both", trace =FALSE, k =log(n))
summary(stepBIC)
stepAIC = stepAIC(fullModel, direction = "both", trace = FALSE)
summary(stepAIC)
#anova test of full model says we can accept null hypothesis that remaining terms in full aren't significant
anova(stepAIC, fullModel)
anova(stepBIC, stepAIC)
#anova tells us to use stepAIC
library(faraway)
library(randomForest)

vif(stepAIC) #no issues with multicolinearity, so random forest will work well


#use all terms that were in stepAIC.
#there are fifteen predictors so mtree of 5
#code takes a while to run
#column vals: 1,2,4,6,7,10,12,13,15,16,19,20,21,22,23
randForestTrain = train[ , c(1,2,4,6,7,10,12,13,15,16,19,20,21,22,23,18)]
randForestTest = test[ , c(1,2,4,6,7,10,12,13,15,16,19,20,21,22,23,18)]
#create a random forest
#oob error is best for mtry = 10
randomForest::tuneRF(x = randForestTrain[ , 1:15], y = train$AdjustedSalary, mtryStart = 5, ntreeTry = 500, stepFactor= 2, improve = .05)
#try a random forest with mtry = 10
randForest = randomForest::randomForest(AdjustedSalary~POS + AGE + MP + X3PAr + FTr + 
                                                STL. + TOV. + USG. + DBPM + VORP + AdjustedPayroll + MinutesPG + 
                                                TS. + AdjustedSalaryCap + RookieContract, data = randForestTrain,xtest = randForestTest[ , 1:15], ytest = test$AdjustedSalary, mtry = 10, ntree =2000, importance = TRUE)
#test MSE of 18440120000000
randForest
testMSE = 18440120000000
testRMS = sqrt(testMSE)
testRMS
#train MSE of 18954780000000
trainMSE = 18954780000000
trainRMS = sqrt(trainMSE)
trainRMS



#we see not much performance increase after 500 trees
plot(randForest, main = "Out of Bag Error and Number of Trees")
#now display all RMSE's
randForest$importance

#print results
randForest
plot(randForest, main = "Hello")

#from this plot, we can see that MinutesPG was the most important feature
randomForest::varImpPlot(randForest, main = "Variable importance plots")
randForest$importance









