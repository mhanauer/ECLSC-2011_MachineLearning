---
title: "ECLS-K-2011"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Need to library packages

```{r}
library(psych)
library(prettyR)
library(Amelia)
library(mitools)
library(MatchIt)
library(reshape2)
library(nlme)
library(dplyr)
library(MissMech)
library(caret)
```
Here we are setting the WD to google drive.  See earlier versions for getting the original data source.

The focus is on self-control, so just grab that.  Need teacher self-report self control  
Get rid of school change, just looking ITT effect so what is the effect of starting in a private school
We need all four self control, child demographics (all binary), parental demographics (all binary).
You have time points 1,2,4 it is ok that there are different intervals between them, because multilevel modeling time points can be spaced out differently.  

Poverty on page 7-49, poverty
Coding parent one employment 35 hours or more or less than 35 hours
Can create SES look on page 7-50 if you need to create it later.
Poverty is 200 percent below
Employment 35 hours or more is one and else two

Education?  
PAR race is white and non-white

Only one research question whether public school (as you define it) versus private school as you define it.  Future researchers will need to gain access to whether a student was in a charter school and start to analyze differences between those options and public and versuss private.
S2REGSKL ECLS1998-1999

If there are only two measurements, just including the baseline, because when we trasform to long version you need to transform all time points the same otherwise it repeats, which is fine, we are just treating those as time invariant variables.

```{r}
setwd("~/Box Sync/PropScore")
#data = read.csv("ELCS-K-2011.csv", header = TRUE)


data1 = cbind(X1TCHCON = data$X1TCHCON, X2TCHCON = data$X2TCHCON, X4TCHCON = data$X4TCHCON,X1RTHET= data$X1RTHET, X2RTHET = data$X2RTHET, X4RTHET = data$X4RTHET, X1MTHET = data$X1MTHET, X2MTHET = data$X2MTHET, X4MTHET = data$X4MTHET, X1BMI = data$X1BMI, X2BMI = data$X2BMI, X4BMI = data$X4BMI,X1HTOTAL = data$X1HTOTAL, X2HTOTAL = data$X2HTOTAL, X4HTOTAL = data$X4HTOTAL, X1PAR1AGE = data$X1PAR1AGE, X2PAR1AGE = data$X2PAR1AGE, X4PAR1AGE = data$X4PAR1AGE, X1PAR1EMP = data$X1PAR1EMP, X2POVTY = data$X2POVTY, X12LANGST = data$X12LANGST,X_CHSEX_R = data$X_CHSEX_R, X1PUBPRI = data$X1PUBPRI, X1PAR1RAC = data$X1PAR1RAC, X1_RACETHP_R = data$X_RACETHP_R)
# Change the -9 to NAs
data1 = apply(data1, 2, function(x){ifelse(x == -9, NA, x)})
#summary(data1)
data1 = as.data.frame(data1)
```
Make the demographics binary first then replace them.  Easier for imputation and not interested in the effects of demographics, just using them as controls.  

Put the demographics back into the data1 data set with them transformed.

Remember that R does not transform NA into zeros, so the code below works.
Remember to reverse code, because public schools is 1 and your hypothesis is that there private schools should have a make difference.

Primary lanuage if english is two.
```{r}
datCat = data1[c(19:25)]
head(datCat)
data1[c(19:25)] = NULL
datCatLang = datCat$X12LANGST
datCat$X12LANGST = NULL
X12LANGST = ifelse(datCatLang == 2, 1, 0)
apply(datCat, 2, function(x){describe.factor(x)})
datCat = data.frame(apply(datCat, 2, function(x){(ifelse(x > 1, 0, 1))}))
head(datCat)
datCat = cbind(datCat, X12LANGST)
## Need to recode the public private indicator
data1 = data.frame(data1, datCat)
data1$X1PUBPRI = ifelse(data1$X1PUBPRI == 1,0,1) 
summary(data1)
```
Descriptives:
Descirptives with no missing values.
Getting descriptives here.  Not a great solution, but we get with and without missing data.  Then change to long later and then impute again and use that data set to do the data analysis. Maybe run more imputations to make sure you get similar results.    

Getting the baseline data only 
```{r}
dataDesc = data1
dim(dataDesc)
dataDesc = na.omit(dataDesc)
dim(dataDesc)
datCon = dataDesc[c(1:18)]
head(datCon)
mean_sd_fun = function(x){
  mean_mean = mean(x)
  sd_sd = sd(x)
  mean_sd = cbind(mean_mean, sd_sd)
}
round(apply(datCon, 2, mean_sd_fun),3)
datCat = dataDesc[c(19:25)]
head(datCat)
apply(datCat, 2, function(x){describe.factor(x)})
```
Need to transform into long form so we don't lose too much data when we delete
```{r}
dat_long = reshape(data1, varying = list(c("X1TCHCON", "X2TCHCON", "X4TCHCON"), c("X1RTHET", "X2RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X4BMI"), c("X1HTOTAL", "X2HTOTAL", "X4HTOTAL"), c("X1PAR1AGE", "X2PAR1AGE", "X4PAR1AGE")), times = c(0,1,2), direction = "long")
```
Do missing at random test
```{r}

R.Version()
#TestMCARNormality(dat_long)
```

Review whole data set
```{r}
describe(dat_long)
```
Get full data set
```{r}
dim(dat_long)
dat_long_complete = na.omit(dat_long)
dim(dat_long_complete)
dat_long_complete
```
Just run a regular regression
```{r}
reg_lm = lm(dat_long_complete$X1TCHCON ~ dat_long_complete$X1PAR1EMP + dat_long_complete$X2POVTY + dat_long_complete$X_CHSEX_R + dat_long_complete$X1PUBPRI + dat_long_complete$X1PAR1RAC + dat_long_complete$X1_RACETHP_R + dat_long_complete$X12LANGST + dat_long_complete$time + dat_long_complete$X1RTHET + dat_long_complete$X1MTHET + dat_long_complete$X1BMI + dat_long_complete$X1HTOTAL + dat_long_complete$X1PAR1AGE, data = dat_long_complete)
summary(reg_lm)
```


Get training set
```{r}
inTrain = createDataPartition(y = dat_long_complete$id, p = .75, list = FALSE)
training = dat_long_complete[inTrain,]
testing = dat_long_complete[-inTrain,] 
```
Now generate the cross validation where you split the training set up ten times and then repeat that process ten times 
```{r}
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)
```
Now try running this model 
```{r}
set.seed(12345)

lmFit1 <- train(X1TCHCON ~ ., data = training, 
                 method = "lm", 
                 trControl = fitControl,
                 verbose = FALSE)
```
See what happened
```{r}
gbmFit1
lmFit1

predict(gbmFit1, newdata = head(testing), type = "prob")
```





