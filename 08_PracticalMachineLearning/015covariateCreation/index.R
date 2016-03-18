## ----setup, cache = F, echo = F, message = F, warning = F, tidy = F------
# make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache=TRUE, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
  if(is.numeric(x)) {
    round(x, getOption('digits'))
  } else {
    paste(as.character(x), collapse = ', ')
  }
})
knit_hooks$set(plot = knitr:::hook_plot_html)

## ----spamData,fig.height=4,fig.width=4-----------------------------------
library(kernlab);data(spam)
spam$capitalAveSq <- spam$capitalAve^2

## ----loadData,cache=TRUE-------------------------------------------------
library(ISLR); library(caret); data(Wage);
inTrain <- createDataPartition(y=Wage$wage,
                              p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

## ----dummyVar,dependson="loadData"---------------------------------------
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dummies,newdata=training))

## ----dependson="dummyVar"------------------------------------------------
nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv

## ----splines,dependson="dummyVar",cache=TRUE-----------------------------
library(splines)
bsBasis <- bs(training$age,df=3) 
bsBasis

## ----dependson="splines",fig.height=4,fig.width=4------------------------
lm1 <- lm(wage ~ bsBasis,data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)

## ----dependson="splines",fig.height=4,fig.width=4------------------------
predict(bsBasis,age=testing$age)

