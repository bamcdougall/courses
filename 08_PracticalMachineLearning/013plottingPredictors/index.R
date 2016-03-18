## ----setup, cache = F, echo = F, message = F, warning = F, tidy = F------
# make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
  if(is.numeric(x)) {
    round(x, getOption('digits'))
  } else {
    paste(as.character(x), collapse = ', ')
  }
})
knit_hooks$set(plot = knitr:::hook_plot_html)

## ----loadData,cache=TRUE-------------------------------------------------
library(ISLR); library(ggplot2); library(caret); library(gridExtra);
data(Wage)
summary(Wage)

## ----trainingTest,dependson="loadData",cache=TRUE------------------------
inTrain <- createDataPartition(y=Wage$wage,
                              p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

## ----dependson="trainingTest",fig.height=4,fig.width=4-------------------
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

## ----dependson="trainingTest",fig.height=4,fig.width=6-------------------
qplot(age,wage,data=training)

## ----dependson="trainingTest",fig.height=4,fig.width=6-------------------
qplot(age,wage,colour=jobclass,data=training)

## ----dependson="trainingTest",fig.height=4,fig.width=6-------------------
qq <- qplot(age,wage,colour=education,data=training)
qq +  geom_smooth(method='lm',formula=y~x)

## ----cut2,dependson="trainingTest",fig.height=4,fig.width=6,cache=TRUE----
cutWage <- cut2(training$wage,g=3)
table(cutWage)

## ----dependson="cut2plot",fig.height=4,fig.width=6,cache=TRUE------------
p1 <- qplot(cutWage,age, data=training,fill=cutWage,
      geom=c("boxplot"))
p1

## ----dependson="cut2plot",fig.height=4,fig.width=9-----------------------
p2 <- qplot(cutWage,age, data=training,fill=cutWage,
      geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)

## ----dependson="cut2",fig.height=4,fig.width=9---------------------------
t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1)

## ----dependson="trainingTest",fig.height=4,fig.width=6-------------------
qplot(wage,colour=education,data=training,geom="density")

