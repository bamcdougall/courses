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

## ----loadPackage,cache=TRUE,fig.height=3.5,fig.width=3.5-----------------
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                              p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)

## ----dependson="loadPackage",cache=TRUE,fig.height=3.5,fig.width=3.5-----
names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

## ----dependson="loadPackage",cache=TRUE,fig.height=3.5,fig.width=3.5-----
X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

## ----prcomp,dependson="loadPackage",cache=TRUE,fig.height=3.5,fig.width=3.5----
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

## ----dependson="prcomp",cache=TRUE,fig.height=3.5,fig.width=3.5----------
prComp$rotation

## ----spamPC,dependson="loadPackage",cache=TRUE,fig.height=3.5,fig.width=3.5----
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

## ----dependson="spamPC",cache=TRUE,fig.height=3.5,fig.width=3.5----------
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

## ----pcaCaret,dependson="spamPC",cache=TRUE,fig.height=3.5,fig.width=3.5----
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)

## ----dependson="pcaCaret",cache=TRUE,fig.height=3.5,fig.width=3.5--------
testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

## ----dependson="pcaCaret",cache=TRUE,fig.height=3.5,fig.width=3.5--------
modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))

