if(require("RevoUtilsMath")){
        setMKLthreads(4)
}

setwd(file.path(normalizePath("~"),"kaggle","homesite"))
library(caret)
library(dplyr)
library(data.table)
library(readr)

#snip <- read_csv(file = "train.csv",n_max = 1000)
#src <- read_csv(file = "train.csv", na = "-1", col_types = sapply(snip,class))
                        

src <- read.csv("train.csv")
out<-src[,"QuoteConversion_Flag"]
tst <- read.csv("test.csv")

# Unify factor levels
all<-rbind(src[,-c(1,3)],tst[,-1])
classes<-sapply(all,class)
for(i in names(classes)[classes=="integer"]) all[,i]<-as.factor(all[,i])
src1<-all[1:nrow(src),]
src1$out<-out
tst1<-all[(nrow(src)+1):nrow(all),]

inTrain <- createDataPartition(src1$out, p = 3/4, list= FALSE)
training <- src1[inTrain, ]
testing <- src1[-inTrain, ]


library(ranger)
#m.rb <- ranger(out~.,training[1:1000,200:ncol(training)], num.trees=50, write.forest=TRUE)
#pp<-predict(m.rb,dat=testing)
#confusionMatrix(round(pp$predictions),testing$out)
#80 %

#have.NA<-which(sapply(training,function(x)sum(is.na(x))>0)))
#m.rb <- ranger(out~.,training[,-have.NA], num.trees=5, write.forest=TRUE)
#pp<-predict(m.rb,dat=testing)
#confusionMatrix(round(pp$predictions),testing$out)
#89%

if(FALSE){training<-src1
have.NA<-which(sapply(training,function(x)sum(is.na(x))>0))
m.rb <- ranger(out~.,training[,-have.NA], num.trees=100, write.forest=TRUE, save.memory=TRUE, verbose=TRUE)
#dput(m.rb,"100.robj")
wr<-data.frame(QuoteNumber=tst[,1])
wr$QuoteConversion_Flag<-round(pr$predictions)
write.csv(wr,"subm.csv", row.names = FALSE, quote = c(1:2))
}

have.NA<-which(sapply(training,function(x)sum(is.na(x))>0))
have.no.levels<-which(sapply(training[,-ncol(training)],function(x)length(levels(x))<2))
mod<-train(out~.,training[,-c(have.NA, have.no.levels)],model="rf")


