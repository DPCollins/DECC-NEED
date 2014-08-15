# // NEED DATA Analysis
library(randomForest)
library(miscTools)
library(ggplot2)
library(caret)
library(pROC)
library(e1071)
library(plyr)

Lon <- read.csv('LOND_forR.csv')

exclude <- names(Lon) %in% c("Count", "X")  # we want to delete Count columns
Lon2 <- Lon[!exclude]

names(Lon2)


ordered <- c("IMD_ENG","FP_ENG","PROP_AGE","FLOORAREABAND","ENERGY_EFFICIENCY_BAND",
"ROOFINSULATIONTHICKNESS","Number.of.Adults.in.Household..H","Length.of.Residency..H.","Number.of.Bedrooms..H.", "LOFT_DEPTH")

fact <- c("CWI", "LI", "PROP_TYPE", "MAIN_HEAT_FUEL", "E7Flag2012", "MAINFUEL_SURR_KEY", "CONSTRUCTION_CODE", "HEATEMITTERTYPE",
"Glazing", "DraughtProofing","SolidWI")

Lon2[,ordered] <- lapply(Lon2[,ordered], ordered)
Lon2[,fact] <- lapply(Lon2[,fact], factor)


y <- as.data.frame(table(Lon2$OA_2011))

ap <- ggplot(y, aes(x=Freq))+geom_histogram(binwidth=1, fill="royalblue3", alpha=.8)
print(ap)

str(Lon2)

#**************************  Random Forest Classification *******************************

print("Random Forest With Downsampling")

# bound <- floor((nrow(Lon2)/4)*1)         #define % of training and test set

bound <- floor((nrow(Lon2)/100)*1)         #define % of training and test set

# cols <- c("GCons2012", "ECons2012", "LI", "CWI", "IMD_ENG")




# cols <- c("GCons2012", "ECons2012", "CWI", "IMD_ENG", "FP_ENG", "MAIN_HEAT_FUEL", "PROP_AGE", "PROP_TYPE", "TOTAL_FLOOR_AREA", "CONSTRUCTION_CODE")

# Lon2 <- Lon2[,cols]
# Lon2$CWI <- as.factor(Lon2$CWI)

Lon2$CWI <- factor(ifelse(Lon2$CWI==0, "Class0", "Class1"))

#Lon2$IMD_ENG <- as.ordered(Lon2$IMD_ENG)


Lon3 <- Lon2[sample(nrow(Lon2)), ]           #sample rows 

gg  <- names(Lon3) %in% c("OA_2011", "GCons2012", "ECons2012")



Lon2.train <- Lon3[1:bound, ]              #get training set
Lon2.test <- Lon3[(bound+1):nrow(Lon3), ] 


OA <- Lon2.test$OA_2011  #OAs for Test set

Lon2.test <- Lon2.test[!gg]
Lon2.train <- Lon2.train[!gg]



table(Lon2.train$CWI)
nmin <- sum(Lon2.train$CWI == "Class1")  # minority class



# RANDOM FOREST #############################################

ctrl <- trainControl(method = "cv",
                      classProbs = TRUE,
                      allowParallel = TRUE,
                      summaryFunction = twoClassSummary)
                      
# set.seed(2)

rfDownsampled <- train(CWI ~ ., data = Lon2.train,
                        method = "rf",
                        ntree = 500,
                        tuneLength = 5,
                        metric = "ROC",
                        trControl = ctrl,
                        ## Tell randomForest to sample by strata. Here, 
                        ## that means within each class
                        strata = Lon2.train$CWI,
                        ## Now specify that the number of samples selected
                        ## within each class should be the same
                        sampsize = rep(nmin, 2))                      
                      
                     

rfUnbalanced <- train(CWI ~ ., data = Lon2.train,
                       method = "rf",
                       ntree = 500,
                       tuneLength = 5,
                       metric = "ROC",
                       trControl = ctrl)                      

xtab <- table(Lon2.test$CWI,predict(rfDownsampled, Lon2.test))

confusionMatrix(xtab)

xtabUN <- table(Lon2.test$CWI,predict(rfUnbalanced, Lon2.test))

confusionMatrix(xtabUN)


# Downsampled data

downProbs <- predict(rfDownsampled, Lon2.test, type = "prob")[,1]

downsampledROC <- roc(response = Lon2.test$CWI, 
                       predictor = downProbs,
                       levels = rev(levels(Lon2.test$CWI)))                      
                      
# Unbalanced data

unbalProbs <- predict(rfUnbalanced, Lon2.test, type = "prob")[,1]

unbalROC <- roc(response = Lon2.test$CWI, 
                 predictor = unbalProbs,
                 levels = rev(levels(Lon2.test$CWI)))




#And finally, we can plot the curves and determine the area under each curve:

plot(downsampledROC, col = rgb(1, 0, 0, .5), lwd = 2)                      

plot(unbalROC, col = rgb(0, 0, 1, .5), lwd = 2, add = TRUE)

legend(.4, .4,
        c("Down-Sampled", "Normal"),
        lwd = rep(2, 1), 
        col = c(rgb(1, 0, 0, .5), rgb(0, 0, 1, .5)))

# Variable importance plots

DSvarImp <- varImp(rfDownsampled)
UBvarImp <- varImp(rfUnbalanced)

plot(DSvarImp, top = 20)



#########################################################################################


Pred <- predict(rfDownsampled, Lon2.test)

PredOA <- cbind(as.data.frame(OA),Pred)

Miss <- PredOA$Pred !=Lon2.test$CWI
MissV <- downProbs[Miss]

Pred2 <- cbind(PredOA[Miss,],downProbs[Miss])
colnames(Pred2)[3] = 'Prob'

dim(Pred2) # total number of misclassified instances

Filt <- Pred2[Pred2$Prob<0.25,]  

Len <- dim(Filt)[1] # number of cases incorrectly labelled 1 with prob > 75% - 0 is positive class.

summary(Filt$OA)


AL <- read.csv('Area_LKUP_SIMP.csv')

U <- as.data.frame(Filt$OA)
U2 <- ddply(U,.(Filt$OA),nrow)
OA_sum <- arrange(U2,desc(V1))

colnames(OA_sum)[1] = 'OA_2011'

# want total number of properties used in Test to calculate % misclassification

head(Pred2) # test output before filter

TotOA <- as.data.frame(count(Pred2$OA))
OA_mrg <- merge(OA_sum,TotOA, by.x = "OA_2011", by.y = "x") 

OA_mrg['prop'] <- 100*(OA_mrg$V1)/OA_mrg$freq

specify_dec <- function(x, k) format(round(x,k), nsmall=k)

OA_mrg['prop'] <- lapply(OA_mrg['prop'], function(x) specify_dec(x,1))

OA_mrg$prop <- as.numeric(OA_mrg$prop)

OA_misclass <- OA_mrg[(OA_mrg$freq > 10) & (OA_mrg$prop > 33.3),]

OA_misclass <- arrange(OA_misclass, desc(prop))





Merge <- merge(OA_sum,AL, by='OA_2011')   # merge with MSOA and LSOA lookup file
M2 <- arrange(Merge, desc(V1))
M3 <- M2[,c(1,2,4,5)]

# Aggregate by MSOA/LSOA and split into deciles


HH  <- ddply(M3, .(MSOA_2011), summarise, sum=sum(V1))
HH2 <- arrange(HH, desc(sum))

Quants <- quantile(HH2$sum, seq(0,1, by=0.1))

Top20 <- HH2[HH2$sum>=Quants[9],]  # top 20%

















                      
