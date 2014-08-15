# // NEED DATA Analysis
library(randomForest)
library(miscTools)
library(ggplot2)
library(caret)
library(pROC)
library(e1071)
library(reshape2)
library(plyr)


File <- read.csv("ReadyDistance.csv")
HBEN <- read.csv("HBenefit/HB_Total.csv")
AL <- read.csv('Area_LKUP_SIMP.csv')
HB2 <- read.csv('HBenefit/Lond_OA_HBen_R.csv')


v <- c('X')
AL <- AL[,!(names(AL) %in% v)]




ordered <- c("IMD_ENG","FP_ENG","PROP_AGE","FLOORAREABAND","ENERGY_EFFICIENCY_BAND",
	"ROOFINSULATIONTHICKNESS","Number.of.Adults.in.Household..H","Length.of.Residency..H.","Number.of.Bedrooms..H.", "LOFT_DEPTH")

fact <- c("PROP_TYPE", "MAIN_HEAT_FUEL", "MAINFUEL_SURR_KEY", "CONSTRUCTION_CODE", "HEATEMITTERTYPE")

File[,ordered] <- lapply(File[,ordered], ordered)
File[,fact] <- lapply(File[,fact], factor)

Del <- c("X","CWI","LI","E7Flag2012","SolidWI","DraughtProofing","Glazing")
Del2 <- colnames(File) %in% Del
File2 <- File[!Del2]

FileX <- merge(x=File2,y=HBEN,by.x="OA_2011", by.y="OA")

FileX2 <- merge(FileX,AL, by="OA_2011")

FileX3 <- merge(x=FileX2, y=HB2, by.x="OA_2011", by.y="OA2011")

# QuantsHB <- quantile(FileX2$HBClaimants, seq(0,1, by=0.1))
# QuantsGC <- quantile(FileX2$GCons2012, seq(0,1, by=0.1))

QuantsHB <- quantile(FileX3$HBPROPN, seq(0,1, by=0.1))
QuantsGC <- quantile(FileX3$GCons2012, seq(0,1, by=0.1))  

# The above is general quartiles without taking prop type usage into account
# we should really split by prop type and calculate individual usage quartiles

Det  <- FileX3[(FileX3["PROP_TYPE"] == '101'), 'GCons2012']
Semi <- FileX3[(FileX3["PROP_TYPE"] == '102'), 'GCons2012'] 
EndT <- FileX3[(FileX3["PROP_TYPE"] == '103'), 'GCons2012']
MidT <- FileX3[(FileX3["PROP_TYPE"] == '104'), 'GCons2012']
Bung <- FileX3[(FileX3["PROP_TYPE"] == '105'), 'GCons2012']
Flat <- FileX3[(FileX3["PROP_TYPE"] == '106'), 'GCons2012']

DQ <- quantile(Det, seq(0,1, by=0.1))[9]
SQ <- quantile(Semi, seq(0,1, by=0.1))[9]
ETQ <- quantile(EndT, seq(0,1, by=0.1))[9]
MTQ <- quantile(MidT, seq(0,1, by=0.1))[9]
BQ <- quantile(Bung, seq(0,1, by=0.1))[9]
FQ <- quantile(Flat, seq(0,1, by=0.1))[9]

# 9 corresponds to top 20% of consumption

LU <- c(DQ,SQ,ETQ,MTQ,BQ,FQ)
names(LU) <- c("101", "102", "103", "104", "105", "106")

FF <- function(x) {
		if (x["GCons2012"] > LU[x["PROP_TYPE"]])
			{x['Consump'] = 1}
		else
			{x['Consump'] = 0}
		}	

FF3 <- apply(FileX3, 1, FF)

FileX3['HighConsump'] = as.data.frame(FF3)

# GC8 <- QuantsGC[8]
HB8 <- QuantsHB[9]


# Top20 <- FileX3[(FileX3$HBPROPN >= HB8) & (FileX3$GCons2012 >= GC8),]


Top20 <- FileX3[(FileX3$HBPROPN >= HB8) & (FileX3$HighConsump == 1),]

Top20CI <- Top20

Top20$WEIGHT <- ((Top20$HBPROPN/HB8)^2*(Top20$GCons2012/GC8))

WTVal <- arrange(Top20, desc(WEIGHT))

A <- (WTVal[(WTVal$Count>10),])  # filter on number of households considered per OA (Count column)

# WTVal.Q <- quantile(WTVal$WEIGHT, seq(0,1, by=0.05))

AB <- A[, c("OA_2011", "WEIGHT")]

Final <- merge(FileX3,AB, by="OA_2011", all=TRUE)

FinalCheck <- Final[complete.cases(Final),]

Final[is.na(Final)] <- 0  # replace NAs with 0

FFinal <- Final[,c("OA_2011","HBClaimants","MSOA_2011","MSOA11NM","LSOA_2011","LSOA11NM","HBPROPN","HHOLDS","HighConsump","WEIGHT")]



# ///////////////// Medium Super Output stats ////////////////////////////////

# Ord <- arrange(count(Top20['MSOA2011']), desc(freq))

# OrdLSOA <- arrange(count(Top20['LSOA11NM'])), desc(freq))


# QMSOA <- quantile(Ord$freq, seq(0,1, by=0.05))

Ord[Ord['freq']>QMSOA[19],]   # top 10% Middle Super output areas


