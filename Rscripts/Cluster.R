# // NEED DATA Analysis
library(randomForest)
library(miscTools)
library(ggplot2)
library(caret)
library(pROC)
library(e1071)
library(reshape2)
library(plyr)

DArrange <- function() {

	Dat <- read.csv('DataForCluster.csv')

	drops <- c("X")

	Dat2 <- Dat[,!(names(Dat) %in% drops)]

	ordered <- c("IMD_ENG","FP_ENG","PROP_AGE","FLOORAREABAND","ENERGY_EFFICIENCY_BAND",
	"ROOFINSULATIONTHICKNESS","Number.of.Adults.in.Household..H","Length.of.Residency..H.","Number.of.Bedrooms..H.", "LOFT_DEPTH")

	fact <- c("CWI", "LI", "PROP_TYPE", "MAIN_HEAT_FUEL", "E7Flag2012", "MAINFUEL_SURR_KEY", "CONSTRUCTION_CODE", "HEATEMITTERTYPE",
	"Glazing", "DraughtProofing","SolidWI")

	Dat2[,ordered] <- lapply(Dat2[,ordered], ordered)
	Dat2[,fact] <- lapply(Dat2[,fact], factor)

	Mode2 <- function(x) { tab <- table(x); names(tab)[which.max(tab)] }


	Dat2$CWI <- factor(ifelse(Dat2$CWI=="Class0", 0, 1))
	Dat2$E7Flag2012 <- factor(ifelse(Dat2$E7Flag2012==1, 1, 0))
	WE<-!(colnames(Dat2) %in% c("OA_2011", "Count"))

	z <- c(ordered, fact)

	d <- colnames(Dat2)
	d <- d[WE]

	bin <- c("E7Flag2012","LI","CWI","SolidWI","DraughtProofing","Glazing") # binary values 
	bin2 <- z %in% bin

	FCT <- z[!bin2]  # factors and ordered - multilevel


	Dat2['Count'] <- 1
	Agg_OA <- aggregate(Dat2['Count'], Dat2['OA_2011'], sum)


	Dat3 <- Dat2
	j = 1
	for (i in d) {
			cat(i, "(",j,"of",length(d),")","\n")
			FC <- Dat2[i]
			if (i %in% FCT)
				{print("Finding Mode of Multilevel Data")
				C1 <- aggregate(FC, Dat2['OA_2011'], Mode2)}
		
			else if (i %in% bin)
				{print("Finding Sum of Binary Data")
				C1 <- aggregate(as.numeric(as.character(FC[,1])), Dat2['OA_2011'], sum)
				colnames(C1)[2] <- i
				}
		
		
			else
				{print("Finding Median of Continuous Data")
				C1 <- aggregate(FC, Dat2['OA_2011'], median)}
		
		
			if ( i == d[1])
				{FF <- as.data.frame(C1)}
			else
				{FF[i] <- C1[i]}
		
			j = j+1
			}

	c = lapply(FF, class)		
	for (t in 1:length(colnames(FF))) {
		if ("character" %in% c[t])
			{FF[,colnames(FF)[t]] <- as.ordered(FF[,colnames(FF)[t]])
			}
			}		
		
	FF["Count"] <- Agg_OA$Count	
	FF["CWIpc"] <- (100*FF["CWI"])/FF["Count"]
	FF["LIpc"] <- (100*FF["LI"])/FF["Count"]
	FF["E7pc"] <- (100*FF["E7Flag2012"])/FF["Count"]
	FF["SolidWIpc"] <- (100*FF["SolidWI"])/FF["Count"]
	FF["DraughtProofingpc"] <- (100*FF["DraughtProofing"])/FF["Count"]
	FF["Glazingpc"] <- (100*FF["Glazing"])/FF["Count"]

	ggplot(FF, aes(x=CWIpc)) + geom_histogram(binwidth=10, colour="black", fill="white")

	Dat3[Dat3["OA_2011"] == "E00000085",]   # test aggregate values by OA (by eye)

	write.csv(FF, file = "ReadyDistanceB.csv")

	return(FF)
	}

# FF <- DArrange()


File <- read.csv("ReadyDistance.csv")

ordered <- c("IMD_ENG","FP_ENG","PROP_AGE","FLOORAREABAND","ENERGY_EFFICIENCY_BAND",
	"ROOFINSULATIONTHICKNESS","Number.of.Adults.in.Household..H","Length.of.Residency..H.","Number.of.Bedrooms..H.", "LOFT_DEPTH")

fact <- c("PROP_TYPE", "MAIN_HEAT_FUEL", "MAINFUEL_SURR_KEY", "CONSTRUCTION_CODE", "HEATEMITTERTYPE")

File[,ordered] <- lapply(File[,ordered], ordered)
File[,fact] <- lapply(File[,fact], factor)

Del <- c("X","CWI","LI","E7Flag2012","SolidWI","DraughtProofing","Glazing")
Del2 <- colnames(File) %in% Del
File2 <- File[!Del2]


