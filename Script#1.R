#COGNI #1 - Read RDS from Xiboca
#if(TRUE){af_df = readRDS("af_df_bergbutME_all.rds")
#berg
#neff
#head(neff)
#head(berg)
install.packages("ggplot2")
library(ggplot2)
install.packages("installr")
library(installr)
library(data.table)
updateR()
#library(openxlsx)
#write.xlsx(neff, "neff_xlsx.xlsx")
#write.xlsx(berg, "af_df_xlsx.xlsx")


install.packages("dplyr")
library(dplyr)

colnames(af_df)
colnames(neff)
testNamesNeff <- c("CHROM", "POS", "ID", "REF")


for(i in 1:length(colnames(neff))){
  if(!identical(neff[colnames(neff)[i]], af_df[colnames(neff)[i]])){
    print(colnames(neff)[i])
    #print(identical(neff[i], af_df[i]))
  }
  else{
    print(paste("Iguais: ", colnames(neff)[i]))
  }
}

pop$Population

#-----Load Data----
setwd("C:/Users/henri/Desktop/Data_Cogni")
neff = readRDS("neff_bergbutME_all.rds")
af_df = readRDS("af_df_bergbutME_all.rds")
controlGen = read.table("control_genes.txt", sep = "\t", header = TRUE)
immuneGen = read.table("immune_genes.txt", sep = "\t", header = TRUE)
matchClark = read.table("match_clark.txt", sep = "\t", header = TRUE)
pop = read.csv("Populations.CSV", sep = ";", header = TRUE)

View(immuneGen)

#---------------

#----Order data by Chrom Number and Position in DNA----
neff <- neff[order(neff$CHROM, neff$POS),]
af_df <- af_df[order(af_df$CHROM, af_df$POS), ]

immuneGen <- immuneGen[order(immuneGen$Chromosome, immuneGen$Start),]
controlGen <- controlGen[order(controlGen$Chromosome, controlGen$Start),]
#---------------

#----------Count and separete by Chromossome Groups------
#Immune
chromNimmune <- c()
#Control
chromcontrol = unique(controlGen$Chromosome)
chromNcontrol <- c()
#Neff
chromNeff = unique(neff$CHROM)
chromNDataNeff <- c()
#AfDf
chromAfDf = unique(af_df$CHROM)
chromNDataAfDf<- c()


#findChromLines <- function(orTable, oT){
  j=1
  k=1
  for (i in chrom) {
    x = TRUE
    while(x == TRUE){
      if(i == orTable[j, oT]){
        chromNimmune[k] = j;
      }
      else{
        x=FALSE
      }
      j = j+1;
    }
    k=k+1;
  }
  print(chromNimmune)
#}


#findChromLine(orTable = immuneGen, oT = 4)

j=1
k=1
for (i in chromcontrol) {
  x = TRUE
  while(x == TRUE && j<=length(controlGen$Chromosome)){
    if(i == controlGen$Chromosome[j]){
      chromNcontrol[k] = j;
    }
    else{
      x=FALSE
    }
    j = j+1;
  }
  k=k+1;
}
print(chromNimmune)
print(chromNDataNeff)
print(chromNDataAfDf)
print(chromNcontrol)


#Create separated immune table
immuneGen2L <- immuneGen[1:chromNimmune[1],]
immuneGen2R <- immuneGen[(chromNimmune[1]+1):chromNimmune[2],]
immuneGen3L <- immuneGen[(chromNimmune[2]+1):chromNimmune[3],]
immuneGen3R <- immuneGen[(chromNimmune[3]+1):chromNimmune[4],]

#Organize immune
immuneGen2L<- immuneGen2L[order(immuneGen2L$Start),]
immuneGen2R<- immuneGen2R[order(immuneGen2R$Start),]
immuneGen3L<- immuneGen3L[order(immuneGen3L$Start),]
immuneGen3R<- immuneGen3R[order(immuneGen3R$Start),]

#Create separated control table
controlGen2L <- controlGen[1:chromNcontrol[1],]
controlGen2R <- controlGen[(chromNcontrol[1]+1):chromNcontrol[2],]
controlGen3L <- controlGen[(chromNcontrol[2]+1):chromNcontrol[3],]
controlGen3R <- controlGen[(chromNcontrol[3]+1):chromNcontrol[4],]

#Organize control
controlGen2L<- controlGen2L[order(controlGen2L$Start),]
controlGen2R<- controlGen2R[order(controlGen2R$Start),]
controlGen3L<- controlGen3L[order(controlGen3L$Start),]
controlGen3R<- controlGen3R[order(controlGen3R$Start),]

#Create separated data
neff2L <- neff[1:chromNDataNeff[1],]
neff2R <- neff[(chromNDataNeff[1]+1):chromNDataNeff[2],]
neff3L <- neff[(chromNDataNeff[2]+1):chromNDataNeff[3],]
neff3R <- neff[(chromNDataNeff[3]+1):chromNDataNeff[4],]

af_df2L <- af_df[1:chromNDataAfDf[1],]
af_df2R <- af_df[(chromNDataAfDf[1]+1):chromNDataAfDf[2],]
af_df3L <- af_df[(chromNDataAfDf[2]+1):chromNDataAfDf[3],]
af_df3R <- af_df[(chromNDataAfDf[3]+1):chromNDataAfDf[4],]

#Organize Data
neff2L<- neff2L[order(neff2L$POS),]
neff2R<- neff2R[order(neff2R$POS),]
neff3L<- neff3L[order(neff3L$POS),]
neff3R<- neff3R[order(neff3R$POS),]

af_df2L<- af_df2L[order(af_df2L$POS),]
af_df2R<- af_df2R[order(af_df2R$POS),]
af_df3L<- af_df3L[order(af_df3L$POS),]
af_df3R<- af_df3R[order(af_df3R$POS),]
#---------------

#-----------------Create dataset only with Immune/Control Gene Data----------
#---Immune---
immuneDataNeff <- c()
immuneDataAf_Df <- c()

#Chromossome 2L
j=1
k=1
for (i in immuneGen2L$Start ) {
  x=TRUE
  while(x == TRUE){
    if(af_df2L$POS[j]<i)
      j = j+1
    else {
      if (af_df2L$POS[j]>=i & af_df2L$POS[j]<=immuneGen2L$End[k]){
        immuneDataAf_Df <- rbind(immuneDataAf_Df, af_df2L[j,])
        j = j+1;
      }
      else
        x=FALSE
    }
  }
  k = k+1;
}

#Chromossome 2R
j=1
k=1
for (i in immuneGen2R$Start ) {
  x=TRUE
  while(x == TRUE){
    if(af_df2R$POS[j]<i)
      j = j+1
    else {
      if (af_df2R$POS[j]>=i & af_df2R$POS[j]<=immuneGen2R$End[k]){
        immuneDataAf_Df <- rbind(immuneDataAf_Df, af_df2R[j,])
        j = j+1;
      }
      else
        x=FALSE
    }
  }
  k = k+1;
}

#Chromossome 3l
j=1
k=1
for (i in immuneGen3L$Start ) {
  x=TRUE
  while(x == TRUE){
    if(af_df3L$POS[j]<i)
      j = j+1
    else {
      if (af_df3L$POS[j]>=i & af_df3L$POS[j]<=immuneGen3L$End[k]){
        immuneDataAf_Df <- rbind(immuneDataAf_Df, af_df3L[j,])
        j = j+1;
      }
      else
        x=FALSE
    }
  }
  k = k+1;
}

#Chromossome 3R
j=1
k=1
for (i in immuneGen3R$Start ) {
  x=TRUE
  while(x == TRUE){
    if(af_df3R$POS[j]<i)
      j = j+1
    else {
      if (af_df3R$POS[j]>=i & af_df3R$POS[j]<=immuneGen3R$End[k]){
        immuneDataAf_Df <- rbind(immuneDataAf_Df, af_df3R[j,])
        j = j+1;
      }
      else
        x=FALSE
    }
  }
  k = k+1;
}

#---Control---
controlDataNeff <- c()
controlDataAf_Df <- c()

#Chromossome 2L
j=1
k=1
for (i in controlGen2L$Start ) {
  x=TRUE
  while(x == TRUE){
    if(af_df2L$POS[j]<i)
      j = j+1
    else {
      if (af_df2L$POS[j]>=i & af_df2L$POS[j]<=controlGen2L$End[k]){
        controlDataAf_Df <- rbind(controlDataAf_Df, af_df2L[j,])
        j = j+1;
      }
      else
        x=FALSE
    }
  }
  k = k+1;
}

#Chromossome 2R
j=1
k=1
for (i in controlGen2R$Start ) {
  x=TRUE
  while(x == TRUE){
    if(af_df2R$POS[j]<i)
      j = j+1
    else {
      if (af_df2R$POS[j]>=i & af_df2R$POS[j]<=controlGen2R$End[k]){
        controlDataAf_Df <- rbind(controlDataAf_Df, af_df2R[j,])
        j = j+1;
      }
      else
        x=FALSE
    }
  }
  k = k+1;
}

#Chromossome 3l
j=1
k=1
for (i in controlGen3L$Start ) {
  x=TRUE
  while(x == TRUE){
    if(af_df3L$POS[j]<i)
      j = j+1
    else {
      if (af_df3L$POS[j]>=i & af_df3L$POS[j]<=controlGen3L$End[k]){
        controlDataAf_Df <- rbind(controlDataAf_Df, af_df3L[j,])
        j = j+1;
      }
      else
        x=FALSE
    }
  }
  k = k+1;
}

#Chromossome 3R
j=1
k=1
for (i in controlGen3R$Start ) {
  x=TRUE
  while(x == TRUE){
    if(af_df3R$POS[j]<i)
      j = j+1
    else {
      if (af_df3R$POS[j]>=i & af_df3R$POS[j]<=controlGen3R$End[k]){
        controlDataAf_Df <- rbind(controlDataAf_Df, af_df3R[j,])
        j = j+1;
      }
      else
        x=FALSE
    }
  }
  k = k+1;
}
#---------------

#-----------------Crossing Data-----------

#Creating subset with populations - Immune
#--PseudoCode--
#--While popin Clinal_set is not null, do:
#--If popIndex == 1
popIndex = 1
immuneDataPop_AfDF <- c()
while(!is.na(pop$Clinal_set[popIndex])){
  if(popIndex==1){
    immuneDataPop_AfDF <- rbind(immuneDataPop_AfDF, select(immuneDataAf_Df, as.character(pop$Population[popIndex])))
    popIndex = popIndex+1
  }
  else{
    immuneDataPop_AfDF <- cbind(immuneDataPop_AfDF, select(immuneDataAf_Df, as.character(pop$Population[popIndex])))
    popIndex = popIndex+1
  }
}

View(controlGen)

#Creating subset with populations - Control


controlDataPop_AfDf <- c()
popIndex = 1

while(!is.na(pop$Clinal_set[popIndex])){
  if(popIndex==1){
    controlDataPop_AfDf <- rbind(controlDataPop_AfDf, select(controlDataAf_Df, as.character(pop$Population[popIndex])))
    popIndex = popIndex+1
  }
  else{
    controlDataPop_AfDf <- cbind(controlDataPop_AfDf, select(controlDataAf_Df, as.character(pop$Population[popIndex])))
    popIndex = popIndex+1
  }
}

#Transform Season in Value for Correlation
SeasonValue <-c()
for (i in 1:length(pop[,1])) {
  if(i<=21){
    if(is.na(pop$Season[i])){
      SeasonValue[i] <- NA
    }
    else{
      if(pop$Season[i] == "Fall"){
        SeasonValue[i] <- 1
      }
      if (pop$Season[i] == "Spring") {
        SeasonValue[i] <- 0
      }
    }
  }
  else{
    SeasonValue[i] <- NA
  }
  print(i)
}
pop <- cbind(pop, SeasonValue)

#Eliminates the NA from pop
oldPop <- pop
pop <- pop[!is.na(pop$Clinal_set),]

#---------------

# -----------------Calculating Beta-------------

#-----Create a new set of pop in the correct organization of the columns-----
rm(popNew)
popNew <- pop[c("Population", "Season", "SeasonValue", "Seasonal_set", "Latitude", "Clinal_set")]
popNew
popNew[,5] <- pop[,3]
oldPopColumns <- pop
pop <- popNew
View(oldPopColumns)
View(popNew)
View(pop)

all.equal(popNew, pop)

#----


#--TESTES DE CÓDIGOS, SELECT----
popIndex
testPop <- pop[(pop[,6]==1),]
View(pop[(pop[,6]==1),])

testSelect1 <- select(immuneDataAf_Df, as.character(pop[(pop$Clinal_set==1),1]))
testSelect2 <- select(immuneDataAf_Df, as.character(pop[(pop[,6]==1),1]))
testSelect3 <- select(immuneDataAf_Df, as.character(testPop[,1]))
all.equal(testSelect1, testSelect2)
View(testSelect2)
rm(testSelect)
?select

View(pop[(pop$Clinal_set==1),1])
View(pop[(pop[,4]==1),1])
#-----


#-------- OLDER OLDER VERSION OF BETA DONT NEED TO OPEN------
#Calculating Correlations
for (l in 3:5) {
  TempDataColumn <- c()
  for(i in 1:length(immuneDataAf_Df[,1])){
    TempData <- c()
    for (j in c(1:21)) {
      TempData <- rbind(TempData, immuneDataPop_AfDF[i,j])
    }
    TempDataColumn <- rbind(TempDataColumn, lm(pop[1:21, l] ~ TempData))
  }
  View(TempDataColumn)
  CorValues <- cbind(BetaValues, TempDataColumn)
}



#TempSeason <- c()
for(i in 1:length(immuneDataAf_Df[,1])){
  TempData <- c()
  for (j in c(1:21)) {
    TempData <- rbind(TempData, immuneDataPop_AfDF[i,j])
  }
  TempSeason <- rbind(TempSeason, cor(pop$SeasonValue[c(1:4,6:21)], TempData[c(1:4,6:21)]))
}

#CorValues <- cbind(CorValues, TempSeason)

#--------

#-------- OLDER VERSION OF BETA DONT NEED TO OPEN------
#---IMMUNE
for (l in 3:5) {
  TempDataColumn <- c()
  for(i in 1:length(immuneDataAf_Df[,1])){
    TempData <- c()
    for (j in c(1:21)) {
      TempData <- rbind(TempData, immuneDataPop_AfDF[i,j])
    }
    TempDataColumn <- rbind(TempDataColumn, lm(pop[1:21, l] ~ TempData))
    if(i==1000) break
  }
  #View(TempDataColumn)
  BetaValues <- cbind(BetaValues, TempDataColumn)
}

#---
TempSeason <- c()
for(i in 1:length(immuneDataAf_Df[,1])){
  TempData <- c()
  for (j in c(1:21)) {
    TempData <- rbind(TempData, immuneDataPop_AfDF[i,j])
  }
  TempSeason <- rbind(TempSeason, cor(pop$SeasonValue[c(1:4,6:21)], TempData[c(1:4,6:21)]))
  if(i==1000) break
}

BetaValues <- cbind(BetaValues, TempSeason)
colnames(BetaValues) <- colnames(pop[3:6])

BetaValues

#Define dataframe with Beta Data
BetaData <- as.data.frame(BetaValues)


#-----Control----
BetaValuesControl <- c()
#BetaValues <- cbind(BetaValues, c(1,2,3,4))

# ---- NOTA SOBRE O LM
#{
# O LM precisa que o valor esteja em numerico. É preciso fazer antes a conversão dos valores em valores numéricos para aí fazer a LM
#}

summary(lm(pop[1:21, 3] ~ t(controlDataPop_AfDf[1,])))$r.squared

for (l in 3:6) {
  TempDataColumn <- c()
  for(i in 1:length(controlDataPop_AfDf[,1])){
    TempDataColumn <- rbind(TempDataColumn, summary(lm(pop[1:21, l] ~ t(controlDataPop_AfDf[i,])))$r.squared)
  }
  print(l)
  BetaValuesControl <- cbind(BetaValuesControl, TempDataColumn)
}

#TempSeason <- c()
for(i in 1:length(controlDataPop_AfDf[,1])){
  TempData <- c()
  for (j in c(1:21)) {
    TempData <- rbind(TempData, controlDataPop_AfDf[i,j])
  }
  TempSeason <- rbind(TempSeason, cor(pop$SeasonValue[c(1:4,6:21)], TempData[c(1:4,6:21)]))
}
#BetaValues <- cbind(BetaValues, TempSeason)


colnames(BetaValuesControl) <- colnames(pop[3:6])

#Define dataframe with Beta Data
BetaDataControl <- as.data.frame(BetaValuesControl)

#--------

#----TESTS PRE FUNCTION ----


View(oldPop)
?glm
BetaValuesData <- createBeta(immuneDataPop_AfDF)
View(BetaValuesData)

testfullBeta <- lm(pop[(pop[,3]==1),4]~t(select(immuneDataPop_AfDF, as.character(pop[(pop[,3]==1),1]))))

ggplot(x = pop[(pop[,4]==1),3], y = t(select(immuneDataPop_AfDF, as.character(pop[(pop[,4]==1),1])))[,21])

summary(testfullBeta)
View(BetaValuesData)
View(pop[(pop[,5]==1),6])
View(pop)
View(t(select(immuneDataPop_AfDF, as.character(pop[(pop[,3]==1),1])))[,1])
testLm <- lm(t(select(immuneDataPop_AfDF, as.character(pop[(pop[,4]==1),1])))[,21]~pop[(pop[,4]==1),3])
testGlm <- glm(t(select(immuneDataPop_AfDF, as.character(pop[(pop[,4]==1),1])))[,1]~pop[(pop[,4]==1),3], family = quasibinomial())
?glm
testGlm2 <- glm(t(select(immuneDataPop_AfDF, as.character(pop[(pop[,4]==1),1])))[,37]~pop[(pop[,4]==1),3], family = quasibinomial())

plot(y = t(select(immuneDataPop_AfDF, as.character(pop[(pop[,4]==1),1])))[,21], x = pop[(pop[,4]==1),3])

testGlm0 <- glm(t(select(immuneDataPop_AfDF, as.character(testpop[(testpop[,4]==0),1])))[,1]~testpop[(testpop[,4]==0), 3], family = quasibinomial())
testpop <- pop[!is.na(pop[,3]),]
View(testpop[(testpop[,4]==0), ])

coef(summary(testGlm0))
?glm

View(t(select(immuneDataPop_AfDF, as.character(pop[(pop[,4]==1),1]))))

summary(testGlm)
testGlm

?binomial

summary(testLm)
coef(summary(testGlm))

View(t(select(immuneDataPop_AfDF, as.character(pop[(pop[,4]==1),1]))))
length(t(select(immuneDataPop_AfDF, as.character(pop[(pop[,6]==1),1]))))
View(t(select(immuneDataPop_AfDF, as.character(pop[(pop[,6]==1),1]))))

immuneDataAf_Df$INFO[1]

testDataFrame <- c()


testSumG2 <- summary(testGlm2)
coef(testSumG)[2,4]

testDataFrame <- rbind(testDataFrame, cbind(coef(testSumG)[2,1], confint(testGlm)[2,1], confint(testGlm)[2,2], coef(testSumG)[2,4]))
testDataFrame <- rbind(testDataFrame, cbind(coef(testSumG2)[2,1], confint(testGlm2)[2,1], confint(testGlm2)[2,2], coef(testSumG2)[2,4]))

coef(testSumG2)
confint(testGlm2)

is.null(testDataFrame)
colnames(testDataFrame) <- c("Beta", "conf 2.5", "conf 97.5", "pv")
rownames(testDataFrame) <- c("GLM1", "GLM2")

rm(testDataFrame)

testLm$coefficients
testCoef <- testSumG$coefficients
testSumG$coefficients
testCoef[(testCoef[,4]<0.05),]


testSum <- summary(testLm)
testSumG <- summary(testGlm)
summary(testLm, correlation = TRUE)

testAnLm <- anova(testLm)
testAnLm
testSumG
coef(testSumG)

testSum$coefficients
testLm$coefficients
testIndex <- c(3,5)
View(createBeta)
View(pop[,(testIndex[1]+1)])
coef()
summary(glm(t(select(immuneDataPop_AfDF, as.character(pop[(pop[,4]==1),1])))[,21]~pop[(pop[,4]==1),3], family = quasibinomial()))

View(t(controlDataPop_AfDf[1,]))

View(immuneDataAf_Df)

View(matchClark)
#---------------

#------Beta Values Calculation----
BetaValuesData <- c()

?rnbinom

summary(lm(pop[1:21, 3] ~ t(controlDataPop_AfDf[1,])))$r.squared

#---Function of creating beta for season and for . X receives the data, y is the population
createBeta <- function(x=c(), j=0, z=1, y=pop, pv = FALSE){
  betaCalc <- c()
  if(is.null(x)||(j!=4&&j!=6)){
    stop("inform x and j correctly")
  }
  if(pv)  {
    #---Creat Subset of poplations of interest
    tempPop <- y[(y[,j]==z),]
    tempX <- t(select(x, as.character(tempPop[,1])))
    for(i in 1:length(tempX[1,])){
      tempGlm <- glm(tempX[,i] ~ tempPop[,(j-1)], family = quasibinomial())
      tempCoef <- coef(summary(tempGlm))
      if(tempCoef[2,4] < 0.05){
        tempRow <- cbind(tempCoef[2,1], confint(profile(tempGlm))[2,1], 
                         confint(profile(tempGlm))[2,2], coef(tempCoef)[2,4])
        betaCalc <- rbind(betaCalc, tempRow)
      }
    }
  }
  else{
    #---Creat Subset of poplations of interest
    tempPop <- y[(y[,j]==z),]
    tempX <- t(select(x, as.character(tempPop[,1])))
    for(i in 1:length(tempX[1,])){
      tempGlm <- glm(tempX[,i] ~ tempPop[,(j-1)], family = quasibinomial())
      tempCoef <- coef(summary(tempGlm))
      View(y)
      print(j)
      print(tempGlm)
      print(confint(profile(tempGlm)))
      stop("PORQUE ESSA MERDA TÁ FAZENDO ISSO")
      tempRow <- cbind(tempCoef[2,1], confint(profile(tempGlm))[2,1], 
                       confint(profile(tempGlm))[2,2], tempCoef[2,4])
      betaCalc <- rbind(betaCalc, tempRow)
    }
  }
  colnames(betaCalc) <- c("Estimate", "CI 2.5%", "CI 97.5%", "p")
  return(betaCalc)
}

#allBetas_LatImm <- createAllBetas(immuneDataPop_AfDF, 4)

View(immuneDataAf_Df)
View(immuneDataPop_AfDF)

beta_Lat_Imm <- createBeta(immuneDataPop_AfDF, 4)
beta_Ssn_Imm <- createBeta(immuneDataPop_AfDF, 6)
beta_Lat_Ctrl <- createBeta(controlDataPop_AfDf, 4)
beta_Ssn_Ctrl <- createBeta(controlDataPop_AfDf, 6)



beta_Lat_Imm <- as.data.frame(beta_Lat_Imm)
beta_Ssn_Imm <- as.data.frame(beta_Ssn_Imm)
beta_Lat_Ctrl <- as.data.frame(beta_Lat_Ctrl)
beta_Ssn_Ctrl <- as.data.frame(beta_Ssn_Ctrl)


View(beta_Lat_Ctrl)

#---------------

#----Beta Values with all pops----

View(pop[!is.na(pop[,3]),])
length(t(select(immuneDataPop_AfDF, as.character(pop[(pop[,6]==1),1]))))
length(t(immuneDataPop_AfDF))
View(pop)

createAllBetas <- function(xa=c(), ja=0, ya=pop){
  tempPop <- ya[!is.na(ya[,ja-1]),]
  tempBetas <- c()
  for (l in 0:1) {
    tempBetas <- rbind(tempBetas, createBeta(x = xa, j = ja, y = tempPop, z=l))
  }
  return(tempBetas)
}

#allBetas_LatImm <- createAllBetas(immuneDataPop_AfDF, 4)
#allBetas_SsnImm <- createAllBetas(immuneDataPop_AfDF, 6)
#allBetas_LatCtrl <- createAllBetas(controlDataPop_AfDf, 4)
#allBetas_SsnCtrl <- createAllBetas(controlDataPop_AfDf, 6)


#----------------

#-----**Top 1% p Selection**---------

#------

#----Tests----

testSub <- allBetas_Lat$p[c(1:10)]

View(allBetas_Lat)
View(allBetas_Lat[allBetas_Lat$ref=="Lat_Ctrl",])

nrow(allBetas_Lat)
nrow(allBetas_Lat)

sortedLat <- c(sort(allBetas_Lat$p, decreasing = FALSE)[c(1:10)])
sortedLat

sort(allBetas_Lat$p, decreasing = FALSE)[1]


table(allBetas_Lat$p == sortedLat)

View(pop[pop$Latitude==c(25.47, 30.99),])

length(beta_Ssn_Ctrl$p)
upBetaLatImm <- cbind(beta_Lat_Imm, ref = rep("Lat_Imm", length(beta_Lat_Imm$p)))
#----

#-----Create all betas-
allBetas <- rbind(cbind(beta_Lat_Imm, ref = rep("Lat_Imm", length(beta_Lat_Imm$p))), 
                  cbind(beta_Lat_Ctrl, ref = rep("Lat_Ctrl", length(beta_Lat_Ctrl$p))),
                  cbind(beta_Ssn_Imm, ref = rep("Ssn_Imm", length(beta_Ssn_Imm$p))),
                  cbind(beta_Ssn_Ctrl, ref = rep("Ssn_Ctrl", length(beta_Ssn_Ctrl$p))))

allBetas_Lat <- rbind(cbind(beta_Lat_Imm, ref = rep("Lat_Imm", length(beta_Lat_Imm$p))), 
                      cbind(beta_Lat_Ctrl, ref = rep("Lat_Ctrl", length(beta_Lat_Ctrl$p))))

allBetas_Ssn <- rbind(cbind(beta_Ssn_Imm, ref = rep("Ssn_Imm", length(beta_Ssn_Imm$p))),
                      cbind(beta_Ssn_Ctrl, ref = rep("Ssn_Ctrl", length(beta_Ssn_Ctrl$p))))


#----Function to create the top table---
getTopN <-function(x, y = 0.01){
  #Order the data by p
  xOrdered <- x[order(x$p),]
  selectTop <- head(xOrdered, n=(ceiling((nrow(xOrdered)*y))))
  tempTable <- table(selectTop[,5])
  cntlNot <- table(xOrdered$ref)[1]-tempTable[1]
  ImmNot <- table(xOrdered$ref)[2]-tempTable[2]
  tempTable <- rbind(tempTable, c(cntlNot, ImmNot))
  rownames(tempTable) <- c("Top 1", "Not Top 1")
  transposedTable <- t(tempTable)
  revTable <- transposedTable[,ncol(transposedTable):1]
  return(revTable)
}

top1Lat <- getTopN(x = allBetas_Lat)
top1Lat
top1Ssn <- getTopN(x = allBetas_Ssn)
top1Ssn
fisher.test(top1Lat)
fisher.test(top1Ssn)

top5Lat <- getTopN(x = allBetas_Lat, y = 0.05)
top10Lat <- getTopN(x = allBetas_Lat, y = 0.1)

top5Ssn <- getTopN(x = allBetas_Ssn, y = 0.05)
top10Ssn <- getTopN(x = allBetas_Ssn, y = 0.1)

top5Lat
top10Lat
top5Ssn
top10Ssn

fisher.test(top5Lat)
fisher.test(top10Lat)
fisher.test(top5Ssn)
fisher.test(top10Ssn)

View(matchClark)
levels(factor(matchClark[matchClark$immune=="immunity",3]))

View(immuneDataAf_Df)

#----

immuneGen
immuneDataAf_Df
View(immuneData)
View(pop)



#Plot Histogram----
library(gridExtra)
library(grid)

clmNames <- colnames(beta_Lat_Ctrl)
clmNames
hists <- c()

View(beta_Lat_Ctrl)
View(sapply(beta_Lat_Imm[(beta_Lat_Imm[,4])<0.05,], FUN = mean))

png(width = 1440, height = 1440, pointsize = 36, filename = "Beta Values with p-Value.png")


#{p1 <- ggplot(data = BetaData, aes(BetaData[,1])) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#005939") + 
  #ggtitle(paste('Distribution of Beta in', clmNames[1])) + theme_get() + xlab(clmNames[1])
#p2 <-ggplot(data = BetaData, aes(BetaData[,2])) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#000070") + 
#  ggtitle(paste('Distribution of Beta in', clmNames[2])) + theme_get() + xlab(clmNames[2])
#p3 <-ggplot(data = BetaData, aes(BetaData[,3])) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#730000") + 
  #ggtitle(paste('Distribution of Beta in', clmNames[3])) + theme_get() + xlab(clmNames[3])
#p4 <-ggplot(data = BetaData, aes(BetaData[,4])) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#540073") + 
#  ggtitle(paste('Distribution of Beta in', clmNames[4])) + theme_get() + xlab(clmNames[4])
#grid.arrange(p1, p3)}

#Control
#BETA GRAPHS WITH P<0.05
p1 <- ggplot(data = beta_Lat_Imm[(beta_Lat_Imm[,4])<0.05,], aes(Estimate)) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#005939") + 
  ggtitle('Beta Distribution - Immune Genes x Latitude') + theme_get() + xlab(clmNames[1])
p2 <-ggplot(data = beta_Lat_Ctrl[(beta_Lat_Ctrl[,4])<0.05,], aes(Estimate)) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#000070") + 
  ggtitle(paste('Beta Distribution - Control Genes x Latitude')) + theme_get() + xlab(clmNames[1])
p3 <-ggplot(data = beta_Ssn_Imm[(beta_Ssn_Imm[,4])<0.05,], aes(Estimate)) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#730000") + 
  ggtitle(paste('Beta Distribution - Immune Genes x Season')) + theme_get() + xlab(clmNames[1])
p4 <-ggplot(data = beta_Ssn_Ctrl[(beta_Ssn_Ctrl[,4])<0.05,], aes(Estimate)) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#540073") + 
  ggtitle(paste('Beta Distribution - Control Genes x Season')) + theme_get() + xlab(clmNames[1])
grid.arrange(p1, p2, p3, p4, top=textGrob("Beta Values with p-Value <0.05",gp=gpar(fontsize=18,font=1)))

#BETA GRAPHS FULL
b1 <- ggplot(data = beta_Lat_Imm, aes(Estimate)) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#005939") + 
  ggtitle('Beta Distribution - Immune Genes x Latitude') + theme_get() + xlab(clmNames[1])
b2 <-ggplot(data = beta_Lat_Ctrl, aes(Estimate)) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#000070") + 
  ggtitle(paste('Beta Distribution - Control Genes x Latitude')) + theme_get() + xlab(clmNames[1])
b3 <-ggplot(data = beta_Ssn_Imm, aes(Estimate)) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#730000") + 
  ggtitle(paste('Beta Distribution - Immune Genes x Season')) + theme_get() + xlab(clmNames[1])
b4 <-ggplot(data = beta_Ssn_Ctrl, aes(Estimate)) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#540073") + 
  ggtitle(paste('Beta Distribution - Control Genes x Season')) + theme_get() + xlab(clmNames[1])
grid.arrange(b1, b2, b3, b4, top=textGrob("Beta Values disregard p-Value",gp=gpar(fontsize=18,font=1)))

#p-Value GRAPHS FULL
pv1 <- ggplot(data = beta_Lat_Imm, aes(p)) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#005939") + 
  ggtitle('p-Value Distribution - Immune Genes x Latitude') + theme_get() + xlab(clmNames[1])
pv2 <-ggplot(data = beta_Lat_Ctrl, aes(p)) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#000070") + 
  ggtitle(paste('Value Distribution - Control Genes x Latitude')) + theme_get() + xlab(clmNames[1])
pv3 <-ggplot(data = beta_Ssn_Imm, aes(p)) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#730000") + 
  ggtitle(paste('Value Distribution - Immune Genes x Season')) + theme_get() + xlab(clmNames[1])
pv4 <-ggplot(data = beta_Ssn_Ctrl, aes(p)) + geom_histogram(aes(y=..ncount..), binwidth = 0.01, fill = "#540073") + 
  ggtitle(paste('Value Distribution - Control Genes x Season')) + theme_get() + xlab(clmNames[1])
grid.arrange(pv1, pv2, pv3, pv4, top=textGrob("p-Value Distribution",gp=gpar(fontsize=18,font=1)))

dev.off()

