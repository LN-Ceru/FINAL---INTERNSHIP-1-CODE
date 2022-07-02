rm(list=ls(all=TRUE))

library(foreign)
library(dplyr)
library(pwr)
library(tidyverse)
library(data.table)
library(tidyverse)
library(haven)
library(labelled)
library(psych)
library(lmtest)
library(car)
library(nlme)
library(interactions)
library(sjPlot)
library(ggeffects)
library(ggplot2)
library(ggpubr)


setwd("C:/Users/gebruiker/OneDrive/Documents/Mres Int1/Mstrs-1")


testdata = read_sav("PHE_20220323_3105_LL_LaurenceNisbet_LVS.sav")
SES = read_sav("PHE_20220323_3105_TVB_LaurenceNisbet_educationParents.sav")

SES = as.data.frame(SES)
SES$SES_max = pmax(SES$educat_m, SES$educat_v, na.rm = TRUE) 
SES$SES_max = SES$SES_max -1


PRS_data = read_sav("PHE_20220303_3105_TVB_LaurenceNisbet_IQ.sav")

PRS_data = as.data.frame(PRS_data)

PRS_data$Platform_numeric = as.numeric(PRS_data$Platform)

PRS_data$Sex_numeric = as.numeric(PRS_data$sex)


Reading_Subset1 = as.data.frame(testdata[c("FID", "PID","BegrLezE3Generatie","BegrLezE3VaardigheidsScore", "BegrLezM4Generatie", 
                                           "BegrLezM4VaardigheidsScore","BegrLezM5Generatie", "BegrLezM5VaardigheidsScore", "BegrLezM6Generatie", 
                                           "BegrLezM6VaardigheidsScore","BegrLezM7Generatie","BegrLezM7VaardigheidsScore", "BegrLezM8Generatie", 
                                           "BegrLezM8VaardigheidsScore","BegrLezE3Makkelijk", "BegrLezM4Makkelijk","BegrLezM5Makkelijk","BegrLezM6Makkelijk",
                                           "BegrLezM7Makkelijk", "BegrLezM8Makkelijk", 'BegrLezE3TestScore', 'BegrLezM4TestScore', 'BegrLezM5TestScore',
                                           'BegrLezM6TestScore','BegrLezM7TestScore', 'BegrLezM8TestScore','age_months_BegrLezE3' ,'age_months_BegrLezM4',
                                           'age_months_BegrLezM5','age_months_BegrLezM6','age_months_BegrLezM7','age_months_BegrLezM8')])

ages_subset = as.data.frame(testdata[c("FID", "PID",'age_months_BegrLezE3' ,'age_months_BegrLezM4',
                                       'age_months_BegrLezM5','age_months_BegrLezM6','age_months_BegrLezM7','age_months_BegrLezM8')])

Reading_Subset1 = remove_val_labels(Reading_Subset1)

ages_subset = remove_val_labels(ages_subset)

#prep for easy tests
for (i in 15:20){
  Reading_Subset1[,i] = as.numeric(unlist(Reading_Subset1[,i]))
}

#prep for ages and scores subsets
for (i in 1:8){
  ages_subset[,i] = as.numeric(unlist(ages_subset[,i]))
}

N1 = seq(1,14,by=2)

for (i in N1){
  Reading_Subset1[,i] = as.numeric(unlist(Reading_Subset1[,i]))
}



#Removing easy tests 
Reading_Subset1[!is.na(Reading_Subset1[,15]),][,c(4,15)] = NA
Reading_Subset1[!is.na(Reading_Subset1[,16]),][,c(6,16)] = NA
Reading_Subset1[!is.na(Reading_Subset1[,17]),][,c(8,17)] = NA
Reading_Subset1[!is.na(Reading_Subset1[,18]),][,c(10,18)] = NA
Reading_Subset1[!is.na(Reading_Subset1[,19]),][,c(12,19)] = NA
Reading_Subset1[!is.na(Reading_Subset1[,20]),][,c(14,20)] = NA


#Selecting Gen2 only
Reading_Subset1[is.na(Reading_Subset1[,3]),][,3] = 999
Reading_Subset1[Reading_Subset1[,3] != 2,][, 3:4] = NA #3

Reading_Subset1[is.na(Reading_Subset1[,5]),][,5] = 999
Reading_Subset1[Reading_Subset1[,5] != 2,][, 5:6] = NA #4

Reading_Subset1[is.na(Reading_Subset1[,7]),][,7] = 999
Reading_Subset1[Reading_Subset1[,7] != 2,][, 7:8] = NA #5

Reading_Subset1[is.na(Reading_Subset1[,9]),][,9] = 999
Reading_Subset1[Reading_Subset1[,9] != 2,][, 9:10] = NA #6

Reading_Subset1[is.na(Reading_Subset1[,11]),][,11] = 999
Reading_Subset1[Reading_Subset1[,11] != 2,][, 11:12] = NA #7

Reading_Subset1[is.na(Reading_Subset1[,13]),][,13] = 999
Reading_Subset1[Reading_Subset1[,13] != 2,][, 13:14] = NA #8






#MISMATCH REMOVAL - GENTLE VER
#=======================

plot(Reading_Subset1$BegrLezE3VaardigheidsScore, Reading_Subset1$BegrLezE3TestScore)
plot(Reading_Subset1$BegrLezM4VaardigheidsScore, Reading_Subset1$BegrLezM4TestScore)
plot(Reading_Subset1$BegrLezM5VaardigheidsScore, Reading_Subset1$BegrLezM5TestScore)
plot(Reading_Subset1$BegrLezM6VaardigheidsScore, Reading_Subset1$BegrLezM6TestScore)
plot(Reading_Subset1$BegrLezM7VaardigheidsScore, Reading_Subset1$BegrLezM7TestScore)
plot(Reading_Subset1$BegrLezM8VaardigheidsScore, Reading_Subset1$BegrLezM8TestScore)


Reading_Subset1[Reading_Subset1['PID'] == 452,][,c(4,21) ] = NA #8
Reading_Subset1[Reading_Subset1['PID'] == 452,][,c(6,22) ] = NA #8
Reading_Subset1[Reading_Subset1['PID'] == 6076,][,c(8,23) ] = NA #8
#Reading_Subset1[Reading_Subset1['PID'] == 6211,][,c(8,23) ] = NA #8
Reading_Subset1[Reading_Subset1['PID'] == 7579,][,c(8,23) ] = NA #8
Reading_Subset1[Reading_Subset1['PID'] == 7926,][,c(8,23) ] = NA #8
Reading_Subset1[Reading_Subset1['PID'] == 7925,][,c(8,23) ] = NA #8


#Bringing outliers to the fence
outlierReplace= function(Dataframe, X){
  Qtiq <- quantile(Dataframe[,X], probs = c(.25, .75), na.rm = TRUE)
  tiqr_comb = IQR(Dataframe[,X], na.rm = TRUE)
  tmax34 <-  Qtiq[2]+2.5*tiqr_comb 
  tmin34 <- Qtiq[1]-2.5*tiqr_comb 
  if (range(na.omit(Dataframe[,X]))[2]>tmax34){
    Dataframe[!is.na(Dataframe[,X]) &  Dataframe[,X] > (tmax34),][,X] = tmax34}
  
  if (range(na.omit(Dataframe[,X]))[1]<tmin34){
    Dataframe[!is.na(Dataframe[,X]) &  Dataframe[,X] < (tmin34),][,X] = tmin34}
  
  return (Dataframe)
}


Reading_Subset1 = outlierReplace(Reading_Subset1, 4)
Reading_Subset1 = outlierReplace(Reading_Subset1, 6)
Reading_Subset1 = outlierReplace(Reading_Subset1, 8)
Reading_Subset1 = outlierReplace(Reading_Subset1, 10)
Reading_Subset1 = outlierReplace(Reading_Subset1, 12)
Reading_Subset1 = outlierReplace(Reading_Subset1, 14)





#Removing all NA Rows
BG_col_select = c(4,6,8,10,12,14)
ReadingSubset = Reading_Subset1[rowSums(is.na(Reading_Subset1[,BG_col_select])) != ncol(Reading_Subset1[,BG_col_select]), ]

#Adding SES to data
ReadingPID = ReadingSubset$PID

BG_MM_PRS = PRS_data[PRS_data$PID %in% ReadingPID,][,c(1:12, 94:104, 141, 153,154 )]
SES_MM_Set = SES[SES$PID %in% ReadingPID,][,c(1,4)]

BG_MM_fams = cbind(ReadingSubset, BG_MM_PRS, SES_MM_Set)
duplicates_BG_MM = duplicated(colnames(BG_MM_fams))
BG_MM_fams = BG_MM_fams[!duplicates_BG_MM]



#Cutting out related family members
set.seed(1)
BG_MM_urelat <- BG_MM_fams %>%
  group_by(FID) %>%
  sample_n(1) %>%
  ungroup() 
BG_MM = BG_MM_fams[BG_MM_fams$PID %in% BG_MM_urelat$PID,]


BG_MM_pid = BG_MM$PID
Ages_MM = ages_subset[ages_subset$PID %in% BG_MM_pid,]


#long conversion 
BG_MM_long =  reshape(data = BG_MM, idvar ='PID',
                      varying = c('BegrLezE3VaardigheidsScore', 'BegrLezM4VaardigheidsScore',
                                  'BegrLezM5VaardigheidsScore', 'BegrLezM6VaardigheidsScore',
                                  'BegrLezM7VaardigheidsScore', 'BegrLezM8VaardigheidsScore'),
                      v.names = 'BG_score', 
                      times = c(0, 0.75, 1.75, 2.75, 3.75, 4.75),
                      #times = c(0,1,2,3,4),
                      direction = 'long')

longtest = BG_MM_long[,c(2,53,53)]

#long conversion for within year ages
Ages_months = reshape(data = Ages_MM, idvar ='PID',
                      varying = c('age_months_BegrLezE3' ,'age_months_BegrLezM4',
                                  'age_months_BegrLezM5','age_months_BegrLezM6',
                                  'age_months_BegrLezM7','age_months_BegrLezM8'),
                      times = c(0, 0.75, 1.75, 2.75, 3.75, 4.75),
                      v.names = 'age_months', 
                      #times = c(0,1,2,3,4),
                      direction = 'long')

Ages_months = unlist(Ages_months$age_months)

#appending in year ages
BG_MM_long = cbind(BG_MM_long, Ages_months)
duplicates_long = duplicated(colnames(BG_MM_long))
BG_MM_long = BG_MM_long[!duplicates_long]


BG_MM_long = remove_attributes(BG_MM_long, attributes = 'format.spss')
BG_MM_long = remove_attributes(BG_MM_long, attributes = 'display_width')



#Standardising and Centring all variables 

BG_MM_long = as.data.frame(scale(BG_MM_long))


#Principle analysis
BG_intercept = gls(BG_score ~ P_inf_SCORE_EA_MRG10_NTRALL_LDp1 + 
                     Platform_numeric + SES_max + PC1 + PC2 + PC3 
                   + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + Sex_numeric + Ages_months,
                   data = BG_MM_long, na.action = na.exclude, method = 'ML') 
BG_random_intercept = lme(BG_score ~ P_inf_SCORE_EA_MRG10_NTRALL_LDp1 + 
                            Platform_numeric + SES_max + PC1 + PC2 + PC3 
                          + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10+ Sex_numeric
                          + Ages_months, data = BG_MM_long, 
                          random = ~1|PID, method = 'ML',
                          na.action = na.exclude, control = list(opt = "optim"))
BG_fix_time = update(BG_random_intercept, .~. + time)
BG_random_time = update(BG_fix_time, random = ~time|PID)
#BG_randoms_ar1 = update(BG_random_time, correlation = corCAR1(0.55, form = ~time|PID))
BG_fix_inter = update(BG_random_time, .~. + time*P_inf_SCORE_EA_MRG10_NTRALL_LDp1)

BG_ANOVA = anova(BG_intercept, BG_random_intercept, BG_fix_time, BG_random_time, BG_fix_inter)
BG_ANOVA
BG_results = summary(BG_random_time)
BG_results

# MATHS 
#===================================================
rm(list=ls(all=TRUE))

library(foreign)
library(dplyr)
library(pwr)
library(tidyverse)
library(data.table)
library(tidyverse)
library(haven)
library(labelled)
library(psych)
library(lmtest)
library(car)
library(nlme)
library(interactions)
library(sjPlot)
library(ggeffects)
library(ggplot2)
library(finnstats)
library(ggpubr)


setwd("C:/Users/gebruiker/OneDrive/Documents/Mres Int1/Mstrs-1")


testdata = read_sav("PHE_20220323_3105_LL_LaurenceNisbet_LVS.sav")
SES = read_sav("PHE_20220323_3105_TVB_LaurenceNisbet_educationParents.sav")

SES = as.data.frame(SES)
SES$SES_max = pmax(SES$educat_m, SES$educat_v, na.rm = TRUE) 


PRS_data = read_sav("PHE_20220303_3105_TVB_LaurenceNisbet_IQ.sav")

PRS_data = as.data.frame(PRS_data)

PRS_data$Platform_numeric = as.numeric(PRS_data$Platform)

PRS_data$Sex_numeric = as.numeric(PRS_data$sex)


Maths_Subset1 = as.data.frame(testdata[c("FID", "PID","RekWisM3Generatie","RekWisM3VaardigheidsScore", "RekWisM4Generatie", 
                                         "RekWisM4VaardigheidsScore","RekWisM5Generatie", "RekWisM5VaardigheidsScore", "RekWisM6Generatie", 
                                         "RekWisM6VaardigheidsScore","RekWisM7Generatie","RekWisM7VaardigheidsScore", "RekWisM8Generatie", 
                                         "RekWisM8VaardigheidsScore",  'RekWisM3TestScore', 'RekWisM4TestScore', 'RekWisM5TestScore',
                                         'RekWisM6TestScore','RekWisM7TestScore', 'RekWisM8TestScore')])

ages_subset = as.data.frame(testdata[c("FID", "PID",'age_months_RekWisM3' ,'age_months_RekWisM4',
                                       'age_months_RekWisM5','age_months_RekWisM6','age_months_RekWisM7')])

ages_subset = remove_val_labels(ages_subset)


Maths_Subset1 = remove_val_labels(Maths_Subset1)


plot(testdata$Input, testdata$RekWisM3VaardigheidsScore, type = 'h')



N1 = seq(1,20,by=2)

for (i in N1){
  Maths_Subset1[,i] = as.numeric(unlist(Maths_Subset1[,i]))
}


#Selecting Gen2 only
Maths_Subset1[is.na(Maths_Subset1[,3]),][,3] = 999
Maths_Subset1[Maths_Subset1[,3] != 2,][, c(3:4,15)] = NA #3

Maths_Subset1[is.na(Maths_Subset1[,5]),][,5] = 999
Maths_Subset1[Maths_Subset1[,5] != 2,][, c(5:6, 16)] = NA #4

Maths_Subset1[is.na(Maths_Subset1[,7]),][,7] = 999
Maths_Subset1[Maths_Subset1[,7] != 2,][, c(7:8, 17)] = NA #5

Maths_Subset1[is.na(Maths_Subset1[,9]),][,9] = 999
Maths_Subset1[Maths_Subset1[,9] != 2,][, c(9:10,18)] = NA #6

Maths_Subset1[is.na(Maths_Subset1[,11]),][,11] = 999
Maths_Subset1[Maths_Subset1[,11] != 2,][, c(11:12,19)] = NA #7

Maths_Subset1[is.na(Maths_Subset1[,13]),][,13] = 999
Maths_Subset1[Maths_Subset1[,13] != 2,][, c(13:14,20)] = NA #8



outlierReplace= function(Dataframe, X){
  Qtiq <- quantile(Dataframe[,X], probs = c(.25, .75), na.rm = TRUE)
  tiqr_comb = IQR(Dataframe[,X], na.rm = TRUE)
  tmax34 <-  Qtiq[2]+2.5*tiqr_comb 
  tmin34 <- Qtiq[1]-2.5*tiqr_comb 
  if (range(na.omit(Dataframe[,X]))[2]>tmax34){
    Dataframe[!is.na(Dataframe[,X]) &  Dataframe[,X] > (tmax34),][,X] = tmax34}
  
  if (range(na.omit(Dataframe[,X]))[1]<tmin34){
    Dataframe[!is.na(Dataframe[,X]) &  Dataframe[,X] < (tmin34),][,X] = tmin34}
  
  return (Dataframe)
}

# removing mismatches

plot(Maths_Subset1$RekWisM3VaardigheidsScore, Maths_Subset1$RekWisM3TestScore)
plot(Maths_Subset1$RekWisM4VaardigheidsScore, Maths_Subset1$RekWisM4TestScore)
plot(Maths_Subset1$RekWisM5VaardigheidsScore, Maths_Subset1$RekWisM5TestScore)
plot(Maths_Subset1$RekWisM6VaardigheidsScore, Maths_Subset1$RekWisM6TestScore)
plot(Maths_Subset1$RekWisM7VaardigheidsScore, Maths_Subset1$RekWisM7TestScore)
plot(Maths_Subset1$RekWisM8VaardigheidsScore, Maths_Subset1$RekWisM8TestScore)


Maths_Subset1[Maths_Subset1['PID'] == 5659,][,c(4,15) ] = NA #3
Maths_Subset1[Maths_Subset1['PID'] == 7895,][,c(4,15) ] = NA #3
Maths_Subset1[Maths_Subset1['PID'] == 7420,][,c(4,15) ] = NA #3
Maths_Subset1[Maths_Subset1['PID'] == 7138,][,c(6,16) ] = NA #4
Maths_Subset1[Maths_Subset1['PID'] == 7138,][,c(8,17) ] = NA #5
Maths_Subset1[Maths_Subset1['PID'] == 6583,][,c(8,17) ] = NA #5
Maths_Subset1[Maths_Subset1['PID'] == 7895,][,c(8,17) ] = NA #5
Maths_Subset1[Maths_Subset1['PID'] == 7138,][,c(10,18) ] = NA #6



Maths_Subset1 = outlierReplace(Maths_Subset1, 4)
Maths_Subset1 = outlierReplace(Maths_Subset1, 6)
Maths_Subset1 = outlierReplace(Maths_Subset1, 8)
Maths_Subset1 = outlierReplace(Maths_Subset1, 10)
Maths_Subset1 = outlierReplace(Maths_Subset1, 12)
Maths_Subset1 = outlierReplace(Maths_Subset1, 14)


#Combinging data frames
RW_col_select = c(4,6,8,10,12,14)
MathsSubset = Maths_Subset1[rowSums(is.na(Maths_Subset1[,RW_col_select])) != ncol(Maths_Subset1[,RW_col_select]), ]

MathsPID = MathsSubset$PID

RW_MM_PRS = PRS_data[PRS_data$PID %in% MathsPID,][,c(1:12, 94:104, 141, 153,154 )]
SES_MM_Set = SES[SES$PID %in% MathsPID,][,c(1,4)]

RW_MM_fams = cbind(MathsSubset, RW_MM_PRS, SES_MM_Set)
duplicates_RW_MM = duplicated(colnames(RW_MM_fams))
RW_MM_fams = RW_MM_fams[!duplicates_RW_MM]



#Remove all only NA ROWS

set.seed(1)
RW_MM_urelat <- RW_MM_fams %>%
  group_by(FID) %>%
  sample_n(1) %>%
  ungroup() 
RW_MM = RW_MM_fams[RW_MM_fams$PID %in% RW_MM_urelat$PID,]

RW_MM_pid = RW_MM$PID
Ages_MM = ages_subset[ages_subset$PID %in% RW_MM_pid,]


#RW_MM[,c(4,6,8,10,12,42)] = as.data.frame(scale(RW_MM[,c(4,6,8,10,12,42)],center = F))


RW_MM_long =  reshape(data = RW_MM, idvar ='PID',
                      varying = c('RekWisM3VaardigheidsScore', 'RekWisM4VaardigheidsScore',
                                  'RekWisM5VaardigheidsScore', 'RekWisM6VaardigheidsScore',
                                  'RekWisM7VaardigheidsScore'),
                      v.names = 'RW_score', 
                      times = c(0, 1, 2, 3, 4),
                      direction = 'long')

Ages_months = reshape(data = Ages_MM, idvar ='PID',
                      varying = c('age_months_RekWisM3' ,'age_months_RekWisM4',
                                  'age_months_RekWisM5','age_months_RekWisM6',
                                  'age_months_RekWisM7'),
                      times = c(0, 1, 2, 3, 4),
                      v.names = 'age_months', 
                      direction = 'long')

Ages_months = unlist(Ages_months$age_months)

RW_MM_long = cbind(RW_MM_long, Ages_months)
duplicates_long = duplicated(colnames(RW_MM_long))
RW_MM_long = RW_MM_long[!duplicates_long]


# Scaling and principle analysis 

RW_MM_long = as.data.frame( scale(RW_MM_long))


RW_intercept = gls(RW_score ~ P_inf_SCORE_EA_MRG10_NTRALL_LDp1 + 
                     Platform_numeric + SES_max + PC1 + PC2 + PC3 
                   + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + Sex_numeric + Ages_months,
                   data = RW_MM_long, na.action = na.exclude, method = 'ML') 
RW_random_intercept = lme(RW_score ~ P_inf_SCORE_EA_MRG10_NTRALL_LDp1 + 
                            Platform_numeric + SES_max + PC1 + PC2 + PC3 
                          + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10+ Sex_numeric
                          + Ages_months, data = RW_MM_long, 
                          random = ~1|PID, method = 'ML',
                          na.action = na.exclude, control = list(opt = "optim"))
RW_fix_time = update(RW_random_intercept, .~. + time)
RW_random_time = update(RW_fix_time, random = ~time|PID)
RW_fix_inter = update(RW_random_time, .~. + time*P_inf_SCORE_EA_MRG10_NTRALL_LDp1)
#RW_randoms_ar1 = update(RW_random_time, correlation = corAR1(0, form = ~time|PID))
#RW_fix_inter = update(RW_randoms_ar1, .~. + time*P_inf_SCORE_EA_MRG10_NTRALL_LDp1)


dev.off()

#RW_ANOVA = anova(RW_intercept, RW_random_intercept, RW_fix_time, RW_random_time, RW_randoms_ar1, RW_fix_inter)
RW_ANOVA = anova(RW_intercept, RW_random_intercept, RW_fix_time, RW_random_time, RW_fix_inter)
RW_ANOVA
RW_results = summary(RW_random_time)
RW_results

#==============================================
#IQ CODE - NOTE THE SAME FOR ALL VARIABLES 
#FOR EACH VARIABLE CHANGE IQ VAR + SELECT SUBSETS AS FOLLOWS = 
# VIQ_Subset = PRS_data[,c(1:12, 75, 77, 79, 81, 83, 85, 57, 59,61, 63, 65, 67, 94:104, 141, 153, 154, 78,84,60,66)]
#==========
#VIQ
VIQ_Subset = PRS_data[,c(1:12, 75, 77, 79, 81, 83, 85, 57, 59,61, 63, 65, 67, 94:104, 141, 153, 154, 78,84,60,66)]
N1 = c(1:13, 16,19,22,25:42)
for (i in N1){
  VIQ_Subset[,i] = as.numeric(unlist(VIQ_Subset[,i]))
}

#=========
#PIQ
piq_Subset = PRS_data[,c(1:12, 76, 77, 79, 82, 83, 85, 58, 59,61, 64, 65, 67, 94:104, 141, 153, 154, 78,84,60,66)]
N1 = c(1:13, 16,19,22,25:42)
for (i in N1){
  piq_Subset[,i] = as.numeric(unlist(piq_Subset[,i]))
}

#============
#TIQ
tiq_Subset = PRS_data[,c(1:12, 74, 77, 79, 80, 83, 85, 56, 59,61, 62, 65, 67, 94:104, 141, 153, 154, 78,84,60,66)]
N1 = c(1:13, 16,19,22,25:42)
for (i in N1){
  tiq_Subset[,i] = as.numeric(unlist(tiq_Subset[,i]))
}


#===============
rm(list=ls(all=TRUE))

library(foreign)
library(dplyr)
library(pwr)
library(tidyverse)
library(data.table)
library(tidyverse)
library(haven)
library(labelled)
library(psych)
library(lmtest)
library(car)
library(nlme)
library(interactions)
library(sjPlot)
library(ggeffects)
library(ggplot2)
library(gplots)
library(ggpubr)


setwd("C:/Users/gebruiker/OneDrive/Documents/Mres Int1/Mstrs-1")


testdata = read_sav("PHE_20220323_3105_LL_LaurenceNisbet_LVS.sav")
SES = read_sav("PHE_20220323_3105_TVB_LaurenceNisbet_educationParents.sav")

SES = as.data.frame(SES)
SES$SES_max = pmax(SES$educat_m, SES$educat_v, na.rm = TRUE) 
SES$SES_max = as.numeric(unlist(SES$SES_max))

PRS_data = read_sav("PHE_20220303_3105_TVB_LaurenceNisbet_IQ.sav")

PRS_data = as.data.frame(PRS_data)

PRS_data$Platform_numeric = as.numeric(PRS_data$Platform)

PRS_data$Sex_numeric = as.numeric(PRS_data$sex)


#
tiq_Subset = PRS_data[,c(1:12, 74, 77, 79, 80, 83, 85, 56, 59,61, 62, 65, 67, 94:104, 141, 153, 154, 78,84,60,66)]
N1 = c(1:13, 16,19,22,25:42)
for (i in N1){
  tiq_Subset[,i] = as.numeric(unlist(tiq_Subset[,i]))
}



outlierReplace= function(Dataframe, X){
  Qtiq <- quantile(Dataframe[,X], probs = c(.25, .75), na.rm = TRUE)
  tiqr_comb = IQR(Dataframe[,X], na.rm = TRUE)
  tmax34 <-  Qtiq[2]+2.5*tiqr_comb 
  tmin34 <- Qtiq[1]-2.5*tiqr_comb 
  if (range(na.omit(Dataframe[,X]))[2]>tmax34){
    Dataframe[!is.na(Dataframe[,X]) &  Dataframe[,X] > (tmax34),][,X] = tmax34}
  
  if (range(na.omit(Dataframe[,X]))[1]<tmin34){
    Dataframe[!is.na(Dataframe[,X]) &  Dataframe[,X] < (tmin34),][,X] = tmin34}
  
  return (Dataframe)
}

#Sorting outliying ages
tiq_Subset[is.na(tiq_Subset[,41]),][,41] = 999
tiq_Subset[tiq_Subset[,41] > 11 | tiq_Subset[,41] < 9, ][,c(19,41)] = NA #age 10

tiq_Subset[is.na(tiq_Subset[,42]),][,42] = 999
tiq_Subset[tiq_Subset[,42] > 13| tiq_Subset[,42] < 11,][,c(22,42)] = NA #age 12


#row selection and frame combination
tiq_col_select = c(13,16,19,22)
tiq_rowclean = tiq_Subset[rowSums(is.na(tiq_Subset[,tiq_col_select])) != ncol(tiq_Subset[,tiq_col_select]), ]
dim(tiq_rowclean)

tiq_pid = tiq_rowclean$PID

SES_MM_Set = SES[SES$PID %in% tiq_pid,][,c(1,4)]

tiq_MM_fams = cbind(tiq_rowclean, SES_MM_Set)
duplicates_tiq_MM = duplicated(colnames(tiq_MM_fams))
tiq_MM_fams = tiq_MM_fams[!duplicates_tiq_MM]

#selecting random family memers
set.seed(23)
tiq_MM_urelat <- tiq_MM_fams %>%
  group_by(FID) %>%
  sample_n(1) %>%
  ungroup() 
tiq_MM = tiq_MM_fams[tiq_MM_fams$PID %in% tiq_MM_urelat$PID,]


#Principle analysis + long conversion

names(tiq_MM)[names(tiq_MM) == "SES[SES$PID %in% tiq_MM_index, ][, c(6)]"] <- "SES_Numeric"
tiq_long = reshape(data = tiq_MM, idvar ='PID',
                   varying = c('iq_5', 'iq_7','iq_10', 'iq_12'),
                   v.names = 'tiq', 
                   times = c(0, 1, 2, 3),
                   direction = 'long')



tiq_long =as.data.frame(scale(tiq_long[,c(1:12,21:41)]))


tiq_intercept = gls(tiq ~ P_inf_SCORE_EA_MRG10_NTRALL_LDp1+ 
                      Platform_numeric + SES_max + PC1 + PC2 + PC3 
                    + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + Sex_numeric
                    + time,
                    data = tiq_long, na.action = na.exclude, method = 'ML') 
tiq_random_intercept = lme(tiq ~ P_inf_SCORE_EA_MRG10_NTRALL_LDp1+ 
                             Platform_numeric + SES_max + PC1 + PC2 + PC3 
                           + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + Sex_numeric
                           +time, data = tiq_long, 
                           random = ~1|PID, method = 'ML',
                           na.action = na.exclude, control = list(opt = "optim"))
tiq_random_time = update(tiq_random_intercept, random = ~time|PID)
tiq_fix_inter = update(tiq_random_time, .~. + time*P_inf_SCORE_EA_MRG10_NTRALL_LDp1)

tiq_anova = anova(tiq_intercept, tiq_random_intercept,tiq_random_time,tiq_fix_inter)
tiq_anova
tiq_results = summary(tiq_fix_inter)
tiq_results