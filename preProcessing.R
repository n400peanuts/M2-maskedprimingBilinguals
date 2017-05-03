# ---- Preprocessing of bilingual masked priming experiment
# ---- The final output is a file called "masterFile" in which there are all the raw data of the experiment
#----- 14/12/2016

#N.B.: ss2 in ITA has one target different from the others subjs (1 & from 3 to 40). Then I chose to upload 1 & 2 seperatly, and in a second moment from 3 to 40.

#---------------------------------------------------------------------------------------------------#
#                               CONCATENATE ALL THE SBJ ENG                                         #
#---------------------------------------------------------------------------------------------------#
rotations <- read.table('C:/Users/Eva Viviani/Documents/GitHub/M2-maskedprimingBilinguals/stimoli/rotations.txt', header = T)

eng = NULL
for (j in 1:40){
  if ((j %% 2) ==0){ #EVEN SUBJ
    boh <- paste("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/ENG/Output_MPrim_Eng_Subj_", j, ".txt", sep = "")
    pilot_eng <- read.table(boh, header = F, skip = 15,dec = ",")
    colnames(pilot_eng) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
    rot_B_eng <- subset(rotations, rotations$Rotation=='B' & rotations$Language=='eng')
    merge(pilot_eng, rot_B_eng, by = "Target", all.x = T)-> pilot_eng
  } else { #ODD
    boh <- paste("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/ENG/Output_MPrim_Eng_Subj_", j, ".txt", sep = "")
    pilot_eng <- read.table(boh, header = F, skip = 15,dec = ",")
    colnames(pilot_eng) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
    rot_A_eng <- subset(rotations, rotations$Rotation=='A' & rotations$Language=='eng')
    merge(pilot_eng, rot_A_eng, by = "Target", all.x = T)-> pilot_eng
  }
  
  
  pilot_eng$Prime.y <- NULL
  pilot_eng$rt <- as.numeric(pilot_eng$rt)
  pilot_eng$Rotation.y <- NULL
  
  eng <- rbind(eng,pilot_eng)
}

#---------------------------------------------------------------------------------------------------#
#                               CONCATENATE ALL THE SBJ ITA                                         #
#---------------------------------------------------------------------------------------------------#
#                                            SS1                                                    #
ss1 <- read.table("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/ITA/Output_MPrim_Ita_Subj_1.txt", header = F,  skip = 15, dec = ",")
colnames(ss1) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
rotA<- subset(rotations, rotations$Rotation=='A' & rotations$Language=='ita')
merge(ss1, rotA, by = "Target", all.x = T)->ss1

ss1$Prime.y <- NULL
ss1$Rotation.y <- NULL

summary(ss1) 

#                                            SS2                                                    #

ss2 <- read.table("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/ITA/Output_MPrim_Ita_Subj_2.txt", header = F,  skip = 15, dec = ",")
colnames(ss2) <- c("Subject", "Age", "Gender", "Handedness", "Rotation.x","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
rotB1<- read.table("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Stimoli/rot_B_persoggetto2.txt", header = T, dec = ",")
colnames(rotB1) <- c("Trial.ID", "Target.ID", "Lexicality", "Morphtype", "Target", "Prime", "Primetype", "Logfreq.Zipf.t", "Lent", "Lenp", "Logfreq.Zipf.p", "Nt", "Np")
merge(ss2, rotB1, by = "Target", all.x = T)->ss2

ss2$Prime.y <- NULL
ss2$Language <- c("ita")
ss2$Language <- as.factor(ss2$Language)

summary(ss2) 

#                                       FROM SS3 TO SS40                                           #                        

ita = NULL
for (j in 3:40){
  if ((j %% 2) ==0){ #EVEN SUBJ
    boh <- paste("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/ITA/Output_MPrim_Ita_Subj_", j, ".txt", sep = "")
    pilot_ita <- read.table(boh, header = F, skip = 15,dec = ",")
    colnames(pilot_ita) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
    rotB<- subset(rotations, rotations$Rotation=='B' & rotations$Language=='ita')
    merge(pilot_ita, rotB, by = "Target", all.x = T)-> pilot_ita
  } else { #ODD
    boh <- paste("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/ITA/Output_MPrim_Ita_Subj_", j, ".txt", sep = "")
    pilot_ita <- read.table(boh, header = F, skip = 15,dec = ",")
    colnames(pilot_ita) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
    rotA<- subset(rotations, rotations$Rotation=='A' & rotations$Language=='ita')
    merge(pilot_ita, rotA, by = "Target", all.x = T)-> pilot_ita
  }
  
  pilot_ita$Prime.y <- NULL
  pilot_ita$rt <- as.numeric(pilot_ita$rt)
  pilot_ita$Rotation.y <- NULL
  
  ita <- rbind(ita,pilot_ita)
}

ita<- rbind(ss2,ita)
ita <- rbind(ss1,ita)

#---------------------------------------------------------------------------------------------------#
#                               MERGE ALL THE SBJ INTO A MASTERFILE                                 #
#---------------------------------------------------------------------------------------------------#
rbind(ita,eng)-> masterFile
masterFile$Logfreq.Zipf.p <- as.numeric(sub(",",".", masterFile$Logfreq.Zipf.p, fixed = T))
masterFile$Logfreq.Zipf.t <- as.numeric(sub(",",".", masterFile$Logfreq.Zipf.t, fixed = T))
#                                    Accuracy column creation                                       # 
masterFile$Accuracy<- 1; 
masterFile$Accuracy[masterFile$Lexicality=="WORD" & masterFile$Resp==1] <- 0 ;
masterFile$Accuracy[masterFile$Lexicality=="NONWORD" & masterFile$Resp==2] <- 0; 

rm(rotations, rotB, rotA, rot_A_eng, rot_B_eng, rotB1, eng, ita, pilot_eng, pilot_ita, ss1, ss2, boh, j)

#---------------------------------------------------------------------------------------------------#
#                                          END                                                      #
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#                                          OSC                                                      #
#---------------------------------------------------------------------------------------------------#
read.table('C:/Users/Eva Viviani/Documents/GitHub/M2-maskedprimingBilinguals/stimoli/OSC.txt', header = T)-> db
colnames(db) <- c('Prime.x', 'OSC')
head(masterFile)
head(db)
subset(masterFile, masterFile$Language=='eng')-> masterfileEng_OSC
summary(masterfileEng_OSC)


merge(masterfileEng_OSC, db, by='Prime.x', all.x=T)-> masterfileEng_OSC
colnames(masterfileEng_OSC) <- c("Prime","Target","Subject","Age","Gender", "Handedness","Rotation",
                                 "Data","OraStart" ,"TrialCount","TrialType","Relatedness","rt","Resp","Trial.ID","Target.ID","Lexicality","Morphtype","Primetype",
                                 "Logfreq.Zipf.t","Lent","Lenp","Logfreq.Zipf.p","Nt","Np","Language","Accuracy","phoneticFluency","phoneticComprehension","morphComprehension",
                                 "spelling","readingComprehension","vocabulary","oralComprehension","AoA1" ,
                                 "AoA2","AoA3","AoA4","AoA5","AoA6", "AoA7","AoA8","AoA9","OSC_Primes")
colnames(db) <- c('Target', 'OSC')
merge(masterfileEng_OSC, db, by='Target', all.x=T)-> masterfileEng_OSC

colnames(masterfileEng_OSC) <- c("Prime","Target","Subject","Age","Gender", "Handedness","Rotation",
                                 "Data","OraStart" ,"TrialCount","TrialType","Relatedness","rt","Resp","Trial.ID","Target.ID","Lexicality","Morphtype","Primetype",
                                 "Logfreq.Zipf.t","Lent","Lenp","Logfreq.Zipf.p","Nt","Np","Language","Accuracy","phoneticFluency","phoneticComprehension","morphComprehension",
                                 "spelling","readingComprehension","vocabulary","oralComprehension","AoA1" ,
                                 "AoA2","AoA3","AoA4","AoA5","AoA6", "AoA7","AoA8","AoA9","OSC_Primes","OSC_Target")
summary(masterfileEng_OSC)

rm(db)
#---------------------------------------------------------------------------------------------------#
#                                          END                                                      #
#---------------------------------------------------------------------------------------------------#
