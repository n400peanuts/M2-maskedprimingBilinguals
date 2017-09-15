# ---- Preprocessing of bilingual masked priming experiment
# ---- The final output are two files called "masterFile" in which there are all the raw data of the experiment
# ---- and "masterfileEng_OSC" with the OSC parameter
#----- 08/09/2017
#Clean the workspace
rm(list = ls())

#Set your own working directory. This should be (and is assumed to be in the rest of the code) the highest point in the gitHub folder:
setwd('C:/Users/Eva Viviani/Documents/GitHub/M2-maskedprimingBilinguals');
localGitDir <- 'C:/Users/Eva Viviani/Documents/GitHub/M2-maskedprimingBilinguals';


#---------------------------------------------------------------------------------------------------#
#                               CONCATENATE ALL THE SBJ ENG                                         #
#---------------------------------------------------------------------------------------------------#
rotations <- read.table(paste(localGitDir,'/stimoli/rotations.txt', sep=''), header = T, dec = ',');
finalNumberofParticipants <- 80

eng = NULL
for (j in 1:finalNumberofParticipants){
  if ((j %% 2) ==0 ){ #EVEN SUBJ
    boh <- paste(localGitDir, '/data/raw data/ENG/Output_MPrim_Eng_Subj_', j, ".txt", sep = "")
    if (file.exists(boh)) {
      pilot_eng <- read.table(boh, header = F, skip = 15,dec = ",")
      colnames(pilot_eng) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
      rot_B_eng <- subset(rotations, rotations$Rotation=='B' & rotations$Language=='eng')
      merge(pilot_eng, rot_B_eng, by = "Target", all.x = T)-> pilot_eng
    } else {next}
  } else { #ODD
    boh <- paste(localGitDir, '/data/raw data/ENG/Output_MPrim_Eng_Subj_', j, ".txt", sep = "")
    if (file.exists(boh)) {
      pilot_eng <- read.table(boh, header = F, skip = 15,dec = ",")
      colnames(pilot_eng) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
      rot_A_eng <- subset(rotations, rotations$Rotation=='A' & rotations$Language=='eng')
      merge(pilot_eng, rot_A_eng, by = "Target", all.x = T)-> pilot_eng
  } else {next}
  }

  pilot_eng$Prime.y <- NULL
  pilot_eng$rt <- as.numeric(pilot_eng$rt)
  pilot_eng$Rotation.y <- NULL
  
  eng <- rbind(eng,pilot_eng)
}

rm(rot_A_eng, rot_B_eng, pilot_eng, boh, j)
#---------------------------------------------------------------------------------------------------#
#                               CONCATENATE ALL THE SBJ ITA                                         #
#---------------------------------------------------------------------------------------------------#
#                                            SS1                                                    #
ss1 <- read.table(paste(localGitDir,'/data/raw data/ITA/Output_MPrim_Ita_Subj_1.txt', sep=''), header = F,  skip = 15, dec = ",")
colnames(ss1) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
rotA<- subset(rotations, rotations$Rotation=='A' & rotations$Language=='ita')
merge(ss1, rotA, by = "Target", all.x = T)->ss1
ss1$Prime.y <- NULL
ss1$Rotation.y <- NULL
summary(ss1) 

#                                            SS2                                                    #
ss2 <- read.table(paste(localGitDir,'/data/raw data/ITA/Output_MPrim_Ita_Subj_2.txt', sep=''), header = F,  skip = 15, dec = ",")
colnames(ss2) <- c("Subject", "Age", "Gender", "Handedness", "Rotation.x","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
rotB1<- read.table(paste(localGitDir,'/Stimoli/rot_B_persoggetto2.txt', sep=''), header = T, dec = ",")
colnames(rotB1) <- c("Trial.ID", "Target.ID", "Lexicality", "Morphtype", "Target", "Prime", "Primetype", "Logfreq.Zipf.t", "Lent", "Lenp", "Logfreq.Zipf.p", "Nt", "Np")
merge(ss2, rotB1, by = "Target", all.x = T)->ss2
ss2$Prime.y <- NULL
ss2$Language <- c('ita')
ss2$Language <- as.factor(ss2$Language)
summary(ss2) 

#                                       FROM SS3 TO SS40                                           #                        

ita = NULL
for (j in 3:finalNumberofParticipants){
  if ((j %% 2) ==0){ #EVEN SUBJ
    boh <- paste(localGitDir, '/data/raw data/ITA/Output_MPrim_Ita_Subj_', j, ".txt", sep = "")
    if (file.exists(boh)) {
      pilot_ita <- read.table(boh, header = F, skip = 15,dec = ",")
      colnames(pilot_ita) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
      rotB<- subset(rotations, rotations$Rotation=='B' & rotations$Language=='ita')
      merge(pilot_ita, rotB, by = "Target", all.x = T)-> pilot_ita
    } else {next}
  } else { #ODD
    boh <- paste(localGitDir, '/data/raw data/ITA/Output_MPrim_Ita_Subj_', j, ".txt", sep = "")
    if (file.exists(boh)) {
    pilot_ita <- read.table(boh, header = F, skip = 15,dec = ",")
    colnames(pilot_ita) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
    rotA<- subset(rotations, rotations$Rotation=='A' & rotations$Language=='ita')
    merge(pilot_ita, rotA, by = "Target", all.x = T)-> pilot_ita
   } else {next}
  }
  pilot_ita$Prime.y <- NULL
  pilot_ita$rt <- as.numeric(pilot_ita$rt)
  pilot_ita$Rotation.y <- NULL
  
  ita <- rbind(ita,pilot_ita)
}

ita<- rbind(ss2,ita)
ita <- rbind(ss1,ita)

rm(rotA, rotations, pilot_ita, ss1, ss2, rotB, rotB1, boh, j)
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
masterFile$Accuracy[ masterFile$Resp==-1] <- 0; 
masterFile$Accuracy[ masterFile$Resp==0] <- 0; 


rm(eng, ita)
#---------------------------------------------------------------------------------------------------#
#                                          END                                                      #
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#                       ADD RESULTS OF PROFICIENCY TEST TO THE MASTERFILE                           #
#---------------------------------------------------------------------------------------------------#
#                 ss1
masterFile$phoneticFluency[masterFile$Subject==1] <- c(15)
masterFile$phoneticComprehension[masterFile$Subject==1] <- c(8)
masterFile$morphComprehension[masterFile$Subject==1] <- c(10)
masterFile$spelling[masterFile$Subject==1] <- c(10)
masterFile$readingComprehension[masterFile$Subject==1] <- c(4)
masterFile$vocabulary[masterFile$Subject==1] <- c(11)
masterFile$oralComprehension[masterFile$Subject==1] <- c(5)

#                 ss2
masterFile$phoneticFluency[masterFile$Subject==2] <- c(11)
masterFile$phoneticComprehension[masterFile$Subject==2] <- c(1)
masterFile$morphComprehension[masterFile$Subject==2] <- c(8)
masterFile$spelling[masterFile$Subject==2] <- c(1)
masterFile$readingComprehension[masterFile$Subject==2] <- c(1)
masterFile$vocabulary[masterFile$Subject==2] <- c(8)
masterFile$oralComprehension[masterFile$Subject==2] <- c(2)

#                 ss3
masterFile$phoneticFluency[masterFile$Subject==3] <- c(32)
masterFile$phoneticComprehension[masterFile$Subject==3] <- c(7)
masterFile$morphComprehension[masterFile$Subject==3] <- c(10)
masterFile$spelling[masterFile$Subject==3] <- c(14)
masterFile$readingComprehension[masterFile$Subject==3] <- c(6)
masterFile$vocabulary[masterFile$Subject==3] <- c(19)
masterFile$oralComprehension[masterFile$Subject==3] <- c(6)

#                 ss4
masterFile$phoneticFluency[masterFile$Subject==4] <- c(39)
masterFile$phoneticComprehension[masterFile$Subject==4] <- c(8)
masterFile$morphComprehension[masterFile$Subject==4] <- c(10)
masterFile$spelling[masterFile$Subject==4] <- c(17)
masterFile$readingComprehension[masterFile$Subject==4] <- c(4)
masterFile$vocabulary[masterFile$Subject==4] <- c(19)
masterFile$oralComprehension[masterFile$Subject==4] <- c(6)

#                 ss5
masterFile$phoneticFluency[masterFile$Subject==5] <- c(13)
masterFile$phoneticComprehension[masterFile$Subject==5] <- c(10)
masterFile$morphComprehension[masterFile$Subject==5] <- c(9)
masterFile$spelling[masterFile$Subject==5] <- c(10)
masterFile$readingComprehension[masterFile$Subject==5] <- c(6)
masterFile$vocabulary[masterFile$Subject==5] <- c(19)
masterFile$oralComprehension[masterFile$Subject==5] <- c(4)

#                 ss6
masterFile$phoneticFluency[masterFile$Subject==6] <- c(45)
masterFile$phoneticComprehension[masterFile$Subject==6] <- c(10)
masterFile$morphComprehension[masterFile$Subject==6] <- c(10)
masterFile$spelling[masterFile$Subject==6] <- c(18)
masterFile$readingComprehension[masterFile$Subject==6] <- c(7)
masterFile$vocabulary[masterFile$Subject==6] <- c(16)
masterFile$oralComprehension[masterFile$Subject==6] <- c(6)

#                 ss7
masterFile$phoneticFluency[masterFile$Subject==7] <- c(22)
masterFile$phoneticComprehension[masterFile$Subject==7] <- c(9)
masterFile$morphComprehension[masterFile$Subject==7] <- c(10)
masterFile$spelling[masterFile$Subject==7] <- c(7)
masterFile$readingComprehension[masterFile$Subject==7] <- c(2)
masterFile$vocabulary[masterFile$Subject==7] <- c(15)
masterFile$oralComprehension[masterFile$Subject==7] <- c(6)

#                 ss8
masterFile$phoneticFluency[masterFile$Subject==8] <- c(26)
masterFile$phoneticComprehension[masterFile$Subject==8] <- c(5)
masterFile$morphComprehension[masterFile$Subject==8] <- c(10)
masterFile$spelling[masterFile$Subject==8] <- c(11)
masterFile$readingComprehension[masterFile$Subject==8] <- c(4)
masterFile$vocabulary[masterFile$Subject==8] <- c(16)
masterFile$oralComprehension[masterFile$Subject==8] <- c(4)

#                 ss9
masterFile$phoneticFluency[masterFile$Subject==9] <- c(28)
masterFile$phoneticComprehension[masterFile$Subject==9] <- c(13)
masterFile$morphComprehension[masterFile$Subject==9] <- c(10)
masterFile$spelling[masterFile$Subject==9] <- c(12)
masterFile$readingComprehension[masterFile$Subject==9] <- c(4)
masterFile$vocabulary[masterFile$Subject==9] <- c(17)
masterFile$oralComprehension[masterFile$Subject==9] <- c(6)

#                 ss10
masterFile$phoneticFluency[masterFile$Subject==10] <- c(11)
masterFile$phoneticComprehension[masterFile$Subject==10] <- c(8)
masterFile$morphComprehension[masterFile$Subject==10] <- c(8)
masterFile$spelling[masterFile$Subject==10] <- c(10)
masterFile$readingComprehension[masterFile$Subject==10] <- c(6)
masterFile$vocabulary[masterFile$Subject==10] <- c(15)
masterFile$oralComprehension[masterFile$Subject==10] <- c(3)

#                 ss11
masterFile$phoneticFluency[masterFile$Subject==11] <- c(34)
masterFile$phoneticComprehension[masterFile$Subject==11] <- c(12)
masterFile$morphComprehension[masterFile$Subject==11] <- c(8)
masterFile$spelling[masterFile$Subject==11] <- c(3)
masterFile$readingComprehension[masterFile$Subject==11] <- c(2)
masterFile$vocabulary[masterFile$Subject==11] <- c(13)
masterFile$oralComprehension[masterFile$Subject==11] <- c(3)

#                 ss12
masterFile$phoneticFluency[masterFile$Subject==12] <- c(13)
masterFile$phoneticComprehension[masterFile$Subject==12] <- c(6)
masterFile$morphComprehension[masterFile$Subject==12] <- c(6)
masterFile$spelling[masterFile$Subject==12] <- c(1)
masterFile$readingComprehension[masterFile$Subject==12] <- c(4)
masterFile$vocabulary[masterFile$Subject==12] <- c(8)
masterFile$oralComprehension[masterFile$Subject==12] <- c(4)

#                 ss13
masterFile$phoneticFluency[masterFile$Subject==13] <- c(2)
masterFile$phoneticComprehension[masterFile$Subject==13] <- c(8)
masterFile$morphComprehension[masterFile$Subject==13] <- c(8)
masterFile$spelling[masterFile$Subject==13] <- c(1)
masterFile$readingComprehension[masterFile$Subject==13] <- c(4)
masterFile$vocabulary[masterFile$Subject==13] <- c(12)
masterFile$oralComprehension[masterFile$Subject==13] <- c(3)

#                 ss14
masterFile$phoneticFluency[masterFile$Subject==14] <- c(24)
masterFile$phoneticComprehension[masterFile$Subject==14] <- c(8)
masterFile$morphComprehension[masterFile$Subject==14] <- c(9)
masterFile$spelling[masterFile$Subject==14] <- c(4)
masterFile$readingComprehension[masterFile$Subject==14] <- c(1)
masterFile$vocabulary[masterFile$Subject==14] <- c(16)
masterFile$oralComprehension[masterFile$Subject==14] <- c(5)

#                 ss15
masterFile$phoneticFluency[masterFile$Subject==15] <- c(22)
masterFile$phoneticComprehension[masterFile$Subject==15] <- c(8)
masterFile$morphComprehension[masterFile$Subject==15] <- c(8)
masterFile$spelling[masterFile$Subject==15] <- c(2)
masterFile$readingComprehension[masterFile$Subject==15] <- c(4)
masterFile$vocabulary[masterFile$Subject==15] <- c(15)
masterFile$oralComprehension[masterFile$Subject==15] <- c(4)

#                 ss16
masterFile$phoneticFluency[masterFile$Subject==16] <- c(30)
masterFile$phoneticComprehension[masterFile$Subject==16] <- c(6)
masterFile$morphComprehension[masterFile$Subject==16] <- c(9)
masterFile$spelling[masterFile$Subject==16] <- c(5)
masterFile$readingComprehension[masterFile$Subject==16] <- c(6)
masterFile$vocabulary[masterFile$Subject==16] <- c(15)
masterFile$oralComprehension[masterFile$Subject==16] <- c(2)

#                 ss17
masterFile$phoneticFluency[masterFile$Subject==17] <- c(22)
masterFile$phoneticComprehension[masterFile$Subject==17] <- c(12)
masterFile$morphComprehension[masterFile$Subject==17] <- c(10)
masterFile$spelling[masterFile$Subject==17] <- c(10)
masterFile$readingComprehension[masterFile$Subject==17] <- c(6)
masterFile$vocabulary[masterFile$Subject==17] <- c(14)
masterFile$oralComprehension[masterFile$Subject==17] <- c(6)

#                 ss18
masterFile$phoneticFluency[masterFile$Subject==18] <- c(39)
masterFile$phoneticComprehension[masterFile$Subject==18] <- c(9)
masterFile$morphComprehension[masterFile$Subject==18] <- c(10)
masterFile$spelling[masterFile$Subject==18] <- c(12)
masterFile$readingComprehension[masterFile$Subject==18] <- c(6)
masterFile$vocabulary[masterFile$Subject==18] <- c(18)
masterFile$oralComprehension[masterFile$Subject==18] <- c(4)

#                 ss19
masterFile$phoneticFluency[masterFile$Subject==19] <- c(8)
masterFile$phoneticComprehension[masterFile$Subject==19] <- c(12)
masterFile$morphComprehension[masterFile$Subject==19] <- c(9)
masterFile$spelling[masterFile$Subject==19] <- c(11)
masterFile$readingComprehension[masterFile$Subject==19] <- c(2)
masterFile$vocabulary[masterFile$Subject==19] <- c(17)
masterFile$oralComprehension[masterFile$Subject==19] <- c(5)

#                 ss20
masterFile$phoneticFluency[masterFile$Subject==20] <- c(24)
masterFile$phoneticComprehension[masterFile$Subject==20] <- c(13)
masterFile$morphComprehension[masterFile$Subject==20] <- c(10)
masterFile$spelling[masterFile$Subject==20] <- c(16)
masterFile$readingComprehension[masterFile$Subject==20] <- c(6)
masterFile$vocabulary[masterFile$Subject==20] <- c(19)
masterFile$oralComprehension[masterFile$Subject==20] <- c(6)

#                 ss21
masterFile$phoneticFluency[masterFile$Subject==21] <- c(19)
masterFile$phoneticComprehension[masterFile$Subject==21] <- c(9)
masterFile$morphComprehension[masterFile$Subject==21] <- c(9)
masterFile$spelling[masterFile$Subject==21] <- c(8)
masterFile$readingComprehension[masterFile$Subject==21] <- c(4)
masterFile$vocabulary[masterFile$Subject==21] <- c(15)
masterFile$oralComprehension[masterFile$Subject==21] <- c(5)

#                 ss22
masterFile$phoneticFluency[masterFile$Subject==22] <- c(25)
masterFile$phoneticComprehension[masterFile$Subject==22] <- c(6)
masterFile$morphComprehension[masterFile$Subject==22] <- c(10)
masterFile$spelling[masterFile$Subject==22] <- c(7)
masterFile$readingComprehension[masterFile$Subject==22] <- c(5)
masterFile$vocabulary[masterFile$Subject==22] <- c(17)
masterFile$oralComprehension[masterFile$Subject==22] <- c(6)

#                 ss23
masterFile$phoneticFluency[masterFile$Subject==23] <- c(23)
masterFile$phoneticComprehension[masterFile$Subject==23] <- c(6)
masterFile$morphComprehension[masterFile$Subject==23] <- c(10)
masterFile$spelling[masterFile$Subject==23] <- c(7)
masterFile$readingComprehension[masterFile$Subject==23] <- c(2)
masterFile$vocabulary[masterFile$Subject==23] <- c(16)
masterFile$oralComprehension[masterFile$Subject==23] <- c(6)

#                 ss24
masterFile$phoneticFluency[masterFile$Subject==24] <- c(19)
masterFile$phoneticComprehension[masterFile$Subject==24] <- c(8)
masterFile$morphComprehension[masterFile$Subject==24] <- c(7)
masterFile$spelling[masterFile$Subject==24] <- c(3)
masterFile$readingComprehension[masterFile$Subject==24] <- c(6)
masterFile$vocabulary[masterFile$Subject==24] <- c(15)
masterFile$oralComprehension[masterFile$Subject==24] <- c(3)

#                 ss25
masterFile$phoneticFluency[masterFile$Subject==25] <- c(16)
masterFile$phoneticComprehension[masterFile$Subject==25] <- c(3)
masterFile$morphComprehension[masterFile$Subject==25] <- c(9)
masterFile$spelling[masterFile$Subject==25] <- c(2)
masterFile$readingComprehension[masterFile$Subject==25] <- c(3)
masterFile$vocabulary[masterFile$Subject==25] <- c(13)
masterFile$oralComprehension[masterFile$Subject==25] <- c(5)

#                 ss26
masterFile$phoneticFluency[masterFile$Subject==26] <- c(29)
masterFile$phoneticComprehension[masterFile$Subject==26] <- c(7)
masterFile$morphComprehension[masterFile$Subject==26] <- c(10)
masterFile$spelling[masterFile$Subject==26] <- c(10)
masterFile$readingComprehension[masterFile$Subject==26] <- c(4)
masterFile$vocabulary[masterFile$Subject==26] <- c(15)
masterFile$oralComprehension[masterFile$Subject==26] <- c(5)

#                 ss27
masterFile$phoneticFluency[masterFile$Subject==27] <- c(24)
masterFile$phoneticComprehension[masterFile$Subject==27] <- c(8)
masterFile$morphComprehension[masterFile$Subject==27] <- c(10)
masterFile$spelling[masterFile$Subject==27] <- c(9)
masterFile$readingComprehension[masterFile$Subject==27] <- c(5)
masterFile$vocabulary[masterFile$Subject==27] <- c(16)
masterFile$oralComprehension[masterFile$Subject==27] <- c(5)

#                 ss28
masterFile$phoneticFluency[masterFile$Subject==28] <- c(18)
masterFile$phoneticComprehension[masterFile$Subject==28] <- c(6)
masterFile$morphComprehension[masterFile$Subject==28] <- c(6)
masterFile$spelling[masterFile$Subject==28] <- c(3)
masterFile$readingComprehension[masterFile$Subject==28] <- c(3)
masterFile$vocabulary[masterFile$Subject==28] <- c(15)
masterFile$oralComprehension[masterFile$Subject==28] <- c(3)

#                 ss29
masterFile$phoneticFluency[masterFile$Subject==29] <- c(27)
masterFile$phoneticComprehension[masterFile$Subject==29] <- c(9)
masterFile$morphComprehension[masterFile$Subject==29] <- c(10)
masterFile$spelling[masterFile$Subject==29] <- c(15)
masterFile$readingComprehension[masterFile$Subject==29] <- c(6)
masterFile$vocabulary[masterFile$Subject==29] <- c(19)
masterFile$oralComprehension[masterFile$Subject==29] <- c(6)

#                 ss30
masterFile$phoneticFluency[masterFile$Subject==30] <- c(13)
masterFile$phoneticComprehension[masterFile$Subject==30] <- c(7)
masterFile$morphComprehension[masterFile$Subject==30] <- c(7)
masterFile$spelling[masterFile$Subject==30] <- c(5)
masterFile$readingComprehension[masterFile$Subject==30] <- c(2)
masterFile$vocabulary[masterFile$Subject==30] <- c(15)
masterFile$oralComprehension[masterFile$Subject==30] <- c(2)

#                 ss31
masterFile$phoneticFluency[masterFile$Subject==31] <- c(12)
masterFile$phoneticComprehension[masterFile$Subject==31] <- c(8)
masterFile$morphComprehension[masterFile$Subject==31] <- c(8)
masterFile$spelling[masterFile$Subject==31] <- c(4)
masterFile$readingComprehension[masterFile$Subject==31] <- c(1)
masterFile$vocabulary[masterFile$Subject==31] <- c(17)
masterFile$oralComprehension[masterFile$Subject==31] <- c(2)

#                 ss32
masterFile$phoneticFluency[masterFile$Subject==32] <- c(20)
masterFile$phoneticComprehension[masterFile$Subject==32] <- c(11)
masterFile$morphComprehension[masterFile$Subject==32] <- c(10)
masterFile$spelling[masterFile$Subject==32] <- c(10)
masterFile$readingComprehension[masterFile$Subject==32] <- c(6)
masterFile$vocabulary[masterFile$Subject==32] <- c(15)
masterFile$oralComprehension[masterFile$Subject==32] <- c(4)

#                 ss33
masterFile$phoneticFluency[masterFile$Subject==33] <- c(8)
masterFile$phoneticComprehension[masterFile$Subject==33] <- c(5)
masterFile$morphComprehension[masterFile$Subject==33] <- c(8)
masterFile$spelling[masterFile$Subject==33] <- c(1)
masterFile$readingComprehension[masterFile$Subject==33] <- c(1)
masterFile$vocabulary[masterFile$Subject==33] <- c(13)
masterFile$oralComprehension[masterFile$Subject==33] <- c(2)

#                 ss34
masterFile$phoneticFluency[masterFile$Subject==34] <- c(34)
masterFile$phoneticComprehension[masterFile$Subject==34] <- c(11)
masterFile$morphComprehension[masterFile$Subject==34] <- c(10)
masterFile$spelling[masterFile$Subject==34] <- c(17)
masterFile$readingComprehension[masterFile$Subject==34] <- c(7)
masterFile$vocabulary[masterFile$Subject==34] <- c(19)
masterFile$oralComprehension[masterFile$Subject==34] <- c(6)

#                 ss35
masterFile$phoneticFluency[masterFile$Subject==35] <- c(10)
masterFile$phoneticComprehension[masterFile$Subject==35] <- c(3)
masterFile$morphComprehension[masterFile$Subject==35] <- c(5)
masterFile$spelling[masterFile$Subject==35] <- c(2)
masterFile$readingComprehension[masterFile$Subject==35] <- c(1)
masterFile$vocabulary[masterFile$Subject==35] <- c(15)
masterFile$oralComprehension[masterFile$Subject==35] <- c(2)

#                 ss36
masterFile$phoneticFluency[masterFile$Subject==36] <- c(31)
masterFile$phoneticComprehension[masterFile$Subject==36] <- c(8)
masterFile$morphComprehension[masterFile$Subject==36] <- c(8)
masterFile$spelling[masterFile$Subject==36] <- c(8)
masterFile$readingComprehension[masterFile$Subject==36] <- c(4)
masterFile$vocabulary[masterFile$Subject==36] <- c(15)
masterFile$oralComprehension[masterFile$Subject==36] <- c(5)

#                 ss37
masterFile$phoneticFluency[masterFile$Subject==37] <- c(13)
masterFile$phoneticComprehension[masterFile$Subject==37] <- c(10)
masterFile$morphComprehension[masterFile$Subject==37] <- c(7)
masterFile$spelling[masterFile$Subject==37] <- c(5)
masterFile$readingComprehension[masterFile$Subject==37] <- c(5)
masterFile$vocabulary[masterFile$Subject==37] <- c(13)
masterFile$oralComprehension[masterFile$Subject==37] <- c(4)

#                 ss38
masterFile$phoneticFluency[masterFile$Subject==38] <- c(10)
masterFile$phoneticComprehension[masterFile$Subject==38] <- c(12)
masterFile$morphComprehension[masterFile$Subject==38] <- c(8)
masterFile$spelling[masterFile$Subject==38] <- c(6)
masterFile$readingComprehension[masterFile$Subject==38] <- c(5)
masterFile$vocabulary[masterFile$Subject==38] <- c(13)
masterFile$oralComprehension[masterFile$Subject==38] <- c(3)

#                 ss39
masterFile$phoneticFluency[masterFile$Subject==39] <- c(17)
masterFile$phoneticComprehension[masterFile$Subject==39] <- c(0)
masterFile$morphComprehension[masterFile$Subject==39] <- c(4)
masterFile$spelling[masterFile$Subject==39] <- c(0)
masterFile$readingComprehension[masterFile$Subject==39] <- c(2)
masterFile$vocabulary[masterFile$Subject==39] <- c(8)
masterFile$oralComprehension[masterFile$Subject==39] <- c(1)

#                 ss40
masterFile$phoneticFluency[masterFile$Subject==40] <- c(24)
masterFile$phoneticComprehension[masterFile$Subject==40] <- c(12)
masterFile$morphComprehension[masterFile$Subject==40] <- c(10)
masterFile$spelling[masterFile$Subject==40] <- c(7)
masterFile$readingComprehension[masterFile$Subject==40] <- c(5)
masterFile$vocabulary[masterFile$Subject==40] <- c(16)
masterFile$oralComprehension[masterFile$Subject==40] <- c(5)

#                 ss41
masterFile$phoneticFluency[masterFile$Subject==41] <- c(29)
masterFile$phoneticComprehension[masterFile$Subject==41] <- c(8)
masterFile$morphComprehension[masterFile$Subject==41] <- c(10)
masterFile$spelling[masterFile$Subject==41] <- c(14)
masterFile$readingComprehension[masterFile$Subject==41] <- c(7)
masterFile$vocabulary[masterFile$Subject==41] <- c(18)
masterFile$oralComprehension[masterFile$Subject==41] <- c(6)

#                 ss42
masterFile$phoneticFluency[masterFile$Subject==42] <- c(21)
masterFile$phoneticComprehension[masterFile$Subject==42] <- c(9)
masterFile$morphComprehension[masterFile$Subject==42] <- c(9)
masterFile$spelling[masterFile$Subject==42] <- c(11)
masterFile$readingComprehension[masterFile$Subject==42] <- c(7)
masterFile$vocabulary[masterFile$Subject==42] <- c(18)
masterFile$oralComprehension[masterFile$Subject==42] <- c(6)

#                 ss43
masterFile$phoneticFluency[masterFile$Subject==43] <- c(17)
masterFile$phoneticComprehension[masterFile$Subject==43] <- c(6)
masterFile$morphComprehension[masterFile$Subject==43] <- c(7)
masterFile$spelling[masterFile$Subject==43] <- c(4)
masterFile$readingComprehension[masterFile$Subject==43] <- c(6)
masterFile$vocabulary[masterFile$Subject==43] <- c(10)
masterFile$oralComprehension[masterFile$Subject==43] <- c(4)

#                 ss44
masterFile$phoneticFluency[masterFile$Subject==44] <- c(15)
masterFile$phoneticComprehension[masterFile$Subject==44] <- c(10)
masterFile$morphComprehension[masterFile$Subject==44] <- c(5)
masterFile$spelling[masterFile$Subject==44] <- c(2)
masterFile$readingComprehension[masterFile$Subject==44] <- c(5)
masterFile$vocabulary[masterFile$Subject==44] <- c(12)
masterFile$oralComprehension[masterFile$Subject==44] <- c(1)

#                 ss45
masterFile$phoneticFluency[masterFile$Subject==45] <- c(21)
masterFile$phoneticComprehension[masterFile$Subject==45] <- c(7)
masterFile$morphComprehension[masterFile$Subject==45] <- c(6)
masterFile$spelling[masterFile$Subject==45] <- c(2)
masterFile$readingComprehension[masterFile$Subject==45] <- c(6)
masterFile$vocabulary[masterFile$Subject==45] <- c(12)
masterFile$oralComprehension[masterFile$Subject==45] <- c(3)

#                 ss46
masterFile$phoneticFluency[masterFile$Subject==46] <- c(18)
masterFile$phoneticComprehension[masterFile$Subject==46] <- c(9)
masterFile$morphComprehension[masterFile$Subject==46] <- c(8)
masterFile$spelling[masterFile$Subject==46] <- c(10)
masterFile$readingComprehension[masterFile$Subject==46] <- c(4)
masterFile$vocabulary[masterFile$Subject==46] <- c(16)
masterFile$oralComprehension[masterFile$Subject==46] <- c(6)

#                 ss47
masterFile$phoneticFluency[masterFile$Subject==47] <- c(23)
masterFile$phoneticComprehension[masterFile$Subject==47] <- c(10)
masterFile$morphComprehension[masterFile$Subject==47] <- c(10)
masterFile$spelling[masterFile$Subject==47] <- c(9)
masterFile$readingComprehension[masterFile$Subject==47] <- c(4)
masterFile$vocabulary[masterFile$Subject==47] <- c(15)
masterFile$oralComprehension[masterFile$Subject==47] <- c(6)


#                 ss48
masterFile$phoneticFluency[masterFile$Subject==48] <- c(27)
masterFile$phoneticComprehension[masterFile$Subject==48] <- c(7)
masterFile$morphComprehension[masterFile$Subject==48] <- c(9)
masterFile$spelling[masterFile$Subject==48] <- c(2)
masterFile$readingComprehension[masterFile$Subject==48] <- c(1)
masterFile$vocabulary[masterFile$Subject==48] <- c(12)
masterFile$oralComprehension[masterFile$Subject==48] <- c(1)

#                 ss49
masterFile$phoneticFluency[masterFile$Subject==49] <- c(36)
masterFile$phoneticComprehension[masterFile$Subject==49] <- c(11)
masterFile$morphComprehension[masterFile$Subject==49] <- c(10)
masterFile$spelling[masterFile$Subject==49] <- c(18)
masterFile$readingComprehension[masterFile$Subject==49] <- c(6)
masterFile$vocabulary[masterFile$Subject==49] <- c(19)
masterFile$oralComprehension[masterFile$Subject==49] <- c(6)

#                 ss50
masterFile$phoneticFluency[masterFile$Subject==50] <- c(27)
masterFile$phoneticComprehension[masterFile$Subject==50] <- c(8)
masterFile$morphComprehension[masterFile$Subject==50] <- c(10)
masterFile$spelling[masterFile$Subject==50] <- c(6)
masterFile$readingComprehension[masterFile$Subject==50] <- c(6)
masterFile$vocabulary[masterFile$Subject==50] <- c(14)
masterFile$oralComprehension[masterFile$Subject==50] <- c(6)

#                 ss51
masterFile$phoneticFluency[masterFile$Subject==51] <- c(23)
masterFile$phoneticComprehension[masterFile$Subject==51] <- c(8)
masterFile$morphComprehension[masterFile$Subject==51] <- c(9)
masterFile$spelling[masterFile$Subject==51] <- c(3)
masterFile$readingComprehension[masterFile$Subject==51] <- c(3)
masterFile$vocabulary[masterFile$Subject==51] <- c(11)
masterFile$oralComprehension[masterFile$Subject==51] <- c(3)

#                 ss52
masterFile$phoneticFluency[masterFile$Subject==52] <- c(16)
masterFile$phoneticComprehension[masterFile$Subject==52] <- c(9)
masterFile$morphComprehension[masterFile$Subject==52] <- c(8)
masterFile$spelling[masterFile$Subject==52] <- c(8)
masterFile$readingComprehension[masterFile$Subject==52] <- c(3)
masterFile$vocabulary[masterFile$Subject==52] <- c(14)
masterFile$oralComprehension[masterFile$Subject==52] <- c(5)

#                 ss53
masterFile$phoneticFluency[masterFile$Subject==53] <- c(27)
masterFile$phoneticComprehension[masterFile$Subject==53] <- c(9)
masterFile$morphComprehension[masterFile$Subject==53] <- c(10)
masterFile$spelling[masterFile$Subject==53] <- c(16)
masterFile$readingComprehension[masterFile$Subject==53] <- c(4)
masterFile$vocabulary[masterFile$Subject==53] <- c(19)
masterFile$oralComprehension[masterFile$Subject==53] <- c(5)

#                 ss54
masterFile$phoneticFluency[masterFile$Subject==54] <- c(22)
masterFile$phoneticComprehension[masterFile$Subject==54] <- c(10)
masterFile$morphComprehension[masterFile$Subject==54] <- c(8)
masterFile$spelling[masterFile$Subject==54] <- c(7)
masterFile$readingComprehension[masterFile$Subject==54] <- c(2)
masterFile$vocabulary[masterFile$Subject==54] <- c(16)
masterFile$oralComprehension[masterFile$Subject==54] <- c(2)

#                 ss55
masterFile$phoneticFluency[masterFile$Subject==55] <- c(0)
masterFile$phoneticComprehension[masterFile$Subject==55] <- c(8)
masterFile$morphComprehension[masterFile$Subject==55] <- c(6)
masterFile$spelling[masterFile$Subject==55] <- c(4)
masterFile$readingComprehension[masterFile$Subject==55] <- c(3)
masterFile$vocabulary[masterFile$Subject==55] <- c(14)
masterFile$oralComprehension[masterFile$Subject==55] <- c(3)

#                 ss56
masterFile$phoneticFluency[masterFile$Subject==56] <- c(19)
masterFile$phoneticComprehension[masterFile$Subject==56] <- c(12)
masterFile$morphComprehension[masterFile$Subject==56] <- c(9)
masterFile$spelling[masterFile$Subject==56] <- c(4)
masterFile$readingComprehension[masterFile$Subject==56] <- c(4)
masterFile$vocabulary[masterFile$Subject==56] <- c(13)
masterFile$oralComprehension[masterFile$Subject==56] <- c(5)

#                 ss57
masterFile$phoneticFluency[masterFile$Subject==57] <- c(26)
masterFile$phoneticComprehension[masterFile$Subject==57] <- c(8)
masterFile$morphComprehension[masterFile$Subject==57] <- c(9)
masterFile$spelling[masterFile$Subject==57] <- c(9)
masterFile$readingComprehension[masterFile$Subject==57] <- c(6)
masterFile$vocabulary[masterFile$Subject==57] <- c(17)
masterFile$oralComprehension[masterFile$Subject==57] <- c(6)

#                 ss58
masterFile$phoneticFluency[masterFile$Subject==58] <- c(18)
masterFile$phoneticComprehension[masterFile$Subject==58] <- c(10)
masterFile$morphComprehension[masterFile$Subject==58] <- c(8)
masterFile$spelling[masterFile$Subject==58] <- c(7)
masterFile$readingComprehension[masterFile$Subject==58] <- c(5)
masterFile$vocabulary[masterFile$Subject==58] <- c(16)
masterFile$oralComprehension[masterFile$Subject==58] <- c(4)

#                 ss59
masterFile$phoneticFluency[masterFile$Subject==59] <- c(26)
masterFile$phoneticComprehension[masterFile$Subject==59] <- c(12)
masterFile$morphComprehension[masterFile$Subject==59] <- c(10)
masterFile$spelling[masterFile$Subject==59] <- c(14)
masterFile$readingComprehension[masterFile$Subject==59] <- c(6)
masterFile$vocabulary[masterFile$Subject==59] <- c(16)
masterFile$oralComprehension[masterFile$Subject==59] <- c(6)

#                 ss61
masterFile$phoneticFluency[masterFile$Subject==61] <- c(16)
masterFile$phoneticComprehension[masterFile$Subject==61] <- c(8)
masterFile$morphComprehension[masterFile$Subject==61] <- c(9)
masterFile$spelling[masterFile$Subject==61] <- c(0)
masterFile$readingComprehension[masterFile$Subject==61] <- c(6)
masterFile$vocabulary[masterFile$Subject==61] <- c(17)
masterFile$oralComprehension[masterFile$Subject==61] <- c(5)

#                 ss63
masterFile$phoneticFluency[masterFile$Subject==63] <- c(31)
masterFile$phoneticComprehension[masterFile$Subject==63] <- c(9)
masterFile$morphComprehension[masterFile$Subject==63] <- c(10)
masterFile$spelling[masterFile$Subject==63] <- c(7)
masterFile$readingComprehension[masterFile$Subject==63] <- c(6)
masterFile$vocabulary[masterFile$Subject==63] <- c(16)
masterFile$oralComprehension[masterFile$Subject==63] <- c(6)

#                 ss64
masterFile$phoneticFluency[masterFile$Subject==64] <- c(19)
masterFile$phoneticComprehension[masterFile$Subject==64] <- c(9)
masterFile$morphComprehension[masterFile$Subject==64] <- c(10)
masterFile$spelling[masterFile$Subject==64] <- c(6)
masterFile$readingComprehension[masterFile$Subject==64] <- c(6)
masterFile$vocabulary[masterFile$Subject==64] <- c(16)
masterFile$oralComprehension[masterFile$Subject==64] <- c(4)

#                 ss65
masterFile$phoneticFluency[masterFile$Subject==65] <- c(26)
masterFile$phoneticComprehension[masterFile$Subject==65] <- c(11)
masterFile$morphComprehension[masterFile$Subject==65] <- c(10)
masterFile$spelling[masterFile$Subject==65] <- c(12)
masterFile$readingComprehension[masterFile$Subject==65] <- c(5)
masterFile$vocabulary[masterFile$Subject==65] <- c(17)
masterFile$oralComprehension[masterFile$Subject==65] <- c(6)

#                 ss66
masterFile$phoneticFluency[masterFile$Subject==66] <- c(26)
masterFile$phoneticComprehension[masterFile$Subject==66] <- c(11)
masterFile$morphComprehension[masterFile$Subject==66] <- c(10)
masterFile$spelling[masterFile$Subject==66] <- c(8)
masterFile$readingComprehension[masterFile$Subject==66] <- c(6)
masterFile$vocabulary[masterFile$Subject==66] <- c(16)
masterFile$oralComprehension[masterFile$Subject==66] <- c(5)

#                 ss67
masterFile$phoneticFluency[masterFile$Subject==67] <- c(19)
masterFile$phoneticComprehension[masterFile$Subject==67] <- c(10)
masterFile$morphComprehension[masterFile$Subject==67] <- c(9)
masterFile$spelling[masterFile$Subject==67] <- c(7)
masterFile$readingComprehension[masterFile$Subject==67] <- c(5)
masterFile$vocabulary[masterFile$Subject==67] <- c(13)
masterFile$oralComprehension[masterFile$Subject==67] <- c(6)

#                 ss69
masterFile$phoneticFluency[masterFile$Subject==69] <- c(20)
masterFile$phoneticComprehension[masterFile$Subject==69] <- c(10)
masterFile$morphComprehension[masterFile$Subject==69] <- c(9)
masterFile$spelling[masterFile$Subject==69] <- c(12)
masterFile$readingComprehension[masterFile$Subject==69] <- c(7)
masterFile$vocabulary[masterFile$Subject==69] <- c(14)
masterFile$oralComprehension[masterFile$Subject==69] <- c(6)

#                 ss70
masterFile$phoneticFluency[masterFile$Subject==70] <- c(20)
masterFile$phoneticComprehension[masterFile$Subject==70] <- c(8)
masterFile$morphComprehension[masterFile$Subject==70] <- c(10)
masterFile$spelling[masterFile$Subject==70] <- c(12)
masterFile$readingComprehension[masterFile$Subject==70] <- c(4)
masterFile$vocabulary[masterFile$Subject==70] <- c(13)
masterFile$oralComprehension[masterFile$Subject==70] <- c(5)

#                 ss71
masterFile$phoneticFluency[masterFile$Subject==71] <- c(27)
masterFile$phoneticComprehension[masterFile$Subject==71] <- c(10)
masterFile$morphComprehension[masterFile$Subject==71] <- c(10)
masterFile$spelling[masterFile$Subject==71] <- c(14)
masterFile$readingComprehension[masterFile$Subject==71] <- c(4)
masterFile$vocabulary[masterFile$Subject==71] <- c(14)
masterFile$oralComprehension[masterFile$Subject==71] <- c(4)

#                 ss73
masterFile$phoneticFluency[masterFile$Subject==73] <- c(30)
masterFile$phoneticComprehension[masterFile$Subject==73] <- c(8)
masterFile$morphComprehension[masterFile$Subject==73] <- c(10)
masterFile$spelling[masterFile$Subject==73] <- c(11)
masterFile$readingComprehension[masterFile$Subject==73] <- c(6)
masterFile$vocabulary[masterFile$Subject==73] <- c(14)
masterFile$oralComprehension[masterFile$Subject==73] <- c(6)

#                 ss75
masterFile$phoneticFluency[masterFile$Subject==75] <- c(27)
masterFile$phoneticComprehension[masterFile$Subject==75] <- c(5)
masterFile$morphComprehension[masterFile$Subject==75] <- c(10)
masterFile$spelling[masterFile$Subject==75] <- c(9)
masterFile$readingComprehension[masterFile$Subject==75] <- c(3)
masterFile$vocabulary[masterFile$Subject==75] <- c(16)
masterFile$oralComprehension[masterFile$Subject==75] <- c(2)

#                 ss77
masterFile$phoneticFluency[masterFile$Subject==77] <- c(39)
masterFile$phoneticComprehension[masterFile$Subject==77] <- c(11)
masterFile$morphComprehension[masterFile$Subject==77] <- c(9)
masterFile$spelling[masterFile$Subject==77] <- c(13)
masterFile$readingComprehension[masterFile$Subject==77] <- c(3)
masterFile$vocabulary[masterFile$Subject==77] <- c(18)
masterFile$oralComprehension[masterFile$Subject==77] <- c(6)

#                 ss80
masterFile$phoneticFluency[masterFile$Subject==80] <- c(23)
masterFile$phoneticComprehension[masterFile$Subject==80] <- c(12)
masterFile$morphComprehension[masterFile$Subject==80] <- c(10)
masterFile$spelling[masterFile$Subject==80] <- c(7)
masterFile$readingComprehension[masterFile$Subject==80] <- c(6)
masterFile$vocabulary[masterFile$Subject==80] <- c(13)
masterFile$oralComprehension[masterFile$Subject==80] <- c(5)


#---------------------------------------------------------------------------------------------------#
#                                              END                                                  #
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#                             ADD RESULTS OF AoA SURVEY TO THE MASTERFILE                           #
#---------------------------------------------------------------------------------------------------#
#                 ss1
masterFile$AoA1[masterFile$Subject==1] <- c(6)
masterFile$AoA2[masterFile$Subject==1] <- c(1)
masterFile$AoA3[masterFile$Subject==1] <- c("school")
masterFile$AoA4[masterFile$Subject==1] <- c(1)
masterFile$AoA5[masterFile$Subject==1] <- c(2)
masterFile$AoA6[masterFile$Subject==1] <- c(1)
masterFile$AoA7[masterFile$Subject==1] <- c(3)
masterFile$AoA8[masterFile$Subject==1] <- c(2)
masterFile$AoA9[masterFile$Subject==1] <- c(2)


#                 ss2
masterFile$AoA1[masterFile$Subject==2] <- c(3)
masterFile$AoA2[masterFile$Subject==2] <- c(1)
masterFile$AoA3[masterFile$Subject==2] <- c("school")
masterFile$AoA4[masterFile$Subject==2] <- c(1)
masterFile$AoA5[masterFile$Subject==2] <- c(1)
masterFile$AoA6[masterFile$Subject==2] <- c(1)
masterFile$AoA7[masterFile$Subject==2] <- c(1)
masterFile$AoA8[masterFile$Subject==2] <- c(3)
masterFile$AoA9[masterFile$Subject==2] <- c(0)

#                 ss3
masterFile$AoA1[masterFile$Subject==3] <- c(4)
masterFile$AoA2[masterFile$Subject==3] <- c(2)
masterFile$AoA3[masterFile$Subject==3] <- c("home")
masterFile$AoA4[masterFile$Subject==3] <- c(1)
masterFile$AoA5[masterFile$Subject==3] <- c(2)
masterFile$AoA6[masterFile$Subject==3] <- c(1)
masterFile$AoA7[masterFile$Subject==3] <- c(4)
masterFile$AoA8[masterFile$Subject==3] <- c(3)
masterFile$AoA9[masterFile$Subject==3] <- c(0)

#                 ss4
masterFile$AoA1[masterFile$Subject==4] <- c(9)
masterFile$AoA2[masterFile$Subject==4] <- c(5)
masterFile$AoA3[masterFile$Subject==4] <- c("school")
masterFile$AoA4[masterFile$Subject==4] <- c(1)
masterFile$AoA5[masterFile$Subject==4] <- c(1)
masterFile$AoA6[masterFile$Subject==4] <- c(2)
masterFile$AoA7[masterFile$Subject==4] <- c(5)
masterFile$AoA8[masterFile$Subject==4] <- c(2)
masterFile$AoA9[masterFile$Subject==4] <- c(4)

#                 ss5
masterFile$AoA1[masterFile$Subject==5] <- c(6)
masterFile$AoA2[masterFile$Subject==5] <- c(3)
masterFile$AoA3[masterFile$Subject==5] <- c("home")
masterFile$AoA4[masterFile$Subject==5] <- c(1)
masterFile$AoA5[masterFile$Subject==5] <- c(1)
masterFile$AoA6[masterFile$Subject==5] <- c(1)
masterFile$AoA7[masterFile$Subject==5] <- c(3)
masterFile$AoA8[masterFile$Subject==5] <- c(2)
masterFile$AoA9[masterFile$Subject==5] <- c(2)

#                 ss6
masterFile$AoA1[masterFile$Subject==6] <- c(7)
masterFile$AoA2[masterFile$Subject==6] <- c(2)
masterFile$AoA3[masterFile$Subject==6] <- c("home")
masterFile$AoA4[masterFile$Subject==6] <- c(1)
masterFile$AoA5[masterFile$Subject==6] <- c(2)
masterFile$AoA6[masterFile$Subject==6] <- c(1)
masterFile$AoA7[masterFile$Subject==6] <- c(5)
masterFile$AoA8[masterFile$Subject==6] <- c(2)
masterFile$AoA9[masterFile$Subject==6] <- c(3)

#                 ss7
masterFile$AoA1[masterFile$Subject==7] <- c(8)
masterFile$AoA2[masterFile$Subject==7] <- c(3)
masterFile$AoA3[masterFile$Subject==7] <- c("school")
masterFile$AoA4[masterFile$Subject==7] <- c(1)
masterFile$AoA5[masterFile$Subject==7] <- c(2)
masterFile$AoA6[masterFile$Subject==7] <- c(1)
masterFile$AoA7[masterFile$Subject==7] <- c(3)
masterFile$AoA8[masterFile$Subject==7] <- c(2)
masterFile$AoA9[masterFile$Subject==7] <- c(1)

#                 ss8
masterFile$AoA1[masterFile$Subject==8] <- c(7)
masterFile$AoA2[masterFile$Subject==8] <- c(3)
masterFile$AoA3[masterFile$Subject==8] <- c("school")
masterFile$AoA4[masterFile$Subject==8] <- c(1)
masterFile$AoA5[masterFile$Subject==8] <- c(2)
masterFile$AoA6[masterFile$Subject==8] <- c(2)
masterFile$AoA7[masterFile$Subject==8] <- c(4)
masterFile$AoA8[masterFile$Subject==8] <- c(1)
masterFile$AoA9[masterFile$Subject==8] <- c(4)

#                 ss9
masterFile$AoA1[masterFile$Subject==9] <- c(0)
masterFile$AoA2[masterFile$Subject==9] <- c(5)
masterFile$AoA3[masterFile$Subject==9] <- c("home")
masterFile$AoA4[masterFile$Subject==9] <- c(1)
masterFile$AoA5[masterFile$Subject==9] <- c(1)
masterFile$AoA6[masterFile$Subject==9] <- c(1)
masterFile$AoA7[masterFile$Subject==9] <- c(5)
masterFile$AoA8[masterFile$Subject==9] <- c(2)
masterFile$AoA9[masterFile$Subject==9] <- c(1)

#                 ss10
masterFile$AoA1[masterFile$Subject==10] <- c(3)
masterFile$AoA2[masterFile$Subject==10] <- c(4)
masterFile$AoA3[masterFile$Subject==10] <- c("home")
masterFile$AoA4[masterFile$Subject==10] <- c(1)
masterFile$AoA5[masterFile$Subject==10] <- c(1)
masterFile$AoA6[masterFile$Subject==10] <- c(2)
masterFile$AoA7[masterFile$Subject==10] <- c(5)
masterFile$AoA8[masterFile$Subject==10] <- c(1)
masterFile$AoA9[masterFile$Subject==10] <- c(3)

#                 ss11
masterFile$AoA1[masterFile$Subject==11] <- c(9)
masterFile$AoA2[masterFile$Subject==11] <- c(3)
masterFile$AoA3[masterFile$Subject==11] <- c("school")
masterFile$AoA4[masterFile$Subject==11] <- c(1)
masterFile$AoA5[masterFile$Subject==11] <- c(2)
masterFile$AoA6[masterFile$Subject==11] <- c(1)
masterFile$AoA7[masterFile$Subject==11] <- c(3)
masterFile$AoA8[masterFile$Subject==11] <- c(2)
masterFile$AoA9[masterFile$Subject==11] <- c(5)

#                 ss12
masterFile$AoA1[masterFile$Subject==12] <- c(11)
masterFile$AoA2[masterFile$Subject==12] <- c(1)
masterFile$AoA3[masterFile$Subject==12] <- c("school")
masterFile$AoA4[masterFile$Subject==12] <- c(1)
masterFile$AoA5[masterFile$Subject==12] <- c(2)
masterFile$AoA6[masterFile$Subject==12] <- c(1)
masterFile$AoA7[masterFile$Subject==12] <- c(3)
masterFile$AoA8[masterFile$Subject==12] <- c(2)
masterFile$AoA9[masterFile$Subject==12] <- c(1)

#                 ss13
masterFile$AoA1[masterFile$Subject==13] <- c(6)
masterFile$AoA2[masterFile$Subject==13] <- c(1)
masterFile$AoA3[masterFile$Subject==13] <- c("school")
masterFile$AoA4[masterFile$Subject==13] <- c(1)
masterFile$AoA5[masterFile$Subject==13] <- c(2)
masterFile$AoA6[masterFile$Subject==13] <- c(1)
masterFile$AoA7[masterFile$Subject==13] <- c(3)
masterFile$AoA8[masterFile$Subject==13] <- c(3)
masterFile$AoA9[masterFile$Subject==13] <- c(0)

#                 ss14
masterFile$AoA1[masterFile$Subject==14] <- c(8)
masterFile$AoA2[masterFile$Subject==14] <- c(2)
masterFile$AoA3[masterFile$Subject==14] <- c("school")
masterFile$AoA4[masterFile$Subject==14] <- c(1)
masterFile$AoA5[masterFile$Subject==14] <- c(1)
masterFile$AoA6[masterFile$Subject==14] <- c(2)
masterFile$AoA7[masterFile$Subject==14] <- c(4)
masterFile$AoA8[masterFile$Subject==14] <- c(1)
masterFile$AoA9[masterFile$Subject==14] <- c(3)

#                 ss15
masterFile$AoA1[masterFile$Subject==15] <- c(8)
masterFile$AoA2[masterFile$Subject==15] <- c(3)
masterFile$AoA3[masterFile$Subject==15] <- c("school")
masterFile$AoA4[masterFile$Subject==15] <- c(1)
masterFile$AoA5[masterFile$Subject==15] <- c(1)
masterFile$AoA6[masterFile$Subject==15] <- c(2)
masterFile$AoA7[masterFile$Subject==15] <- c(3)
masterFile$AoA8[masterFile$Subject==15] <- c(1)
masterFile$AoA9[masterFile$Subject==15] <- c(4)

#                 ss16
masterFile$AoA1[masterFile$Subject==16] <- c(8)
masterFile$AoA2[masterFile$Subject==16] <- c(2)
masterFile$AoA3[masterFile$Subject==16] <- c("school")
masterFile$AoA4[masterFile$Subject==16] <- c(1)
masterFile$AoA5[masterFile$Subject==16] <- c(2)
masterFile$AoA6[masterFile$Subject==16] <- c(1)
masterFile$AoA7[masterFile$Subject==16] <- c(2)
masterFile$AoA8[masterFile$Subject==16] <- c(2)
masterFile$AoA9[masterFile$Subject==16] <- c(1)

#                 ss17
masterFile$AoA1[masterFile$Subject==17] <- c(6)
masterFile$AoA2[masterFile$Subject==17] <- c(3)
masterFile$AoA3[masterFile$Subject==17] <- c("school")
masterFile$AoA4[masterFile$Subject==17] <- c(1)
masterFile$AoA5[masterFile$Subject==17] <- c(2)
masterFile$AoA6[masterFile$Subject==17] <- c(1)
masterFile$AoA7[masterFile$Subject==17] <- c(3)
masterFile$AoA8[masterFile$Subject==17] <- c(2)
masterFile$AoA9[masterFile$Subject==17] <- c(4)

#                 ss18
masterFile$AoA1[masterFile$Subject==18] <- c(8)
masterFile$AoA2[masterFile$Subject==18] <- c(5)
masterFile$AoA3[masterFile$Subject==18] <- c("school")
masterFile$AoA4[masterFile$Subject==18] <- c(1)
masterFile$AoA5[masterFile$Subject==18] <- c(2)
masterFile$AoA6[masterFile$Subject==18] <- c(2)
masterFile$AoA7[masterFile$Subject==18] <- c(5)
masterFile$AoA8[masterFile$Subject==18] <- c(1)
masterFile$AoA9[masterFile$Subject==18] <- c(4)

#                 ss19
masterFile$AoA1[masterFile$Subject==19] <- c(8)
masterFile$AoA2[masterFile$Subject==19] <- c(4)
masterFile$AoA3[masterFile$Subject==19] <- c("school")
masterFile$AoA4[masterFile$Subject==19] <- c(1)
masterFile$AoA5[masterFile$Subject==19] <- c(2)
masterFile$AoA6[masterFile$Subject==19] <- c(2)
masterFile$AoA7[masterFile$Subject==19] <- c(4)
masterFile$AoA8[masterFile$Subject==19] <- c(1)
masterFile$AoA9[masterFile$Subject==19] <- c(3)

#                 ss20
masterFile$AoA1[masterFile$Subject==20] <- c(0)
masterFile$AoA2[masterFile$Subject==20] <- c(5)
masterFile$AoA3[masterFile$Subject==20] <- c("home")
masterFile$AoA4[masterFile$Subject==20] <- c(1)
masterFile$AoA5[masterFile$Subject==20] <- c(2)
masterFile$AoA6[masterFile$Subject==20] <- c(1)
masterFile$AoA7[masterFile$Subject==20] <- c(4)
masterFile$AoA8[masterFile$Subject==20] <- c(2)
masterFile$AoA9[masterFile$Subject==20] <- c(5)

#                 ss21
masterFile$AoA1[masterFile$Subject==21] <- c(6)
masterFile$AoA2[masterFile$Subject==21] <- c(3)
masterFile$AoA3[masterFile$Subject==21] <- c("school")
masterFile$AoA4[masterFile$Subject==21] <- c(1)
masterFile$AoA5[masterFile$Subject==21] <- c(2)
masterFile$AoA6[masterFile$Subject==21] <- c(1)
masterFile$AoA7[masterFile$Subject==21] <- c(4)
masterFile$AoA8[masterFile$Subject==21] <- c(2)
masterFile$AoA9[masterFile$Subject==21] <- c(4)

#                 ss22
masterFile$AoA1[masterFile$Subject==22] <- c(3)
masterFile$AoA2[masterFile$Subject==22] <- c(2)
masterFile$AoA3[masterFile$Subject==22] <- c("school")
masterFile$AoA4[masterFile$Subject==22] <- c(1)
masterFile$AoA5[masterFile$Subject==22] <- c(2)
masterFile$AoA6[masterFile$Subject==22] <- c(2)
masterFile$AoA7[masterFile$Subject==22] <- c(4)
masterFile$AoA8[masterFile$Subject==22] <- c(2)
masterFile$AoA9[masterFile$Subject==22] <- c(4)

#                 ss23
masterFile$AoA1[masterFile$Subject==23] <- c(7)
masterFile$AoA2[masterFile$Subject==23] <- c(2)
masterFile$AoA3[masterFile$Subject==23] <- c("school")
masterFile$AoA4[masterFile$Subject==23] <- c(1)
masterFile$AoA5[masterFile$Subject==23] <- c(1)
masterFile$AoA6[masterFile$Subject==23] <- c(2)
masterFile$AoA7[masterFile$Subject==23] <- c(5)
masterFile$AoA8[masterFile$Subject==23] <- c(1)
masterFile$AoA9[masterFile$Subject==23] <- c(3)

#                 ss24
masterFile$AoA1[masterFile$Subject==24] <- c(7)
masterFile$AoA2[masterFile$Subject==24] <- c(2)
masterFile$AoA3[masterFile$Subject==24] <- c("school")
masterFile$AoA4[masterFile$Subject==24] <- c(1)
masterFile$AoA5[masterFile$Subject==24] <- c(2)
masterFile$AoA6[masterFile$Subject==24] <- c(1)
masterFile$AoA7[masterFile$Subject==24] <- c(3)
masterFile$AoA8[masterFile$Subject==24] <- c(2)
masterFile$AoA9[masterFile$Subject==24] <- c(1)

#                 ss25
masterFile$AoA1[masterFile$Subject==25] <- c(11)
masterFile$AoA2[masterFile$Subject==25] <- c(4)
masterFile$AoA3[masterFile$Subject==25] <- c("school")
masterFile$AoA4[masterFile$Subject==25] <- c(1)
masterFile$AoA5[masterFile$Subject==25] <- c(2)
masterFile$AoA6[masterFile$Subject==25] <- c(1)
masterFile$AoA7[masterFile$Subject==25] <- c(4)
masterFile$AoA8[masterFile$Subject==25] <- c(2)
masterFile$AoA9[masterFile$Subject==25] <- c(3)

#                 ss26
masterFile$AoA1[masterFile$Subject==26] <- c(6)
masterFile$AoA2[masterFile$Subject==26] <- c(2)
masterFile$AoA3[masterFile$Subject==26] <- c("school")
masterFile$AoA4[masterFile$Subject==26] <- c(1)
masterFile$AoA5[masterFile$Subject==26] <- c(1)
masterFile$AoA6[masterFile$Subject==26] <- c(2)
masterFile$AoA7[masterFile$Subject==26] <- c(3)
masterFile$AoA8[masterFile$Subject==26] <- c(1)
masterFile$AoA9[masterFile$Subject==26] <- c(4)

#                 ss27
masterFile$AoA1[masterFile$Subject==27] <- c(11)
masterFile$AoA2[masterFile$Subject==27] <- c(3)
masterFile$AoA3[masterFile$Subject==27] <- c("school")
masterFile$AoA4[masterFile$Subject==27] <- c(1)
masterFile$AoA5[masterFile$Subject==27] <- c(2)
masterFile$AoA6[masterFile$Subject==27] <- c(1)
masterFile$AoA7[masterFile$Subject==27] <- c(4)
masterFile$AoA8[masterFile$Subject==27] <- c(2)
masterFile$AoA9[masterFile$Subject==27] <- c(1)

#                 ss28
masterFile$AoA1[masterFile$Subject==28] <- c(8)
masterFile$AoA2[masterFile$Subject==28] <- c(4)
masterFile$AoA3[masterFile$Subject==28] <- c("school")
masterFile$AoA4[masterFile$Subject==28] <- c(1)
masterFile$AoA5[masterFile$Subject==28] <- c(2)
masterFile$AoA6[masterFile$Subject==28] <- c(1)
masterFile$AoA7[masterFile$Subject==28] <- c(3)
masterFile$AoA8[masterFile$Subject==28] <- c(3)
masterFile$AoA9[masterFile$Subject==28] <- c(0)

#                 ss29
masterFile$AoA1[masterFile$Subject==29] <- c(6)
masterFile$AoA2[masterFile$Subject==29] <- c(3)
masterFile$AoA3[masterFile$Subject==29] <- c("school")
masterFile$AoA4[masterFile$Subject==29] <- c(1)
masterFile$AoA5[masterFile$Subject==29] <- c(1)
masterFile$AoA6[masterFile$Subject==29] <- c(2)
masterFile$AoA7[masterFile$Subject==29] <- c(3)
masterFile$AoA8[masterFile$Subject==29] <- c(1)
masterFile$AoA9[masterFile$Subject==29] <- c(4)

#                 ss30
masterFile$AoA1[masterFile$Subject==30] <- c(7)
masterFile$AoA2[masterFile$Subject==30] <- c(3)
masterFile$AoA3[masterFile$Subject==30] <- c("school")
masterFile$AoA4[masterFile$Subject==30] <- c(1)
masterFile$AoA5[masterFile$Subject==30] <- c(2)
masterFile$AoA6[masterFile$Subject==30] <- c(1)
masterFile$AoA7[masterFile$Subject==30] <- c(3)
masterFile$AoA8[masterFile$Subject==30] <- c(3)
masterFile$AoA9[masterFile$Subject==30] <- c(0)

#                 ss31
masterFile$AoA1[masterFile$Subject==31] <- c(6)
masterFile$AoA2[masterFile$Subject==31] <- c(4)
masterFile$AoA3[masterFile$Subject==31] <- c("school")
masterFile$AoA4[masterFile$Subject==31] <- c(1)
masterFile$AoA5[masterFile$Subject==31] <- c(2)
masterFile$AoA6[masterFile$Subject==31] <- c(1)
masterFile$AoA7[masterFile$Subject==31] <- c(3)
masterFile$AoA8[masterFile$Subject==31] <- c(3)
masterFile$AoA9[masterFile$Subject==31] <- c(0)

#                 ss32
masterFile$AoA1[masterFile$Subject==32] <- c(8)
masterFile$AoA2[masterFile$Subject==32] <- c(3)
masterFile$AoA3[masterFile$Subject==32] <- c("school")
masterFile$AoA4[masterFile$Subject==32] <- c(1)
masterFile$AoA5[masterFile$Subject==32] <- c(2)
masterFile$AoA6[masterFile$Subject==32] <- c(1)
masterFile$AoA7[masterFile$Subject==32] <- c(3)
masterFile$AoA8[masterFile$Subject==32] <- c(3)
masterFile$AoA9[masterFile$Subject==32] <- c(0)

#                 ss33
masterFile$AoA1[masterFile$Subject==33] <- c(3)
masterFile$AoA2[masterFile$Subject==33] <- c(2)
masterFile$AoA3[masterFile$Subject==33] <- c("school")
masterFile$AoA4[masterFile$Subject==33] <- c(1)
masterFile$AoA5[masterFile$Subject==33] <- c(1)
masterFile$AoA6[masterFile$Subject==33] <- c(1)
masterFile$AoA7[masterFile$Subject==33] <- c(3)
masterFile$AoA8[masterFile$Subject==33] <- c(2)
masterFile$AoA9[masterFile$Subject==33] <- c(2)

#                 ss34
masterFile$AoA1[masterFile$Subject==34] <- c(8)
masterFile$AoA2[masterFile$Subject==34] <- c(4)
masterFile$AoA3[masterFile$Subject==34] <- c("school")
masterFile$AoA4[masterFile$Subject==34] <- c(1)
masterFile$AoA5[masterFile$Subject==34] <- c(2)
masterFile$AoA6[masterFile$Subject==34] <- c(1)
masterFile$AoA7[masterFile$Subject==34] <- c(4)
masterFile$AoA8[masterFile$Subject==34] <- c(2)
masterFile$AoA9[masterFile$Subject==34] <- c(3)

#                 ss35
masterFile$AoA1[masterFile$Subject==35] <- c(6)
masterFile$AoA2[masterFile$Subject==35] <- c(2)
masterFile$AoA3[masterFile$Subject==35] <- c("school")
masterFile$AoA4[masterFile$Subject==35] <- c(1)
masterFile$AoA5[masterFile$Subject==35] <- c(2)
masterFile$AoA6[masterFile$Subject==35] <- c(1)
masterFile$AoA7[masterFile$Subject==35] <- c(2)
masterFile$AoA8[masterFile$Subject==35] <- c(3)
masterFile$AoA9[masterFile$Subject==35] <- c(0)

#                 ss36
masterFile$AoA1[masterFile$Subject==36] <- c(6)
masterFile$AoA2[masterFile$Subject==36] <- c(3)
masterFile$AoA3[masterFile$Subject==36] <- c("school")
masterFile$AoA4[masterFile$Subject==36] <- c(1)
masterFile$AoA5[masterFile$Subject==36] <- c(1)
masterFile$AoA6[masterFile$Subject==36] <- c(2)
masterFile$AoA7[masterFile$Subject==36] <- c(1)
masterFile$AoA8[masterFile$Subject==36] <- c(1)
masterFile$AoA9[masterFile$Subject==36] <- c(4)

#                 ss37
masterFile$AoA1[masterFile$Subject==37] <- c(6)
masterFile$AoA2[masterFile$Subject==37] <- c(3)
masterFile$AoA3[masterFile$Subject==37] <- c("school")
masterFile$AoA4[masterFile$Subject==37] <- c(1)
masterFile$AoA5[masterFile$Subject==37] <- c(2)
masterFile$AoA6[masterFile$Subject==37] <- c(2)
masterFile$AoA7[masterFile$Subject==37] <- c(4)
masterFile$AoA8[masterFile$Subject==37] <- c(1)
masterFile$AoA9[masterFile$Subject==37] <- c(3)

#                 ss38
masterFile$AoA1[masterFile$Subject==38] <- c(6)
masterFile$AoA2[masterFile$Subject==38] <- c(3)
masterFile$AoA3[masterFile$Subject==38] <- c("school")
masterFile$AoA4[masterFile$Subject==38] <- c(1)
masterFile$AoA5[masterFile$Subject==38] <- c(2)
masterFile$AoA6[masterFile$Subject==38] <- c(2)
masterFile$AoA7[masterFile$Subject==38] <- c(5)
masterFile$AoA8[masterFile$Subject==38] <- c(1)
masterFile$AoA9[masterFile$Subject==38] <- c(3)

#                 ss39
masterFile$AoA1[masterFile$Subject==39] <- c(9)
masterFile$AoA2[masterFile$Subject==39] <- c(2)
masterFile$AoA3[masterFile$Subject==39] <- c("school")
masterFile$AoA4[masterFile$Subject==39] <- c(1)
masterFile$AoA5[masterFile$Subject==39] <- c(2)
masterFile$AoA6[masterFile$Subject==39] <- c(1)
masterFile$AoA7[masterFile$Subject==39] <- c(3)
masterFile$AoA8[masterFile$Subject==39] <- c(3)
masterFile$AoA9[masterFile$Subject==39] <- c(0)

#                 ss40
masterFile$AoA1[masterFile$Subject==40] <- c(6)
masterFile$AoA2[masterFile$Subject==40] <- c(1)
masterFile$AoA3[masterFile$Subject==40] <- c("school")
masterFile$AoA4[masterFile$Subject==40] <- c(1)
masterFile$AoA5[masterFile$Subject==40] <- c(2)
masterFile$AoA6[masterFile$Subject==40] <- c(1)
masterFile$AoA7[masterFile$Subject==40] <- c(2)
masterFile$AoA8[masterFile$Subject==40] <- c(3)
masterFile$AoA9[masterFile$Subject==40] <- c(0)

#                 ss41
masterFile$AoA1[masterFile$Subject==41] <- c(6)
masterFile$AoA2[masterFile$Subject==41] <- c(5)
masterFile$AoA3[masterFile$Subject==41] <- c("school")
masterFile$AoA4[masterFile$Subject==41] <- c(1)
masterFile$AoA5[masterFile$Subject==41] <- c(2)
masterFile$AoA6[masterFile$Subject==41] <- c(1)
masterFile$AoA7[masterFile$Subject==41] <- c(3)
masterFile$AoA8[masterFile$Subject==41] <- c(2)
masterFile$AoA9[masterFile$Subject==41] <- c(2)

#                 ss42
masterFile$AoA1[masterFile$Subject==42] <- c(10)
masterFile$AoA2[masterFile$Subject==42] <- c(4)
masterFile$AoA3[masterFile$Subject==42] <- c("school")
masterFile$AoA4[masterFile$Subject==42] <- c(1)
masterFile$AoA5[masterFile$Subject==42] <- c(2)
masterFile$AoA6[masterFile$Subject==42] <- c(1)
masterFile$AoA7[masterFile$Subject==42] <- c(5)
masterFile$AoA8[masterFile$Subject==42] <- c(2)
masterFile$AoA9[masterFile$Subject==42] <- c(2)


#                 ss43
masterFile$AoA1[masterFile$Subject==43] <- c(11)
masterFile$AoA2[masterFile$Subject==43] <- c(3)
masterFile$AoA3[masterFile$Subject==43] <- c("school")
masterFile$AoA4[masterFile$Subject==43] <- c(1)
masterFile$AoA5[masterFile$Subject==43] <- c(1)
masterFile$AoA6[masterFile$Subject==43] <- c(2)
masterFile$AoA7[masterFile$Subject==43] <- c(4)
masterFile$AoA8[masterFile$Subject==43] <- c(1)
masterFile$AoA9[masterFile$Subject==43] <- c(3)

#                 ss44
masterFile$AoA1[masterFile$Subject==44] <- c(12)
masterFile$AoA2[masterFile$Subject==44] <- c(2)
masterFile$AoA3[masterFile$Subject==44] <- c("school")
masterFile$AoA4[masterFile$Subject==44] <- c(1)
masterFile$AoA5[masterFile$Subject==44] <- c(2)
masterFile$AoA6[masterFile$Subject==44] <- c(1)
masterFile$AoA7[masterFile$Subject==44] <- c(2)
masterFile$AoA8[masterFile$Subject==44] <- c(2)
masterFile$AoA9[masterFile$Subject==44] <- c(1)

#                 ss45
masterFile$AoA1[masterFile$Subject==45] <- c(6)
masterFile$AoA2[masterFile$Subject==45] <- c(5)
masterFile$AoA3[masterFile$Subject==45] <- c("school")
masterFile$AoA4[masterFile$Subject==45] <- c(1)
masterFile$AoA5[masterFile$Subject==45] <- c(2)
masterFile$AoA6[masterFile$Subject==45] <- c(2)
masterFile$AoA7[masterFile$Subject==45] <- c(5)
masterFile$AoA8[masterFile$Subject==45] <- c(1)
masterFile$AoA9[masterFile$Subject==45] <- c(3)

#                 ss46
masterFile$AoA1[masterFile$Subject==46] <- c(6)
masterFile$AoA2[masterFile$Subject==46] <- c(3)
masterFile$AoA3[masterFile$Subject==46] <- c("school")
masterFile$AoA4[masterFile$Subject==46] <- c(1)
masterFile$AoA5[masterFile$Subject==46] <- c(2)
masterFile$AoA6[masterFile$Subject==46] <- c(1)
masterFile$AoA7[masterFile$Subject==46] <- c(3)
masterFile$AoA8[masterFile$Subject==46] <- c(2)
masterFile$AoA9[masterFile$Subject==46] <- c(2)

#                 ss47
masterFile$AoA1[masterFile$Subject==47] <- c(6)
masterFile$AoA2[masterFile$Subject==47] <- c(3)
masterFile$AoA3[masterFile$Subject==47] <- c("school")
masterFile$AoA4[masterFile$Subject==47] <- c(1)
masterFile$AoA5[masterFile$Subject==47] <- c(2)
masterFile$AoA6[masterFile$Subject==47] <- c(1)
masterFile$AoA7[masterFile$Subject==47] <- c(3)
masterFile$AoA8[masterFile$Subject==47] <- c(2)
masterFile$AoA9[masterFile$Subject==47] <- c(2)

#                 ss48
masterFile$AoA1[masterFile$Subject==48] <- c(8)
masterFile$AoA2[masterFile$Subject==48] <- c(3)
masterFile$AoA3[masterFile$Subject==48] <- c("school")
masterFile$AoA4[masterFile$Subject==48] <- c(1)
masterFile$AoA5[masterFile$Subject==48] <- c(2)
masterFile$AoA6[masterFile$Subject==48] <- c(1)
masterFile$AoA7[masterFile$Subject==48] <- c(4)
masterFile$AoA8[masterFile$Subject==48] <- c(3)
masterFile$AoA9[masterFile$Subject==48] <- c(0)

#                 ss49
masterFile$AoA1[masterFile$Subject==49] <- c(4)
masterFile$AoA2[masterFile$Subject==49] <- c(5)
masterFile$AoA3[masterFile$Subject==49] <- c("school")
masterFile$AoA4[masterFile$Subject==49] <- c(1)
masterFile$AoA5[masterFile$Subject==49] <- c(2)
masterFile$AoA6[masterFile$Subject==49] <- c(1)
masterFile$AoA7[masterFile$Subject==49] <- c(4)
masterFile$AoA8[masterFile$Subject==49] <- c(2)
masterFile$AoA9[masterFile$Subject==49] <- c(2)

#                 ss50
masterFile$AoA1[masterFile$Subject==50] <- c(9)
masterFile$AoA2[masterFile$Subject==50] <- c(5)
masterFile$AoA3[masterFile$Subject==50] <- c("school")
masterFile$AoA4[masterFile$Subject==50] <- c(1)
masterFile$AoA5[masterFile$Subject==50] <- c(1)
masterFile$AoA6[masterFile$Subject==50] <- c(2)
masterFile$AoA7[masterFile$Subject==50] <- c(5)
masterFile$AoA8[masterFile$Subject==50] <- c(1)
masterFile$AoA9[masterFile$Subject==50] <- c(4)

#                 ss51
masterFile$AoA1[masterFile$Subject==51] <- c(15)
masterFile$AoA2[masterFile$Subject==51] <- c(2)
masterFile$AoA3[masterFile$Subject==51] <- c("school")
masterFile$AoA4[masterFile$Subject==51] <- c(1)
masterFile$AoA5[masterFile$Subject==51] <- c(2)
masterFile$AoA6[masterFile$Subject==51] <- c(1)
masterFile$AoA7[masterFile$Subject==51] <- c(2)
masterFile$AoA8[masterFile$Subject==51] <- c(2)
masterFile$AoA9[masterFile$Subject==51] <- c(1)

#                 ss52
masterFile$AoA1[masterFile$Subject==52] <- c(6)
masterFile$AoA2[masterFile$Subject==52] <- c(5)
masterFile$AoA3[masterFile$Subject==52] <- c("home")
masterFile$AoA4[masterFile$Subject==52] <- c(1)
masterFile$AoA5[masterFile$Subject==52] <- c(1)
masterFile$AoA6[masterFile$Subject==52] <- c(1)
masterFile$AoA7[masterFile$Subject==52] <- c(5)
masterFile$AoA8[masterFile$Subject==52] <- c(2)
masterFile$AoA9[masterFile$Subject==52] <- c(4)

#                 ss53
masterFile$AoA1[masterFile$Subject==53] <- c(5)
masterFile$AoA2[masterFile$Subject==53] <- c(5)
masterFile$AoA3[masterFile$Subject==53] <- c("home")
masterFile$AoA4[masterFile$Subject==53] <- c(1)
masterFile$AoA5[masterFile$Subject==53] <- c(2)
masterFile$AoA6[masterFile$Subject==53] <- c(1)
masterFile$AoA7[masterFile$Subject==53] <- c(4)
masterFile$AoA8[masterFile$Subject==53] <- c(2)
masterFile$AoA9[masterFile$Subject==53] <- c(2)

#                 ss54
masterFile$AoA1[masterFile$Subject==54] <- c(6)
masterFile$AoA2[masterFile$Subject==54] <- c(3)
masterFile$AoA3[masterFile$Subject==54] <- c("school")
masterFile$AoA4[masterFile$Subject==54] <- c(1)
masterFile$AoA5[masterFile$Subject==54] <- c(2)
masterFile$AoA6[masterFile$Subject==54] <- c(1)
masterFile$AoA7[masterFile$Subject==54] <- c(4)
masterFile$AoA8[masterFile$Subject==54] <- c(2)
masterFile$AoA9[masterFile$Subject==54] <- c(1)

#                 ss55
masterFile$AoA1[masterFile$Subject==55] <- c(10)
masterFile$AoA2[masterFile$Subject==55] <- c(2)
masterFile$AoA3[masterFile$Subject==55] <- c("school")
masterFile$AoA4[masterFile$Subject==55] <- c(1)
masterFile$AoA5[masterFile$Subject==55] <- c(2)
masterFile$AoA6[masterFile$Subject==55] <- c(2)
masterFile$AoA7[masterFile$Subject==55] <- c(3)
masterFile$AoA8[masterFile$Subject==55] <- c(1)
masterFile$AoA9[masterFile$Subject==55] <- c(2)

#                 ss56
masterFile$AoA1[masterFile$Subject==56] <- c(6)
masterFile$AoA2[masterFile$Subject==56] <- c(4)
masterFile$AoA3[masterFile$Subject==56] <- c("school")
masterFile$AoA4[masterFile$Subject==56] <- c(1)
masterFile$AoA5[masterFile$Subject==56] <- c(2)
masterFile$AoA6[masterFile$Subject==56] <- c(1)
masterFile$AoA7[masterFile$Subject==56] <- c(3)
masterFile$AoA8[masterFile$Subject==56] <- c(3)
masterFile$AoA9[masterFile$Subject==56] <- c(0)

#                 ss57
masterFile$AoA1[masterFile$Subject==57] <- c(6)
masterFile$AoA2[masterFile$Subject==57] <- c(2)
masterFile$AoA3[masterFile$Subject==57] <- c("school")
masterFile$AoA4[masterFile$Subject==57] <- c(1)
masterFile$AoA5[masterFile$Subject==57] <- c(2)
masterFile$AoA6[masterFile$Subject==57] <- c(1)
masterFile$AoA7[masterFile$Subject==57] <- c(4)
masterFile$AoA8[masterFile$Subject==57] <- c(3)
masterFile$AoA9[masterFile$Subject==57] <- c(0)

#                 ss58
masterFile$AoA1[masterFile$Subject==58] <- c(4)
masterFile$AoA2[masterFile$Subject==58] <- c(3)
masterFile$AoA3[masterFile$Subject==58] <- c("school")
masterFile$AoA4[masterFile$Subject==58] <- c(1)
masterFile$AoA5[masterFile$Subject==58] <- c(2)
masterFile$AoA6[masterFile$Subject==58] <- c(1)
masterFile$AoA7[masterFile$Subject==58] <- c(3)
masterFile$AoA8[masterFile$Subject==58] <- c(3)
masterFile$AoA9[masterFile$Subject==58] <- c(0)


#                 ss59
masterFile$AoA1[masterFile$Subject==59] <- c(6)
masterFile$AoA2[masterFile$Subject==59] <- c(4)
masterFile$AoA3[masterFile$Subject==59] <- c("school")
masterFile$AoA4[masterFile$Subject==59] <- c(1)
masterFile$AoA5[masterFile$Subject==59] <- c(2)
masterFile$AoA6[masterFile$Subject==59] <- c(1)
masterFile$AoA7[masterFile$Subject==59] <- c(4)
masterFile$AoA8[masterFile$Subject==59] <- c(2)
masterFile$AoA9[masterFile$Subject==59] <- c(2)

#                 ss61
masterFile$AoA1[masterFile$Subject==61] <- c(7)
masterFile$AoA2[masterFile$Subject==61] <- c(4)
masterFile$AoA3[masterFile$Subject==61] <- c("school")
masterFile$AoA4[masterFile$Subject==61] <- c(1)
masterFile$AoA5[masterFile$Subject==61] <- c(1)
masterFile$AoA6[masterFile$Subject==61] <- c(2)
masterFile$AoA7[masterFile$Subject==61] <- c(4)
masterFile$AoA8[masterFile$Subject==61] <- c(2)
masterFile$AoA9[masterFile$Subject==61] <- c(4)

#                 ss63
masterFile$AoA1[masterFile$Subject==63] <- c(4)
masterFile$AoA2[masterFile$Subject==63] <- c(4)
masterFile$AoA3[masterFile$Subject==63] <- c("home")
masterFile$AoA4[masterFile$Subject==63] <- c(1)
masterFile$AoA5[masterFile$Subject==63] <- c(2)
masterFile$AoA6[masterFile$Subject==63] <- c(1)
masterFile$AoA7[masterFile$Subject==63] <- c(3)
masterFile$AoA8[masterFile$Subject==63] <- c(3)
masterFile$AoA9[masterFile$Subject==63] <- c(0)

#                 ss64
masterFile$AoA1[masterFile$Subject==64] <- c(8)
masterFile$AoA2[masterFile$Subject==64] <- c(2)
masterFile$AoA3[masterFile$Subject==64] <- c("school")
masterFile$AoA4[masterFile$Subject==64] <- c(1)
masterFile$AoA5[masterFile$Subject==64] <- c(2)
masterFile$AoA6[masterFile$Subject==64] <- c(1)
masterFile$AoA7[masterFile$Subject==64] <- c(3)
masterFile$AoA8[masterFile$Subject==64] <- c(2)
masterFile$AoA9[masterFile$Subject==64] <- c(1)

#                 ss65
masterFile$AoA1[masterFile$Subject==65] <- c(8)
masterFile$AoA2[masterFile$Subject==65] <- c(3)
masterFile$AoA3[masterFile$Subject==65] <- c("school")
masterFile$AoA4[masterFile$Subject==65] <- c(1)
masterFile$AoA5[masterFile$Subject==65] <- c(2)
masterFile$AoA6[masterFile$Subject==65] <- c(1)
masterFile$AoA7[masterFile$Subject==65] <- c(4)
masterFile$AoA8[masterFile$Subject==65] <- c(2)
masterFile$AoA9[masterFile$Subject==65] <- c(4)

#                 ss66
masterFile$AoA1[masterFile$Subject==66] <- c(6)
masterFile$AoA2[masterFile$Subject==66] <- c(2)
masterFile$AoA3[masterFile$Subject==66] <- c("school")
masterFile$AoA4[masterFile$Subject==66] <- c(1)
masterFile$AoA5[masterFile$Subject==66] <- c(2)
masterFile$AoA6[masterFile$Subject==66] <- c(1)
masterFile$AoA7[masterFile$Subject==66] <- c(4)
masterFile$AoA8[masterFile$Subject==66] <- c(2)
masterFile$AoA9[masterFile$Subject==66] <- c(3)

#                 ss67
masterFile$AoA1[masterFile$Subject==67] <- c(4)
masterFile$AoA2[masterFile$Subject==67] <- c(1)
masterFile$AoA3[masterFile$Subject==67] <- c("school")
masterFile$AoA4[masterFile$Subject==67] <- c(1)
masterFile$AoA5[masterFile$Subject==67] <- c(2)
masterFile$AoA6[masterFile$Subject==67] <- c(1)
masterFile$AoA7[masterFile$Subject==67] <- c(2)
masterFile$AoA8[masterFile$Subject==67] <- c(2)
masterFile$AoA9[masterFile$Subject==67] <- c(1)

#                 ss69
masterFile$AoA1[masterFile$Subject==69] <- c(6)
masterFile$AoA2[masterFile$Subject==69] <- c(4)
masterFile$AoA3[masterFile$Subject==69] <- c("school")
masterFile$AoA4[masterFile$Subject==69] <- c(1)
masterFile$AoA5[masterFile$Subject==69] <- c(1)
masterFile$AoA6[masterFile$Subject==69] <- c(1)
masterFile$AoA7[masterFile$Subject==69] <- c(3)
masterFile$AoA8[masterFile$Subject==69] <- c(2)
masterFile$AoA9[masterFile$Subject==69] <- c(1)

#                 ss70
masterFile$AoA1[masterFile$Subject==70] <- c(5)
masterFile$AoA2[masterFile$Subject==70] <- c(5)
masterFile$AoA3[masterFile$Subject==70] <- c("school")
masterFile$AoA4[masterFile$Subject==70] <- c(1)
masterFile$AoA5[masterFile$Subject==70] <- c(2)
masterFile$AoA6[masterFile$Subject==70] <- c(1)
masterFile$AoA7[masterFile$Subject==70] <- c(4)
masterFile$AoA8[masterFile$Subject==70] <- c(2)
masterFile$AoA9[masterFile$Subject==70] <- c(4)

#                 ss71
masterFile$AoA1[masterFile$Subject==71] <- c(5)
masterFile$AoA2[masterFile$Subject==71] <- c(4)
masterFile$AoA3[masterFile$Subject==71] <- c("school")
masterFile$AoA4[masterFile$Subject==71] <- c(1)
masterFile$AoA5[masterFile$Subject==71] <- c(2)
masterFile$AoA6[masterFile$Subject==71] <- c(2)
masterFile$AoA7[masterFile$Subject==71] <- c(3)
masterFile$AoA8[masterFile$Subject==71] <- c(1)
masterFile$AoA9[masterFile$Subject==71] <- c(3)

#                 ss73
masterFile$AoA1[masterFile$Subject==73] <- c(4)
masterFile$AoA2[masterFile$Subject==73] <- c(4)
masterFile$AoA3[masterFile$Subject==73] <- c("school")
masterFile$AoA4[masterFile$Subject==73] <- c(1)
masterFile$AoA5[masterFile$Subject==73] <- c(2)
masterFile$AoA6[masterFile$Subject==73] <- c(1)
masterFile$AoA7[masterFile$Subject==73] <- c(3)
masterFile$AoA8[masterFile$Subject==73] <- c(3)
masterFile$AoA9[masterFile$Subject==73] <- c(0)

#                 ss75
masterFile$AoA1[masterFile$Subject==75] <- c(8)
masterFile$AoA2[masterFile$Subject==75] <- c(3)
masterFile$AoA3[masterFile$Subject==75] <- c("school")
masterFile$AoA4[masterFile$Subject==75] <- c(1)
masterFile$AoA5[masterFile$Subject==75] <- c(2)
masterFile$AoA6[masterFile$Subject==75] <- c(2)
masterFile$AoA7[masterFile$Subject==75] <- c(3)
masterFile$AoA8[masterFile$Subject==75] <- c(1)
masterFile$AoA9[masterFile$Subject==75] <- c(3)

#                 ss77
masterFile$AoA1[masterFile$Subject==77] <- c(8)
masterFile$AoA2[masterFile$Subject==77] <- c(3)
masterFile$AoA3[masterFile$Subject==77] <- c("school")
masterFile$AoA4[masterFile$Subject==77] <- c(1)
masterFile$AoA5[masterFile$Subject==77] <- c(1)
masterFile$AoA6[masterFile$Subject==77] <- c(1)
masterFile$AoA7[masterFile$Subject==77] <- c(4)
masterFile$AoA8[masterFile$Subject==77] <- c(2)
masterFile$AoA9[masterFile$Subject==77] <- c(2)

#                 ss80
masterFile$AoA1[masterFile$Subject==80] <- c(6)
masterFile$AoA2[masterFile$Subject==80] <- c(3)
masterFile$AoA3[masterFile$Subject==80] <- c("school")
masterFile$AoA4[masterFile$Subject==80] <- c(1)
masterFile$AoA5[masterFile$Subject==80] <- c(2)
masterFile$AoA6[masterFile$Subject==80] <- c(1)
masterFile$AoA7[masterFile$Subject==80] <- c(3)
masterFile$AoA8[masterFile$Subject==80] <- c(3)
masterFile$AoA9[masterFile$Subject==80] <- c(0)

masterFile$AoA3 <- as.factor(masterFile$AoA3)

#---------------------------------------------------------------------------------------------------#
#                           Sumup of proficiency                                                    #
#---------------------------------------------------------------------------------------------------#

masterFile$overallProf <- apply(masterFile[28:34],1,FUN = sum);

#---------------------------------------------------------------------------------------------------#
#                                          END                                                      #
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#                                          OSC                                                      #
#---------------------------------------------------------------------------------------------------#
read.table(paste(localGitDir, '/stimoli/OSC.txt', sep=''), header = T)-> db
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
                                 "AoA2","AoA3","AoA4","AoA5","AoA6", "AoA7","AoA8","AoA9","overallProf","OSC_Primes")
colnames(db) <- c('Target', 'OSC')
merge(masterfileEng_OSC, db, by='Target', all.x=T)-> masterfileEng_OSC

colnames(masterfileEng_OSC) <- c("Prime","Target","Subject","Age","Gender", "Handedness","Rotation",
                                 "Data","OraStart" ,"TrialCount","TrialType","Relatedness","rt","Resp","Trial.ID","Target.ID","Lexicality","Morphtype","Primetype",
                                 "Logfreq.Zipf.t","Lent","Lenp","Logfreq.Zipf.p","Nt","Np","Language","Accuracy","phoneticFluency","phoneticComprehension","morphComprehension",
                                 "spelling","readingComprehension","vocabulary","oralComprehension","AoA1" ,
                                 "AoA2","AoA3","AoA4","AoA5","AoA6", "AoA7","AoA8","AoA9", "overallProf","OSC_Primes","OSC_Target")
summary(masterfileEng_OSC)

rm(db, localGitDir, finalNumberofParticipants)
#---------------------------------------------------------------------------------------------------#
#                                          END                                                      #
#---------------------------------------------------------------------------------------------------#
