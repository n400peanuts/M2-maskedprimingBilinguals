#---------------------------------------------------------------------------------------------------#
#                                   M2-maskedprimingBilinguals                                      #
#---------------------------------------------------------------------------------------------------#
#This is the code used to upload, check and analyze data from a masked morphological priming experiment on Italian-English bilinguals.
# 14/12/2016

#---------------------------------------------------------------------------------------------------#
#                             CONCATENATE ALL THE ROTATIONS ENG SBJ                                 #
#---------------------------------------------------------------------------------------------------#
eng = NULL
for (j in 1:40){
  if ((j %% 2) ==0){
    boh <- paste("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/ENG/Output_MPrim_Eng_Subj_", j, ".txt", sep = "")
    pilot_eng <- read.table(boh, header = F, skip = 15,dec = ",")
    colnames(pilot_eng) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
    rot_B_eng <- read.table("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Stimoli/subtlex-UK/rot_B_eng_comp.txt", header = T)
    colnames(rot_B_eng) <- c("Trial.ID", "Target.ID", "Lexicality", "Morphtype", "Target", "Prime", "Primetype", "Logfreq.Zipf.t", "Lent", "Lenp", "Logfreq.Zipf.p", "Nt", "Np")
    merge(pilot_eng, rot_B_eng, by = "Target", all.x = T)-> pilot_eng
  } else { #ODD
    boh <- paste("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/ENG/Output_MPrim_Eng_Subj_", j, ".txt", sep = "")
    pilot_eng <- read.table(boh, header = F, skip = 15,dec = ",")
    colnames(pilot_eng) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
    rot_A_eng <- read.table("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Stimoli/subtlex-UK/rot_A_eng_comp.txt", header = T)
    colnames(rot_A_eng) <- c("Trial.ID", "Target.ID", "Lexicality", "Morphtype", "Target", "Prime", "Primetype", "Logfreq.Zipf.t", "Lent", "Lenp", "Logfreq.Zipf.p", "Nt", "Np")
    merge(pilot_eng, rot_A_eng, by = "Target", all.x = T)-> pilot_eng
  }
  pilot_eng$Prime.y <- NULL
  pilot_eng$Language <- c("eng")
  pilot_eng$Language <- as.factor(pilot_eng$Language)
  pilot_eng$rt <- as.numeric(pilot_eng$rt)
  eng <- rbind(eng,pilot_eng)
}
write.table(eng, "C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/Sumup RT+Proficiency/SumupEng.txt", row.names= F, col.names = T, quote = F)

#---------------------------------------------------------------------------------------------------#
#                             CONCATENATE ALL THE ROTATIONS ITA SBJ                                 #
#---------------------------------------------------------------------------------------------------#
#                                            SS1                                                    #
ss1 <- read.table("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/ITA/Output_MPrim_Ita_Subj_1.txt", header = F,  skip = 15, dec = ",")
colnames(ss1) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
rotA<- read.table("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Stimoli/rotAcompl.txt", header = T, dec = ",")
colnames(rotA) <- c("Trial.ID", "Target.ID", "Lexicality", "Morphtype", "Target", "Prime", "Primetype", "Logfreq.Zipf.t", "Lent", "Lenp", "Logfreq.Zipf.p", "Nt", "Np")
merge(ss1, rotA, by = "Target", all.x = T)->ss1
ss1$Prime.y <- NULL
ss1$Language <- c("ita")
ss1$Language <- as.factor(ss1$Language)

#                                            SS2                                                    #
ss2 <- read.table("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/ITA/Output_MPrim_Ita_Subj_2.txt", header = F,  skip = 15, dec = ",")
colnames(ss2) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
rotB1<- read.table("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Stimoli/rot_B_persoggetto2.txt", header = T, dec = ",")
colnames(rotB1) <- c("Trial.ID", "Target.ID", "Lexicality", "Morphtype", "Target", "Prime", "Primetype", "Logfreq.Zipf.t", "Lent", "Lenp", "Logfreq.Zipf.p", "Nt", "Np")
merge(ss2, rotB1, by = "Target", all.x = T)->ss2
ss2$Prime.y <- NULL
ss2$Language <- c("ita")
ss2$Language <- as.factor(ss2$Language)

#                                       FROM SS3 TO SS40                                           #                        
ita = NULL
for (j in 3:40){
  if ((j %% 2) ==0){
    boh <- paste("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/ITA/Output_MPrim_Ita_Subj_", j, ".txt", sep = "")
    pilot_ita <- read.table(boh, header = F, skip = 15,dec = ",")
    colnames(pilot_ita) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
    rotB<- read.table("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Stimoli/rotBcompl.txt", header = T, dec = ",")
    colnames(rotB) <- c("Trial.ID", "Target.ID", "Lexicality", "Morphtype", "Target", "Prime", "Primetype", "Logfreq.Zipf.t", "Lent", "Lenp", "Logfreq.Zipf.p", "Nt", "Np")
    merge(pilot_ita, rotB, by = "Target", all.x = T)-> pilot_ita
  } else { #ODD
    boh <- paste("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/ITA/Output_MPrim_Ita_Subj_", j, ".txt", sep = "")
    pilot_ita <- read.table(boh, header = F, skip = 15,dec = ",")
    colnames(pilot_ita) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
    rotA<- read.table("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Stimoli/rotAcompl.txt", header = T, dec = ",")
    colnames(rotA) <- c("Trial.ID", "Target.ID", "Lexicality", "Morphtype", "Target", "Prime", "Primetype", "Logfreq.Zipf.t", "Lent", "Lenp", "Logfreq.Zipf.p", "Nt", "Np")
    merge(pilot_ita, rotA, by = "Target", all.x = T)-> pilot_ita
  }
  pilot_ita$Prime.y <- NULL
  pilot_ita$Language <- c("ita")
  pilot_ita$Language <- as.factor(pilot_ita$Language)
  pilot_ita$rt <- as.numeric(pilot_ita$rt)  
  ita <- rbind(ita,pilot_ita)
}
ita<- rbind(ss2,ita)
ita <- rbind(ss1,ita)
write.table(ita, "C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/Sumup RT+Proficiency/SumupIta.txt", row.names= F, col.names = T, quote = F)

#---------------------------------------------------------------------------------------------------#
#                               MERGE ITA AND ENG IN A MASTERFILE                                   #
#---------------------------------------------------------------------------------------------------#
rbind(ita,eng)->masterFile
masterFile$Logfreq.Zipf.p <- as.numeric(sub(",",".", masterFile$Logfreq.Zipf.p, fixed = T))
masterFile$Logfreq.Zipf.t <- as.numeric(sub(",",".", masterFile$Logfreq.Zipf.t, fixed = T))
#                                    Accuracy column creation                                       # 
masterFile$Accuracy<- 1; 
masterFile$Accuracy[masterFile$Lexicality=="WORD" & masterFile$Resp==1] <- 0 ;
masterFile$Accuracy[masterFile$Lexicality=="NONWORD" & masterFile$Resp==2] <- 0; 

write.table(masterFile, "C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Output/Sumup RT+Proficiency/masterFile.txt", row.names= F, col.names = T, quote = F )

#---------------------------------------------------------------------------------------------------#
#                      ADD RESULTS OF PROFICIENCY TEST TO THE MASTERFILE                            #
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

#---------------------------------------------------------------------------------------------------#
#                            GRAPH OF PROFICIENCY DISTRIBUTION                                      #
#---------------------------------------------------------------------------------------------------#
masterProficiency <- masterFile[,c(2,28:34)] 
masterProficiency <- unique(masterProficiency) 

filename<- par(mfg=c(nrow,ncol))
jpeg(paste(as.character(filename),"Proficiency.jpg",sep=""), res=200, height=1654, width=2339);
layout(matrix(c(1,2,3,3), nrow=2, byrow=T), heights=c(2,1));	 
par(mfrow=c(3,3))
for (i in 2:8) {
  hist(masterProficiency[,i], main = names(masterProficiency)[i])
}
dev.off();
par(mfrow=c(1,1))

#---------------------------------------------------------------------------------------------------#
#                                    ITA PRE-PROCESS DATA                                           #
#---------------------------------------------------------------------------------------------------#
#Diagnostics.Rfunction of Davide Crepaldi (see: http://www.davidecrepaldi.net/wordpress/software-utilities-2/)
subset(masterFile, Language=="ita")-> masterfileIta

setwd("")
sbj.id<- masterfileIta$Subject
acc<- masterfileIta$Accuracy
lexicality<- masterfileIta$Lexicality
target<-masterfileIta$Target
rt<-masterfileIta$rt

source("diagnostics.r")
diagnostics.f(rt, acc, sbj.id, target, lexicality, preprocessITA)

#---------------------------------------------------------------------------------------------------#
#                                       ITA ACCURACY DATA                                           #
#---------------------------------------------------------------------------------------------------#
#Right upper panel shows that 6 words were classified wrong > 70% of the cases. 
#Subject num. 16 declared to see clearly the prime.
#Bottom panel shows few outlier datapoint before 250ms and after 1600.
#Removal of these data from the dataset.

subset(target.diagnostics, acc<0.7)-> parolebrutte #•giusto per sapere quali sono
subset(masterfileIta, masterfileIta$rt>250 & masterfileIta$rt<1600 &masterfileIta$Subject!=16 & masterfileIta$Target!= "congruo" & masterfileIta$Target!= "guado" & masterfileIta$Target!= "guano" & masterfileIta$Target!= "uggia" & masterfileIta$Target!= "vello" & masterfileIta$Target!= "avo" & masterfileIta$Lexicality=="WORD") -> dataAccITA
summary(dataAccITA)

#---------------------------------------------------------------------------------------------------#
#                                      ITA RT DATA                                                  #
#---------------------------------------------------------------------------------------------------#
subset(dataAccITA, dataAccITA$Accuracy==1)-> datartITA
round(xtabs(datartITA$rt ~ datartITA$Morphtype + datartITA$Primetype) / xtabs(~datartITA$Morphtype + datartITA$Primetype), digits = 0)

#---------------------------------------------------------------------------------------------------#
#                                    ENG PRE-PROCESS DATA                                           #
#---------------------------------------------------------------------------------------------------#
#Diagnostics.Rfunction of Davide Crepaldi (see: http://www.davidecrepaldi.net/wordpress/software-utilities-2/)
subset(masterFile, Language=="eng") -> masterfileEng

setwd("")
sbj.id <- masterfileEng$Subject
acc <- masterfileEng$Accuracy
lexicality <- masterfileEng$Lexicality
target <- masterfileEng$Target
rt <- masterfileEng$rt

source("diagnostics.r")
diagnostics.f(rt, acc, sbj.id, target, lexicality, preprocessITA)

#---------------------------------------------------------------------------------------------------#
#                                       ENG ACCURACY DATA                                           #
#---------------------------------------------------------------------------------------------------#
#Right upper panel shows that subject 22 and 26 means and SDs are too far from the others, and the accuracy is less than 40%
#Bottom panel shows data points before 250ms, it is unlikely an answer so fast.
#Removal of these data from the dataset
subset(masterfileEng, masterfileEng$rt>250 & masterfileEng$rt<1900 & masterfileEng$Subject!=22 & masterfileEng$Subject!=26 & masterfileEng$Lexicality=="WORD") -> dataAccENG
summary(dataAccENG)

#---------------------------------------------------------------------------------------------------#
#                                            ENG RT DATA                                            #
#---------------------------------------------------------------------------------------------------#
subset(dataAccENG, dataAccENG$Accuracy2==1)-> datartENG
round(xtabs(datartENG$rt ~ datartENG$Morphtype + datartENG$Primetype) / xtabs(~datartENG$Morphtype + datartENG$Primetype), digits = 0)
