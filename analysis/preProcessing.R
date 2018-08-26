# ---- Preprocessing of bilingual masked priming experiment
# ---- The final output is called "masterFile" in which there are all the raw data of the experiment
#----- 10/08/2018
#Clean the workspace
rm(list = ls());

#Set your own working directory. This should be (and is assumed to be in the rest of the code) the highest point in the gitHub folder:
localGitDir <- 'C:/Users/eva_v/Documents/GitHub/M2-maskedprimingBilinguals';
#localGitDir <- '~/Google Drive File Stream/My Drive/research/misc/m2-maskedMorphPrimingBilinguals/git/M2-maskedprimingBilinguals/';
setwd(localGitDir);

#-----------------------------------------------------------------------------------------------------#
####                               CONCATENATE ALL THE SBJ ENG                                     ####
#-----------------------------------------------------------------------------------------------------#
rotations <- read.table(paste(localGitDir,'/stimuli/stimuli.txt', sep=''), header = T, dec = ',');
rotations$OSCp <- as.numeric(sub(",",".", rotations$OSCp, fixed = T));
rotations$OSCt <- as.numeric(sub(",",".", rotations$OSCt, fixed = T));

finalNumberofParticipants <- 84;

eng = NULL;
for (j in 1:finalNumberofParticipants){
  if ((j %% 2) ==0 ){ #EVEN SUBJ
    boh <- paste(localGitDir, '/data/eng/Output_MPrim_Eng_Subj_', j, ".txt", sep = "")
    if (file.exists(boh)) {
      pilot_eng <- read.table(boh, header = F, skip = 15,dec = ",")
      colnames(pilot_eng) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
      rot_B_eng <- subset(rotations, rotations$Rotation=='B' & rotations$Language=='eng')
      merge(pilot_eng, rot_B_eng, by = "Target", all.x = T)-> pilot_eng
    } else {next}
  } else { #ODD
    boh <- paste(localGitDir, '/data/eng/Output_MPrim_Eng_Subj_', j, ".txt", sep = "")
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
  pilot_eng$Relatedness <- NULL
  
  eng <- rbind(eng,pilot_eng)
};

rm(rot_A_eng, rot_B_eng, pilot_eng, boh, j);
#----------------------------------------------------------------------------------------------------#
####                               CONCATENATE ALL THE SBJ ITA                                    ####
#----------------------------------------------------------------------------------------------------#
#We substituted 'boccia' with 'gomito' in rot B after the sbj 2. 
#Therefore there are 1201 targets in rotations, instead of 1200.

ita = NULL;
for (j in 1:finalNumberofParticipants){
  if ((j %% 2) ==0){ #EVEN SUBJ
    boh <- paste(localGitDir, '/data/ita/Output_MPrim_Ita_Subj_', j, ".txt", sep = "")
    if (file.exists(boh)) {
      pilot_ita <- read.table(boh, header = F, skip = 15,dec = ",")
      colnames(pilot_ita) <- c("Subject", "Age", "Gender", "Handedness", "Rotation","Data", "OraStart", "TrialCount", "TrialType", "Prime", "Target", "Relatedness", "rt", "Resp")
      rotB<- subset(rotations, rotations$Rotation=='B' & rotations$Language=='ita')
      merge(pilot_ita, rotB, by = "Target", all.x = T)-> pilot_ita
    } else {next}
  } else { #ODD
    boh <- paste(localGitDir, '/data/ita/Output_MPrim_Ita_Subj_', j, ".txt", sep = "")
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
  pilot_ita$Relatedness <- NULL
  
  ita <- rbind(ita,pilot_ita)
};

rm(rotA, rotations, pilot_ita, rotB, boh, j);
#----------------------------------------------------------------------------------------------------#
####                               MERGE ALL THE SBJ INTO A MASTERFILE                            ####
#----------------------------------------------------------------------------------------------------#
rbind(ita,eng)-> masterFile;
masterFile$Logfreq.Zipf.p <- as.numeric(sub(",",".", masterFile$Logfreq.Zipf.p, fixed = T));
masterFile$Logfreq.Zipf.t <- as.numeric(sub(",",".", masterFile$Logfreq.Zipf.t, fixed = T));
#                                    Accuracy column creation                                       # 
masterFile$Accuracy<- 1; 
masterFile$Accuracy[masterFile$Lexicality=="WORD" & masterFile$Resp==1] <- 0 ;
masterFile$Accuracy[masterFile$Lexicality=="NONWORD" & masterFile$Resp==2] <- 0; 
masterFile$Accuracy[ masterFile$Resp==-1] <- 0; 


rm(eng, ita);

#---------------------------------------------------------------------------------------------------------#
####                       ADD RESULTS OF PROFICIENCY TEST AND AoA TO THE MASTERFILE                   ####
#---------------------------------------------------------------------------------------------------------#
read.table(paste(localGitDir, '/stimuli/proficiency&AoA.txt', sep = ''), header = T)-> profAoA
merge(masterFile, profAoA, by = 'Subject')-> masterFile;

colnames(masterFile) <- c('subject', 'target', 'age', 'gender', 'handedness', 'rotation',
                          'data', 'oraStart', 'trialCount', 'trialType', 'prime', 'rt',
                          'resp', 'trialId', 'targetId', 'lexicality', 'morphType', 'relatedness',
                          'logfreqzipfT', 'lenT', 'lenP', 'logfreqzipfP', 'nT', 'nP',
                          'language', 'oscP', 'oscT', 'accuracy', 'phonemicFluency', 'phonemicComprehension',
                          'morphComprehension', 'spelling', 'readingComprehension', 'vocabulary', 'oralComprehension',
                          'aoa1', 'aoa2', 'aoa3', 'aoa4', 'aoa5', 'aoa6');

#---------------------------------------------------------------------------------------------------------#
####                              WRITE THE OUTPUT 'MASTERFILE.TXT'                                    ####
#---------------------------------------------------------------------------------------------------------#
write.table(masterFile, paste(localGitDir, '/stimuli/masterFile.txt', sep = ''), row.names = F, col.names = T, quote = F);
rm(finalNumberofParticipants, localGitDir, profAoA);


