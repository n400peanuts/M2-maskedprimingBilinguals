#-----------------------------------------------------------------------------------------------#
# Data preprocessing                                                                            #
# Morphological masked priming experiment on L1-ITA, L2-ENG bilingual speakers/readers          #
# Paper titled 'L2 form priming turns into morphological facilitation with growing proficiency' #
# Submitted to JML, September 2018                                                              #  
# Eva Viviani and Davide Crepaldi, SISSA                                                        #
#-----------------------------------------------------------------------------------------------#

#This script merges the individual data files from each individual participant; fix variables names and nature, when necessary; import and merge stimuli features; and import and merge proficiency and AoA data for each participant. It generates a dataset ready for the analysis, which is called 'preprocessedData.txt'.

#Clean the workspace
rm(list = ls());

#Set your own working directory. This should be (and is assumed to be in the rest of the code) the highest point in your local gitHub folder:
localGitDir <- '';
setwd(localGitDir);

#------------------------------------------------------------------#
#### concatenate data from individual participants, English set ####
#------------------------------------------------------------------#
finalNumberofParticipants <- 84;

eng = NULL;
for (j in 1:finalNumberofParticipants)
  {
  fileName <- paste(localGitDir, '/rawData/eng/Output_MPrim_Eng_Subj_', j, ".txt", sep = "");
  if (file.exists(fileName)) 
      {
      temp <- read.table(fileName, header = F, skip = 15); #the parameter dec should disappear, there's no decimals in the output files
      eng <- rbind(eng, temp);
      }
      else 
      {next};
  print(j);
  };

summary(eng);
rm(temp, finalNumberofParticipants, fileName, j);

#------------------------------------------------------------------#
#### concatenate data from individual participants, Italian set ####
#------------------------------------------------------------------#
finalNumberofParticipants <- 84;

ita = NULL;
for (j in 1:finalNumberofParticipants)
{
  fileName <- paste(localGitDir, '/rawData/ita/Output_MPrim_Ita_Subj_', j, ".txt", sep = "");
  if (file.exists(fileName)) 
  {
    temp <- read.table(fileName, header = F, skip = 15);
    ita <- rbind(ita, temp);
  }
  else 
  {next};
  print(j);
};

summary(ita);
rm(temp, finalNumberofParticipants, fileName, j);

#--------------------------------------------------------------------#
#### join all data in the masterfile, and fix variables as needed ####
#--------------------------------------------------------------------#
rbind(ita,eng) -> masterFile;
rm(ita,eng);

#rename variables
colnames(masterFile) <- c("subject", "age", "gender", "handedness", "rotation","data", "oraStart", "trialCount", "trialType", "prime", "target", "relatedness", "rt", "resp");
#get rid of variables we don't need
masterFile <- masterFile[,c('subject', 'age', 'gender', 'handedness', 'trialCount', 'prime', 'target', 'rt', 'resp')];

#-----------------------------------------#
#### import feature stimuli, and merge ####
#-----------------------------------------#
stimFeatures <- read.table(paste(localGitDir, '/stimuli/stimuli.txt', sep=''), header=T, sep='\t', dec='.'); 
summary(stimFeatures);
#we won't need these:
stimFeatures$trialId <- NULL;
stimFeatures$targetId <- NULL;

stimFeatures$blender <- as.factor(paste(stimFeatures$prime, stimFeatures$target, sep=''));
masterFile$blender <- as.factor(paste(masterFile$prime, masterFile$target, sep=''));
masterFile <- merge(masterFile, stimFeatures, by='blender');
#we lose one datapoint cause sbj2 had a wrong target in the ITA set ('boccia' instead of 'gomito')

#let's doublecheck everything's fine
summary(masterFile);
summary(as.character(masterFile$target.x)==as.character(masterFile$target.y));
summary(as.character(masterFile$prime.x)==as.character(masterFile$prime.y)); #all fine

masterFile$blender <- NULL;
masterFile$prime.x <- NULL;
masterFile$target.x <- NULL;
names(masterFile)[10:11] <- c('target','prime');

#------------------------#
#### compute accuracy ####
#------------------------#
masterFile$accuracy <- 1; 
masterFile$accuracy[masterFile$lexicality=="word" & masterFile$resp==1] <- 0 ;
masterFile$accuracy[masterFile$lexicality=="nonword" & masterFile$resp==2] <- 0; 
masterFile$accuracy[masterFile$resp==-1] <- 0; 

summary(masterFile);

#------------------------------------#
#### add proficiency and AoA data ####
#------------------------------------#
sbjFeatures <- read.table(paste(localGitDir, '/rawData/proficiencyAoA.txt', sep = ''), header = T);
head(sbjFeatures);
summary(sbjFeatures);

masterFile <- merge(masterFile, sbjFeatures, by = 'subject');
head(masterFile);
summary(masterFile);

#--------------------------------------------#
#### write the preprocessed data out of R ####
#--------------------------------------------#
write.table(masterFile, paste(localGitDir, '/preprocessedData.txt', sep=''), row.names=F, col.names=T, sep='\t', dec='.');

rm(sbjFeatures, stimFeatures, localGitDir);
