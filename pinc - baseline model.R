            #################################
###########################################################
# Preventative Inhibitory Negative Causality (PINC) Model #
###########################################################
            #################################
            
# INSTALL 'COMBOS' PACKAGE 
require(hier.part)
            
install.packages('glue')
library(glue)


#############              
## OBJECTS ##
#############
objects_full = combos(12)$binary
modified_objects_full = objects_full[-c(1:793,2510:4095),]



## OUTCOME EVENTS ##
outcomes = data.frame(x = c('1 0','0 1'))
names(outcomes) = NULL
rownames(outcomes) = NULL

# Detector ON: 1 0
# Detector OFF: 0 1


# PREVENTATIVE CAUSES
prev_objects_stim = as.data.frame(matrix(NA, nrow=1000, ncol=12))
for(i in 1:nrow(modified_objects_full)){
  if(modified_objects_full[i,1]==1|modified_objects_full[i,2]==1|modified_objects_full[i,3]==1|modified_objects_full[i,4]==1|modified_objects_full[i,5]==1|modified_objects_full[i,6]==1){
    prev_objects_stim[i,] = NA
  } else {
    prev_objects_stim[i,] = modified_objects_full[i,]
  }
}
prev_objects_stim = na.omit(prev_objects_stim)
names(prev_objects_stim) = NULL
rownames(prev_objects_stim) = c(1:7)



# GENERATIVE CAUSES
gen_objects_stim = as.data.frame(matrix(NA, nrow=1000, ncol=12))
for(i in 1:nrow(modified_objects_full)){
  if(modified_objects_full[i,7]==1|modified_objects_full[i,8]==1|modified_objects_full[i,9]==1|modified_objects_full[i,10]==1|modified_objects_full[i,11]==1|modified_objects_full[i,12]==1){
    gen_objects_stim[i,] = NA
  } else {
    gen_objects_stim[i,] = modified_objects_full[i,]
  }
}

gen_objects_stim = na.omit(gen_objects_stim)
names(gen_objects_stim) = NULL
rownames(gen_objects_stim) = c(1:7)



# ABSENT CAUSE
absent_cause = data.frame(x = c('0 0 0 0 0 0 0 0 0 0 0 0'))
names(absent_cause) = NULL
rownames(absent_cause) = NULL



# PRETRAINING
sink('pretrain_prev_gen_55.ex')
for(i in 1:nrow(gen_objects_stim)){
  # Generative Causes #
  cat(paste("I:",rownames(gen_objects_stim)[i], "\n", sep=" "))
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(gen_objects_stim[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  # Preventative Causes #
  cat("\n")
  cat(paste("name:",rownames(prev_objects_stim)[i], "\n", sep=" "))
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(prev_objects_stim[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}
sink()
