            #################################
###########################################################
# Preventative Inhibitory Negative Causality (PINC) Model #
###########################################################
            #################################
            
# This script can be used to geenrate a pretraining, habituation, and
# testing set. Critically, the pretraining set for this script includes
# preventative, ambigous, AND causal cases.
            
            
# INSTALL 'COMBOS' PACKAGE 
require(hier.part)


#############              
## OBJECTS ##
#############
objects_full = combos(16)$binary


####################
## OUTCOME EVENTS ##
####################
outcomes = data.frame(x = c('1 0','0 1'))
names(outcomes) = NULL

# Detector ON: 1 0
# Detector OFF: 0 1




# PREVENTATIVE CAUSES
prev_objects_stim = as.data.frame(matrix(NA, nrow=100, ncol=16))
for(i in 1:nrow(objects_full)){
  if(objects_full[i,5]==1|objects_full[i,6]==1|objects_full[i,7]==1|objects_full[i,8]==1|objects_full[i,9]==1|objects_full[i,10]==1|objects_full[i,11]==1|objects_full[i,12]==1|objects_full[i,13]==1|objects_full[i,14]==1|objects_full[i,15]==1|objects_full[i,16]==1){
    prev_objects_stim[i,] = NA
  } else {
    prev_objects_stim[i,] = objects_full[i,]
  }
}
prev_objects_stim = na.omit(prev_objects_stim)
names(prev_objects_stim) = NULL
rownames(prev_objects_stim) = c(1:15)





# GENERATIVE CAUSES
gen_objects_stim = as.data.frame(matrix(NA, nrow=100, ncol=16))
for(i in 1:nrow(objects_full)){
  if(objects_full[i,1]==1|objects_full[i,2]==1|objects_full[i,3]==1|objects_full[i,4]==1|objects_full[i,9]==1||objects_full[i,10]==1|objects_full[i,11]==1|objects_full[i,12]==1|objects_full[i,13]==1|objects_full[i,14]==1|objects_full[i,15]==1|objects_full[i,16]==1){
    gen_objects_stim[i,] = NA
  } else {
    gen_objects_stim[i,] = objects_full[i,]
  }
}

gen_objects_stim = na.omit(gen_objects_stim)
names(gen_objects_stim) = NULL
rownames(gen_objects_stim) = c(1:15)





# AMBIGUOUS CAUSES 1
ambig_objects_stim_1 = as.data.frame(matrix(NA, nrow=100, ncol=16))
for(i in 1:nrow(objects_full)){
  if(objects_full[i,1]==1|objects_full[i,2]==1|objects_full[i,3]==1|objects_full[i,4]==1|objects_full[i,5]==1|objects_full[i,6]==1|objects_full[i,7]==1|objects_full[i,8]==1|objects_full[i,13]==1|objects_full[i,14]==1|objects_full[i,15]==1|objects_full[i,16]==1){
    ambig_objects_stim_1[i,] = NA
  } else {
    ambig_objects_stim_1[i,] = objects_full[i,]
  }
}

ambig_objects_stim_1 = na.omit(ambig_objects_stim_1)
names(ambig_objects_stim_1) = NULL
rownames(ambig_objects_stim_1) = c(1:15)





# AMBIGUOUS CAUSES 2
ambig_objects_stim_2 = as.data.frame(matrix(NA, nrow=100, ncol=16))
for(i in 1:nrow(objects_full)){
  if(objects_full[i,1]==1|objects_full[i,2]==1|objects_full[i,3]==1|objects_full[i,4]==1|objects_full[i,5]==1|objects_full[i,6]==1|objects_full[i,7]==1|objects_full[i,8]==1|objects_full[i,9]==1|objects_full[i,10]==1|objects_full[i,11]==1|objects_full[i,12]==1){
    ambig_objects_stim_2[i,] = NA
  } else {
    ambig_objects_stim_2[i,] = objects_full[i,]
  }
}

ambig_objects_stim_2 = na.omit(ambig_objects_stim_2)
names(ambig_objects_stim_2) = NULL
rownames(ambig_objects_stim_2) = c(1:15)


# ABSENT CAUSE
absent_cause = data.frame(x = c('0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0'))
names(absent_cause) = NULL
rownames(absent_cause) = NULL


# AMBIGUOUS OBJECTS
ambig_objects = data.frame(x = c('1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0', 
                                 '0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1'))
rownames(ambig_objects) = NULL
names(ambig_objects) = NULL


# PRETRAINING
sink('pretrain_prev_gen_ambig.ex')
for(i in 1:nrow(gen_objects_stim)){
  # Generative Causes Left #
  cat(paste("name:","GenerativeCauseLeft",rownames(gen_objects_stim)[i], "\n", sep=""))
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
  
  
  # Generative Causes Right #
  cat(paste("name:","GenerativeCauseRight",rownames(gen_objects_stim)[i], "\n", sep=""))
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(gen_objects_stim[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  
  # Preventative Causes Right #
  cat("\n")
  cat(paste("name:","PreventativeCauseRight",rownames(prev_objects_stim)[i], "\n", sep=""))
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
  
  
  # Preventative Causes Left #
  cat("\n")
  cat(paste("name:","PreventativeCauseLeft",rownames(prev_objects_stim)[i], "\n", sep=""))
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(prev_objects_stim[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  
  # Ambiguous Causes 1 Right #
  cat("\n")
  cat(paste("name:","Ambiguous1CauseRight",rownames(ambig_objects_stim_1)[i], "\n", sep=""))
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(ambig_objects_stim_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  # Ambiguous Causes 2 Left #
  cat("\n")
  cat(paste("name:","Ambiguous1CauseLeft",rownames(ambig_objects_stim_1)[i], "\n", sep=""))
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(ambig_objects_stim_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  # Ambiguous Causes 2 Right #
  cat("\n")
  cat(paste("name:","Ambiguous2CauseRight",rownames(ambig_objects_stim_2)[i], "\n", sep=""))
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(ambig_objects_stim_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  # Ambiguous Causes 2 Left #
  cat("\n")
  cat(paste("name:","Ambiguous2CauseLeft",rownames(ambig_objects_stim_2)[i], "\n", sep=""))
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(ambig_objects_stim_2[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}
sink()





# HABITUATION - AMBIGUOUS OBJECTS
sink('habit_ambig_prev_gen_ambig.ex')
# A+
cat(paste("name:","A+", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(ambig_objects[1,], sep = "\t", quote = FALSE, row.names = FALSE)
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

# B-
cat(paste("name:","B-", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
# Object B
cat(paste("(Object_B)", sep="\t"))
print(ambig_objects[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste("(Input_Activation)", sep="\t"))
print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat("\n")

cat(paste("T:", "\n", sep="\t"))
cat(paste("(Output_Activation)", sep="\t"))
print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()


# TEST - AMBIGUOUS OBJECTS
sink('test_ambig_prev_gen_ambig.ex')
# Violation 1
cat(paste("name:","violation1", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(ambig_objects[1,], sep = "\t", quote = FALSE, row.names = FALSE)
# Object B
cat(paste("(Object_B)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste("(Input_Activation)", sep="\t"))
print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat("\n")

cat(paste("T:", "\n", sep="\t"))
cat(paste("(Output_Activation)", sep="\t"))
print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# Violation 2
cat(paste("name:","violation2", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
# Object B
cat(paste("(Object_B)", sep="\t"))
print(ambig_objects[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste("(Input_Activation)", sep="\t"))
print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat("\n")

cat(paste("T:", "\n", sep="\t"))
cat(paste("(Output_Activation)", sep="\t"))
print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# Ambiguous 1
cat(paste("name:","ambiguous1", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(ambig_objects[1,], sep = "\t", quote = FALSE, row.names = FALSE)
# Object B
cat(paste("(Object_B)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste("(Input_Activation)", sep="\t"))
print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat("\n")

cat(paste("T:", "\n", sep="\t"))
cat(paste("(Output_Activation)", sep="\t"))
print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# Ambiguous 2
cat(paste("name:","ambiguous2", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
# Object B
cat(paste("(Object_B)", sep="\t"))
print(ambig_objects[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste("(Input_Activation)", sep="\t"))
print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat("\n")

cat(paste("T:", "\n", sep="\t"))
cat(paste("(Output_Activation)", sep="\t"))
print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# Familiar 1
cat(paste("name:","familiar1", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(ambig_objects[1,], sep = "\t", quote = FALSE, row.names = FALSE)
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

# Familiar 2
cat(paste("name:","familiar2", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
# Object B
cat(paste("(Object_B)", sep="\t"))
print(ambig_objects[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste("(Input_Activation)", sep="\t"))
print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat("\n")

cat(paste("T:", "\n", sep="\t"))
cat(paste("(Output_Activation)", sep="\t"))
print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()



# HABITUATION - examines the influence of the starting state of the machine independent of objects A & B
sink('habit_all_absent_prev_gen_ambig.ex')
# A+
cat(paste("name:","A+", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
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

# B-
cat(paste("name:","B-", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
# Object B
cat(paste("(Object_B)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste("(Input_Activation)", sep="\t"))
print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat("\n")

cat(paste("T:", "\n", sep="\t"))
cat(paste("(Output_Activation)", sep="\t"))
print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()

# TEST - examines the influence of the starting state of the machine, independent of objects A & B
sink('test_all_absent_prev_gen_ambig.ex')
# Violation 1
cat(paste("name:","violation1", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
# Object B
cat(paste("(Object_B)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste("(Input_Activation)", sep="\t"))
print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat("\n")

cat(paste("T:", "\n", sep="\t"))
cat(paste("(Output_Activation)", sep="\t"))
print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# Violation 2
cat(paste("name:","violation2", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
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


# Ambiguous 1
cat(paste("name:","ambiguous1", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
# Object B
cat(paste("(Object_B)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste("(Input_Activation)", sep="\t"))
print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat("\n")

cat(paste("T:", "\n", sep="\t"))
cat(paste("(Output_Activation)", sep="\t"))
print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# Ambiguous 2
cat(paste("name:","ambiguous2", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
# Object B
cat(paste("(Object_B)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste("(Input_Activation)", sep="\t"))
print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat("\n")

cat(paste("T:", "\n", sep="\t"))
cat(paste("(Output_Activation)", sep="\t"))
print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# Familiar 1
cat(paste("name:","familiar1", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
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

# Familiar 2
cat(paste("name:","familiar2", "\n", sep=""))
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(Object_A)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
# Object B
cat(paste("(Object_B)", sep="\t"))
print(absent_cause[1,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste("(Input_Activation)", sep="\t"))
print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat("\n")

cat(paste("T:", "\n", sep="\t"))
cat(paste("(Output_Activation)", sep="\t"))
print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()


