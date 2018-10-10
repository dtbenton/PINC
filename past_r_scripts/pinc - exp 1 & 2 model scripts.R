              #################################
###########################################################
# Preventative Inhibitory Negative Causality (PINC) Model #
###########################################################
              #################################

# This script can be used to generate the training and test files 
# that correspond to experiments 0A and 0B with infants. 
# This script can also be extended to model the developmental trajectory
# of preventative causal perception in infants between 6 months and 18 months.

              
              
## OBJECTS ##
objects = data.frame(x = c('1 1 1 1 1 0 0 0 0 0', 
                           '0 0 0 0 0 1 1 1 1 1',
                           '0 0 0 0 0 0 0 0 0 0'))
names(objects) = NULL

# A: 1 1 1 1 1 0 0 0 0 0
# B: 0 0 0 0 0 1 1 1 1 1 
# Absent: 0 0 0 0 0 0 0 0 0 0


## OUTCOME EVENTS ##
outcomes = data.frame(x = c('1 0','0 1'))
names(outcomes) = NULL

# Detector ON: 1 0
# Detector OFF: 0 1

## FIRST HABITUATION INSTANTIATION ##
# Non-autoencoder
sink('pincmodel_train_instantiation_1.ex')
for(i in 1:1){
  # A+ event #
  cat(paste("name: AplusEvent", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  # B- event #
  cat("\n")
  cat(paste("name: BminusEvent", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
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

## FIRST TEST INSTANTIATION ##
sink('pincmodel_test_instantiation_1.ex')
for(i in 1:1){
  # Familiar Event_1_A #
  cat(paste("name: FamiliarEvent1A", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  # Familiar Event_2_B #
  cat("\n")
  cat(paste("name: FamiliarEvent2B", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  # Novel Event_1_A #
  cat("\n")
  cat(paste("name: NovelEvent1A", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  # Novel Event_2_B #
  cat("\n")
  cat(paste("name: NovelEvent2B", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  # Ambiguous Event_1_A #
  cat("\n")
  cat(paste("name: AmbiguousEvent1A", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  # Ambiguous Event_2_B #
  cat("\n")
  cat(paste("name: AmbiguousEvent2B", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
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





## SECOND HABITUATION INSTANTIATION ##
# Faux-autoencoder
sink('pincmodel_train_instantiation_2.ex')
for(i in 1:1){
  # A+ event #
  cat(paste("name: AplusEvent", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A_out)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B_out)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  # B- event #
  cat("\n")
  cat(paste("name: BminusEvent", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A_out)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B_out)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}
sink()



## SECOND TEST INSTANTIATION ##
sink('pincmodel_test_instantiation_2.ex')
for(i in 1:1){
  # Familiar Event_1_A #
  cat(paste("name: FamiliarEvent1A", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A_out)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B_out)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  # Familiar Event_2_B #
  cat("\n")
  cat(paste("name: FamiliarEvent2B", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A_out)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B_out)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  # Novel Event_1_A #
  cat("\n")
  cat(paste("name: NovelEvent1A", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A_out)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B_out)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  # Novel Event_2_B #
  cat("\n")
  cat(paste("name: NovelEvent2B", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A_out)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B_out)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  # Ambiguous Event_1_A #
  cat("\n")
  cat(paste("name: AmbiguousEvent1A", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A_out)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B_out)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  # Ambiguous Event_2_B #
  cat("\n")
  cat(paste("name: AmbiguousEvent2B", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A_out)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B_out)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
}
sink()



## EXP0B INSTANTIATION ##
# Non-autoencoder
sink('pincmodel_train0B_instantiation_1.ex')
for(i in 1:1){
  # A+ event #
  cat(paste("name: AplusEvent", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Input_Activation)", sep="\t"))
  print(outcomes[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat("\n")
  
  cat(paste("T:", "\n", sep="\t"))
  cat(paste("(Output_Activation)", sep="\t"))
  print(outcomes[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  # B- event #
  cat("\n")
  cat(paste("name: BminusEvent", "\n", sep="\t")) 
  cat(paste("I:", "\n", sep="\t"))
  # Object A
  cat(paste("(Object_A)", sep="\t"))
  print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  # Object B
  cat(paste("(Object_B)", sep="\t"))
  print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
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