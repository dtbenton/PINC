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
objects_full = combos(20)$binary


# PREVENTATIVE CAUSES
prev_objects_stim = as.data.frame(matrix(NA, nrow=100, ncol=12))
for(i in 1:nrow(objects_full)){
  if(objects_full[i,6]==1|objects_full[i,7]==1|objects_full[i,8]==1|objects_full[i,9]==1|objects_full[i,10]==1|objects_full[i,11]==1|objects_full[i,12]==1|objects_full[i,13]==1|objects_full[i,14]==1|objects_full[i,15]==1|objects_full[i,16]==1|objects_full[i,17]==1|objects_full[i,18]==1|objects_full[i,19]==1|objects_full[i,20]==1){
    prev_objects_stim[i,] = NA
  } else {
    prev_objects_stim[i,] = objects_full[i,]
  }
}
prev_objects_stim = na.omit(prev_objects_stim)
names(prev_objects_stim) = NULL
rownames(prev_objects_stim) = c(1:7)





# GENERATIVE CAUSES
gen_objects_stim = as.data.frame(matrix(NA, nrow=nrow(objects_full), ncol=12))
for(i in 1:nrow(objects_full)){
  if(objects_full[i,1]==1|objects_full[i,2]==1|objects_full[i,3]==1|objects_full[i,4]==1|objects_full[i,5]==1|objects_full[i,6]==1|objects_full[i,7]==1|objects_full[i,15]==1){
    gen_objects_stim[i,] = NA
  } else {
    gen_objects_stim[i,] = objects_full[i,]
  }
}

gen_objects_stim = na.omit(gen_objects_stim)
names(gen_objects_stim) = NULL
rownames(gen_objects_stim) = c(1:7)





# AMBIGUOUS CAUSES 1
ambig_objects_stim_1 = as.data.frame(matrix(NA, nrow=nrow(objects_full), ncol=12))
for(i in 1:nrow(objects_full)){
  if(objects_full[i,1]==1|objects_full[i,2]==1|objects_full[i,3]==1|objects_full[i,4]==1|objects_full[i,5]==1|objects_full[i,6]==1|objects_full[i,7]==1|objects_full[i,8]==1|objects_full[i,9]==1|objects_full[i,10]==1|objects_full[i,11]==1|objects_full[i,12]==1|objects_full[i,13]==1|objects_full[i,14]==1){
    ambig_objects_stim_1[i,] = NA
  } else {
    ambig_objects_stim_1[i,] = objects_full[i,]
  }
}

ambig_objects_stim_1 = na.omit(ambig_objects_stim_1)
names(ambig_objects_stim_1) = NULL





# AMBIGUOUS CAUSES 2
ambig_objects_stim_2 = as.data.frame(matrix(NA, nrow=nrow(objects_full), ncol=12))
for(i in 1:nrow(objects_full)){
  if(objects_full[i,1]==1|objects_full[i,2]==1|objects_full[i,3]==1|objects_full[i,4]==1|objects_full[i,5]==1|objects_full[i,6]==1|objects_full[i,7]==1|objects_full[i,8]==1|objects_full[i,9]==1|objects_full[i,10]==1|objects_full[i,11]==1|objects_full[i,12]==1|objects_full[i,13]==1|objects_full[i,14]==1){
    ambig_objects_stim_2[i,] = NA
  } else {
    ambig_objects_stim_2[i,] = objects_full[i,]
  }
}

ambig_objects_stim_2 = na.omit(ambig_objects_stim_1)
names(ambig_objects_stim_2) = NULL

