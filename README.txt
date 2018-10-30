##########
# README #
##########

The files in this folder can be used to run different versions of the computational model. Specifically, there are two network script files and several different pretraining, habituation, and test files.




### model_instantiation_ambig.in ###
The "model_instantiation_ambig.in" file loads a network with one set of hidden units and different input and two groups in the input and output layer. Each group corresponds to the left-right location of the pretraining objects and the habituation and test stimuli. The network is name is appended with "ambig" to denote the fact that the pretraining set includes preventative causes, generative causes, AND ambiguous causes. The one downside to this network--which will become evident the longer you "habituate" the network is that it's susceptible to catastrophic interference.


### model_instantiation_ambig_fast_slow.in ###
The "model_instantiation_ambig_fast_slow.in" file is identical to the "model_instantiation_ambig.in" in all respects except that it implements a fast-slow learning system. There are several *fixes* to prevent catastrophic interference (e.g., interleaving or sparse representations), but I've chosen to implement a fast-slow system because it's somewhat biologically plausible and instantiates a rudimentary model of working memory.


### pretrain_prev_gen_ambig_freq.ex; pretrain_prev_gen_ambig.ex ###
These files can, and should, be used to provide the networks with "real-world" exposure to preventative, generative, and ambiguous cases. The only difference between both example files is that the former can be used to manipulate how frequently infants experience causes of a particular kind. The frequency of exposure to each case in the latter case is equal.


### habit_all_absent_prev_gen_ambig.ex; test_all_absent_prev_gen_ambig.ex ####
These files can be used to habituate networks to and test them on on a set in which only state information is known. I created these example sets strictly to ensure that pretraining on preventative, generative, and ambiguous cases did not lead to a bias for particular state change patterns. You can actually *ignore* this file.


### habit_ambig_prev_gen_ambig.ex; test_ambig_prev_gen_ambig.ex ###
These files can be used to habituate networks to and test them on the habituation and test stimuli to which infants will be habituated and tested.
