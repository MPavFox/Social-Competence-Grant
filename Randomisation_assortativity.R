set.seed(2)

#Randomisation for assessing reciprocity in adult rats
n = 16 #number of total subjects
ids = paste0("rat",1:n)
#randomly select 6 groups out of the 8
groups = sample(1:8,6)

#Second step: counterbalance experimental condition and phases across groups
#Third step: randomise testing order across groups
#Fourth step: Find partners, as subjects that were already tested from the same group OR tested in ontogeny that did not participate in the randomisation

###RANDOMISE SUBJECTS#-#-
#First create dataframe with information of subjects that need to be excluded because were already tested for PR during ontogeny
exclude = data.frame(group = c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8),
                     subject = c("rat1","rat4","rat7",#group1
                                 "rat7","rat1","rat3",#group2
                                 "rat4","rat3","rat6",#group3
                                 "rat8","rat11","rat14",#group4
                                 "rat10","rat1","rat15",#group5
                                 "rat3","rat13","rat11",#group6
                                 "rat15","rat6","rat2",#group7
                                 "rat4","rat16","rat5"))#group8


#Dataframe where to input data
rand_data = data.frame(group = "",
                       subject = "")

for(i in 1:length(groups)){
    temp = data.frame(group = rep(groups[i], n),
                      subject = sample(ids))
    
    #Randomise subjects
    ids_excl = exclude$subject[exclude$group == groups[i]]
    temp = temp[-match(ids_excl, temp$subject),]#remove rats that were tested
    
    #Bind to rand_data
    rand_data = rbind(rand_data,temp)
      }
    #Remove first empty row
    rand_data = rand_data[-1, ]
    
  #Counterbalance conditions and phases
    conditions = c("control","experimental")
    phases = c("open","close")   
    #Conditions
    rand_data$condition1 = sample(rep(conditions,39))
    rand_data$condition2 = ifelse(rand_data$condition1 == "control","experimental","control")
    #Phases
    rand_data$phase1_cond1 = sample(rep(phases, 39))
    rand_data$phase2_cond1 = ifelse(rand_data$phase1_cond1 == "open","close","open")
    rand_data$phase1_cond2 = sample(rep(phases, 39))
    rand_data$phase2_cond2 = ifelse(rand_data$phase1_cond2 == "open","close","open")
    
  
    #RANDOMISE TESTING ORDER ACROSS GROUPS#-#-
    rand_data = rand_data[sample(1:78),]
    
    #Add partners restricting to animals that have been already tested (this includes those tested during ontogeny)
    rand_data$partner = NA#initialise column
    
    #Loop through each group
    for(i in 1:length(groups)){
        data = rand_data[rand_data$group == groups[i],]
        ids_excl = exclude$subject[exclude$group == groups[i]]#recover excluded animals
        #Loop through each rat per group
        for(k in 1:nrow(data)){
          if(k == 1){#only for first rat sample from rats tested in ontogeny
            data$partner[k] = sample(ids_excl,1)
          }
          #For other rats, sample from those tested before including those in ontogeny and WITHOUT REPETITION
          else{
          list_ids = data$subject[1:(k-1)]#vector containing all ids tested before
            #Append ids tested during ontogeny
            list_ids = c(ids_excl,list_ids)
            #Remove those that have already participated as partners
            list_ids = list_ids[-match(data$partner[!is.na(data$partner)],list_ids)]
            #Sample to find partner
            data$partner[k] = sample(list_ids,1)
          }
        }
        
        #Add partners to main dataframe
        rand_data$partner[rand_data$group == groups[i]] = data$partner
    }
        
    #reorder to follow conditions
    rand_data = rand_data[,c(1:2,9,3,5:6,4,7:8)]
    
    #Save randomisation 
    write.csv(rand_data, "C:/Users/mp660/OneDrive - University of St Andrews/Social Competence Grant/Experimental design/randomisation_assortativity.csv", row.names=FALSE)   
    
    
