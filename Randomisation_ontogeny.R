
#DEFINE FUNCTIONS USED 
#Subfunction 1: randomSubj
#Randomise subjects within tests
#Detects automatically if the number of subjects to test are more or less than the number of tests, and will repeat IDs accordingly
#For our specific purpose, even if the number of subjects is greater than the number of test, it will repeat IDs (same.ids = TRUE). Can be modified by setting it to FALSE, this will draw from the whole population
randomSubj = function(ids, reps, sample_l, tests,same.ids = TRUE){#same.ids specify if we want to keep consistent the individuals tested across blocks even if there are more to sample from 
                      if(length(ids) < sample_l){#If number of animals to sample from is smaller than sample length (number of tests per block)
                        #Sample with repetition (number of repetitions defined by "reps")
                        rand_ids = sample(rep(ids, reps),sample_l)
                        #make sure same id does not appear consecutively
                        while(any(rle(rand_ids)$lengths > 1)){
                          rand_ids = sample(rep(ids, reps),sample_l)
                        }
                      }
        
           else{#If number of animals to sample is equal or greater than sample length:
             #we can either force repetition (same.ids == TRUE) or sample without repetition (same.ids == FALSE)(one test per rat)
                     if(same.ids == TRUE){#forcing repetition
                        rand_ids = rep(sample(ids,(sample_l/reps)),reps)
                        #make sure same id does not appear consecutively
                        while(any(rle(rand_ids)$lengths > 1)){
                          rand_ids = rep(sample(ids,(sample_l/reps)),reps)
                              }
                            }
                      else{#If same.ids == FALSE
                           #Sample without repetition
                           rand_ids = sample(ids,sample_l)
                           while(any(rle(rand_ids)$lengths > 1)){
                             rand_ids = sample(rand_ids,sample_l)
                           }
                          }
                      }
  
                #RANDOMISE TESTS
                #Sample tests without repetition as sample length equals number of tests
                rand_test = sample(tests,sample_l)
                #Keep sampling tests until subjects for G and H are not the same
                idx_GH = which(rand_test == "G"|rand_test == "H")#First identify ids for those tests
                while(any(rle(rand_ids[idx_GH])$lengths > 1)){
                  rand_test = sample(tests,sample_l)
                  idx_GH = which(rand_test == "G"|rand_test == "H")   
                }
                #Create a temporary vector with id and test 
                P_sample = paste(rand_ids,rand_test)  
                return(P_sample)
              }

#FUNCTION 1: randomPeriod
#Uses Ids and test randomised by randomSubj and organise them by blocks (periods)
#Makes sure that no animal is tested with the same test across periods
#Give as output a dataframe where subjects are randomised per test within periods and within groups. columns = (group, period,subject,test)
randomPeriod = function(g,tests,p,ids,sample_l,reps){
            #Create dataframe where to store data
            rand_exp = data.frame(group = "",#determined by number of randomisation (one per group)
                                  period = "",
                                  subject = "",
                                  test = "")
            for(i in 1:g){
              #Create a temporary dataframe where info for each group will be stored (1 column per block)
              randomisations = data.frame(matrix(NA, nrow = sample_l, ncol = p))
              #within each group randomise per period (p)
              for(k in 1:p){
                  #For period 1, choose individuals to be randomised
                  if(k == 1){
                    #Call function to randomise subjects
                      P_sample = randomSubj(ids, reps, sample_l, tests,same.ids = TRUE)
                    }#Repetitions are not a problem for the first period
      
                  else{#If period 2 or 3
                     if(length(ids) < sample_l){#if small group consider all ids
                        P_sample = randomSubj(ids, reps, sample_l, tests,same.ids = TRUE)
                        #Check for repetitions
                        while(any(P_sample %in% unlist(randomisations))){
                          #Use function to randomise Ids and tests
                          P_sample = randomSubj(ids, reps, sample_l, tests,same.ids = TRUE)
                        }
                     } 
                      else{#If large groups consider only ids from first period
                        P_sample = randomSubj(ids = rand_exp$subject[rand_exp$group == i & rand_exp$period == 1], reps = reps, sample_l, tests,same.ids = FALSE)
                        #Check for repetitions and keep looping if there is any
                        while(any(P_sample %in% unlist(randomisations))){
                          #Use function to randomise Ids and tests
                          P_sample = randomSubj(ids = rand_exp$subject[rand_exp$group == i & rand_exp$period == 1], reps = reps, sample_l, tests,same.ids = FALSE)
                     }
                      }
                  }
                #Store in randomisations dataframe
                randomisations[,k] = P_sample
                
                #Fill main dataframe
                temp = data.frame(group = rep(i, sample_l),#determined by number of randomisations (one per group)
                                  period = rep(k, sample_l),
                                  subject = rep("",sample_l),
                                  test = rep("",sample_l))
                
                split_data = stringr::str_split_fixed(randomisations[,k], " ", 2)
                temp$subject = split_data[,1]
                temp$test = split_data[,2]
                rand_exp = rbind(rand_exp,temp)
              }
            }
            #Remove first empty row
            rand_exp = rand_exp[-1, ]
            return(rand_exp)
            }

#FUNCTION 2: randomPartner
#Requires as input the dataframe generated by randomPeriod containing (group, period,subject,test)
#THIS FUNCTION IS STILL VERY SPECIFIC FOR OUR PURPOSE, NEEDS TO BE MADE MORE GENERIC**
#Specifics include:
#Randomise partners for a subset of test (not all test require random partners or cagemates as partners)
#Some of those partners that are not randomised is because they are reciprocal, that is, are focal and partner simultaneously in the Generosity/Helping test
#One of the test requires 2 partners
randomPartner = function(dataframe,g,p,soc_test = 5){ 
            dataframe$partner = ""#initialise column where partner will be
            #Loop to run through each period within groups
           for(i in 1:g){
            for(k in 1:p){
              #subset to group and block/period of interest
              data =  dataframe[dataframe$group == i & dataframe$period == k,] 
              #Create list of subjects for test that do require a partner
              idx_subj = which(data$test== "ST"|data$test== "PR"|data$test == "EC"|
                                 data$test == "STI"|data$test == "G")
              subjects = data$subject[idx_subj]
              #RANDOMISE PARTNERS
              #First exclude once those that do not participate in randomisation (G and H)
              idx = which(data$test == "G"|data$test == "H")
              ids = data$subject[-idx]
              
              #Randomise list to find partners for all test that require one (soc_test = 5)
              rand_partner = sample(ids,soc_test)
              #Keep randomising until 1) there is no match between subjects and partners, 2) if partner for "G" and "PR" is the same as test are very similar,
              #3) other rat in generosity is not subject in helping
              while(any(subjects == rand_partner)|rand_partner[which(data$test[idx_subj] == "G")] == rand_partner[which(data$test[idx_subj] == "PR")]|
                    rand_partner[which(data$test[idx_subj] == "G")] == data$subject[data$test ==  "H"]){
                rand_partner = sample(ids,soc_test)
              }
              
              #Add to dataframe
              data$partner[idx_subj] = rand_partner
              #Add reciprocal partners
              data$partner[data$test == "G"] = paste0(data$partner[data$test == "G"],",",data$subject[data$test == "H"])
              data$partner[data$test == "H"] = data$subject[data$test == "G"]
              
              #Add to main dataframe
              dataframe$partner[dataframe$group == i & dataframe$period == k] = data$partner
            }
          }
          return(dataframe)
  }

#FUNCTION 3: randomCond
#Counterbalance conditions (control and experimental as default) within tests and periods across groups
#Requires as input the dataframe generated by randomPeriod containing (group, period,subject,test), output from randomPartner also works
randomCond = function(dataframe,p, tests,conditions = c("control","experimental")){
             rand_exp = dataframe
             #initialise columns 
             rand_exp$condition1 = ""
             rand_exp$condition2 = ""
             
             for(i in 1:p){#number of periods
               #subset data per period
               data = rand_exp[rand_exp$period == i,]
               
               #Loop through each test in data to randomise for the tests required
               for(k in 1:length(tests)){
                 #If test is one of those that require counterbalancing (helping not included as it depends on Generosity test)
                 if(tests[k] == "G"|tests[k] == "EC"|tests[k] == "PR"|tests[k] == "NR"){
                   #find idx for that test in the period
                   idx_test = which(data$test == tests[k])
                   #randomise conditions per test
                   rand_cond = sample(rep(conditions, 4),length(tests))
                   #Assign condition1 to test
                   data$condition1[idx_test] = rand_cond
                   #Assign condition 2
                   data$condition2[idx_test] = ifelse(data$condition1[idx_test] == "control","experimental","control")
                 }
                 #If is not one of the tests
                 else{
                   #find idx for that test in the period
                   idx_test = which(data$test == tests[k])
                   #for other tests there is no control 
                   data$condition1[idx_test] = "experimental"
                   data$condition2[idx_test] = "-"
                 }
                 
               }
               #Add data to main dataframe
               rand_exp$condition1[rand_exp$period == i] = data$condition1
               rand_exp$condition2[rand_exp$period == i] = data$condition2
             }
            
             return(rand_exp) 
        }

#FUNCTION4: randomPhase
#Counterbalance phases (open,close) within conditions, test and period across groups
#Requires as input the dataframe generated by randomCond containing (group,period,subject,test,partner,condition1,condition2)
randomPhase = function(dataframe,p,phases = c("open","close"),tests,conditions= c("control","experimental")){
              rand_exp = dataframe

              #initialise columns right after control and experiment columns
              rand_exp$phase1_cond1 = ""
              rand_exp$phase2_cond1 = ""
              rand_exp$phase1_cond2 = ""
              rand_exp$phase2_cond2 = ""
              #reorder to follow conditions
              rand_exp = rand_exp[,c(1:6,8:9,7,10:11)]
              
              #Phases should be counterbalanced per condition, test and period across groups
              for(i in 1:p){#number of periods
                #subset data per period
                data = rand_exp[rand_exp$period == i,]
                
                #Loop through each test in data to randomise for the tests required
                for(k in 1:length(tests)){
                  #If test is one of those that require randomisation for phases (Helping not included as it depends on Generosity test)
                  if(tests[k] == "G"|tests[k] == "PR"){
                    #RANDOMISE PER TEST/CONDITION
                    for(j in 1:length(conditions)){
                      idx_test_cond = which(data$test == tests[k] & data$condition1 == conditions[j])
                      #Fill random phases for conditions
                      data$phase1_cond1[idx_test_cond] = sample(rep(phases, 2), 4)
                      data$phase1_cond2[idx_test_cond] = sample(rep(phases, 2), 4)
                      data$phase2_cond1[idx_test_cond] = ifelse(data$phase1_cond1[idx_test_cond] == "open","close","open")
                      data$phase2_cond2[idx_test_cond] = ifelse(data$phase1_cond2[idx_test_cond] == "open","close","open")
                    }
                  }
                  else{
                    data$phase1_cond1[data$test == tests[k]] = "-"
                    data$phase2_cond1[data$test == tests[k]] = "-"
                    data$phase1_cond2[data$test == tests[k]] = "-"
                    data$phase2_cond2[data$test == tests[k]] = "-"
                  }
      
      
                }
                #Add data to main dataframe
                rand_exp$phase1_cond1[rand_exp$period == i] = data$phase1_cond1
                rand_exp$phase2_cond1[rand_exp$period == i] = data$phase2_cond1
                rand_exp$phase1_cond2[rand_exp$period == i] = data$phase1_cond2
                rand_exp$phase2_cond2[rand_exp$period == i] = data$phase2_cond2
              }
              return(rand_exp)
            }

#-#-#-#-#-##-#-#-#-#--#-#-#-#-#-#-#-#-#-#-#-#--#-#-#-
#DEFINE VARIABLES TO BE USED
#g = Number of groups to randomise
#Define tests (abbreviations)
tests = c("H", "G", "EC","ST", "STI", "SM","PR","NR")
#p = blocks or periods in our case
n = 16#Number of individuals per group
#sample_l = Length of sample (determined by the number of tests per block)
#reps = Number of repetitions if any of the parameters above needs to be repeated

#SPECIFY PARAMETERS
g = 8;tests = tests;p = 3;ids = paste0("rat",1:n);sample_l = 8;reps = 2L

#Generates replicable randomisations
set.seed(1)

#Use function to randomise IDs and tests
rand_exp = randomPeriod(g = g,tests = tests,p = p,ids = ids,sample_l = sample_l,reps = reps)
#Randomise partners
rand_exp = randomPartner(dataframe = rand_exp,g = g,p = p,soc_test = 5) 
#Counterbalance conditions within periods
rand_exp = randomCond(rand_exp, p = 3, tests = tests)#conditions as default
#Counterbalance phases within conditions,test,periods across groups
rand_exp = randomPhase(rand_exp,p,tests = tests)#phases and conditions as default


#Add housing condition
rand_exp$housing = ifelse(n == 4, "small","large")
#Rename to merge randomisations for both housings
rand_small = rand_exp
#rm(rand_exp)

rand_large = rand_exp
#rm(rand_exp)

#Merge
rand_final = rbind(rand_small,rand_large) 

#Helping will be assessed simultaneously while testing generosity so there is no need to randomise testing times
helping = rand_final[rand_final$test == "H",]
#remove helping from main dataframe
rand_final = rand_final[rand_final$test != "H",]

#We need to randomise individual testing times between groups and conditions but within periods
rand_final[rand_final$period == 1,] = rand_final[rand_final$period == 1,][sample(1:112),]#first period
rand_final[rand_final$period == 2,] = rand_final[rand_final$period == 2,][sample(1:112),]#second period
rand_final[rand_final$period == 3,] = rand_final[rand_final$period == 3,][sample(1:112),]#third period

#Export as text/excel
#Save independent randomisations for small and large groups
write.csv(rand_small, "C:/Users/mp660/OneDrive - University of St Andrews/Social Competence Grant/Experimental design/randomisation_SG.csv", row.names=FALSE)
write.csv(rand_large, "C:/Users/mp660/OneDrive - University of St Andrews/Social Competence Grant/Experimental design/randomisation_LG.csv", row.names=FALSE)

#Save final randomisation (second partner in G correspond is the focal for helping)
write.csv(rand_final, "C:/Users/mp660/OneDrive - University of St Andrews/Social Competence Grant/Experimental design/randomisation_final.csv", row.names=FALSE)

#-#-#-#-#-##-#-#-#-#--#-#-#-#-#-#-#-#-#-#-#-#--#-#-#-

