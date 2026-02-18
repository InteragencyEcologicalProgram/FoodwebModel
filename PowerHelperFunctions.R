#Power helper functions
library(tidyverse)
library(pwr)
library(simr)
load("data/ZoopsforPower.RData")
load("Power/outputs/ProbabilityOfEffects.Rdata")
#this version has the datasets for each species
load("Power/zoopsbyspecies.RData")


PTAtax = c("Acartia_UnID Juvenile",
           "Acartiella sinensis Adult",
           # "Bosmina longirostris Adult",
           "Tortanus_UnID Juvenile",
           "Sinocalanus_UnID Juvenile",
           "Pseudodiaptomus forbesi Adult",
           "Daphnia_UnID Adult")
PTAtaxM = c("Alienacanthomysis macropsis Adult",
            # "Americorophium stimpsoni Adult",
            # "Orientomysis aspera Adult",
            "Orientomysis hwanhaiensis Adult",
            "Neomysis mercedis Adult",
            "Hyalella_UnID Adult",
            "Sinocorophium alienense Adult",
            "Hyperacanthomysis longirostris Adult")




#Filter to the time of year when taxa is most abundant
Filters = function(taxname, #name of taxon
                   data = zoopsLT2){ #dataset to filter, defaults to zoopsLT2 if not specified
  require(tidyverse)
  data = filter(data, Taxlifestage == taxname) #filter to taxon of interest
  
  #summarize by month and select the highest 5 months
  #but I need to make sure they are consecutive
  monthsdat = data %>%
    group_by(Month, Taxlifestage) %>%
    summarize(CPUE = mean(CPUE))%>%
    arrange(desc(CPUE)) %>%
    ungroup() %>%
    slice(1)
  
  #Deal with cases where consecutive months cross new years
  months = c(monthsdat$Month, monthsdat$Month+1, monthsdat$Month+2,monthsdat$Month-1,monthsdat$Month-2)
  months = case_when(months < 1 ~ months+12,
                     months > 12 ~ months-12,
                     TRUE ~ months)
  #filter data to months with highest catch
  datafiltered = dplyr::filter(data, Month %in% months) %>%
    mutate(logCPUE = log(CPUE +1, base =10),
           Yearf = as.factor(Year)) 
}

#######################################################################################
#now a function to test hte differfence betwen years within a region. OR subregion
SSPower = function(Dat, #dataset to use 
                   Years, #years to test - must be a sequence of consecuitve yeasr
                   RegionOrSubregion = "Region",
                   RegionX, #region or subregion to assess
                   EffectSize,
                   Breaks = c(2,5,10,20,30,50)) { #effect size
  
  #set up empty data frame
  stuff =   data.frame(tries = NA, sucess = NA, Breaks = NA, N = NA,  Comparison = NA, Effect = NA)
  
  #filter to region of interest
  if(RegionOrSubregion == "Region"){
  Dat2 = dplyr::filter(Dat, Region == RegionX) %>%
    dplyr::mutate(PA = case_when(CPUE ==0 ~0,
                                 TRUE ~ 1))
  } else{
    Dat2 = dplyr::filter(Dat, Subregion == RegionX)  %>%
      dplyr::mutate(PA = case_when(CPUE ==0 ~0,
                                   TRUE ~ 1))
  }
  #calculate percentage of zero catch. If there is more than 75% zeros, don't move on. 
  
  zip = sum(Dat2$PA/nrow(Dat2))
  
  if(zip < 0.25) {
    stuff =   data.frame(tries = 0, sucess = 0, Breaks = 0, N = 0, Comparison = NA, Region = RegionX,
                         Effect = EffectSize)
  } else {
    
  #iterate over each pair of years
  for(i in 1:(length(Years)-1)) {
    Dat3 = dplyr::filter(Dat2, Year %in% c(Years[i], Years[i+1]))
    
    #print region and year so we kno wwhere we are
    print(RegionX)
    print(Years[i])
    
    #second empty data frame
    stuffa =   data.frame(tries = NA, sucess = NA, Breaks = NA, N = NA, Comparison = NA, Effect = NA, Region = RegionX)
    
    #if there is no catch, power is 0
    if(sum(Dat3$CPUE) == 0 | length(unique(Dat3$Year)) ==1| length(unique(Dat3$Month)) < 5) {
      df = data.frame(tries = 0, sucess = 0, Breaks = 0, N = 0, Comparison = paste(Years[i], Years[i+1]), 
                      Effect = EffectSize, Region = RegionX)
      stuffa = bind_rows(stuffa, df)
    } else {
      
      #run a power simulation for each sample size (samples per region per month)
      for (breaks in Breaks) {
        
        #initial model on raw data

        mod = try(lmer(logCPUE ~ Yearf  + (1|Month), data = Dat3, 
                   control = lmerControl(check.conv.singular = "ignore")))
        if(class(mod)== "try-error") {
          df = data.frame(tries = 0, sucess = 0, Breaks = 0, N = 0, Comparison = paste(Years[i], Years[i+1]), 
                          Effect = EffectSize, Region = RegionX)
          stuffa = bind_rows(stuffa, df)
        } else {
        #reset the effect size
        fixyr = paste("Yearf", Years[i+1], sep = "")
        fixef(mod)[fixyr] = EffectSize
        print(breaks)
        nsim = 10
        
        #Replicate random subsets of data within each group to simulate increased/decreased sample size
        if(RegionOrSubregion == "Region"){
        object <-   group_by(Dat3, Month, Region, Yearf) %>%
          mutate(.simr_repl = cur_group_id())
        } else {
          object <-   group_by(Dat3, Month, Subregion, Yearf) %>%
            mutate(.simr_repl = cur_group_id())
        }
        newdata = dplyr::group_by(object, .simr_repl) %>%
          slice_sample(n =breaks, replace = TRUE)
        
        #test to make sure new data isn't zero catch
        if(sum(newdata$CPUE) == 0) {
          df = data.frame(tries = 0, sucess = 0, Breaks = breaks, N = 0, Comparison = paste(Years[i], Years[i+1]), 
                          Effect = EffectSize, Region = RegionX)
          stuffa = bind_rows(stuffa, df)
        } else {
          
          #set up the first model
          mod2 = try(lmer(logCPUE ~ Yearf  +  (1|Month), data = newdata, 
                      control = lmerControl(check.conv.singular = "ignore")))
          if(class(mod2)== "try-error") {
            df = data.frame(tries = 0, sucess = 0, Breaks = 0, N = 0, Comparison = paste(Years[i], Years[i+1]), 
                            Effect = EffectSize, Region = RegionX)
            stuffa = bind_rows(stuffa, df)
          } else{
          #set the effect size
          fixef(mod2)[fixyr] = EffectSize
          
          #start paralell sessions
          Sys.setenv(OMP_NUM_THREADS=1)
          plan(multisession,workers=16) 
          
          #Run the power analysis
          pstests <- future_replicate(nsim, try(powerSim(mod2, nsim=50, 
                                                     progress = FALSE, observedPowerWarning = FALSE)),
                                      future.globals = c("lmer","mod", "powerSim", "newdata", "dat", "mod2"),
                                      simplify = FALSE)
          plan(sequential)
          
          #set up final data frame
          foo = sum(sapply(pstests, function(stuff){stuff$x}))
          bar = sum(sapply(pstests, function(stuff){stuff$n}))
          df = data.frame(tries = bar, sucess = foo, Breaks = breaks, N = nrow(newdata), Comparison = paste(Years[i], Years[i+1]), 
                          Effect = EffectSize, Region = RegionX)
          
          stuffa = bind_rows(stuffa, df)
        }}}
      }
      
      
    }
    stuff = dplyr::bind_rows(stuff, stuffa) 
  }
  }
  return(stuff)
}

################################################################################################
#######################################################################################
#now a function to test hte differfence betwen years For the whole Delta
SSPowerD = function(Dat, #dataset to use 
                   Years, #years to test - must be a sequence of consecuitve yeasr
                   EffectSize,
                   Breaks = c(2,5,10,20,30,50)) { #effect size
  
  #set up empty data frame
  stuff =   data.frame(tries = NA, sucess = NA, Breaks = NA, N = NA,  
                       Comparison = NA, Effect = EffectSize)
  
  #calculate percentage of zero catch. If there is more than 75% zeros, don't move on. 
  
  Dat = mutate(Dat, PA = case_when(CPUE ==0 ~ 0,
                                   TRUE ~1))
  
  zip = sum(Dat$PA/nrow(Dat))
  
  if(zip < 0.25) {
    stuff =   data.frame(tries = 0, Breaks = 0, N = 0, 
                         Comparison = as.character(Years[1]), Effect = EffectSize)
  } else {
    
  
  #iterate over each pair of years
  for(i in 1:(length(Years)-1)) {
    Dat3 = dplyr::filter(Dat, Year %in% c(Years[i], Years[i+1]))
    
    #print region and year so we kno wwhere we are
    print(Years[i])
    
    #second empty data frame
    stuffa =   data.frame(tries = NA, sucess = NA, Breaks = NA, 
                          N = NA, Comparison = NA, Effect = EffectSize)
    
    #if there is no catch, power is 0
    if(sum(Dat3$CPUE) == 0 | length(unique(Dat3$Year)) ==1| length(unique(Dat3$Month)) < 5) {
      df = data.frame(tries = 0, sucess = 0, Breaks = 0, N = 0, 
                      Comparison = paste(Years[i], Years[i+1]), 
                      Effect = EffectSize)
      stuffa = bind_rows(stuffa, df)
    } else {
      
      #run a power simulation for each sample size (samples per region per month)
      for (breaks in Breaks) {
        
        #initial model on raw data
        
        mod = try(lmer(logCPUE ~ Yearf  + (1|Month) + (1|Region), data = Dat3, 
                       control = lmerControl(check.conv.singular = "ignore")))
        if(class(mod)== "try-error") {
          df = data.frame(tries = 0, sucess = 0, Breaks = 0, N = 0, 
                          Comparison = paste(Years[i], Years[i+1]), 
                          Effect = EffectSize)
          stuffa = bind_rows(stuffa, df)
        } else {
          #reset the effect size
          fixyr = paste("Yearf", Years[i+1], sep = "")
          fixef(mod)[fixyr] = EffectSize
          print(breaks)
          nsim = 10
          
          #Replicate random subsets of data within each group to simulate increased/decreased sample size
            object <-   group_by(Dat3, Month, Region, Yearf) %>%
              mutate(.simr_repl = cur_group_id())
          newdata = dplyr::group_by(object, .simr_repl) %>%
            slice_sample(n =breaks, replace = TRUE)
          
          #test to make sure new data isn't zero catch
          if(sum(newdata$CPUE) == 0) {
            df = data.frame(tries = 0, sucess = 0, Breaks = breaks, N = 0, 
                            Comparison = paste(Years[i], Years[i+1]), 
                            Effect = EffectSize)
            stuffa = bind_rows(stuffa, df)
          } else {
            
            #set up the first model
            mod2 = try(lmer(logCPUE ~ Yearf  +  (1|Month) + (1|Region), data = newdata, 
                            control = lmerControl(check.conv.singular = "ignore")))
            if(class(mod2)== "try-error") {
              df = data.frame(tries = 0, sucess = 0, Breaks = 0, N = 0, 
                              Comparison = paste(Years[i], Years[i+1]), 
                              Effect = EffectSize)
              stuffa = bind_rows(stuffa, df)
            } else{
              #set the effect size
              fixef(mod2)[fixyr] = EffectSize
              
              #start paralell sessions
              Sys.setenv(OMP_NUM_THREADS=1)
              plan(multisession,workers=16) 
              
              #Run the power analysis
              pstests <- future_replicate(nsim, try(powerSim(mod2, nsim=50, 
                                                             progress = FALSE, observedPowerWarning = FALSE)),
                                          future.globals = c("lmer","mod", "powerSim", "newdata", "dat", "mod2"),
                                          simplify = FALSE)
              plan(sequential)
              
              #set up final data frame
              foo = sum(sapply(pstests, function(stuff){stuff$x}))
              bar = sum(sapply(pstests, function(stuff){stuff$n}))
              df = data.frame(tries = bar, sucess = foo, Breaks = breaks, 
                              N = nrow(newdata), Comparison = paste(Years[i], Years[i+1]), 
                              Effect = EffectSize)
              
              stuffa = bind_rows(stuffa, df)
            }}}
      }
      
      
    }
    stuff = dplyr::bind_rows(stuff, stuffa) 
  }
  }
  return(stuff)
}


#######################################################################################
#now a function to test hte differfence betwen months within a region. 
MonthPower = function(Dat, #dataset to use 
                   Years, #years to test - must be a sequence of consecuitve yeasr
                   RegionX, #region or subregion to assess
                   RegionOrSubregion = "Region",
                   EffectSize) { #effect size
  require(pwr)
  #set up empty data frame
  stuff =   data.frame(Power = NA, Breaks = NA, N = NA,  Comparison = NA, Effect = EffectSize)
  
  #filter to region of interest
  if(RegionOrSubregion == "Region") {
  Dat2 = dplyr::filter(Dat, Region == RegionX) %>%
    dplyr::mutate(Monthf = as.factor(Month),
                  PA = case_when(CPUE ==0 ~0,
                                 TRUE ~ 1))
  } else {
    Dat2 = dplyr::filter(Dat, Subregion == RegionX) %>%
      dplyr::mutate(Monthf = as.factor(Month),
                    PA = case_when(CPUE ==0 ~0,
                                   TRUE ~ 1))
  }
  
  #calculate percentage of zero catch. If there is more than 75% zeros, don't move on. 
  
  zip = sum(Dat2$PA/nrow(Dat2))
  
  if(zip < 0.25) {
    stuff =   data.frame(Power = 0, Breaks = 0, N = 0, Comparison = as.character(Years[1]),
                         Region = RegionX, Effect = EffectSize)
  } else {
  
  #iterate over each pair of months
  for(i in 1:length(Years)) {
    Dat3 = dplyr::filter(Dat2, Year == Years[i])
    
    #print region and year so we kno wwhere we are
    print(RegionX)
    print(Years[i])
    
    #second empty data frame
    stuffa =   data.frame(Power =0, Breaks = NA, N = NA, Comparison = NA, 
                          Effect = EffectSize, Region = RegionX, Month = NA)
    Months = unique(Dat3$Month)
    #Now each pair of months at a time
    for(j in 1:5) {
      if(j+1 ==6) Dat4 = filter(Dat3, Monthf %in% c(Months[5], Months[1])) else Dat4 = filter(Dat3, Monthf %in% c(Months[j], Months[j+1]))
    
    
    
    #if there is no catch, power is 0
    if(sum(Dat4$CPUE) == 0) {
      df = data.frame(Power =0, Breaks = 0, N = 0, Comparison = paste(j, j+1), Year = Years[i],
                      Effect = EffectSize, Region = RegionX)
      stuffa = bind_rows(stuffa, df)
    } else {
      #if we only ended up with one month, power is 0
      if(length(unique(Dat4$Monthf))<2) {
        df = data.frame(Power =0, Breaks = 0, N = 0, Comparison = paste(j, j+1), Year = Years[i],
                        Effect = EffectSize, Region = RegionX)
        stuffa = bind_rows(stuffa, df)
      } else {
      
      #run a power simulation for each sample size (samples per region per month)
      for (breaks in c(2,5,10,20,30,50)) {
        
        
        #Replicate random subsets of data within each group to simulate increased/decreased sample size
        object <-   group_by(Dat4, Monthf, Region, Yearf) %>%
          mutate(.simr_repl = cur_group_id())
        
        newdata = dplyr::group_by(object, .simr_repl) %>%
          slice_sample(n =breaks, replace = TRUE)
        
        #test to make sure new data isn't zero catch
        if(sum(newdata$CPUE) == 0) {
          df = data.frame(Power =0, Breaks = breaks, N = 0, Comparison = paste(j, j+1), Year = Years[i], 
                          Effect = EffectSize, Region = RegionX, Month =Months[j])
          stuffa = bind_rows(stuffa, df)
        } else {
          #since this is just a linear model, not a mixed model, we can simplify. I think. 
          #set up the first model
          mod2 = try(lm(logCPUE ~ Monthf, data = newdata))
          cohensD = EffectSize/sd(newdata$logCPUE)
          pow = pwr.f2.test(u = 1, v = mod2$df.residual, f2 = cohensD, sig.level = 0.05)$power

          df = data.frame(Power = pow, Breaks = breaks, N = nrow(newdata), Comparison = paste(j, j+1), Year = Years[i], 
                          Effect = EffectSize, Region = RegionX, Month =Months[j])
          
          stuffa = bind_rows(stuffa, df)
        }
      }
    }
      
    }
    }
    stuff = dplyr::bind_rows(stuff, stuffa) 
  }
  }
  return(stuff)
}


#########################################################################


#######################################################################################
#now a function to test hte differfence betwen months across the whole Delta. 
MonthPowerD = function(Dat, #dataset to use 
                      Years, #years to test - must be a sequence of consecuitve yeasr
                    
                      EffectSize) { #effect size
  require(pwr)
  #set up empty data frame
  stuff =   data.frame(Power = NA, Breaks = NA, N = NA,  Comparison = NA, Effect = NA)
  
  Dat2 = Dat %>%
    dplyr::mutate(Monthf = as.factor(Month), PA = case_when(CPUE ==0 ~ 0, TRUE ~1))
  
  #calculate percentage of zero catch. If there is more than 75% zeros, don't move on. 
  
  zip = sum(Dat2$PA/nrow(Dat2))
  
  if(zip < 0.25) {
    stuff =   data.frame(Power = 0, Breaks = 0, N = 0, Comparison = as.character(Years[1]),
                          Effect = EffectSize)
  } else {
    
  
  #iterate over each pair of months
  for(i in 1:length(Years)) {
    Dat3 = dplyr::filter(Dat2, Year == Years[i])
    
    #print year so we kno wwhere we are
    print(Years[i])
    
    #second empty data frame
    stuffa =   data.frame(Power =0, Breaks = NA, N = NA, Comparison = NA, Effect = NA, Month = NA)
    Months = unique(Dat3$Month)
    #Now each pair of months at a time
    for(j in 1:5) {
      if(j+1 ==6) Dat4 = filter(Dat3, Monthf %in% c(Months[5], Months[1])) else Dat4 = filter(Dat3, Monthf %in% c(Months[j], Months[j+1]))
      
      
      
      #if there is no catch, power is 0
      if(sum(Dat4$CPUE) == 0) {
        df = data.frame(Power =0, Breaks = 0, N = 0, Comparison = paste(Years[i], Years[i+1]), Month = Months[j],
                        Effect = EffectSize)
        stuffa = bind_rows(stuffa, df)
      } else {
        
        #run a power simulation for each sample size (samples per region per month)
        for (breaks in c(2,5,10,20,30,50)) {
          
          
          #Replicate random subsets of data within each group to simulate increased/decreased sample size
          object <-   group_by(Dat4, Monthf, Region, Yearf) %>%
            mutate(.simr_repl = cur_group_id())
          
          newdata = dplyr::group_by(object, .simr_repl) %>%
            slice_sample(n =breaks, replace = TRUE)
          
          #test to make sure new data isn't zero catch
          if(sum(newdata$CPUE) == 0) {
            df = data.frame(Power =0, Breaks = breaks, N = 0, Comparison = paste(Years[i], Years[i+1]), 
                            Effect = EffectSize,  Month =Months[j])
            stuffa = bind_rows(stuffa, df)
          } else {
            #since this is just a linear model, not a mixed model, we can simplify. I think. 
            #set up the first model
            mod2 = lm(logCPUE ~ Monthf, data = newdata)
            cohensD = EffectSize/sd(newdata$logCPUE)
            pow = pwr.f2.test(u = 1, v = mod2$df.residual, f2 = cohensD, sig.level = 0.05)$power
            
            df = data.frame(Power = pow, Breaks = breaks, N = nrow(newdata), Comparison = paste(j, j+1), Year = Years[i], 
                            Effect = EffectSize, Month =Months[j])
            
            stuffa = bind_rows(stuffa, df)
          }
        }
      }
      
    }
    stuff = dplyr::bind_rows(stuff, stuffa) 
  }
}
      return(stuff)
}


#########################################################################


binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)
}

#######################################################################################
#now a function to test hte differfence betwen years within a region. 

#try a different way of expanding the data
MonthPower2 = function(Dat, #dataset to use 
                      Years, #years to test - must be a sequence of consecuitve yeasr
                      RegionX, #region to assess
                      EffectSize) { #effect size
  require(pwr)
  #set up empty data frame
  stuff =   data.frame(Power = NA, Breaks = NA, N = NA,  Comparison = NA, Effect = NA)
  
  #filter to region of interest
  Dat2 = dplyr::filter(Dat, Region == RegionX) %>%
    dplyr::mutate(Monthf = as.factor(Month))
  
  #iterate over each pair of months
  for(i in 1:length(Years)) {
    Dat3 = dplyr::filter(Dat2, Year == Years[i])
    
    #print region and year so we kno wwhere we are
    print(RegionX)
    print(Years[i])
    
    #second empty data frame
    stuffa =   data.frame(Power =0, Breaks = NA, N = NA, Comparison = NA, Effect = NA, Region = RegionX, Month = NA)
    Months = unique(Dat3$Month)
    #Now each pair of months at a time
    for(j in 1:5) {
      if(j+1 ==6) Dat4 = filter(Dat3, Monthf %in% c(Months[5], Months[1])) else Dat4 = filter(Dat3, Monthf %in% c(Months[j], Months[j+1]))
      
      
      
      #if there is no catch, power is 0
      if(sum(Dat4$CPUE) == 0) {
        df = data.frame(Power =0, Breaks = 0, N = 0, Comparison = paste(Years[i], Years[i+1]), Month = Months[j],
                        Effect = EffectSize, Region = RegionX)
        stuffa = bind_rows(stuffa, df)
      } else {
        
        #run a power simulation for each sample size (samples per region per month)
        for (breaks in c(2,5,10,20,30,50)) {
          
          
          #Replicate random subsets of data within each group to simulate increased/decreased sample size
          object <-   group_by(Dat4, Monthf, Region, Yearf) %>%
            mutate(.simr_repl = cur_group_id())
          
          foo = group_by(Dat4, Monthf, Yearf) %>%
            summarize(meanC = mean(logCPUE), sdC = sd(logCPUE))
          
          newdata = dplyr::group_by(object, .simr_repl) %>%
            slice_sample(n =breaks, replace = TRUE) %>%
            left_join(foo) %>%
            mutate(logCPUE2 = rnorm(breaks, mean = meanC, sd = sdC))
          
          #test to make sure new data isn't zero catch
          if(sum(newdata$logCPUE2) == 0) {
            df = data.frame(Power =0, Breaks = breaks, N = 0, Comparison = paste(Years[i], Years[i+1]), 
                            Effect = EffectSize, Region = RegionX, Month =Months[j])
            stuffa = bind_rows(stuffa, df)
          } else {
            #since this is just a linear model, not a mixed model, we can simplify. I think. 
            #set up the first model
            # mod2 = lm(logCPUE ~ Monthf, data = newdata)
            cohensD = EffectSize/sd(newdata$logCPUE2)
            pow = pwr.f2.test(u = 1, v = mod2$df.residual, f2 = cohensD, sig.level = 0.05)$power
            
            df = data.frame(Power = pow, Breaks = breaks, N = nrow(newdata), Comparison = paste(j, j+1), Year = Years[i], 
                            Effect = EffectSize, Region = RegionX, Month =Months[j])
            
            stuffa = bind_rows(stuffa, df)
          }
        }
      }
      
    }
    stuff = dplyr::bind_rows(stuff, stuffa) 
  }
  return(stuff)
}

#################################################################################################

#FlowPower3 does a power analysis on flow-abundance relationship sfor the entire Delta,
flowPower3 = function(Dat, #dataset to use
                      Years,  #Years to replicate over
                      EffectSize, #effect size of year

                      nYears=5,
                      Breaks = c(2,5,10,20, 30)) { #number of years to test - defaults to 5
  #set up empty data frame
  stuff =   data.frame(tries = NA, sucess = NA, Breaks = NA, N = NA,  Comparison = NA, Effect = EffectSize)
  
  #filter to the N year period of interest and do it for each N year period in the data
  for(i in 1:(length(Years)-nYears)) {
    Dat3 = dplyr::filter(Dat, Year %in% c(Years[i]:Years[i+(nYears-1)])) %>%
      mutate(PA = case_when(CPUE ==0 ~ 0,
                            TRUE ~ 1))
    print(Years[i])
    stuffa =   data.frame(tries = NA, sucess = NA, Breaks = NA, N = NA, Comparison = NA, Effect = EffectSize)
    
    #if there is no catch, or if there is more than 75% zero catch power is zero
    if(sum(Dat3$CPUE) == 0 | sum(Dat3$PA)/nrow(Dat3) <0.25) {
      df = data.frame(tries = 0, sucess = 0, Breaks = 0, N = 0, Comparison = paste(Years[i], Years[i+1]), 
                      Effect = EffectSize)
      stuffa = bind_rows(stuffa, df)
    } else {
      
      #iterate over all sample sizes

      for (breaks in Breaks) {
        
        #log transform outflow
        Dat3 = mutate(Dat3, logOUT = log(OUT, 10))
        
        #set up first model
        mod = lmer(logCPUE ~ logOUT+ (1|Yearf)  + (1|Month)+ (1|Region) , data = Dat3, 
                   control = lmerControl(check.conv.singular = "ignore"))
        
        #set effect size
        fixef(mod)["logOUT"] = EffectSize
        print(breaks)
        nsim = 10
        
        object <-   group_by(Dat3, Month, Region, Yearf) %>%
          mutate(.simr_repl = cur_group_id())
        #resample
        newdata = dplyr::group_by(object, .simr_repl) %>%
          slice_sample(n =breaks, replace = TRUE)
        
        
        if(sum(newdata$CPUE) == 0) {
          df = data.frame(tries = 0, sucess = 0, Breaks = breaks, N = 0, Comparison = paste(Years[i], Years[i+nYears]), 
                          Effect = EffectSize)
          stuffa = bind_rows(stuffa, df)
        } else {
          #second version of model with new sample size
          mod2 = lmer(logCPUE ~ logOUT+ (1|Yearf)  +(1|Month) + (1|Region), data = newdata, 
                      control = lmerControl(check.conv.singular = "ignore"))
          
          #set fixed effect
          fixef(mod2)["logOUT"] = EffectSize
          Sys.setenv(OMP_NUM_THREADS=1)
          plan(multisession,workers=16) 
          
          #power analysis
          pstests <- future_replicate(nsim, powerSim(mod2, nsim=50, 
                                                     progress = FALSE, observedPowerWarning = FALSE),
                                      future.globals = c("lmer","mod", "powerSim", "newdata", "dat", "mod2"),
                                      simplify = FALSE)
          plan(sequential)
          foo = sum(sapply(pstests, function(stuff){stuff$x}))
          bar = sum(sapply(pstests, function(stuff){stuff$n}))
          df = data.frame(tries = bar, sucess = foo, Breaks = breaks, N = nrow(newdata), 
                          Comparison = paste(Years[i], Years[i+nYears]), 
                          Effect = EffectSize)
          
          stuffa = bind_rows(stuffa, df)
        }
      }
      
      
    }
    stuff = dplyr::bind_rows(stuff, stuffa) 
  }
  return(stuff)
}

########################################################################
#this function looks at the flow-abundance relationship one region at a time. 

flowPowerRegion = function(Dat, Years, EffectSize, nYears=5, RegionX) {
  print(EffectSize)
  print(RegionX)
  
  #empty data frame to fill
  stuff =   data.frame(tries = NA, sucess = NA, Breaks = NA, N = NA,  Comparison = NA, Effect = EffectSize)
  
  #iterate over each N year period in your dataset
  for(i in 1:(length(Years)-nYears)) {
    
    #filter to regions and years of interest
    Dat3 = dplyr::filter(Dat, Year %in% c(Years[i]:Years[i+(nYears-1)]), 
                         Region == RegionX) %>%
      mutate(PA = case_when(CPUE ==0 ~0,
                            TRUE ~ 1))
    print(Years[i])
    stuffa =   data.frame(tries = NA, sucess = NA, Breaks = NA, N = NA, Comparison = NA, Effect = EffectSize)
    
    #Now power if there is zero catch
    if(sum(Dat3$CPUE) == 0 | sum(Dat3$PA)/nrow(Dat3)<0.25) {
      df = data.frame(tries = 0, sucess = 0, Breaks = 0, N = 0, Comparison = paste(Years[i], Years[i+1]), 
                      Effect = EffectSize)
      stuffa = bind_rows(stuffa, df)
    } else {
      
      #iterate over several sample sizes
      for (breaks in c(2, 5, 10,20,30, 50)) {
        
        #set up first model
        Dat3 = mutate(Dat3, logOUT = log(OUT, 10))
        mod = lmer(logCPUE ~ logOUT+ (1|Yearf)  + (1|Month) , data = Dat3, 
                   control = lmerControl(check.conv.singular = "ignore"))
        #change fixed effects
        fixef(mod)["logOUT"] = EffectSize
        print(breaks)
        nsim = 10
        
        #change sample size
        object <-   group_by(Dat3, Month, Region, Yearf) %>%
          mutate(.simr_repl = cur_group_id())
        
        newdata = dplyr::group_by(object, .simr_repl) %>%
          slice_sample(n =breaks, replace = TRUE)
        if(sum(newdata$CPUE) == 0) {
          df = data.frame(tries = 0, sucess = 0, Breaks = breaks, N = 0, 
                          Comparison = paste(Years[i], Years[i+nYears]), 
                          Effect = EffectSize)
          stuffa = bind_rows(stuffa, df)
        } else {
          
          #power analysis
          mod2 = lmer(logCPUE ~ logOUT+ (1|Yearf)  +(1|Month) , data = newdata, 
                      control = lmerControl(check.conv.singular = "ignore"))
          fixef(mod2)["logOUT"] = EffectSize
          Sys.setenv(OMP_NUM_THREADS=1)
          plan(multisession,workers=16) 
          
          pstests <- future_replicate(nsim, powerSim(mod2, nsim=50, 
                                                     progress = FALSE, observedPowerWarning = FALSE),
                                      future.globals = c("lmer","mod", "powerSim", "newdata", "dat", "mod2"),
                                      simplify = FALSE)
          plan(sequential)
          foo = sum(sapply(pstests, function(stuff){stuff$x}))
          bar = sum(sapply(pstests, function(stuff){stuff$n}))
          df = data.frame(tries = bar, sucess = foo, Breaks = breaks, N = nrow(newdata), 
                          Comparison = paste(Years[i], Years[i+nYears]), 
                          Effect = EffectSize, Region = RegionX)
          
          stuffa = bind_rows(stuffa, df)
        }
      }
      
      
    }
    stuff = dplyr::bind_rows(stuff, stuffa) 
  }
  return(stuff)
}


####################################################################################
#function to calculate necessary sample size for a 809% power given a sample/power curve

binomialthingy = function(dat) {
  if(sum(dat$tries) == 0) Break80 = NA else {
  dat$faliure = dat$tries - dat$sucess
  mod = glm(cbind(sucess, faliure) ~ Breaks, family = "binomial", data = dat)
  #80% power coresponds to 4:1 odds. the coeffiecient is the log of the ods ratio
  #so to get the number of breaks needed for 80% power I do... log(4) = Breaks*a + b, or Breaks = (log(4)-b)/a
  Break80 = as.numeric((log(4)- coef(mod)[1])/coef(mod)[2]) 
  }
  return(Break80)
}

allcombs = data.frame(Taxlifestage = c(PTAtax, PTAtaxM),SizeClass = c(rep("Meso", 6), rep("Macro",6))) %>%
  cross_join(data.frame(Effect = Effects)) %>%
  cross_join(data.frame(Region  = unique(pseudo$Region))) 


allzoop_n2 = group_by(filter(all_filtered_zoops, Source != "DOP", Taxlifestage != "Bosmina longirostris Adult"), 
                      Month, Taxlifestage, Region, Year) %>%
  summarize(N = n()) %>%
  filter(Year == 2019) %>%
  group_by(Taxlifestage, Region) %>%
  summarize(Breaks = mean(N)) %>%
  mutate(Taxa = case_when(Taxlifestage =="Acartia_UnID Juvenile" ~ "Acartia",
                          Taxlifestage =="Acartiella sinensis Adult" ~ "Acartiella",
                          Taxlifestage =="Pseudodiaptomus forbesi Adult" ~ "Pseudodiaptomus",
                          Taxlifestage =="Tortanus_UnID Juvenile" ~ "Tortanus",
                          Taxlifestage == "Sinocalanus_UnID Juvenile" ~ "Sinocalanus",
                          Taxlifestage == "Daphnia_UnID Adult" ~ "Daphnia",
                          Taxlifestage == "Hyalella_UnID Adult" ~ "Hyalella",
                          Taxlifestage == "Orientomysis hwanhaiensis Adult" ~ "Orientomysis",
                          Taxlifestage == "Alienacanthomysis macropsis Adult" ~ "Alienacanthomysis",
                          Taxlifestage == "Hyperacanthomysis longirostris Adult" ~ "Hyperacanthomysis",
                          Taxlifestage == "Sinocorophium alienense Adult" ~ "Sinocorophium",
                          Taxlifestage == "Neomysis mercedis Adult" ~ "N. mercedis"))


#now the mean over all the regions

allzoop_nAnnual = allzoop_n2 %>%
  group_by(Taxlifestage) %>%
  summarize(Breaks = mean(Breaks)) %>%
  mutate(SizeClass = case_when(Taxlifestage %in% PTAtaxM ~ "Macro",
                               TRUE ~ "Meso"),
         Taxon = case_when(Taxlifestage =="Acartia_UnID Juvenile" ~ "Acartia",
                           Taxlifestage =="Acartiella sinensis Adult" ~ "Acartiella",
                           Taxlifestage =="Pseudodiaptomus forbesi Adult" ~ "Pseudodiaptomus",
                           Taxlifestage =="Tortanus_UnID Juvenile" ~ "Tortanus",
                           Taxlifestage == "Sinocalanus_UnID Juvenile" ~ "Sinocalanus",
                           Taxlifestage == "Daphnia_UnID Adult" ~ "Daphnia",
                           Taxlifestage == "Hyalella_UnID Adult" ~ "Hyalella",
                           Taxlifestage == "Orientomysis hwanhaiensis Adult" ~ "Orientomysis",
                           Taxlifestage == "Alienacanthomysis macropsis Adult" ~ "Alienacanthomysis",
                           Taxlifestage == "Hyperacanthomysis longirostris Adult" ~ "Hyperacanthomysis",
                           Taxlifestage == "Sinocorophium alienense Adult" ~ "Sinocorophium",
                           Taxlifestage == "Neomysis mercedis Adult" ~ "N. mercedis"))




#now for the monthly version
#Eh, this really isn't set up for a binomial model, so I'm going to have to fudge it a bit.
binomialthingyM = function(dat) {
  dat$sucess = round(dat$Power)*100
  dat$faliure = 100- round(dat$Power)*100
  mod = glm(cbind(sucess, faliure) ~ Breaks, family = "binomial", data = dat)
  #80% power coresponds to 4:1 odds. the coeffiecient is the log of the ods ratio
  #so to get the number of breaks needed for 80% power I do... log(4) = Breaks*a + b, or Breaks = (log(4)-b)/a
  Break80 = as.numeric((log(4)- coef(mod)[1])/coef(mod)[2])
  return(Break80)
}


