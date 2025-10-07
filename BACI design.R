#demo of BACI design with FRP data

library(tidyverse)
library(sf)
library(deltamapr)

library(lme4)
library(lmerTest)
library(effects)
library(DHARMa)
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.269.5


FRP_Mac =  read_csv("data/macroinvert_FRP2023.csv")
visits = read_csv("data/sitevisit_FRP2023.csv")

#Let's look for impact of restoration on Lower Yolo Ranch, Tule Red,and Winter Island
#Control sites will be Liberty Island, Blacklock, Grizzly Bay, Ryer Island, and Cache Slough, andBrowns island

MACvisits = filter(visits, Location %in% c("Cache Slough", "Liberty Island", "Wildlands",
                                        "Grizzly Bay", "Ryer Island", "Browns Island", "Winter Island",
                                        "Tule Red", "Lower Yolo Ranch")) %>%
  left_join(FRP_Mac) %>% #attach bug catch data
  filter(!is.na(CPUE)) %>% #get rid of things with out catch data
  
  #add dates retored sites were retored
  mutate(BeforeAfter = case_when(Date <= ymd("2021-09-01") & Location %in% c("Lower Yolo Ranch", "Wildlands", "Liberty Island", "Cache Slough") ~ "Before",
                                 Date <= ymd("2019-09-25") & Location %in% c("Tule Red", "Grizzly Bay", "Ryer Island") ~ "Before",
                                 Date <= ymd("2019-09-25") & Location %in% c("Winter Island", "Browns Island") ~ "Before",
                                 TRUE ~ "After"),
         BeforeAfter = factor(BeforeAfter, levels = c("Before", "After")), #put in right order
         
         Group = case_when(Location %in% c("Lower Yolo Ranch", "Wildlands", "Liberty Island", "Cache Slough") ~ "Cache",
                                 Location %in% c("Tule Red", "Grizzly Bay", "Ryer Island") ~ "Grizzly",
                                 Location %in% c("Winter Island", "Browns Island") ~ "Confluence"),
         
         ControlImpact = case_when(Location %in% c("Cache Slough", "Wildlands", "Liberty Island",
                                                   "Grizzly Bay", "Ryer Island", "Browns Island") ~ "Control",
                                   TRUE ~ "Impact"))

#quick plot of the data
ggplot(MACvisits, aes(x = Date, y = log(CPUE+1), color = ControlImpact))+
  geom_point(aes(shape = BeforeAfter))+ facet_wrap(~Group)+ geom_smooth()

#What a mess!

#let' sjust look at one taxon - chironomids.
#first we have to add in the zeros

MACvisitszeros = pivot_wider(MACvisits, id_cols = c(Group, Location, Date, BeforeAfter, ControlImpact, SampleID_frp,
                                                    Temp, SC, pH, DO, Turbidity, GearTypeAbbreviation,effort),
                             names_from = CommonName, values_from = CPUE, values_fn = sum, values_fill = 0) %>%
  pivot_longer(cols = c(Crangonyx:last_col()), names_to = "CommonName", values_to = "CPUE")

#filter out chironomids
Chironomids = filter(MACvisitszeros, CommonName == "Chironomid larvae")

ggplot(Chironomids, aes(x = Date, y = log(CPUE+1), color = ControlImpact))+
  geom_point(aes(shape = BeforeAfter))+ facet_wrap(~Group)+ geom_smooth()

ggplot(Chironomids, aes(x = ControlImpact, y = log(CPUE+1), fill = BeforeAfter))+
  geom_boxplot()+ facet_wrap(~Group)

#this looks like there were more chironomids in the "before" treatment in Cahce and the Confluence, but 
#the relationship was there in both control and impact sites. There is also a TON of variance.

#model it
mod1 = lmer(log(CPUE+1) ~ ControlImpact*BeforeAfter + Date + (1|Group)+ (1|Location), 
            data = Chironomids)

plot(mod1)
#gross. 

plot(simulateResiduals(mod1))
#ouch. 
testZeroInflation(mod1)
#it's super-super zero inflated. THis is why science is hard. 

summary(mod1)
plot(allEffects(mod1))

#we need to use teh glmmTMB package for a zero-inflated negative binomial model
library(glmmTMB)
Chironomids = mutate(Chironomids, Date2 = as.numeric(Date))

mod2 = glmmTMB(CPUE ~ ControlImpact*BeforeAfter + Date2 + (1|Group)+ (1|Location), 
               ziformula = ~(1|Group)+ (1|Location), family = "nbinom2",  data = Chironomids)
summary(mod2)
plot(simulateResiduals(mod2))
#still doesn't look great, but I've seen worse.

#plot the effects
modeffs = allEffects(mod2)
BAeff = as.data.frame(modeffs[[2]])
ggplot(BAeff, aes(x = BeforeAfter, y = fit, color = ControlImpact, group = ControlImpact)) + 
  geom_point(size =4)+ geom_line()+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5)+ scale_y_log10()

#this tells us there are fewer chironomids in the "after" than the "before", but the change is similar
#in both control and impact sites, so probably not much effect of the intervention. 