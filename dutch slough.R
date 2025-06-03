#check out the dutch slough data

library(tidyverse)

DSfish = read.csv("data/dutch slough/DS_fish_2021-2023.csv") 

test = filter(DSfish, is.na(CommonName))
test2 = filter(DSfish, CommonName == "") %>%
  select(Code, CommonName) %>%
  distinct()

fishlookup = select(DSfish, Code, CommonName) %>% distinct() %>%
  arrange(Code)
#so there are some entries with no common name, but the code has common names for other enteries?~


DSWQ = read.csv("data/dutch slough/DS_waterquality_2021-2023.csv")%>%
  select(-X)
#apparently the water quality is continuous tracks. 

#grab the first point from each track to tie to the image. 
#or would it be better to tie it to the specific time?
#Or I can use the mean. 
DSWQsum = group_by(DSWQ, skeleton) %>%
  summarize(across(Date:Lon, first))

#Some of the entries have SPC and Cond, some just TDS and Sal, some three of the above, some all four.
#Why?
#depends on whether the machine was working right and in the right sepces. If it's not there, may habe been outside the range. 
#boat churns things up and causes issues. 

#missing a lot of the skeleton values?
DSWQmissing = filter(DSWQ, is.na(skeleton))
#whew! none missing

DSEffort = read.csv("data/dutch slough/DS_metadata_2021-2023.csv")

#it looks like the site and habitat type info is only inthe skeleton value, not anywhere else. 
DSfishall = left_join(DSfish, DSEffort) %>%
  left_join(DSWQsum) %>%
  mutate(Number = case_when(Code == "NO" ~ 0,
                            TRUE ~ Number),
         Site = case_when(str_detect(skeleton, "DB") ~ "Deep Borrow",
                          str_detect(skeleton, "DSCT") ~ "Dutch Slough Control",
                          str_detect(skeleton, "EMC") ~ "Emerson Channel",
                          str_detect(skeleton, "GBC") ~ "Gilbert Channel",
                          TRUE ~ "I Dunno"),
         HabitatType =  case_when(str_detect(skeleton, "NS") ~ "Near Shore",
                                  str_detect(skeleton, "OW") ~ "Open Water",
                                  TRUE ~ "I Dunno"))

DSfishallsum = group_by(DSfishall, Date, Code, start_time, volume, Temp.C, DO.mg.L, 
                        SPC, SAL, Turb_FNU, Chlorophyll, Lat, Lon, Site, HabitatType) %>%
  summarize(Count = sum(Number)) %>%
  pivot_wider(names_from = Code, values_from = Count, values_fill = 0) %>%
  pivot_longer(cols = c(AMS:WHS), names_to = "Code", values_to = "Count") %>%
  mutate(Date = ymd(Date))

ggplot(DSfishallsum, aes(x = as.factor(Date), y = Count, fill = Code)) + geom_col()+
  facet_wrap(~Site)+ theme(axis.text.x = element_text(angle = 90))

#now the zooplankton
DSzoops = read_csv("data/dutch slough/DS_zoops_2021-2023.csv")

DSzoopsall = DSzoops %>%
  left_join(DSEffort) %>%
  left_join(DSWQsum) %>%
  mutate(Site = case_when(str_detect(skeleton, "DB") ~ "Deep Borrow",
                          str_detect(skeleton, "DSCT") ~ "Dutch Slough Control",
                          str_detect(skeleton, "EMC") ~ "Emerson Channel",
                          str_detect(skeleton, "GBC") ~ "Gilbert Channel",
                          TRUE ~ "I Dunno"),
         HabitatType =  case_when(str_detect(skeleton, "NS") ~ "Near Shore",
                                  str_detect(skeleton, "OW") ~ "Open Water",
                                  TRUE ~ "I Dunno"))

DSzoopmissing = filter(DSzoopsall, is.na(Date))
#still missing some water quality for zoops too  

#Do we know the size of the field of view for zoops?
#Or the volume?
#can you see alol the critters in the zoop image? Or will the ones in the back be out of focus?

#pictures overlap. Want to have them just far apart to we don't miss data. Calculate speed of boat and volume of wtaer.
#percentage of net below the surface. 
#area within the camera. 
#can see everythign in between the two lenses, don't miss anything. 
#can get the value, different in 21 and 22 than 23. 