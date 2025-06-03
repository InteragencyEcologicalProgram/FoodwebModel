#results of poll

library(tidyverse)
library(readxl)

ideas = read_excel("Foodweb synthesis ideas(1-14).xlsx")

ideas = mutate(ideas, AnalysisStructure = case_when(str_detect(structure, "Create an") ~ "One average wetland",
                                                    str_detect(structure, "Compare managed") ~ "Managed v Tidal v Flood",
                                                               str_detect(structure, "Compare 5-8") ~ "Compare 5-8 Tidal Wetlands",
                                                    TRUE ~ "Other"))
test2 = str_split(ideas$questions, ";", simplify =T) %>%
  as.data.frame()
ideas$Q1 = test2$V1
ideas$Q2 = test2$V2
ideas$Q3 = test2$V3
ideas$Q4 = test2$V4

ideaslong = pivot_longer(ideas, cols = c(Q1:Q4), names_to = "question", values_to = "Question") %>%
  filter(Question != "")

Qs = read_csv("Questions.csv")

ideaslong = left_join(ideaslong, Qs) 

ggplot(filter(ideaslong, !is.na(shortquestion)), aes(x = shortquestion, fill = Category)) + geom_bar()+
  theme_bw()+
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust =1))+
   scale_fill_manual(values = c("firebrick", "sienna", "darkgreen", "blue","orange",  "springgreen3" ))

  
ggplot(ideas, aes(x = AnalysisStructure, fill = AnalysisStructure)) + geom_bar()+
  theme_bw()+
  scale_fill_manual(values = c("darkblue", "slategrey", "pink2", "green3"))


