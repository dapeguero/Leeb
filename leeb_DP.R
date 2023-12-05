rm(list=ls()) #clears the environment 
library(tidyverse)
library(ggplot2)
library(dplyr)
library("writexl")
library(stats)
library(openxlsx)
library(stats)
library(viridis)
library(extrafont)
library(multcomp)



#import data

leeb <- read.csv("data/leeb_data.csv")
  
leeb <-
  leeb%>%  
  gather(8,key= stat, value = "value") %>% 
  mutate(storage_time=factor(storage_time)) %>% 
  
  mutate(drying_type=factor(drying_type)) %>% 
 
  mutate(insect=factor(insect)) %>% 
  
  mutate(treatment=factor(treatment)) %>% 
  
  mutate(parameter=factor(parameter)) %>% 
  mutate(sample=factor(sample)) %>% 
  
  mutate(rep=factor(rep))
  

leeb
#leebsummary of descriptive statistics
leeb_summary <-
  leeb %>% 
  group_by(insect,storage_time,drying_type,sample,treatment,parameter) %>%
  summarise(mean=mean(value),
            stdev=sd(value),
            max=max(value),
            min=min(value))
leeb_summary


write_xlsx(leeb_summary, "data/leeb_summary.xlsx")

colnames(leeb)


leeb_oven<-leeb %>% 
  filter(drying_type == "oven")

leeb_oven
#mean of bsfl shelf life test with all parameters for oven and leeb only
bsfl_shelf_test_mean <-
 leeb_summary %>% 
  filter(!treatment=="micro") %>% 
  filter(!drying_type== "micro") %>% 
  filter(!insect=="mw")
  
bsfl_shelf_test_mean



#do tvc 
bsfl_shelf_test_aerob_tvc <-
  bsfl_shelf_test_mean %>% 
  filter(!parameter== "aw") %>% 
  filter(!parameter=="moisture_content") %>% 
  filter(!parameter=="anaer.tvc") %>% 
  filter(!parameter=="anaer.bsc") %>% 
  filter(!parameter=="") %>% 
  filter(!parameter=="bsc") %>% 
  filter(!parameter=="ym")


bsfl_shelf_test_aerob_tvc
#plotting leeb tvc bsfl data months 0--6
#bsfl_shelf_test_bsf<-
 # bsfl_shelf_test_mean %>% 
bsfl_shelf_test_aerob_tvc %>% 

ggplot(aes(storage_time, mean, fill=treatment))+
  geom_bar(position= position_dodge(width=0.9),stat="identity")+
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.4, position=position_dodge(0.9))+
  
  scale_fill_grey(labels=c("Control", "LEEB"), name=NULL)+
  
  geom_hline(yintercept=1.95, linetype="dashed", color = "red", size=1)+
  scale_y_continuous(limits = c(0,4.5))+
  labs(x="Storage time (months)",
       y= "Log"[10]~"cfu/g dried BSFL ")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(strip.text.x = element_text(size = 12, color= "black"))+
  theme(strip.text.y = element_text(size = 12, color= "black"))+
  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size = 12, color = "black"))+
  theme(axis.title.x = element_text(size = 12, color = "black"))+
  theme(axis.text.x = element_text(size=14, color = "black"))+
  theme(legend.text = element_text(size = 12, color="black"))+
  ggtitle("(a) Total Viable Count")

ggsave("output/bsfl_aerbo_tvc.jpeg", units = "cm",
       width = 6,height = 8,dpi = 1200)


#same code but with bsc for bsfl
bsfl_shelf_test_aerob_bsc <-
  bsfl_shelf_test_mean %>% 
  filter(!parameter== "aw") %>% 
  filter(!parameter=="moisture_content") %>% 
  filter(!parameter=="anaer.tvc") %>% 
  filter(!parameter=="anaer.bsc") %>% 
  filter(!parameter=="") %>% 
  filter(!parameter=="tvc") %>% 
  filter(!parameter=="ym")

bsfl_shelf_test_aerob_bsc

bsfl_shelf_test_aerob_bsc %>% 
  
  ggplot(aes(storage_time, mean, fill=treatment_condition))+
  geom_bar(position= position_dodge(width=0.9),stat="identity")+
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.4, position=position_dodge(0.9))+
  scale_fill_grey(labels=c("Control", "LEEB"), name=NULL)+
  
  geom_hline(yintercept=2, linetype="dashed", color = "red", size=1)+
  
  scale_y_continuous(limits = c(0,4.5))+
  labs(x="Storage time (months)",
       y= "Log"[10]~"cfu/g dried BSFL")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(strip.text.x = element_text(size = 12, color= "black"))+
  theme(strip.text.y = element_text(size = 12, color= "black"))+
  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size = 12, color = "black"))+
  theme(axis.title.x = element_text(size = 12, color = "black"))+
  theme(axis.text.x = element_text(size=14, color = "black"))+
  theme(legend.text = element_text(size = 12, color="black"))+
  ggtitle("(b) BSC")
ggsave("output/bsfl_aerbo_bsc.jpeg", units = "cm",
       width = 6,height = 8,dpi = 1200)
  



#same code but with anaerobic tvc for bsfl
bsfl_shelf_test_anaerob_tvc <-
  bsfl_shelf_test_mean %>% 
  filter(!parameter== "aw") %>% 
  filter(!parameter=="moisture_content") %>% 
  filter(!parameter=="bsc") %>% 
  filter(!parameter=="anaer.bsc") %>% 
  filter(!parameter=="") %>% 
  filter(!parameter=="tvc") %>% 
  filter(!parameter=="ym") %>% 
  filter(!storage_time==2)


bsfl_shelf_test_anaerob_tvc
bsfl_shelf_test_anaerob_tvc %>% 
  
  ggplot(aes(storage_time, mean, fill=treatment_condition))+
  geom_bar(position= position_dodge(width=0.9),stat="identity")+
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.4, position=position_dodge(0.9))+
  scale_fill_grey(labels=c("Control", "LEEB"), name=NULL)+
  
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1)+
  
  scale_y_continuous(limits = c(0,4.5))+
  labs(x="Storage time (months)",
       y= "Log"[10]~"cfu/g dried BSFL")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(strip.text.x = element_text(size = 12, color= "black"))+
  theme(strip.text.y = element_text(size = 12, color= "black"))+
  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size = 12, color = "black"))+
  theme(axis.title.x = element_text(size = 12, color = "black"))+
  theme(axis.text.x = element_text(size=14, color = "black"))+
  theme(legend.text = element_text(size = 12, color="black"))+
  ggtitle("(d) Anaer. TVC")
ggsave("output/bsfl_anaerbo_tvc.jpeg", units = "cm",
       width = 6,height = 8,dpi = 1200)


#same code but with anaerobic bsc for bsfl
bsfl_shelf_test_anaerob_bsc <-
  bsfl_shelf_test_mean %>% 
  filter(!parameter== "aw") %>% 
  filter(!parameter=="moisture_content") %>% 
  filter(!parameter=="bsc") %>% 
  filter(!parameter=="anaer.tvc") %>% 
  filter(!parameter=="") %>% 
  filter(!parameter=="tvc") %>% 
  filter(!parameter=="ym")


bsfl_shelf_test_anaerob_bsc %>% 
  
  ggplot(aes(storage_time, mean, fill=treatment_condition))+
  geom_bar(position= position_dodge(width=0.9),stat="identity")+
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.4, position=position_dodge(0.9))+
  scale_fill_grey(labels=c("Control", "LEEB"), name=NULL)+
  
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1)+
  
  scale_y_continuous(limits = c(0,4.5))+
  labs(x="Storage time (months)",
       y= "Log"[10]~"cfu/g dried BSFL")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(strip.text.x = element_text(size = 12, color= "black"))+
  theme(strip.text.y = element_text(size = 12, color= "black"))+
  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size = 12, color = "black"))+
  theme(axis.title.x = element_text(size = 12, color = "black"))+
  theme(axis.text.x = element_text(size=14, color = "black"))+
  theme(legend.text = element_text(size = 12, color="black"))+
  ggtitle("(e) Anaer. BSC")
ggsave("output/bsfl_anaerbo_bsc.jpeg", units = "cm",
       width = 6,height = 8,dpi = 1200)


#same code but with yeast and moulds for bsfl
bsfl_shelf_test_aerob_ym <-
  bsfl_shelf_test_mean %>% 
  filter(!parameter== "aw") %>% 
  filter(!parameter=="moisture_content") %>% 
  filter(!parameter=="bsc") %>% 
  filter(!parameter=="anaer.tvc") %>% 
  filter(!parameter=="") %>% 
  filter(!parameter=="tvc") %>% 
  filter(!parameter=="anaer.bsc")

bsfl_shelf_test_aerob_ym
bsfl_shelf_test_aerob_ym %>% 
  
  ggplot(aes(storage_time, mean, fill=treatment_condition))+
  geom_bar(position= position_dodge(width=0.9),stat="identity")+
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.4, position=position_dodge(0.9))+
  scale_fill_grey(labels=c("Control", "LEEB"), name=NULL)+
  
  geom_hline(yintercept=2, linetype="dashed", color = "red", size=1)+
  
  scale_y_continuous(limits = c(0,4.5))+
  labs(x="Storage time (months)",
       y= "Log"[10]~"cfu/g dried BSFL")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(strip.text.x = element_text(size = 12, color= "black"))+
  theme(strip.text.y = element_text(size = 12, color= "black"))+
  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size = 12, color = "black"))+
  theme(axis.title.x = element_text(size = 12, color = "black"))+
  theme(axis.text.x = element_text(size=14, color = "black"))+
  theme(legend.text = element_text(size = 12, color="black"))+
  ggtitle("(c) Yeast & Moulds")
ggsave("output/bsfl_yeastmoulds.jpeg", units = "cm",
       width = 6,height = 8,dpi = 1200)



  #mean of mw shelf life test with all parameters for oven and leeb only
  mw_shelf_test_mean <-
    leeb_summary %>% 
    filter(!treatment=="micro") %>% 
    filter(!drying_type== "micro") %>% 
    filter(!insect=="bsfl")
  mw_shelf_test_mean
  
  #mean of tvc for mealworm shelf lfie
  
  mw_shelf_test_aerob_tvc <-
    mw_shelf_test_mean %>% 
    filter(!parameter== "aw") %>% 
    filter(!parameter=="moisture_content") %>% 
    filter(!parameter=="anaer.tvc") %>% 
    filter(!parameter=="anaer.bsc") %>% 
    filter(!parameter=="") %>% 
    filter(!parameter=="bsc") %>% 
    filter(!parameter=="ym")
  
  
  mw_shelf_test_aerob_tvc %>% 
    
    ggplot(aes(storage_time, mean, fill=treatment_condition))+
    geom_bar(position= position_dodge(width=0.9),stat="identity")+
    geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.4, position=position_dodge(0.9))+
    scale_fill_grey(labels=c("Control", "LEEB"), name=NULL)+

    geom_hline(yintercept=1.95, linetype="dashed", color = "red", size=1)+
    scale_y_continuous(limits = c(0,7))+
    labs(x="Storage time (months)",
         y= "Log"[10]~"cfu/g dried mealworm ")+
    theme_classic()+
    theme(legend.position="bottom")+
    theme(strip.text.x = element_text(size = 12, color= "black"))+
    theme(strip.text.y = element_text(size = 12, color= "black"))+
    theme(axis.text.y = element_text(size=12, color="black")) +
    theme(axis.title.y = element_text(size = 12, color = "black"))+
    theme(axis.title.x = element_text(size = 12, color = "black"))+
    theme(axis.text.x = element_text(size=14, color = "black"))+
    theme(legend.text = element_text(size = 12, color="black"))+
    ggtitle("(a) Total Viable Count")
  
 ggsave("output/mw_aerbo_tvc.jpeg", units = "cm",
        width = 6,height = 8,dpi = 1200)
 
 #same code but with bsc 
 mw_shelf_test_aerob_bsc <-
   mw_shelf_test_mean %>% 
   filter(!parameter== "aw") %>% 
   filter(!parameter=="moisture_content") %>% 
   filter(!parameter=="anaer.tvc") %>% 
   filter(!parameter=="anaer.bsc") %>% 
   filter(!parameter=="") %>% 
   filter(!parameter=="tvc") %>% 
   filter(!parameter=="ym")
 
 
 mw_shelf_test_aerob_bsc %>% 
   
   ggplot(aes(storage_time, mean, fill=treatment_condition))+
   geom_bar(position= position_dodge(width=0.9),stat="identity")+
   geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.4, position=position_dodge(0.9))+
   scale_fill_grey(labels=c("Control", "LEEB"), name=NULL)+
   
   geom_hline(yintercept=2, linetype="dashed", color = "red", size=1)+
   
   scale_y_continuous(limits = c(0,7))+
   labs(x="Storage time (months)",
        y= "Log"[10]~"cfu/g dried mealworm")+
   theme_classic()+
   theme(legend.position="bottom")+
   theme(strip.text.x = element_text(size = 12, color= "black"))+
   theme(strip.text.y = element_text(size = 12, color= "black"))+
   theme(axis.text.y = element_text(size=12, color="black")) +
   theme(axis.title.y = element_text(size = 12, color = "black"))+
   theme(axis.title.x = element_text(size = 12, color = "black"))+
   theme(axis.text.x = element_text(size=14, color = "black"))+
   theme(legend.text = element_text(size = 12, color="black"))+
   ggtitle("(b) BSC")
 ggsave("output/mw_aerbo_bsc.jpeg", units = "cm",
        width = 6,height = 8,dpi = 1200)
 
 #same code but with anaerobic tvc 
 mw_shelf_test_anaerob_tvc <-
   mw_shelf_test_mean %>% 
   filter(!parameter== "aw") %>% 
   filter(!parameter=="moisture_content") %>% 
   filter(!parameter=="bsc") %>% 
   filter(!parameter=="anaer.bsc") %>% 
   filter(!parameter=="") %>% 
   filter(!parameter=="tvc") %>% 
   filter(!parameter=="ym")
 
 mw_shelf_test_anaerob_tvc
 mw_shelf_test_anaerob_tvc %>% 
   
   ggplot(aes(storage_time, mean, fill=treatment_condition))+
   geom_bar(position= position_dodge(width=0.9),stat="identity")+
   geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.4, position=position_dodge(0.9))+
   scale_fill_grey(labels=c("Control", "LEEB"), name=NULL)+
   
   geom_hline(yintercept=1, linetype="dashed", color = "red", size=1)+
   
   scale_y_continuous(limits = c(0,7))+
   labs(x="Storage time (months)",
        y= "Log"[10]~"cfu/g dried mealworm")+
   theme_classic()+
   theme(legend.position="bottom")+
   theme(strip.text.x = element_text(size = 12, color= "black"))+
   theme(strip.text.y = element_text(size = 12, color= "black"))+
   theme(axis.text.y = element_text(size=12, color="black")) +
   theme(axis.title.y = element_text(size = 12, color = "black"))+
   theme(axis.title.x = element_text(size = 12, color = "black"))+
   theme(axis.text.x = element_text(size=14, color = "black"))+
   theme(legend.text = element_text(size = 12, color="black"))+
   ggtitle("(d) Anaer. TVC ")

 ggsave("output/mw_anaerob_tvc.jpeg", units = "cm",
        width = 6,height = 8,dpi = 1200)
 
 #same code but with anaerobic bsc 
 mw_shelf_test_anaerob_bsc <-
   mw_shelf_test_mean %>% 
   filter(!parameter== "aw") %>% 
   filter(!parameter=="moisture_content") %>% 
   filter(!parameter=="bsc") %>% 
   filter(!parameter=="anaer.tvc") %>% 
   filter(!parameter=="") %>% 
   filter(!parameter=="tvc") %>% 
   filter(!parameter=="ym")

 mw_shelf_test_anaerob_bsc %>% 
   
   ggplot(aes(storage_time, mean, fill=treatment_condition))+
   geom_bar(position= position_dodge(width=0.9),stat="identity")+
   geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.4, position=position_dodge(0.9))+
   scale_fill_grey(labels=c("Control", "LEEB"), name=NULL)+
   
   geom_hline(yintercept=1, linetype="dashed", color = "red", size=1)+
   
   scale_y_continuous(limits = c(0,7))+
   labs(x="Storage time (months)",
        y= "Log"[10]~"cfu/g dried mealworm")+
   theme_classic()+
   theme(legend.position="bottom")+
   theme(strip.text.x = element_text(size = 12, color= "black"))+
   theme(strip.text.y = element_text(size = 12, color= "black"))+
   theme(axis.text.y = element_text(size=12, color="black")) +
   theme(axis.title.y = element_text(size = 12, color = "black"))+
   theme(axis.title.x = element_text(size = 12, color = "black"))+
   theme(axis.text.x = element_text(size=14, color = "black"))+
   theme(legend.text = element_text(size = 12, color="black"))+
   ggtitle("(e) Anaer. BSC ")
 
 ggsave("output/mw_anaerob_bsc.jpeg", units = "cm",
        width = 6,height = 8,dpi = 1200)
 
 #mean of yeast and moulds for mealworm shelf lfie
 
 mw_shelf_test_aerob_ym <-
   mw_shelf_test_mean %>% 
   filter(!parameter== "aw") %>% 
   filter(!parameter=="moisture_content") %>% 
   filter(!parameter=="tvc") %>%
   filter(!parameter=="anaer.tvc") %>% 
   filter(!parameter=="anaer.bsc") %>% 
   filter(!parameter=="") %>% 
   filter(!parameter=="bsc")
 
 
 mw_shelf_test_aerob_ym %>% 
   
   ggplot(aes(storage_time, mean, fill=treatment_condition))+
   geom_bar(position= position_dodge(width=0.9),stat="identity")+
   geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.4, position=position_dodge(0.9))+
   scale_fill_grey(labels=c("Control", "LEEB"), name=NULL)+
   
   geom_hline(yintercept=2, linetype="dashed", color = "red", size=1)+
 
   scale_y_continuous(limits = c(0,7))+
 
   labs(x="Storage time (months)",
        y="Log"[10]~ "cfu/g dried mealworm") +
   theme_classic()+
   theme(legend.position="bottom")+
   theme(strip.text.x = element_text(size = 12, color= "black"))+
   theme(strip.text.y = element_text(size = 12, color= "black"))+
   theme(axis.text.y = element_text(size=12, color="black")) +
   theme(axis.title.y = element_text(size = 12, color = "black"))+
   theme(axis.title.x = element_text(size = 12, color = "black"))+
   theme(axis.text.x = element_text(size=14, color = "black"))+
   theme(legend.text = element_text(size = 12, color="black"))+
   ggtitle("(c) Yeast & Moulds")
 
 ggsave("output/mw_aerbo_ym.jpeg", units = "cm",
        width = 6,height = 8,dpi = 1200)
 
