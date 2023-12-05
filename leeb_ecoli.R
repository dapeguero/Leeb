rm(list=ls()) 

library(tidyverse)
library(ggplot2)
library(dplyr)
library("writexl")
#install.packages("mdthemes")
library(mdthemes)

#importing data
leeb_heeb_ecoli <-read.csv("data/LEEb_ecoli.csv", header= TRUE,
                     sep= ',')


leeb_heeb_ecoli
#data of ecoli using leeb
#combine data into one column and changing name
leeb_heeb_ecoli <-
  leeb_heeb_ecoli%>%
  
  mutate(treatment_new=case_when(treatment=="untreated" ~"Control",
                            treatment=="250keV"~"LEEB",
                            treatment=="dose0"~"0",
                            treatment=="dose1"~"1",
                            treatment=="dose3"~"3",
                            treatment=="dose5"~"5",
                            treatment=="dose10"~"10")) %>% 
  mutate(treatment_new= factor(treatment_new),
         treatment=factor(treatment),
         sample=factor(Sample))



         
str(leeb_heeb_ecoli)

leeb_heeb_ecoli

leeb_ecoli<-leeb_heeb_ecoli %>% 
  filter(!treatment_new=="0") %>% 
  filter(!treatment_new=="1") %>% 
  filter(!treatment_new=="3") %>% 
  filter(!treatment_new=="5") %>% 
  filter(!treatment_new=="10") 


  
leeb_ecoli

#
# Perform ANOVA forleeb_ecoli
sg_anova_leeb_ecoli <- aov(log_concen ~ treatment_new, data = leeb_ecoli) 
                         
sg_anova_leeb_ecoli
# Perform Tukey's HSD test for ndf_in
sg_tukey_leeb_ecoli <- TukeyHSD(sg_anova_leeb_ecoli)

# Check ANOVA assumptions
par(mfrow = c(2,2))
plot(sg_anova_leeb_ecoli)

sg_tukey_leeb_ecoli


#descriptive statistics for leeb after e coli 
leeb_ecoli_mean <-
  leeb_heeb_ecoli%>% 
  filter(!treatment_new=="0") %>% 
  filter(!treatment_new=="1") %>% 
  filter(!treatment_new=="3") %>% 
  filter(!treatment_new=="5") %>% 
  filter(!treatment_new=="10") %>% 
  
  group_by(treatment_new) %>%
  summarise(mean=mean(log_concen),
            stdev=sd(log_concen),
            max=max(log_concen),
            min=min(log_concen))
leeb_ecoli_mean

leeb_ecoli_mean %>% 
  
  ggplot(aes(treatment_new,mean))+
 
  geom_point(size=3)+
  
  #geom_segment(aes(x = "Control", y=8.06, xend = "LEEB", yend = 4.71), arrow =arrow(length=unit(0.5, "cm")),
   #            linetype="dashed", color = "black")+
  
  geom_point(data = leeb_ecoli,aes(treatment_new,log_concen),
             alpha=0.2, size=3)+
 # geom_jitter(data = leeb_ecoli, aes(treatment_new, log_concen), 
  #            alpha=0.2, width = 0.2, height = 0.2, size=3)+
  geom_hline((aes(yintercept=2)),linetype="dashed", color="red")+
  
  annotate("text", x=c("Control", "LEEB"), 
           y=c(9,6), 
           label=c("a","b"), color="black", size=4)+
 
  scale_y_continuous(limits = c(0, 10), 
                     breaks = seq(0, 10, 2))+
  labs(x="", y=expression(paste("Log"[10]~ "CFU/g of dried BSFL")))+
  
  theme(plot.width = unit(4, "cm"), plot.height = unit(6, "cm"))+
  theme_classic()+
  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.text.x = element_text(size=12, color = "black"))+
  theme(axis.title.y = element_text(size = 10, color = "black"))

ggsave("output/leeb_ecoli_v1.jpeg", units = "cm",width = 8,height = 10,
       dpi = 1200)



#heeb data to calculate D10 value 

heeb_ecoli<-
  leeb_heeb_ecoli %>% 
  filter(!treatment_new=="Control") %>% 
  filter(!treatment_new=="LEEB")  
 # filter(!treatment_new=="10") 

heeb_ecoli


#calculating mean for heeb ecoli samples 
heeb_ecoli_mean <-
  leeb_heeb_ecoli%>% 
  filter(!treatment_new=="Control") %>% 
  filter(!treatment_new=="LEEB") %>% 
  #filter(!treatment_new=="10") %>% 
  group_by(treatment_new) %>%   
  summarise(mean=mean(log_concen),
            stdev=sd(log_concen))
            

heeb_ecoli_mean$treatment_new<- factor(leeb_heeb_ecoli$treatment_new, 
  levels = c("0", "1","3","5","10"))

heeb_ecoli_mean
#graphing it all 
heeb_ecoli_mean %>% 
  
  ggplot(aes(treatment_new,mean))+
  
  geom_point(size=3)+
  
 # geom_segment(aes(x = "Control", y=8.06, xend = "LEEB", yend = 4.71), arrow =arrow(length=unit(0.5, "cm")),
  #             linetype="dashed", color = "black")+
  
  geom_point(data = heeb_ecoli,aes(treatment_new,log_concen),
             alpha=0.2, size=3)+
  geom_hline((aes(yintercept=2)),linetype="dashed", color="red")+

  scale_y_continuous(limits = c(0, 10), 
                     breaks = seq(0, 10, 2))+
  labs(x="Dose (kGy)", y=expression(paste("Log"[10]~ "CFU/g of dried BSFL")))+
  
  theme(plot.width = unit(4, "cm"), plot.height = unit(6, "cm"))+
  theme_classic()+
  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.text.x = element_text(size=12, color = "black"))+
  theme(axis.title.y = element_text(size = 10, color = "black"))
ggsave("output/heeb_ecoli.jpeg", units = "cm",width = 8,height = 10,
       dpi = 1200)
