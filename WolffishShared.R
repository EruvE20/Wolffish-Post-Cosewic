library(dplyr)
library(ggplot2)
library(tidyverse)

##Wolffish Shared Project File


# RV Database -------------------------------------------------------------
# import data 
spring4VsW <- read.csv("spring4VsW.csv", header = T) %>%
  filter(TYPE == 1) %>% 
  select(-X)
spring4VsW_lengths <- read.csv("spring4VsW_lengths.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)
spring4VsW_null <- read.csv("spring4VsW_null.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)
spring4X <- read.csv("spring4X.csv", header = T) %>%
  filter(TYPE == 1) %>% 
  select(-X)
spring4x_lengths <- read.csv("spring4x_lengths.csv", header = T) %>%
  filter(TYPE == 1) %>% 
  select(-X)
spring4X_null <- read.csv("spring4X_null.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)
georges <- read.csv("georges.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)
georges_lengths <- read.csv("georges_lengths.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)
georges_null <- read.csv("georges_null.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)
spring <- read.csv("spring.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)
spring_lengths <- read.csv("spring_lengths.csv", header = T) %>%
  filter(TYPE == 1) %>% 
  select(-X)
spring_null <- read.csv("spring_null.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)
summer <- read.csv("summer.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)
summer_lengths <- read.csv("summer_lengths.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)
summer_null <- read.csv("summer_null.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)
fall <- read.csv("fall.csv", header = T) %>%
  filter(TYPE == 1) %>% 
  select(-X)
fall_lengths <- read.csv("fall_lengths.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)
fall_null <- read.csv("fall_null.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X)



#Data Summaries for number of sets per year, per season/survey --------
yrcatch_spring4VsW <- spring4VsW %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n())

yrcatch_spring4x <- spring4X %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n())

yrcatch_spring <- spring %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n())

yrcatch_fall<- fall %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n())

yrcatch_georges <- georges %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n())


yrcatch_summer <- summer %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n())

#frequency of occurrence --------------------------------------

#4VsW frequency of occurrence with bargraph 

propcatch_spring4VsW <- spring4VsW_null %>% 
  mutate(wolf = ifelse(CODE==50, 'Y', "N")) %>% 
  select(YEAR, SETNO, wolf) %>% 
  group_by(YEAR, wolf) %>% 
  summarise (n = n()) %>% 
  group_by(YEAR) %>% 
  mutate(total = sum(n)) %>% 
  filter(wolf=="Y") %>% 
  mutate(proportion = 100*(n/total)) 

propcatch_spring4VsW %>%
  ggplot(propcatch_spring4VsW, mapping = aes(YEAR, proportion)) +
  geom_bar(stat="identity") +
  theme_classic()

#frequency for occurrence of 4x spring RV survey

propcatch_spring4x <- spring4X_null %>% 
  mutate(wolf = ifelse(CODE==50, 'Y', "N")) %>% 
  select(YEAR, SETNO, CODE, wolf) %>% 
  group_by(YEAR, wolf) %>% 
  summarise (n = n()) %>% 
  group_by(YEAR) %>% 
  mutate(total = sum(n)) %>% 
  filter(wolf=="Y") %>% 
  mutate(proportion = 100*(n/total))

propcatch_spring4x %>%
  ggplot(propcatch_spring4x, mapping = aes(YEAR, proportion)) +
  geom_bar(stat="identity") +
  theme_classic()


#frequency for occurrence of the spring RV survey

propcatch_spring <- spring_null %>% 
  mutate(wolf = ifelse(CODE==50, 'Y', "N")) %>% 
  select(YEAR, SETNO, CODE, wolf) %>% 
  group_by(YEAR, wolf) %>% 
  summarise (n = n()) %>% 
  group_by(YEAR) %>% 
  mutate(total = sum(n)) %>% 
  filter(wolf=="Y") %>% 
  mutate(proportion = 100*(n/total))

propcatch_spring %>%
  ggplot(propcatch_spring, mapping = aes(YEAR, proportion)) +
  geom_bar(stat="identity") +
  theme_classic()
  
#Frequency of occurrence for fall RV survey 


propcatch_fall <- fall_null %>% 
  mutate(wolf = ifelse(CODE==50, 'Y', "N")) %>% 
  select(YEAR, SETNO, CODE, wolf) %>% 
  group_by(YEAR, wolf) %>% 
  summarise (n = n()) %>% 
  group_by(YEAR) %>% 
  mutate(total = sum(n)) %>% 
  filter(wolf=="Y") %>% 
  mutate(proportion = 100*(n/total))

propcatch_fall %>%
  ggplot(propcatch_fall, mapping = aes(YEAR, proportion)) +
  geom_bar(stat="identity") +
  theme_classic()

#Frequency for occurrence of summer RV survey

propcatch_summer <- summer_null %>% 
  mutate(wolf = ifelse(CODE==50, 'Y', "N")) %>% 
  select(YEAR, SETNO, CODE, wolf) %>% 
  group_by(YEAR, wolf) %>% 
  summarise (n = n()) %>% 
  group_by(YEAR) %>% 
  mutate(total = sum(n)) %>% 
  filter(wolf=="Y") %>% 
  mutate(proportion = 100*(n/total))

propcatch_summer %>%
  ggplot(propcatch_summer, mapping = aes(YEAR, proportion)) +
  geom_bar(stat="identity") +
  theme_classic()


#frequency of occurrence of georges bank 
propcatch_georges <- georges_null %>% 
  mutate(wolf = ifelse(CODE==50, 'Y', "N")) %>% 
  select(YEAR, SETNO, CODE, wolf) %>% 
  group_by(YEAR, wolf) %>% 
  summarise (n = n()) %>% 
  group_by(YEAR) %>% 
  mutate(total = sum(n)) %>% 
  filter(wolf=="Y") %>% 
  mutate(proportion = 100*(n/total))

propcatch_georges %>%
  ggplot(propcatch_georges, mapping = aes(YEAR, proportion)) +
  geom_bar(stat="identity") +
  theme_classic()


#length frequency distributions ---------------------------------------

#SPRING 4VSW
spring4VsW_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="green", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))

#SPRING 4X
spring4x_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))
#SPRING
spring_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))

#GEORGES 
georges_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))

#FALL
fall_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))

#SUMMER 
summer_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))


#abundance of mature vs immature ---------------------------------------------


#GEOM_SMOOTH + geom_point 
spring4VsW_lengths <- spring4VsW_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature'))

spring4VsW_lengths %>% 
  group_by(YEAR, maturity) %>% 
  drop_na(maturity) %>%
  summarize(abundance = length(FLEN)) %>% 
  ggplot(aes(x=YEAR, y=abundance, col=maturity))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "loess", se = FALSE, size = 2) +
  labs(y="Abundance (number at length)", x='Year)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))



#SPRING 4X
spring4x_lengths <- spring4x_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature')) #be careful when mutating, can add duplicates

str(data)

spring4x_lengths %>% 
  group_by(YEAR, maturity) %>% 
  drop_na(maturity) %>%
  summarize(abundance = length(FLEN)) %>% #only keeps what you summarized 
  mutate(year_data = ifelse(YEAR <2000, 'pre' , 'post')) %>%
  ggplot(aes(x=YEAR, y=abundance, col=maturity))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "loess", se = FALSE, size = 2)+
  labs(y="Abundance (number at length)", x='Year)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15)) +
  facet_wrap(~year_data)


# spring 
spring_lengths <- spring_lengths %>% 
  select(YEAR == <1995)
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature')) 

spring_lengths %>% 
  group_by(YEAR, maturity) %>% 
  drop_na(maturity) %>%
  summarize(abundance = length(FLEN)) %>% 
  ggplot(aes(x=YEAR, y=abundance, col=maturity))+
  geom_point()+
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, size = 2) +
  labs(y="Abundance (number at length)", x='Year)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))



# Fall 
fall_lengths <- fall_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature'))

fall_lengths %>% 
  group_by(YEAR, maturity) %>% 
  drop_na(maturity) %>%
  summarize(abundance = length(FLEN)) %>% 
  ggplot(aes(x=YEAR, y=abundance, col=maturity))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "loess", se = FALSE, size = 2)+
  labs(y="Abundance (number at length)", x='Year)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))

# Summer 
summer_lengths <- summer_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature')) 


data <- summer_lengths %>% 
  group_by(YEAR, maturity) %>% 
  drop_na(maturity) %>%
  summarize(abundance = length(FLEN)) %>%
  mutate(year_data = ifelse(YEAR <2000, 'pre' , 'post')) 

data$year_data= factor(data$year_data, levels=c('pre', 'post'))

  data %>%
  ggplot(aes(x=YEAR, y=abundance, col=maturity))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "loess", se = FALSE, size = 2) +
  labs(y="Abundance (number at length)", x='(Year)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15)) +
  facet_wrap(~year_data , scales = 'free_x') 
  

  #dash line representing the 1995 collapse 
  data %>%
    ggplot(aes(x=YEAR, y=abundance, col=maturity))+
    geom_point()+
    geom_line()+
    geom_vline(xintercept = 1995, linetype = 'dashed', color = 'black') +
    geom_smooth(method = "loess", se = FALSE, size = 2) +
    labs(y="Abundance (number at length)", x='(Year)')+
    theme_classic()+
    theme(text = element_text(size=15),
          axis.text = element_text(size=15)) 
  
  

str(summer_lengths)

#GEORGES 
georges_lengths <- georges_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature'))

georges_lengths %>% 
  group_by(YEAR, maturity) %>% 
  drop_na(maturity) %>%
  summarize(abundance = length(FLEN)) %>% 
  ggplot(aes(x=YEAR, y=abundance, col=maturity))+
  geom_point() +
  geom_line()+
  geom_smooth(method = "loess", se = FALSE, size = 2) +
  labs(y="Abundance (number at length)", x='Year)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))

# Log transformed catch rate ---------

#Summer 

#mature
summer_lengths <- summer_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature')) 

summer_lengths %>% 
  group_by(YEAR, maturity) %>% 
  drop_na(maturity) %>%
  summarize(total_lengths = sum(FLEN)) %>%
  filter(maturity == 'mature') %>%
  ggplot(aes (YEAR, total_lengths)) +
  geom_point(color = 'blue') +
  scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  theme_classic () +
  ggtitle("mature (<53)")

#immature
summer_lengths_immature <- summer_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature')) 

summer_lengths_immature %>% 
  group_by(YEAR, maturity) %>% 
  drop_na(maturity) %>%
  summarize(total_lengths = sum(FLEN)) %>%
  filter(maturity == 'immature') %>%
  ggplot(aes (YEAR, total_lengths)) +
  geom_point(color = 'blue') +
  scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  theme_classic () +
  ggtitle("immature(>53)")

#all length groups 
summer_all_lengths<- summer_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature')) 

summer_all_lengths %>% 
  group_by(YEAR, maturity) %>% 
  drop_na(maturity) %>%
  summarize(total_lengths = sum(FLEN))

summer_all_lengths %>%
  ggplot(aes (YEAR, total_lengths)) +
  geom_point(color = 'blue') +
  scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  theme_classic () +
  ggtitle("total lengths") +

lm_eqn <- function(summer_all_lengths){
  m <- lm(y ~ x, summer_all_lengths);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

p1 <- summer_all_lengths + geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)






#REDFISH ------
redfish <- read.csv("spring4VsW.csv", header = T) %>%
  select(-X)
redfish_lengths <- read.csv("redfish_lengths.csv", header = T) %>% 
  select(-X)
redfish_null <- read.csv("redfish_null.csv", header = T) %>% 
  select(-X)


#Frequency of occurrence:
propcatch_redfish <- redfish_null %>% 
  mutate(wolf = ifelse(CODE==50, 'Y', "N")) %>% 
  select(YEAR, SETNO, wolf) %>% 
  group_by(YEAR, wolf) %>% 
  summarise (n = n()) %>% 
  group_by(YEAR) %>% 
  mutate(total = sum(n)) %>% 
  filter(wolf=="Y") %>% 
  mutate(proportion = 100*(n/total)) 

propcatch_redfish %>%
  ggplot(propcatch_redfish, mapping = aes(YEAR, proportion)) +
  geom_bar(stat="identity") +
  theme_classic()

#Frequency of lengths 
redfish_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))

#Abundance 
redfish_lengths <- redfish_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature'))

redfish_lengths %>% 
  group_by(YEAR, maturity) %>% 
  drop_na(maturity) %>%
  summarize(abundance = length(FLEN)) %>% 
  ggplot(aes(x=YEAR, y=abundance, col=maturity))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "loess", se = FALSE, size = 2) +
  labs(y="Abundance (number at length)", x='Year)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))


# ISDB Database ---------------------------------------------------------
isdb <- read.csv("isdb.csv", header = T) %>%
  select(-X)
isdb_lengths <- read.csv("isdb_lengths.csv", header = T) %>%
  select(-X)
isdb_null <- read.csv("isdb_null.csv", header = T) %>%
  select(-X)
str(isdb_null)

#filter out relevant surveys
ITQ <- isdb %>% filter(TRIPCD_ID == 7051)
lobster_survey <- isdb_null %>% filter(TRIPCD_ID == 7065)
lobster_commercial <- isdb %>% filter(TRIPCD_ID == 2550) 
sentinel_4VN <- isdb %>% filter(TRIPCD_ID == 7052)
sentinel_4VSW <- isdb %>% filter(TRIPCD_ID == 7050)
halibut_observer <- isdb %>% filter(TRIPCD_ID == 30)
halibut_longline <- isdb %>% filter(TRIPCD_ID == 7057)
snowcrab <- isdb %>% filter(TRIPCD_ID == 7061)
redfish <- isdb %>% filter(TRIPCD_ID == 23)
cod <- isdb %>% filter(TRIPCD_ID == 7001)
flatfish <- isdb %>% filter(TRIPCD_ID == 49)
shrimp <- isdb %>% filter(TRIPCD_ID == 2210)
silverhake <- isdb %>% filter(TRIPCD_ID == 14) 

snowcrab_lengths <- isdb_lengths %>% filter(TRIPCD_ID ==7061)
ITQ_lobster <- isdb %>% filter(TRIPCD_ID %in% c(7051, 7065))
Lobster_surv <- isdb_lengths %>% filter(TRIPCD_ID == 7065)


#summaries for est_catches ----------

yrcatch_sentinel_4VN <- sentinel_4VN %>%
  select(YEAR, EST_COMBINED_WT) %>% 
  group_by(YEAR) %>% 
  summarise (EST_COMBINED_WT = sum(EST_COMBINED_WT)) # do sum instead of n - gives sum for the year 
plot(yrcatch_sentinel_4VN)

yrcatch_sentinel_4VSW <- sentinel_4VSW %>%
  select(YEAR, EST_COMBINED_WT) %>% 
  group_by(YEAR) %>% 
  summarise (EST_COMBINED_WT = sum(EST_COMBINED_WT))
plot(yrcatch_sentinel_4VSW)

yrcatch_ITQ <- ITQ %>%
  select(YEAR, EST_COMBINED_WT) %>% 
  group_by(YEAR) %>% 
  summarise (EST_COMBINED_WT = sum(EST_COMBINED_WT))
plot(yrcatch_ITQ)

yrcatch_snowcrab <- snowcrab %>%
  select(YEAR, EST_COMBINED_WT) %>% 
  group_by(YEAR) %>% 
  summarise (EST_COMBINED_WT = sum(EST_COMBINED_WT))
plot(yrcatch_snowcrab)

yrcatch_shrimp <- shrimp %>%
  select(YEAR, EST_COMBINED_WT) %>% 
  group_by(YEAR) %>% 
  summarise (EST_COMBINED_WT = sum(EST_COMBINED_WT))
plot(yrcatch_shrimp)

yrcatch_cod <- cod %>%
  select(YEAR, EST_COMBINED_WT) %>% 
  group_by(YEAR) %>% 
  summarise (EST_COMBINED_WT = sum(EST_COMBINED_WT))
plot(yrcatch_cod)

yrcatch_flatfish <- flatfish %>%
  select(YEAR, EST_COMBINED_WT) %>% 
  group_by(YEAR) %>% 
  summarise (EST_COMBINED_WT = sum(EST_COMBINED_WT))
plot(yrcatch_flatfish)

yrcatch_silverhake <- silverhake %>%
  select(YEAR, EST_COMBINED_WT) %>% 
  group_by(YEAR) %>% 
  summarise (EST_COMBINED_WT = sum(EST_COMBINED_WT))
plot(yrcatch_silverhake)

yrcatch_hailbut_observer <- halibut_observer %>%
  select(YEAR, EST_COMBINED_WT) %>% 
  group_by(YEAR) %>% 
  summarise (EST_COMBINED_WT = sum(EST_COMBINED_WT))
plot(yrcatch_hailbut_observer)

yrcatch_halibut_longline <- halibut_longline %>%
  select(YEAR, EST_COMBINED_WT) %>% 
  group_by(YEAR) %>% 
  summarise (EST_COMBINED_WT = sum(EST_COMBINED_WT))
plot(yrcatch_halibut_longline)


#Frequency of occurrence with bar graph ---------------

# proportions are not even 1% .. therefore coming up .52 on table 
snowcrab <- isdb_null %>% filter(TRIPCD_ID == 7061)

propcatch_snowcrab <-snowcrab  %>% 
  mutate(snow_crab = ifelse(SPECCD_ID==50, 'Y', "N")) %>% 
  select(YEAR, SET_NO, snow_crab) %>% 
  group_by(YEAR, snow_crab) %>% 
  summarise (n = n()) %>% 
  group_by(YEAR) %>% 
  mutate(total = sum(n)) %>% 
  filter(snow_crab=="Y") %>% 
  mutate(proportion = (n/total)) 

propcatch_snowcrab %>%
  ggplot(propcatch_spring4VsW, mapping = aes(YEAR, proportion)) +
  geom_bar(stat="identity") +
  theme_classic()

ITQ_lobster <- isdb_null %>% filter(TRIPCD_ID == 7051)

propcatch_itq <- ITQ_lobster %>% 
  mutate(itq.7051 = ifelse(SPECCD_ID==50, 'Y', "N")) %>% 
  select(YEAR, SET_NO, itq.7051) %>% 
  group_by(YEAR, itq.7051) %>% 
  summarise (n = n()) %>% 
  group_by(YEAR) %>% 
  mutate(total = sum(n)) %>% 
  filter(itq.7051=="Y") %>% 
  mutate(proportion = (n/total)) 

propcatch_itq %>%
  ggplot(propcatch_spring4VsW, mapping = aes(YEAR, proportion)) +
  geom_bar(stat="identity") +
  theme_classic()

halibut_longline <- isdb_null %>% filter(TRIPCD_ID == 7057)

propcatch_HL <- halibut_longline %>% 
  mutate(wolf.HL = ifelse(SPECCD_ID==50, 'Y', "N")) %>% 
  select(YEAR, SET_NO, wolf.HL) %>% 
  group_by(YEAR, wolf.HL) %>% 
  summarise (n = n()) %>% 
  group_by(YEAR) %>% 
  mutate(total = sum(n)) %>% 
  filter(wolf.HL=="Y") %>% 
  mutate(proportion = (n/total)) 

propcatch_HL %>%
  ggplot(mapping = aes(YEAR, proportion)) +
  geom_bar(stat="identity") +
  theme_classic()


cod <- isdb_null %>% filter(TRIPCD_ID == 7001)

propcatch_cod <-  cod %>% 
  mutate(wolf.cod = ifelse(SPECCD_ID==50, 'Y', "N")) %>% 
  select(YEAR, SET_NO, wolf.cod) %>% 
  group_by(YEAR, wolf.cod) %>% 
  summarise (n = n()) %>% 
  group_by(YEAR) %>% 
  mutate(total = sum(n)) %>% 
  filter(wolf.cod=="Y") %>% 
  mutate(proportion = (n/total)) 

propcatch_cod %>%
  ggplot(mapping = aes(YEAR, proportion)) +
  geom_bar(stat="identity") +
  theme_classic()


# Frequency of lengths -------
snowcrab %>% 
  ggplot(aes(EST_COMBINED_WT))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))

# Frequency of ITQ+LOBSTER SURVEY together 
# NO LENGTHS FOR 7065 
ITQ_lobster_lengths <- isdb_lengths %>% filter(TRIPCD_ID %in% c(7051, 7065))

ITQ_lobster_lengths %>%
mutate(lobster = TRIPCD_ID) %>%
ggplot(aes(FISH_LENGTH))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15)) +
  facet_wrap(~lobster)

#Frequency of sentinel_4VN 
# no lengths either? multiple set_types 
sentinel_4VN <- isdb %>% filter(TRIPCD_ID == 7052)

sentinel_4VN_lengths <- isdb_lengths %>% filter(TRIPCD_ID ==7052) 

sentinel_4VN_lengths %>% 
  ggplot(aes(FISH_LENGTH))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))


# counts of species at length in the sentinel_4VSW -- only 1? not coming up properly? 
sentinel_4VSW <- isdb %>% filter(TRIPCD_ID == 7050)
sentinel_4VSW_lengths <- isdb_lengths %>% filter(TRIPCD_ID == 7050)

sentinel_4VSW_lengths %>% 
  ggplot(aes(FISH_LENGTH))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))

#COD
cod <- isdb %>% filter(TRIPCD_ID == 7001)
cod_lengths <- isdb_lengths %>% filter(TRIPCD_ID ==7001) 

cod_lengths %>% 
  ggplot(aes(FISH_LENGTH))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))
#Shrimp
shrimp <- isdb %>% filter(TRIPCD_ID == 2210)

shrimp_lengths <- isdb_lengths %>% filter (TRIPCD_ID == 2210)
shrimp_lengths %>% 
  ggplot(aes(FISH_LENGTH))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))

#Playing with gear (boxplots and density) ---------

# density graphs of how often certain gear types appeared 
str(isdb)
isdb_gear <- isdb %>%
  select (YEAR, TRIPCD_ID, GEARCD_ID, EST_COMBINED_WT) 

isdb_gear$GEARCD_ID <- as.factor(isdb_gear$GEARCD_ID)
str(isdb_gear)

isdb_gear %>%
  mutate(years = ifelse(YEAR <1995, 'Pre', 'Post')) %>%
  ggplot(aes(x=GEARCD_ID)) +
  geom_density (fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  theme_classic() +
  facet_wrap(~years)

# bar plot/ box plot (more informative) x = gear, y= est_weight 
# combine similar gear (e.g all long line, all trawls togeahter) -- do mutate to combine (do a casewhen not ifelse)

isdb_boxplot <- isdb %>%
  select(YEAR, TRIPCD_ID, GEARCD_ID, EST_COMBINED_WT) %>%
  mutate(gear_category = case_when(GEARCD_ID %in% c(50, 51, 53) ~ "Longline",
                                   GEARCD_ID %in% c(9, 11, 12, 13, 15, 16, 19, 192, 110) ~ "Trawl",
                                   GEARCD_ID %in% c(21, 22) ~"Seine", 
                                   GEARCD_ID == 62 ~"Covered Pots", 
                                   GEARCD_ID == 71 ~"Dredge")) 
isdb_boxplot %>%
  ggplot(aes(x=gear_category, y= EST_COMBINED_WT)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

