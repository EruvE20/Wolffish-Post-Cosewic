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



#Data Summaries
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


#Frequencies of Occurence
#GROUP ALL DATA
spring4VsW$SURVEY <- "4VsW"
spring4X$SURVEY <- "4X"
georges$SURVEY <- "Georges Bank"
spring$SURVEY <- "Spring"
summer$SURVEY <- "Summer"
fall$SURVEY <- "Fall"
redfish$SURVEY <- "Redfish"


a <- spring4VsW %>% 
  filter(TYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)

b <- spring4X %>% 
  filter(TYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)

c <- georges %>% 
  filter(TYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)

d <- spring %>% 
  filter(TYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)

e <- summer %>% 
  filter(TYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)

f <- fall %>% 
  filter(TYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)

g <- redfish %>% 
  filter(XTYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)


rv <- rbind(a,b,c,d,e,f,g)



#full nulls
spring4VsW_null$SURVEY <- "4VsW"
spring4X_null$SURVEY <- "4X"
georges_null$SURVEY <- "Georges Bank"
spring_null$SURVEY <- "Spring"
summer_null$SURVEY <- "Summer"
fall_null$SURVEY <- "Fall"
redfish_null$SURVEY <- "Redfish"


a <- spring4VsW_null %>% 
  filter(TYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)

b <- spring4X_null %>% 
  filter(TYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)

c <- georges_null %>% 
  filter(TYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)

d <- spring_null %>% 
  filter(TYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)

e <- summer_null %>% 
  filter(TYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)

f <- fall_null %>% 
  filter(TYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)

g <- redfish_null %>% 
  filter(XTYPE == 1) %>% 
  select(YEAR, MISSION, SETNO, SDATE, lat = LATITUDE, long = LONGITUDE, STRAT, AREA, GEAR, DUR, DIST, SPEED, SAMPWGT, TOTWGT, TOTNO, SURVEY)


rv_null <- rbind(a,b,c,d,e,f,g)




#summarize sets
rv_summary <- rv %>% 
  group_by(SURVEY, MISSION) %>% 
  summarise(sets = length(unique(SETNO))) %>% 
  group_by(SURVEY) %>% 
  summarise(sets=sum(sets))

rv_null_summary <- rv_null %>%
  group_by(SURVEY, MISSION) %>% 
  summarise(nullsets = length(unique(SETNO))) %>% 
  group_by(SURVEY) %>% 
  summarise(nullsets=sum(nullsets))

rv_summary <- left_join(rv_summary, rv_null_summary, by="SURVEY") %>% 
  mutate(occurance = 100*(sets/nullsets))



#Annual
rv_summary <- rv %>% 
  group_by(SURVEY, YEAR) %>% 
  summarise(sets = length(unique(SETNO)))

rv_null_summary <- rv_null %>%
  group_by(SURVEY, YEAR) %>% 
  summarise(nullsets = length(unique(SETNO)))

rv_summary <- left_join(rv_summary, rv_null_summary, by=c("SURVEY", "YEAR")) %>% 
  mutate(occurance = 100*(sets/nullsets))


ggplot(rv_summary, aes(YEAR, occurance, colour=SURVEY))+
  geom_line()+
  theme_classic()+
  facet_wrap(~SURVEY, scales = "free_x")









#FIGURES (THIS IS WHERE YOU USE THE _length TABLES)
#length frequency distributions

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

#abundance of mature vs immatur8


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



#REDFISH
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

#filter out relevant surveys
ITQ <- isdb %>% filter(TRIPCD_ID == 7051)
lobster_survey <- isdb %>% filter(TRIPCD_ID == 7065)
lobster_commercial <- isdb %>% filter(TRIPCD_ID == 2550) 
sentinel_4VN <- isdb %>% filter(TRIPCD_ID == 7052)
sentinel_4VSW <- isdb %>% filter(TRIPCD_ID == 7050)
halibut_commercial <- isdb %>% filter(TRIPCD_ID == 30)
halibut_longline <- isdb %>% filter(TRIPCD_ID == 7057)
snowcrab <- isdb %>% filter(TRIPCD_ID == 7061)
redfish <- isdb %>% filter(TRIPCD_ID == 23)
cod <- isdb %>% filter(TRIPCD_ID == 7001)
flatfish <- isdb %>% filter(TRIPCD_ID == 49)
shrimp <- isdb %>% filter(TRIPCD_ID == 2210)
silverhake <- isdb %>% filter(TRIPCD_ID == 14)


#Halibut Industry Longline Survey
halibut_longline <- isdb %>% filter(TRIPCD_ID == 7057)
unique(halibut_longline$SET_TYPE)

halibut_longline %>% 
  group_by(SET_TYPE) %>% 
  summarize(trips = length(unique(FISHSET_ID)))

halibut_fixed <- halibut_longline %>% 
  filter(SETCD_ID == 4)



#4VSW Setinel Survey
sentinel <- isdb %>% filter(TRIPCD_ID == 7050)
unique(sentinel$SET_TYPE)

sentinel %>% 
  group_by(SET_TYPE) %>% 
  summarize(trips = length(unique(FISHSET_ID)))

sentinel <- sentinel %>% 
  filter(SETCD_ID == 5)


#ITQ
ITQ <- isdb %>% filter(TRIPCD_ID == 7051)
unique(ITQ$SET_TYPE)

ITQ %>% 
  group_by(SET_TYPE) %>% 
  summarize(trips = length(unique(FISHSET_ID)))

ITQ <- ITQ %>% 
  filter(SETCD_ID == 4)


#LOBSTER
lobster <- isdb %>% filter(TRIPCD_ID == 7065)
unique(lobster$SET_TYPE)


#SNOWCRAB
snowcrab <- isdb %>% filter(TRIPCD_ID == 7061)
unique(snowcrab$SET_TYPE)

snowcrab %>% 
  group_by(SET_TYPE) %>% 
  summarize(trips = length(unique(FISHSET_ID)))

snowcrab <- snowcrab %>% 
  filter(SETCD_ID == 4)




#NULL SURVEYS
#Halibut Industry Longline Survey
halibut_longline_null <- isdb_null %>% filter(TRIPCD_ID == 7057)
unique(halibut_longline_null$SET_TYPE)

halibut_longline_null %>% 
  group_by(SET_TYPE) %>% 
  summarize(trips = length(unique(FISHSET_ID)))

halibut_fixed_null <- halibut_longline_null %>% 
  filter(SETCD_ID == 4)

unique(halibut_fixed_null$SET_TYPE)


#4VSW Setinel Survey
sentinel_null <- isdb_null %>% filter(TRIPCD_ID == 7050)
unique(sentinel_null$SET_TYPE)

sentinel_null %>% 
  group_by(SET_TYPE) %>% 
  summarize(trips = length(unique(FISHSET_ID)))

sentinel_null <- sentinel_null %>% 
  filter(SETCD_ID == 5)


#ITQ
ITQ_null <- isdb_null %>% filter(TRIPCD_ID == 7051)
unique(ITQ_null$SET_TYPE)

ITQ_null %>% 
  group_by(SET_TYPE) %>% 
  summarize(trips = length(unique(FISHSET_ID)))

ITQ_null <- ITQ_null %>% 
  filter(SETCD_ID == 4)


#LOBSTER
lobster_null <- isdb_null %>% filter(TRIPCD_ID == 7065)
unique(lobster_null$SET_TYPE)

lobster_null %>% 
  group_by(SET_TYPE) %>% 
  summarize(trips = length(unique(FISHSET_ID)))

lobster_null <- lobster_null %>% 
  filter(SETCD_ID == 4)


#SNOWCRAB
snowcrab_null <- isdb_null %>% filter(TRIPCD_ID == 7061)
unique(snowcrab_null$SET_TYPE)

snowcrab_null %>% 
  group_by(SET_TYPE) %>% 
  summarize(trips = length(unique(FISHSET_ID)))

snowcrab_null <- snowcrab_null %>% 
  filter(SETCD_ID == 4)



#look at sampling years
a <- halibut_fixed %>% select(YEAR, TRIP_TYPE)
b <- sentinel %>% select(YEAR, TRIP_TYPE)
c <- ITQ %>% select(YEAR, TRIP_TYPE)
d <- lobster %>% select(YEAR, TRIP_TYPE)
e <- snowcrab %>% select(YEAR, TRIP_TYPE)


sampled <- rbind(a,b,c,d,e) %>%
  ungroup()

sampled <- as.data.frame(table(sampled))
sampled$observed <- ifelse(sampled$Freq>0, 'X', '')

#convert to wide format
sampled <- sampled %>%
  select(-c(Freq))

sampled <- spread(sampled, YEAR, observed)



#Proportion Catch
a <- halibut_fixed_null %>% select(YEAR, TRIP_TYPE, FISHSET_ID)
b <- sentinel_null %>% select(YEAR, TRIP_TYPE, FISHSET_ID)
c <- ITQ_null %>% select(YEAR, TRIP_TYPE, FISHSET_ID)
d <- lobster_null %>% select(YEAR, TRIP_TYPE, FISHSET_ID)
e <- snowcrab_null %>% select(YEAR, TRIP_TYPE, FISHSET_ID)

sampled_null <- rbind(a,b,c,d,e)


f <- halibut_fixed %>% select(YEAR, TRIP_TYPE, FISHSET_ID)
g <- sentinel %>% select(YEAR, TRIP_TYPE, FISHSET_ID)
h <- ITQ %>% select(YEAR, TRIP_TYPE, FISHSET_ID)
i <- lobster %>% select(YEAR, TRIP_TYPE, FISHSET_ID)
j <- snowcrab %>% select(YEAR, TRIP_TYPE, FISHSET_ID)

isb_surveys <- rbind(f,g,h,i,j)

#summarize sets
isdb_summary <- isb_surveys %>% 
  group_by(TRIP_TYPE) %>% 
  summarise(sets = length(unique(FISHSET_ID)))

isdb_null_summary <- sampled_null %>% 
  group_by(TRIP_TYPE) %>% 
  summarise(nullsets = length(unique(FISHSET_ID)))

isdb_summary <- left_join(isdb_summary, isdb_null_summary, by="TRIP_TYPE") %>% 
  mutate(occurance = 100*(sets/nullsets))




