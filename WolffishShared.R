library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(scales)
install.packages("gridExtra")
install.packages("rmarkdown")
install.packages("scales")

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
redfish <- read.csv("redfish.csv", header = T) %>%
  select(-X)
redfish_lengths <- read.csv("redfish_lengths.csv", header = T) %>% 
  select(-X)
redfish_null <- read.csv("redfish_null.csv", header = T) %>% 
  select(-X)




#Data summaries for Strata (4VWX) -----

spring4VsW_lengths <- read.csv("spring4VsW_lengths.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X) %>%
  select (SETNO, STRAT, NAME, TOTWGT, TOTNO, FLEN)

spring4x_lengths <- read.csv("spring4x_lengths.csv", header = T) %>%
  filter(TYPE == 1) %>% 
  select(-X) %>%
  select (SETNO, STRAT, NAME, TOTWGT, TOTNO, FLEN)

georges_lengths <- read.csv("georges_lengths.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X) %>%
  select (SETNO, STRAT, NAME, TOTWGT, TOTNO, FLEN)

spring_lengths <- read.csv("spring_lengths.csv", header = T) %>%
  filter(TYPE == 1) %>% 
  select(-X) %>%
  select (SETNO, STRAT, NAME, TOTWGT, TOTNO, FLEN)


fall_lengths <- read.csv("fall_lengths.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X) %>%
  select (SETNO, STRAT, NAME, TOTWGT, TOTNO, FLEN)

redfish_lengths <- read.csv("redfish_lengths.csv", header = T) %>% 
  select(-X) %>%
  select (SETNO, STRAT, NAME, TOTWGT, TOTNO, FLEN)

summer_lengths <- read.csv("summer_lengths.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X) %>%
  select (SETNO, STRAT, NAME, TOTWGT, TOTNO, FLEN)
summer_null <- read.csv("summer_null.csv", header = T) %>% 
  filter(TYPE == 1) %>% 
  select(-X) %>%
  select (SETNO, STRAT, NAME, TOTWGT, TOTNO)

summer_summaries <- summer_null %>%
  select(STRAT, SETNO) %>%
  filter(!is.na(STRAT)) %>%
  group_by(STRAT) %>% 
  summarise(nullset = length(unique(SETNO)))

summer_SETS <- summer_lengths %>% 
  select(STRAT, SETNO, TOTWGT, TOTNO, FLEN)%>%
  filter(!is.na(STRAT)) %>%
  group_by(STRAT) %>% 
  summarise(set = length(unique(SETNO)),
            sum_catch = sum(TOTWGT),
            mean_totwgt = mean(TOTWGT),
            total_count = length(TOTNO) 
            )

summer_summary <- left_join(summer_summaries, summer_SETS, by="STRAT") %>% 
  mutate(occurance = 100*(set/nullset))



#Data Summaries for number of sets per year, per season/survey --------
yrcatch_spring4VsW <- spring4VsW %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n())
yrcatch_spring4VsW %>%
  ggplot(aes(YEAR, n)) +
  geom_point() +
  theme_classic()

yrcatch_spring4x <- spring4X %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n()) 

  yrcatch_spring4x %>%
  ggplot(aes(YEAR, n)) +
  geom_point() +
  theme_classic()
  

yrcatch_spring <- spring %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n())

yrcatch_spring %>%
  ggplot(aes(YEAR, n)) +
  geom_point() +
  theme_classic()

yrcatch_fall<- fall %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n())

yrcatch_fall %>%
  ggplot(aes(YEAR, n)) +
  geom_point() +
  theme_classic()

yrcatch_georges <- georges %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n())

yrcatch_georges %>%
  ggplot(aes(YEAR, n)) +
  geom_point() +
  theme_classic()

yrcatch_summer <- summer %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n())

yrcatch_summer %>%
  ggplot(aes(YEAR, n)) +
  geom_point() +
  theme_classic()

yrcatch_redfish <- redfish %>% 
  select(YEAR, SETNO) %>% 
  group_by(YEAR) %>% 
  summarise (n = n())

yrcatch_redfish %>%
  ggplot(aes(YEAR, n)) +
  geom_point() +
  theme_classic()


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



#full nulls------
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
  filter(TYPE == 1) %>% 
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

rv_summary %>%
  ggplot(propcatch_spring4VsW, mapping = aes(YEAR, proportion)) +
  geom_bar(stat="identity") +
  theme_classic()
  



#Annual -- -do this for ISDB as well 
#add titles to all plots 
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


springVSW_freq <- rv_lengths %>% 
  select(NAME, YEAR, MISSION, SETNO, SAMPWGT, TOTWGT, TOTNO, FLEN) %>%
  filter(NAME == "4VSW")
  


#length frequency distributions ---------------------------------------
# this would be good to grid range 


#SPRING 4VSW
j <- spring4VsW_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15)) +
  ggtitle("SPRING 4VsW")

#SPRING 4X
k<- spring4x_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15)) +
  ggtitle("SPRING 4X")

#SPRING
l <- spring_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15)) +
  ggtitle("SPRING")

#GEORGES 
m <- georges_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15)) +
  ggtitle("GEORGES BANK (5Z)")

#FALL
n <- fall_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15)) +
  ggtitle("FALL")


#SUMMER 
o <- summer_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15)) +
  ggtitle("SUMMER")

#REDFISH 
p <- redfish_lengths %>% 
  ggplot(aes(FLEN))+
  geom_histogram(binwidth = 5, col="black", fill="black", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15)) +
  ggtitle("REDFISH")

 grid.arrange(j,k,l,m,n,o,p, nrow = 2)


#abundance of mature vs immature ---------------------------------------------
#GEOM_SMOOTH + geom_point 
 
#standardized for final report 
 #

#SPRING 4VSW
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


# spring  -- took out 1995 ----- 
spring_lengths <- spring_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature')) %>%
  filter(YEAR != "1995")

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

#facet wrap graphs of all lengths --------------------

spring4VsW_lengths$SURVEY <- "4VsW"
spring4x_lengths$SURVEY <- "4X"
georges_lengths$SURVEY <- "Georges Bank"
spring_lengths$SURVEY <- "Spring"
summer_lengths$SURVEY <- "Summer"
fall_lengths$SURVEY <- "Fall"
redfish_lengths$SURVEY <- "Redfish"

aa <- georges_lengths %>%
  filter (TYPE == 1) %>%
  select (YEAR, FLEN, SURVEY)

bb <- summer_lengths %>%
  filter (TYPE == 1) %>%
  select (YEAR, FLEN, SURVEY)

cc <- fall_lengths  %>%
filter (TYPE == 1) %>%
  select (YEAR, FLEN, SURVEY)

dd <- spring_lengths %>%
  filter (TYPE == 1) %>%
  select (YEAR, FLEN, SURVEY)

ee <- spring4VsW_lengths  %>%
filter (TYPE == 1) %>%
  select (YEAR, FLEN, SURVEY)

ff <- spring4x_lengths %>%
  filter (TYPE == 1) %>%
  select (YEAR, FLEN, SURVEY)

gg <- redfish_lengths %>%
filter (TYPE == 1) %>%
  select (YEAR, FLEN, SURVEY)

survey_lengths <- rbind(aa, bb, cc, dd, ee, ff, gg)

sl <- survey_lengths %>%
mutate(maturity = ifelse(FLEN <53, 'immature', 'mature'))

sl %>% 
  group_by(YEAR, maturity, SURVEY) %>% 
  drop_na(maturity) %>%
  summarize(abundance = length(FLEN)) %>% 
  ggplot(aes(x=YEAR, y=abundance, col=maturity))+
  geom_line()+
  theme_classic()+
  facet_wrap(~SURVEY, scales = "free_x")
  

# Log transformed catch rate ---------

#Summer 

#can get summary results for slope and R - do geom text and display in on the graph 

#mature
summer_lengths <- summer_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature')) 


summer_lengths_mature <- summer_lengths %>% 
  filter(maturity == 'mature')%>%
  group_by(YEAR) %>% 
  summarize(total_lengths = length(FLEN)) 


H <- ggplot(summer_lengths_mature, aes (YEAR, total_lengths)) +
  geom_point(color = 'blue') +
  scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  theme_classic () +
  ggtitle("Summer mature (>53)") 

#immature

summer_lengths_immature <- summer_lengths %>% 
  filter(maturity == 'immature')%>%
  group_by(YEAR) %>% 
  summarize(total_lengths = length(FLEN)) 
  

 G <- ggplot(summer_lengths_immature, aes (YEAR, total_lengths)) +
  geom_point(color = 'blue') +
  scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  theme_classic () +
  ggtitle("Summer immature (<53)") 
 
 

#all length groups 
 summer_lengths_total <- summer_lengths %>% 
   group_by(YEAR) %>% 
   summarize(total_lengths = length(FLEN)) 
 
 
 I <- ggplot(summer_lengths_total, aes (YEAR, total_lengths)) +
   geom_point(color = 'blue') +
   scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
   geom_smooth(method = 'lm', se = FALSE, color = 'black') +
   theme_classic () +
   ggtitle("Summer total lengths") 


#Georges 

georges_lengths <- georges_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature')) 

georges_catchrate <- georges_lengths %>% 
  filter(maturity ==  'mature') %>%
  group_by(YEAR) %>% 
  summarize(total_lengths = length(FLEN)) 


E <- ggplot(georges_catchrate, aes (YEAR, total_lengths)) +
  geom_point(color = 'blue') +
  scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  theme_classic () +
  ggtitle("Georges Bank (5z) mature") 

# immature 
georges_catchrates <- georges_lengths %>% 
  filter(maturity ==  'immature') %>%
  group_by(YEAR) %>% 
  summarize(total_lengths = length(FLEN)) 


P <- ggplot(georges_catchrates, aes (YEAR, total_lengths)) +
  geom_point(color = 'blue') +
  scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  theme_classic () +
  ggtitle("Georges Bank (5z) immature") 

#totals of georges bank
georges_catch <- georges_lengths %>% 
  group_by(YEAR) %>% 
  summarize(total_lengths = length(FLEN)) 


D <- ggplot(georges_catch, aes (YEAR, total_lengths)) +
  geom_point(color = 'blue') +
  scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  theme_classic () +
  ggtitle("Georges Bank (5z) total") 


#Spring_4x

#mature 
spring4X_catchrate <- spring4x_lengths %>% 
  mutate(maturity = ifelse(FLEN <53, 'immature', 'mature')) 

spring4X_mature <- spring4X_catchrate %>% 
  filter(maturity == "mature") %>%
  group_by(YEAR) %>% 
  summarize(total_lengths = length(FLEN)) 

catches <- ggplot(spring4X_mature, aes (YEAR, total_lengths)) +
  geom_point(color = 'blue') +
  scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  theme_classic () +
  ggtitle("Spring 4X mature") 


#immature 
spring4X_immature <- spring4X_catchrate %>% 
  filter(maturity == "immature") %>%
  group_by(YEAR) %>% 
  summarize(total_lengths = length(FLEN)) 

spring_immature <- ggplot(spring4X_immature, aes(YEAR, total_lengths)) +
  geom_point(color = 'blue') +
  scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  theme_classic () +
  ggtitle("Spring 4X immature")

#Combined

spring4X_totalcatch <- spring4X_catchrate %>% 
  group_by(YEAR) %>% 
  summarize(total_lengths = length(FLEN)) 

spring4X_totalcatches <- ggplot(spring4X_totalcatch, aes(YEAR, total_lengths)) +
  geom_point(color = 'blue') +
  scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  theme_classic () +
  ggtitle("Spring 4X total")

grid.arrange(G,H,I,P,E,D, spring_immature, catches, spring4X_totalcatches, nrow=3, ncol = 3)




# Can't figure out regression line + equation -----------------
lm_eqn = function(summer_all_lengths)
  {
  m = lm(y ~ x, summer_all_lengths);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}
p1 = p + geom_text(data=summer_all_lengths,aes(x = 25, y = 300,label=V1), parse = TRUE, inherit.aes=FALSE)



#Redfish 
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
lobster_commercial <- isdb %>% filter(TRIPCD_ID == 2550) 
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

#NULL SURVEYS------
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
  filter(SETCD_ID == 11)



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



#Proportion Catch (Frequency of Occurrence) ------
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


#combined graph of abundance ----------------------

#gridExtra 
#gridExtra :: look up 
#cow plot 
#ggsave 

#double check through SQL

halibut_fixed_lengths$CMSURVEY <- "Halibut longline"
sentinel_fixed_lengths$CMSURVEY <- "4VsW Sentinel"
ITQ_lengths$CMSURVEY <- "4x mobile survey"
snowcrab_fixed_lengths$CMSURVEY <- "Snow Crab"

#Halibut Industry Longline Survey
halibut_longline_lengths <- isdb_lengths %>% filter(TRIPCD_ID == 7057)

halibut_fixed_lengths <- halibut_longline_lengths %>% 
  filter(SETCD_ID == 4) 
str(halibut_fixed_lengths)

A <- halibut_fixed_lengths %>%
  select (YEAR, EST_COMBINED_WT, CMSURVEY,FISH_LENGTH) %>%
  filter(!is.na(FISH_LENGTH))



#4VSW Sentinel Survey
sentinel_lengths <- isdb_lengths %>% filter(TRIPCD_ID == 7050)

sentinel_fixed_lengths <- sentinel_lengths %>% 
  filter(SETCD_ID == 5)

B <- sentinel_fixed_lengths %>%
  select (YEAR, EST_COMBINED_WT, CMSURVEY, FISH_LENGTH) %>%
  filter(!is.na(FISH_LENGTH))


#ITQ
ITQ_lengths <- isdb_lengths %>% filter(TRIPCD_ID == 7051)

ITQ_fixed_lengths <- ITQ_lengths %>% 
  filter(SETCD_ID == 4)

C <- ITQ_lengths %>%
  select (YEAR, EST_COMBINED_WT, CMSURVEY, FISH_WEIGHT, FISH_LENGTH) %>%
  filter(!is.na(FISH_LENGTH))


#LOBSTER
lobster_lengths <- isdb_lengths %>% filter(TRIPCD_ID == 7065)
unique(lobster_lengths$SET_TYPE)

#summaries (total weight, average weight, mean caught, total caught, strata, gear etc.)--------

#SNOWCRAB for ecosystem survey(code 11/MPA areas) ----

snowcrab_sum_fixed <- isdb_lengths %>% 
  filter(TRIPCD_ID ==7061) %>%
  filter(YEAR != 2004) %>%
  filter(SETCD_ID == 4) %>%
  select (YEAR, EST_COMBINED_WT, EST_NUM_CAUGHT,STATION, GEAR)%>%
  group_by(YEAR) %>% 
  summarize(
    sum_wt = sum(EST_COMBINED_WT),
    mean_weight = mean(EST_COMBINED_WT),
    count_caught = length(EST_NUM_CAUGHT), 
    mean_caught = mean(EST_NUM_CAUGHT),
     )

snowcrab_sum_ecosystem <- isdb_lengths %>% 
  filter(TRIPCD_ID ==7061) %>%
  filter(YEAR != 2004) %>%
  filter(SETCD_ID == 11) %>%
  select (YEAR, EST_COMBINED_WT, EST_NUM_CAUGHT,STATION, GEAR, FISH_LENGTH)%>%
  group_by(YEAR) %>% 
  summarize(
    sum_wt = sum(EST_COMBINED_WT),
    mean_weight = mean(EST_COMBINED_WT),
    count_caught = length(EST_NUM_CAUGHT), 
    mean_caught = mean(EST_NUM_CAUGHT),
    count_fish_length = length(FISH_LENGTH),
    mean_length = mean(FISH_LENGTH)
  )
  
snowcrab_sum_ecosystem %>% 
  ggplot(aes(FISH_LENGTH))+
  geom_histogram(binwidth = 5, col="black", fill="green", alpha=.2)+
  labs(y="Frequency (count)", x='Length (cm)')+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=15))


ITQ_lobster <- isdb %>% filter(TRIPCD_ID %in% c(7051, 7065))
Lobster_surv <- isdb_lengths %>% filter(TRIPCD_ID == 7065)



# ITQ Survey (Summaries) ------

ITQ_lengths <- isdb_lengths

 ITQ_summary <- isdb_lengths %>% 
  filter(TRIPCD_ID ==7051) %>%
  filter(SETCD_ID == 4) %>%
  select (YEAR, EST_COMBINED_WT, EST_NUM_CAUGHT,STATION, GEAR)%>%
  group_by(YEAR) %>% 
  summarize(
    sum_wt = sum(EST_COMBINED_WT),
    mean_weight = mean(EST_COMBINED_WT),
    count_caught = length(EST_NUM_CAUGHT), 
    mean_caught = mean(EST_NUM_CAUGHT),
  )

 
  ITQ_stratum <- isdb_lengths %>%
    filter(TRIPCD_ID == 7051) %>%
    filter(SETCD_ID == 4) %>%
    select(STRATUM_ID, EST_COMBINED_WT, SET_NO) %>%
    group_by(STRATUM_ID) %>%
    summarize (
      sum_catch = sum(EST_COMBINED_WT), 
      total_sets = length(SET_NO)
    )
 
 #Proportion of wolffish sets by stratum  
  
ITQ_N <- ITQ_null %>%
  select(STRATUM_ID, SET_NO) %>%
  filter(!is.na(STRATUM_ID)) %>%
  group_by(STRATUM_ID) %>% 
  summarise(nullsets = length(unique(SET_NO)))


ITQ_SETS <- ITQ %>% 
  select(STRATUM_ID, SET_NO)%>%
  filter(!is.na(STRATUM_ID)) %>%
  group_by(STRATUM_ID) %>% 
  summarise(sets = length(unique(SET_NO)))


ITQ_summary <- left_join(ITQ_N, ITQ_SETS, by="STRATUM_ID") %>% 
  mutate(occurance = 100*(sets/nullsets)) 


# 4VsW sentinel survey summaries ----

# 4VsW Summaries: 

sentinel_summaries <- isdb %>% 
  filter(TRIPCD_ID == 7050) %>%
  filter(SETCD_ID == 5) %>%
  select (YEAR, EST_COMBINED_WT, EST_NUM_CAUGHT,STATION, GEAR)%>%
  group_by(YEAR) %>% 
  summarize(
    sum_wt = sum(EST_COMBINED_WT),
    mean_weight = mean(EST_COMBINED_WT),
    count_caught = length(EST_NUM_CAUGHT), 
    mean_caught = mean(EST_NUM_CAUGHT))

sentinel4VsW_stratum <- isdb %>%
  filter(TRIPCD_ID == 7050) %>%
  filter(SETCD_ID == 5) %>%
  select(STRATUM_ID, EST_COMBINED_WT, SET_NO) %>%
  group_by(STRATUM_ID) %>%
  summarize (
    sum_catch = sum(EST_COMBINED_WT), 
    total_sets = length(SET_NO)
  )

sentinel_null <- isdb_null 

sentinel_N <- sentinel_null %>%
  select(STRATUM_ID, SET_NO) %>%
  filter(!is.na(STRATUM_ID)) %>%
  group_by(STRATUM_ID) %>% 
  summarise(nullset = length(unique(SET_NO)))

sentinel_SETS <- sentinel %>% 
  select(STRATUM_ID, SET_NO)%>%
  filter(!is.na(STRATUM_ID)) %>%
  group_by(STRATUM_ID) %>% 
  summarise(set = length(unique(SET_NO)))


sentinel_summary <- left_join(sentinel_N,sentinel_SETS, by="STRATUM_ID") %>% 
  mutate(occurance = 100*(set/nullset))





# Shrimp ----

shrimp_summary <- isdb %>% 
  filter(TRIPCD_ID ==2210) %>%
  filter(SETCD_ID == 1) %>%
  select (NAFAREA_ID, EST_COMBINED_WT, EST_NUM_CAUGHT, SET_NO, GEAR) %>%
  filter(GEAR =="OTB") %>%
  group_by(NAFAREA_ID) %>%
  summarize (
    sum_catch = sum(EST_COMBINED_WT), 
    total_sets = length(unique(SET_NO)))

shrimp_summary <- isdb %>% 
  filter(TRIPCD_ID ==2210) %>%
  filter(SETCD_ID == 1) %>%
  select (NAFAREA_ID, EST_COMBINED_WT, EST_NUM_CAUGHT, SET_NO, GEAR) %>%
  filter(GEAR =="OTS") %>%
  group_by(NAFAREA_ID) %>%
  summarize (
    sum_catch = sum(EST_COMBINED_WT), 
    total_sets = length(unique(SET_NO)))


# 4VN Sentinel Survey summaries 

Sentinel_4VN_summary <- isdb_lengths %>%
  filter(TRIPCD_ID == 7052) %>%
  filter(SETCD_ID == 4) %>%
  select (YEAR, EST_COMBINED_WT, EST_NUM_CAUGHT,SET_NO)%>%
  group_by(YEAR) %>% 
  summarize(
    sum_wt = sum(EST_COMBINED_WT),
    mean_weight = mean(EST_COMBINED_WT),
    count_caught = length(EST_NUM_CAUGHT), 
    mean_caught = mean(EST_NUM_CAUGHT),
  )

Sentinel4VN_SETS <- isdb_lengths %>% 
  filter(TRIPCD_ID == 7052) %>%
  filter(SETCD_ID == 4) %>%
  select(YEAR, SET_NO)%>%
  group_by(YEAR) %>% 
  summarise(sets = length(unique(SET_NO)))

Sentinel_4VN_summary_SETID_5 <- isdb_lengths %>%
  filter(TRIPCD_ID == 7052) %>%
  filter(SETCD_ID == 5) %>%
  select (YEAR, EST_COMBINED_WT, EST_NUM_CAUGHT,SET_NO)%>%
  group_by(YEAR) %>% 
  summarize(
    sum_wt = sum(EST_COMBINED_WT),
    mean_weight = mean(EST_COMBINED_WT),
    count_caught = length(EST_NUM_CAUGHT), 
    mean_caught = mean(EST_NUM_CAUGHT),
  )

  Sentinel4VN_SETS_SETID_5 <- isdb_lengths %>% 
    filter(TRIPCD_ID == 7052) %>%
    filter(SETCD_ID == 5) %>%
    select(YEAR, SET_NO)%>%
    group_by(YEAR) %>% 
    summarise(sets = length(unique(SET_NO)))

Sentinel_4VN_summary_SETID6 <- isdb_lengths %>%
  filter(TRIPCD_ID == 7052) %>%
  filter(SETCD_ID == 6)
  select (YEAR, EST_COMBINED_WT, EST_NUM_CAUGHT,SET_NO)%>%
  group_by(YEAR) %>% 
  summarize(
    sum_wt = sum(EST_COMBINED_WT),
    mean_weight = mean(EST_COMBINED_WT),
    count_caught = length(EST_NUM_CAUGHT), 
    mean_caught = mean(EST_NUM_CAUGHT),
  )

Sentinel4VN_SETS_SETID_6 <- isdb_lengths %>% 
  filter(TRIPCD_ID == 7052) %>%
  filter(SETCD_ID == 6) %>%
  select(YEAR, SET_NO)%>%
  group_by(YEAR) %>% 
  summarise(sets = length(unique(SET_NO)))

Sentinel_4VN_summary_SETID_10 <- isdb_lengths %>%
  filter(TRIPCD_ID == 7052) %>%
  filter(SETCD_ID == 10) %>%
  select (YEAR, EST_COMBINED_WT, EST_NUM_CAUGHT,SET_NO)%>%
  group_by(YEAR) %>% 
  summarize(
    sum_wt = sum(EST_COMBINED_WT),
    mean_weight = mean(EST_COMBINED_WT),
    count_caught = length(EST_NUM_CAUGHT), 
    mean_caught = mean(EST_NUM_CAUGHT),
  )

Sentinel4VN_SETS_SETID_10 <- isdb_lengths %>% 
  filter(TRIPCD_ID == 7052) %>%
  filter(SETCD_ID == 10) %>%
  select(YEAR, SET_NO)%>%
  group_by(YEAR) %>% 
  summarise(sets = length(unique(SET_NO)))


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





                     

# landings for ISDB -----


#cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          #"#F0E442", "#0072B2", "#D55E00", "#CC79A7")

isdb <- read.csv("isdb.csv", header = T) %>%
  select(-X)

industry_sampled <- isdb %>%
  ungroup() %>%
  select(YEAR, TRIP_TYPE)

industry_sampled <- as.data.frame(table(industry_sampled))
industry_sampled$sampled <- ifelse(industry_sampled$Freq>0, 'X', '')

#convert to wide format
industry_sampled <- industry_sampled %>%
  select(-c(Freq))

industry_sampled <- spread(industry_sampled, YEAR, sampled)



landings_industrial <- read.csv("isdb.csv", header = T) %>%
  select(-X) %>%
  filter(TRIP_TYPE %in% c( "COD", "HADDOCK", "POLLOCK", "CRAB", "FLATFISH", "HALIBUT", "LOBSTER", "REDFISH", "SCALLOP", "SHRIMP")) %>%
  select(YEAR, STRATUM_ID, TRIPCD_ID, SET_NO, EST_KEPT_WT, TRIP_TYPE) %>%
  replace(is.na(.), 0) %>%
  group_by(YEAR) %>%
  summarize(total_wt = sum(EST_KEPT_WT), 
            triptype = factor (TRIP_TYPE))

landings_commericalsurveys <- read.csv("isdb.csv", header = T) %>%
  select(-X) %>%
  filter(TRIP_TYPE %in% c( "4VN SENTINEL SURVEY", "4VSW SENTINEL PROGRAM", "HALIBUT LONGLINE SURVEY", "SNOWCRAB SURVEY", "LOBSTER SURVEY")) %>%
  select(YEAR, STRATUM_ID, TRIPCD_ID, SET_NO, EST_KEPT_WT, TRIP_TYPE) %>%
  replace(is.na(.), 0) %>%
  group_by(YEAR) %>%
  summarize(total_wt = sum(EST_KEPT_WT), 
            triptype = factor (TRIP_TYPE))

landings %>%
  ggplot(aes(x=YEAR, y=total_wt, fill=triptype)) + 
  geom_bar( stat="identity") +
  scale_y_continuous(trans = 'log10', labels = function(x) format(x, scientific = FALSE)) +
  theme_classic() 

grid.arrange()

  
str(landings)

#landings %>%
  ggplot(aes(x=YEAR, y=total_wt, color = triptype))+
  geom_point() +
  geom_line()+
  theme_classic()

#landings %>%
  ggplot(aes(x=YEAR, y=total_wt))+
  geom_point() +
  geom_line()+
  theme_classic() +
  facet_wrap(~triptype)

 
  