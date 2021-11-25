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



#Proportion of catch: 4VsW
#THIS IS WHERE YOU USE THE NULL SETS

#frequency of occurrence 

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






#distribution maps
aggregator = function(df = NULL,
                      lat.field = "LATITUDE",
                      lon.field = "LONGITUDE",
                      agg.fields = NULL,
                      agg.minutes = 5) {
  
  agg = agg.minutes / 60
  
  df = df[!is.na(df[lat.field]) & !is.na(df[lon.field]), ]
  
  df$LATITUDE = (round(df[, lat.field] / agg) * agg) #+ 0.5 * agg
  df$LONGITUDE = (round(df[, lon.field] / agg) * agg) #- 0.5 * agg
  
  for (i in 1:length(agg.fields)) {
    if (nrow(df[is.na(df[, agg.fields[i]]), ]) > 0)
      df[is.na(df[, agg.fields[i]]), ][, agg.fields[i]] <- 0
  }
  df[agg.fields] <- sapply(df[agg.fields], as.numeric)
  df.agg = as.data.frame(as.list(aggregate(
    df[agg.fields],
    by = df[c("LATITUDE", "LONGITUDE")],
    FUN = function(x)
      c(
        #MEAN = round(mean(x), 4)
        # CNT = round(length(x), 4),
        SUM = round(sum(x), 4)
      )
  )))
  colnames(df.agg)[colnames(df.agg)=="LATITUDE"] <- lat.field
  colnames(df.agg)[colnames(df.agg)=="LONGITUDE"] <- lon.field
  return(df.agg)
}

sample <- read.csv(file = "sample.csv")
#replace NAs with zeroes - these are the nullsets, which we will want to know about
sample[["TOTNO"]][is.na(sample[["TOTNO"]])] <- -1
sample[["TOTWGT"]][is.na(sample[["TOTWGT"]])] <- -1
#for each of mature and immature, extract data and nullsets
mat <- sample[(is.na(sample$maturity) & sample$TOTNO==-1) | sample$maturity == "mature",]
immat <- sample[(is.na(sample$maturity) & sample$TOTNO==-1) | sample$maturity == "immature ",]

test5 <- aggregator(mat, lat.field = "lat", lon.field = "long", agg.fields = c("TOTNO", "TOTWGT"), agg.minutes = 5)
test30 <- aggregator(mat, lat.field = "lat", lon.field = "long", agg.fields = c("TOTNO", "TOTWGT"), agg.minutes = 30)

mature5 <- ggplot() +
  geom_point(data=test5[test5$TOTNO>0,], aes(long, lat, size = TOTNO), alpha = 2/3) +
  geom_point(data=test5[test5$TOTNO<0,], aes(long, lat), shape=3, size= 0.1)

mature30 <- ggplot() +
  geom_point(data=test30[test30$TOTNO>0,], aes(long, lat, size = TOTNO), alpha = 2/3) +
  geom_point(data=test30[test30$TOTNO<0,], aes(long, lat), shape=3, size= 0.1)


#What needs to be done
#1. fix the bubble function 
#2. play around with aggregates to find most representative option
#3. generate maps for all surveys


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
halibut_observey <- isdb %>% filter(TRIPCD_ID == 30)
halibut_longline <- isdb %>% filter(TRIPCD_ID == 7057)
snowcrab <- isdb %>% filter(TRIPCD_ID == 7061)
redfish <- isdb %>% filter(TRIPCD_ID == 23)
cod <- isdb %>% filter(TRIPCD_ID == 7001)
flatfish <- isdb %>% filter(TRIPCD_ID == 49)
shrimp <- isdb %>% filter(TRIPCD_ID == 2210)
silverhake <- isdb %>% filter(TRIPCD_ID == 14) 