############################################## 1. set up

pacman::p_load(ggplot2,
               knitr,
               arm,
               dplyr,
               stringr,
               rstan,
               rstanarm,
               readr,
               tidyverse,
               hrbrthemes,
               corrplot,
               lmer,
               lmer4,
               lattice,
               gridExtra,
               GGally)

############################################## 2. Data Cleaning and Combining

# import two dataset
co2 <- read.csv("CO2EmissionsCanada.csv", header = T)
battery <- read.csv("MY2012-2021BatteryElectricVehicles.csv", header = T)

# clean "battery" dataset
battery <- battery[-c(1),c(2,3,4,6,7,11,12,13,15)]
battery <- battery %>%
  add_column(Engine.Size.L="NA")
battery <- battery %>%
  add_column(Cylinders="NA")
battery <- battery[,c(1,2,3,10,11,4,5,6,7,8,9)]
colnames(battery) <- c("Make","Model","VehicleClass","EngineSize","Cylinders","Transmission","FuelType","FC_City","FC_Hwy","FC_Comb","CO2Emissions")
battery <- battery[order(battery$Make),]
battery <- distinct(battery,Make,Model,VehicleClass,.keep_all=TRUE)
battery <- transform(battery,EngineSize=as.numeric(EngineSize),
                     Cylinders=as.integer(Cylinders),
                     FC_City=as.numeric(FC_City),
                     FC_Hwy=as.numeric(FC_Hwy),
                     FC_Comb=as.numeric(FC_Comb),
                     CO2Emissions=as.integer(CO2Emissions))
battery <- mutate(battery,Make=toupper(Make),Model=toupper(Model),VehicleClass=toupper(VehicleClass))

# clean "co2" dataset
co2 <- co2[,-c(11)]
colnames(co2) <- c("Make","Model","VehicleClass","EngineSize","Cylinders","Transmission","FuelType","FC_City","FC_Hwy","FC_Comb","CO2Emissions")

# combine two dataset
identical(names(co2), names(battery))
co2_final <- rbind(co2,battery)
co2_final <- co2_final[order(co2_final$Make),]

############################################## 3. Exploratory Data Analysis

# calculate "Make" amount
Make_data <- co2_final %>% 
  group_by(Make) %>% summarise(Count = n())

#################### variable "Make" barplot
make_barplot <-
ggplot(data=Make_data, aes(x=Make, y=Count)) +
  geom_bar(stat='identity', fill=rainbow(n=length(Make_data$Count))) +
  theme(axis.text.x=element_text(angle=90, vjust=0.7), plot.title=element_text(size=15)) +
  labs(x="Brand", y="Amount of vehicle", title="The amount of vehicle in different brands")

#################### "co2" boxplot
co2_boxplot <-
ggplot(data=co2_final, aes(x=Make, y=,CO2Emissions, fill=Make)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, vjust=0.7), legend.position='none') +
  labs(x="Brand", y="CO2 emission", title="The CO2 emission of vehicle in different brands")

#################### distribution of CO2 emissions histogram
co2_histogram <-
ggplot(data=co2_final, aes(x=CO2Emissions)) +
  geom_histogram(aes(y=..density..),binwidth=5, fill="#666666", color="#e9ecef") +
  geom_density(lwd=1.5,linetype=1,colour="#FF0033") +
  theme(plot.title=element_text(size=15)) +
  labs(x="CO2 emissions", y="Count", title="Distribution of CO2 emissions")

# prepare for ggpairs
Corrplot_data <- co2_final[,-c(1,2,3,6)]

#################### ggpairs
ggpairs <-
ggpairs(Corrplot_data) +
  theme_bw()

# prepare for the pie plot
Percent_data_1 <- co2_final %>% 
  group_by(VehicleClass) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent=`n`/sum(`n`)) 
Percent_data_1$label <- scales::percent(Percent_data_1$percent)

Percent_data_2 <- co2_final %>% 
  group_by(EngineSize) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent=`n`/sum(`n`)) 
Percent_data_2$label <- scales::percent(Percent_data_2$percent)

Percent_data_3 <- co2_final %>% 
  group_by(Cylinders) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent=`n`/sum(`n`)) 
Percent_data_3$label <- scales::percent(Percent_data_3$percent)

Percent_data_4 <- co2_final %>% 
  group_by(Transmission) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent=`n`/sum(`n`)) 
Percent_data_4$label <- scales::percent(Percent_data_4$percent)

Percent_data_5 <- co2_final %>% 
  group_by(FuelType) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent=`n`/sum(`n`)) 
Percent_data_5$label <- scales::percent(Percent_data_5$percent)

#################### pie plot: VehicleClass, EngineSize, Cylinders, Transmission, FuelType 
pieplot1 <-
ggplot(data=Percent_data_1)+
  geom_bar(aes(x="", y=percent, fill=VehicleClass), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(percent)-percent/2), label=Percent_data_1$label)

pieplot2 <-
ggplot(data=Percent_data_2)+
  geom_bar(aes(x="", y=percent, fill=EngineSize), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(percent)-percent/2), label=Percent_data_2$label)

pieplot3 <-
ggplot(data=Percent_data_3)+
  geom_bar(aes(x="", y=percent, fill=Cylinders), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(percent)-percent/2), label=Percent_data_3$label)

pieplot4 <-
ggplot(data=Percent_data_4)+
  geom_bar(aes(x="", y=percent, fill=Transmission), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(percent)-percent/2), label=Percent_data_4$label)

pieplot5 <-
ggplot(data=Percent_data_5)+
  geom_bar(aes(x="", y=percent, fill=FuelType), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(percent)-percent/2), label=Percent_data_5$label)


