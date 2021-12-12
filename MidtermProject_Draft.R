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
co2_combine <- rbind(co2,battery)
co2_combine <- co2_final[order(co2_final$Make),]

############################################## 3. Exploratory Data Analysis

# variable "Make" barplot
make_data <- co2_combine %>% 
             group_by(Make) %>% 
             summarise(Count = n())

make_barplot <-
  ggplot(data=make_data, aes(x=Make, y=Count)) +
  geom_bar(stat='identity', fill=rainbow(n=length(Make_data$Count))) +
  theme(axis.text.x=element_text(angle=90, vjust=0.7), plot.title=element_text(size=15)) +
  labs(x="Brand", y="Amount of vehicle", title="The amount of vehicle in different brands")

# "co2" boxplot
co2_boxplot <-
  ggplot(data=co2_combine, aes(x=Make, y=,CO2Emissions, fill=Make)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, vjust=0.7), legend.position='none') +
  labs(x="Brand", y="CO2 emission", title="The CO2 emission of vehicle in different brands")

# ggpairs
corrplot_data <- co2[,-c(1,2,3,6)]

ggpairs <- 
  ggpairs(corrplot_data) +
  theme_bw()

# transform variables to the factor type
co2_final <- transform(co2,Make=as.factor(Make),
                       Model=as.factor(Model),
                       VehicleClass=as.factor(VehicleClass),
                       EngineSize=as.factor(EngineSize),
                       Cylinders=as.factor(Cylinders),
                       Transmission=as.factor(Transmission),
                       FuelType=as.factor(FuelType))

# stack plot
stackplot <- co2_combine %>%
             count(EngineSize,Cylinders)

stackplot <-
  ggplot(data=stackplot) +
  geom_col(aes(x=Cylinders, y=n, fill=EngineSize)) 

############################################## 4. Model Fitting








ggplot(data=co2_final,aes(y=CO2Emissions,x=FC_Comb,FuelType=factor(FuelType)))+
  geom_point(aes(y=CO2Emissions,x=FC_Comb,color=FuelType),alpha=0.2)+
  geom_smooth(aes(y=CO2Emissions,x=FC_Comb,color=FuelType),se=F,method="lm")+
  xlab("Fuel consumption combination")+
  ylab("CO2 emissions")+
  theme(legend.position="right")

ggplot(data=co2_final,aes(y=CO2Emissions,x=FC_Comb,Cylinders=factor(Cylinders)))+
  geom_point(aes(y=CO2Emissions,x=FC_Comb,color=Cylinders),alpha=0.2)+
  geom_smooth(aes(y=CO2Emissions,x=FC_Comb,color=Cylinders),se=F,method="lm")+
  xlab("Fuel consumption combination")+
  ylab("CO2 emissions")+
  theme(legend.position="right")

ggplot(data=co2_final,aes(y=CO2Emissions,x=FC_Comb,Transmission=factor(Transmission)))+
  geom_point(aes(y=CO2Emissions,x=FC_Comb,color=Transmission),alpha=0.2)+
  geom_smooth(aes(y=CO2Emissions,x=FC_Comb,color=Transmission),se=F,method="lm")+
  xlab("Fuel consumption combination")+
  ylab("CO2 emissions")+
  theme(legend.position="right")

ggplot(data=co2_final,aes(y=CO2Emissions,x=FC_Comb,VehicleClass=factor(VehicleClass)))+
  geom_point(aes(y=CO2Emissions,x=FC_Comb,color=VehicleClass),alpha=0.2)+
  geom_smooth(aes(y=CO2Emissions,x=FC_Comb,color=VehicleClass),se=F,method="lm")+
  xlab("Fuel consumption combination")+
  ylab("CO2 emissions")+
  theme(legend.position="right")

