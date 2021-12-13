############################################## 1. Set up

#library
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
               lme4,
               lmerTest,
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
co2_combine <- co2_combine[order(co2_combine$Make),]

############################################## 3. Encode vehicle class

# encode vehicle class
co2_VehicleClass <- data.frame(co2[["VehicleClass"]],
                       encode_ordinal(co2[["VehicleClass"]], 
                                      order = c("MINICOMPACT","SUBCOMPACT","COMPACT","MID-SIZE","FULL-SIZE","STATION WAGON - SMALL","STATION WAGON - MID-SIZE","TWO-SEATER","PICKUP TRUCK - SMALL","SUV - SMALL","PICKUP TRUCK - STANDARD","SUV - STANDARD","MINIVAN","VAN - CARGO","SPECIAL PURPOSE VEHICLE","VAN - PASSENGER")),
                       useNA = "ifany")
colnames(co2_VehicleClass)[1] <- "VehicleClass"
colnames(co2_VehicleClass)[2] <- "VehicleClass_Value"
co2_VehicleClass <- co2_VehicleClass[,-c(3)]

# combine two dataset
co2_final <- cbind(co2, co2_VehicleClass[c("VehicleClass_Value")])

# final data type
co2_final <- transform(co2_final,Make=as.factor(Make),
                           Model=as.factor(Model),
                           VehicleClass=as.factor(VehicleClass),
                           FuelType=as.factor(FuelType),
                           EngineSize=as.numeric(EngineSize),
                           Cylinders=as.numeric(Cylinders),
                           CO2Emissions=as.numeric(CO2Emissions),
                           Transmission=as.character(Transmission))

# simplify transmission
co2_final2 <- co2_final
co2_final2$Transmission[co2_final2$Transmission=="A10"|co2_final2$Transmission =="A4"
                        |co2_final2$Transmission=="A5"|co2_final2$Transmission=="A6"|co2_final2$Transmission=="A7"
                        |co2_final2$Transmission=="A8"|co2_final2$Transmission=="A9"] <- "A"
co2_final2$Transmission[co2_final2$Transmission == "AM5"|co2_final2$Transmission == "AM6"
                        |co2_final2$Transmission == "AM7"|co2_final2$Transmission == "AM8"|co2_final2$Transmission == "AM9"] <- "AM"
co2_final2$Transmission[co2_final2$Transmission == "AS10"|co2_final2$Transmission == "AS4"
                        |co2_final2$Transmission == "AS5"|co2_final2$Transmission == "AS6"|co2_final2$Transmission == "AS7"
                        |co2_final2$Transmission == "AS8"|co2_final2$Transmission == "AS9"] <- "AS"
co2_final2$Transmission[co2_final2$Transmission == "AV"|co2_final2$Transmission == "AV10"
                        |co2_final2$Transmission == "AV6"|co2_final2$Transmission == "AV7"|co2_final2$Transmission == "AV8"] <- "AV"
co2_final2$Transmission[co2_final2$Transmission=="M5"|co2_final2$Transmission=="M6"
                        |co2_final2$Transmission=="M7"] <- "M"
co2_final2 <- transform(co2_final2,Transmission=as.factor(Transmission))

############################################## 4. Exploratory Data Analysis

# variable "Make" barplot
make_data <- co2_combine %>% 
             group_by(Make) %>% 
             summarise(Count = n())

make_barplot <-
  ggplot(data=make_data, aes(x=Make, y=Count)) +
  geom_bar(stat='identity', fill=rainbow(n=length(make_data$Count))) +
  theme(axis.text.x=element_text(angle=90, vjust=0.7), plot.title=element_text(size=15)) +
  labs(x="Brand", y="Amount of vehicle", title="The amount of vehicle in different brands")

# "co2" boxplot
co2_boxplot <-
  ggplot(data=co2_combine, aes(x=Make, y=,CO2Emissions, fill=Make)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, vjust=0.7), legend.position='none') +
  labs(x="Brand", y="CO2 emission", title="The CO2 emission of vehicle in different brands")

# ggpairs
# corrplot_data <- co2_final[,-c(1,2,3,6)]
# 
# ggpairs <- 
#   ggpairs(corrplot_data) +
#   theme_bw()

# transform variables to the factor type
co2_stackplot <- transform(co2,Make=as.factor(Make),
                       Model=as.factor(Model),
                       VehicleClass=as.factor(VehicleClass),
                       EngineSize=as.factor(EngineSize),
                       Cylinders=as.factor(Cylinders),
                       Transmission=as.factor(Transmission),
                       FuelType=as.factor(FuelType),
                       CO2Emissions=as.numeric(CO2Emissions))

# stack plot
stackplot <- co2_stackplot %>%
             count(EngineSize,Cylinders)

stackplot <-
  ggplot(data=stackplot) +
  geom_col(aes(x=Cylinders, y=n, fill=EngineSize)) 

# individual schools separately
individual_plot <-
  ggplot(co2_combine) +
  geom_point() +
  aes(x=FC_Comb,y=CO2Emissions,color=factor(Make)) +
  geom_smooth(method="lm",se=FALSE) +
  facet_wrap(~Make)

############################################## 5. Model prepare

# determining whether using log transmission
density_fueltype <-
  ggplot(data=co2_final,aes(x=CO2Emissions))+
  geom_density(aes(color=factor(FuelType)))+
  labs(title='xxx',x='CO2Emissions',color='FuelType')+
  geom_density(aes(x=CO2Emissions))

density_fueltype_log <-
  ggplot(data=co2_final,aes(x=log(CO2Emissions)))+
  geom_density(aes(color=factor(FuelType)))+
  labs(title='xxx',x='CO2Emissions',color='FuelType')+
  geom_density(aes(x=log(CO2Emissions)))

# determining whether varying slope and intercept
varying_fueltype <-
ggplot(data=co2_final,aes(y=CO2Emissions,x=FC_Comb,FuelType=factor(FuelType)))+
  geom_point(aes(y=CO2Emissions,x=FC_Comb,color=FuelType),alpha=0.2)+
  geom_smooth(aes(y=CO2Emissions,x=FC_Comb,color=FuelType),se=F,method="lm")+
  xlab("Fuel consumption combination")+
  ylab("CO2 emissions")+
  theme(legend.position="right")

varing_transmission <-
ggplot(data=co2_final2,aes(y=CO2Emissions,x=FC_Comb,Transmission=factor(Transmission)))+
  geom_point(aes(y=CO2Emissions,x=FC_Comb,color=Transmission),alpha=0.2)+
  geom_smooth(aes(y=CO2Emissions,x=FC_Comb,color=Transmission),se=F,method="lm")+
  xlab("Fuel consumption combination")+
  ylab("CO2 emissions")+
  theme(legend.position="right")

############################################## 6. Model fitting




############################################## 7. Model checking













