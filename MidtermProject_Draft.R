############################################## 1. Data Cleaning and Combining

# Import two dataset
co2 <- read.csv("CO2EmissionsCanada.csv", header = T)
battery <- read.csv("MY2012-2021BatteryElectricVehicles.csv", header = T)

# Clean "battery" dataset
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

# Clean "co2" dataset
co2 <- co2[,-c(11)]
colnames(co2) <- c("Make","Model","VehicleClass","EngineSize","Cylinders","Transmission","FuelType","FC_City","FC_Hwy","FC_Comb","CO2Emissions")

# Combine two dataset
identical(names(co2), names(battery))
co2_combine <- rbind(co2,battery)
co2_combine <- co2_combine[order(co2_combine$Make),]

############################################## 2. Encode vehicle class

# Encode vehicle class
co2_VehicleClass <- data.frame(co2[,3])
colnames(co2_VehicleClass) <- c("VehicleClass")
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="MINICOMPACT"] <- c(2405)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="SUBCOMPACT"] <- c(2830)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="COMPACT"] <- c(3110)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="MID-SIZE"] <- c(3395)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="FULL-SIZE"] <- c(3400)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="STATION WAGON - SMALL"] <- c(3680)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="STATION WAGON - MID-SIZE"] <- c(4500)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="TWO-SEATER"] <- c(4500)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="PICKUP TRUCK - SMALL"] <- c(6000)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="SUV - SMALL"] <- c(6000)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="PICKUP TRUCK - STANDARD"] <- c(8500)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="SUV - STANDARD"] <- c(10000)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="MINIVAN"] <- c(8500)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="VAN - CARGO"] <- c(8500)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="SPECIAL PURPOSE VEHICLE"] <- c(8500)
co2_VehicleClass$VehicleClass_Value[co2_VehicleClass$VehicleClass=="VAN - PASSENGER"] <- c(10000)

# Below code: same as the result above, but using another simpler method
# co2_VehicleClass <- data.frame(co2[["VehicleClass"]],
#                     encode_ordinal(co2[["VehicleClass"]], 
#                     order = c("MINICOMPACT","SUBCOMPACT","COMPACT","MID-SIZE","FULL-SIZE",
#                               "STATION WAGON - SMALL","STATION WAGON - MID-SIZE",
#                               "TWO-SEATER","PICKUP TRUCK - SMALL","SUV - SMALL",
#                               "PICKUP TRUCK - STANDARD","SUV - STANDARD","MINIVAN","VAN - CARGO",
#                               "SPECIAL PURPOSE VEHICLE","VAN - PASSENGER")),
#                     useNA = "ifany")
# colnames(co2_VehicleClass)[1] <- "VehicleClass"
# colnames(co2_VehicleClass)[2] <- "VehicleClass_Value"
# co2_VehicleClass <- co2_VehicleClass[,-c(3)]

# Combine two dataset
co2_final <- cbind(co2, co2_VehicleClass[c("VehicleClass_Value")])

# Transform final data type
co2_final <- transform(co2_final,Make=as.factor(Make),
                           Model=as.factor(Model),
                           VehicleClass=as.factor(VehicleClass),
                           FuelType=as.factor(FuelType),
                           EngineSize=as.numeric(EngineSize),
                           Cylinders=as.numeric(Cylinders),
                           CO2Emissions=as.numeric(CO2Emissions),
                           Transmission=as.character(Transmission))

# Simplify transmission
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

############################################## 3. Exploratory Data Analysis

# # Variable "Make" barplot
# make_data <- co2_combine %>% 
#              group_by(Make) %>% 
#              summarise(Count = n())
# 
# make_barplot <-
#   ggplot(data=make_data, aes(x=Make, y=Count)) +
#   geom_bar(stat='identity', fill=rainbow(n=length(make_data$Count))) +
#   theme(axis.text.x=element_text(angle=90, vjust=0.7), plot.title=element_text(size=15)) +
#   labs(x="Brand", y="Amount of vehicle", title="The amount of vehicle in different brands")

# Boxplot of co2
co2_boxplot <-
  ggplot(data=co2_combine, aes(x=Make, y=,CO2Emissions, fill=Make)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, vjust=0.7), legend.position='none') +
  labs(x="Brand", y="CO2 emission", title="The CO2 emission of vehicle in different brands")

# Transform variables to the factor type
co2_stackplot <- transform(co2_final2,Make=as.factor(Make),
                       Model=as.factor(Model),
                       VehicleClass=as.factor(VehicleClass),
                       EngineSize=as.factor(EngineSize),
                       Cylinders=as.factor(Cylinders),
                       Transmission=as.factor(Transmission),
                       FuelType=as.factor(FuelType),
                       CO2Emissions=as.numeric(CO2Emissions))

# Stack plot1
stackplot_count1 <- co2_stackplot %>%
  count(Make,Transmission)
stackplot1 <-
  ggplot(data=stackplot_count1) +
  geom_col(aes(x=Make, y=n, fill=Transmission)) +
  theme(axis.text.x=element_text(angle=90, vjust=0.5), plot.title=element_text(size=15)) 

# # stack plot2
# stackplot_count2 <- co2_stackplot %>%
#   count(Make,Cylinders)
# stackplot2 <-
#   ggplot(data=stackplot_count2) +
#   geom_col(aes(x=Make, y=n, fill=Cylinders)) +
#   theme(axis.text.x=element_text(angle=90, vjust=0.5), plot.title=element_text(size=15)) 

# Stack plot3
stackplot_count3 <- co2_stackplot %>%
  count(Make,FuelType)
stackplot3 <-
  ggplot(data=stackplot_count3) +
  geom_col(aes(x=Make, y=n, fill=FuelType)) +
  theme(axis.text.x=element_text(angle=90, vjust=0.7), plot.title=element_text(size=15)) 

# stack plot4
stackplot_count4 <- co2_stackplot %>%
  count(VehicleClass,Transmission)
stackplot4 <-
  ggplot(data=stackplot_count4) +
  geom_col(aes(x=VehicleClass, y=n, fill=Transmission)) +
  theme(axis.text.x=element_text(angle=90, vjust=0.7), plot.title=element_text(size=15)) 

# stack plot5
stackplot_count5 <- co2_stackplot %>%
  count(VehicleClass,Cylinders)
stackplot5 <-
  ggplot(data=stackplot_count5) +
  geom_col(aes(x=VehicleClass, y=n, fill=Cylinders)) +
  theme(axis.text.x=element_text(angle=90, vjust=0.7), plot.title=element_text(size=15)) 

# Stack plot6
stackplot_count6 <- co2_stackplot %>%
  count(VehicleClass,FuelType)
stackplot6 <-
  ggplot(data=stackplot_count6) +
  geom_col(aes(x=VehicleClass, y=n, fill=FuelType)) +
  theme(axis.text.x=element_text(angle=90, vjust=0.7), plot.title=element_text(size=15)) 

# Stack plot7
stackplot_count7 <- co2_stackplot %>%
  count(Make,VehicleClass)
stackplot7 <-
  ggplot(data=stackplot_count7) +
  geom_col(aes(x=Make, y=n, fill=VehicleClass)) +
  theme(axis.text.x=element_text(angle=90, vjust=0.5), plot.title=element_text(size=15))

# Individual schools separately1
individual_plot1 <-
  ggplot(co2_combine) +
  geom_point() +
  aes(x=FC_Comb,y=CO2Emissions,color=factor(Make)) +
  geom_smooth(method="lm",se=FALSE) +
  facet_wrap(~Make)

# # Individual schools separately2
# individual_plot2 <-
#   ggplot(co2_combine) +
#   geom_point() +
#   aes(x=FC_Comb,y=CO2Emissions,color=factor(Cylinders)) +
#   geom_smooth(method="lm",se=FALSE) +
#   facet_wrap(~Cylinders)

############################################## 4. Model prepare

# Determining whether using log transmission
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

density_tramsmission <-
  ggplot(data=co2_final,aes(x=CO2Emissions))+
  geom_density(aes(color=factor(Transmission)))+
  labs(title='xxx',x='CO2Emissions',color='Transmission')+
  geom_density(aes(x=CO2Emissions))

density_tramsmission_log <-
  ggplot(data=co2_final,aes(x=log(CO2Emissions)))+
  geom_density(aes(color=factor(Transmission)))+
  labs(title='xxx',x='CO2Emissions',color='Transmission')+
  geom_density(aes(x=log(CO2Emissions)))

# Determining whether varying slope and intercept
varying_fueltype <-
ggplot(data=co2_final,aes(y=CO2Emissions,x=FC_Comb,FuelType=factor(FuelType)))+
  geom_point(aes(y=CO2Emissions,x=FC_Comb,color=FuelType),alpha=0.2)+
  geom_smooth(aes(y=CO2Emissions,x=FC_Comb,color=FuelType),se=F,method="lm")+
  xlab("Fuel consumption combination")+
  ylab("CO2 emissions")+
  theme(legend.position="right")

varing_transmission <-
ggplot(data=co2_final,aes(y=CO2Emissions,x=EngineSize,Transmission=factor(Transmission)))+
  geom_point(aes(y=CO2Emissions,x=EngineSize,color=Transmission),alpha=0.2)+
  geom_smooth(aes(y=CO2Emissions,x=EngineSize,color=Transmission),se=F,method="lm")+
  xlab("Engine size")+
  ylab("CO2 emissions")+
  theme(legend.position="right")

############################################## 5. Model fitting

# Fit model1
model1 <- lmer(CO2Emissions ~ VehicleClass_Value + EngineSize + FC_Comb + (1+EngineSize|Transmission) + (1+FC_Comb|FuelType), data=co2_final)

# Fit model2
# model2 <- lmer(log(CO2Emissions) ~ VehicleClass_Value + EngineSize + FC_Comb + (1+EngineSize|Transmission) + (1+FC_Comb|FuelType), data=co2_final2)

# summary(model1)

# coef(model1)

# fixef(model1)

# ranef(model1)

############################################## 6. Model checking

# Residual plot and Q-Q plot
re <- plot(model1)
qq <- qqmath(model1)






