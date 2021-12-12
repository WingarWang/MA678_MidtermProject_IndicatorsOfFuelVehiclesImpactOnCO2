# pie plot
# prepare
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

# draw
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
