library(tidyverse)
library(ggpubr)
# Read datasets
tuesdata <- tidytuesdayR::tt_load(2022, week = 18)

# Set plotting area


# Average cost of power
avg_cost <- tuesdata$average_cost

# Plot
ac <- ggplot(avg_cost, aes(x= year, y=gas_mwh))+ geom_line(col='linen')+
  geom_line(aes(x = year, y = solar_mwh), col = 'gold')+
  geom_line(aes(x = year, y = wind_mwh), col = 'cyan')+
  theme_dark() + xlab('Year') + ylab('$US/MWh')+
  geom_label(label = 'Solar', x = 2009, y = 171)+
  geom_label(label = 'Wind', x = 2009, y = 77)+
  geom_label(label = 'Gas', x = 2009, y = 62)+
  ggtitle("US Average Cost of Power")

# Capacity
capacity <- tuesdata$capacity
capacity <- capacity %>% select(type, year, total_gw)

# Plot
cap <- ggplot(capacity, aes(year, total_gw, col = type))+ geom_line()+
  theme_dark()+ ylab('Total Gigawatts')+ xlab('Year')+
  geom_label(data = capacity %>% filter(year == 2019), 
             aes(label = type), col='black')+ ggtitle('Total Capacity')

# Solar
solar <- tuesdata$solar

scale_factor = (max(solar$solar_capacity)-min(solar$solar_capacity))/
  (max(solar$solar_mwh)-min(solar$solar_mwh))

# Plot
sol <- ggplot(solar, aes(date, solar_mwh))+ geom_line(col = 'coral', alpha = 0.4)+
  scale_y_continuous('$US/MWh', sec.axis = 
                       sec_axis(~.*scale_factor, name= 'GW Capacity'))+
  geom_line(aes(x = date, y = (scale_factor^-1)*solar_capacity),
            col = 'plum2', alpha = 0.4)+ theme_dark()+ 
  geom_smooth(aes(date, solar_mwh), se = FALSE, 
              col = 'coral', alpha = 0.8)+
  geom_smooth(aes(x = date, y = (scale_factor^-1)*solar_capacity),
              se = FALSE, col = 'plum2', alpha = 0.8)+
  xlab('Date')+ ggtitle('Solar Power Price and Capacity Over Time')+
  geom_label(label = 'Price', x = as.Date('2009-03-01'), y = 190)+
  geom_label(label = 'Capacity', x = as.Date('2009-03-01'), y = 55)

# Wind
wind <- tuesdata$wind

wind_scale_factor = (max(wind$wind_capacity)-min(wind$wind_capacity))/
  (max(wind$wind_mwh)-min(wind$wind_mwh))

# Plot
wd <- ggplot(wind, aes(date, wind_mwh))+ geom_line(col = 'cyan', alpha = 0.4)+
  scale_y_continuous('$US/MWh', sec.axis = 
                       sec_axis(~.*wind_scale_factor, name= 'GW Capacity'))+
  geom_line(aes(x = date, y = (wind_scale_factor^-1)*wind_capacity),
            col = 'darkgoldenrod1', alpha = 0.4)+ theme_dark()+ 
  geom_smooth(aes(date, wind_mwh), se = FALSE, 
              col = 'cyan', alpha = 0.8)+
  geom_smooth(aes(x = date, y = (wind_scale_factor^-1)*wind_capacity),
              se = FALSE, col = 'darkgoldenrod1', alpha = 0.8)+
  xlab('Date')+ ggtitle('Wind Power Price and Capacity Over Time')+
  geom_label(label = 'Price', x = as.Date('2009-03-01'), y = 88)+
  geom_label(label = 'Capacity', x = as.Date('2009-03-01'), y = 25)

# Plot all four in same window
ggarrange(ac, cap, sol, wd, ncol = 2, nrow = 2)

