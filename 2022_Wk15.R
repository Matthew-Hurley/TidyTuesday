library(tidytuesdayR)
library(tidyverse)

# Read data
tuesdata <- tidytuesdayR::tt_load(2022, week = 15)

fuel <- tuesdata$fuel_gdp
fuel <- fuel %>% filter(Year == 2016)

death <- tuesdata$death_source
death <- death %>% filter(Year == 2016)

fuel <- fuel %>% left_join(death, by = c('Entity'='Entity'))

fuel <- rename( fuel, Access = `Access to clean fuels and technologies for cooking (% of population)`,
                GDP = `GDP per capita, PPP (constant 2017 international $)`,
                Population = `Population (historical estimates)`,
                Deaths = `Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Rate)`)

ggplot(fuel, aes(log(GDP), Access, size = Deaths)) + 
  geom_smooth(formula = y ~ log(x), method = 'lm', alpha = 0.3, color = 'plum2', se = FALSE, show.legend = FALSE)+
  geom_point(alpha = 0.7, color = 'navy') +
  ylim(0,100) + ylab('Access to clean fuels and technologies for cooking (% of population)') +
  xlab('GDP per capita (Log scale)') + labs(size = 'Indoor pollution\n death rate') +
  theme(panel.background = element_rect(fill = "lightcyan1",
                                        colour = "lightcyan1",
                                        size = 0.5, linetype = "solid")) +
  ggtitle('Indoor Pollution Worldwide 2016')
