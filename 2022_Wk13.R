# Tidy Tuesday Week 13 2022

library(tidytuesdayR)
library(tidyverse)

# Read data
tuesdata <- tidytuesdayR::tt_load('2022-03-29')
sports <- tuesdata$sports

# Have a look at the data
head(sports)
summary(sports)

# Aggregate relevant data
relevant <- sports %>% summarize(total_exp_women = sum(exp_women, na.rm = TRUE), 
                     total_exp_men = sum(exp_men, na.rm = TRUE),
                     total_part_women = sum(sum_partic_women, na.rm = TRUE),
                     total_part_men = sum(sum_partic_men, na.rm = TRUE))

# Compute proportions
props <- relevant %>% transmute(exp_women = total_exp_women / (total_exp_women + total_exp_men),
                                exp_men = total_exp_men / (total_exp_women + total_exp_men),
                                part_women = total_part_women / (total_part_women + total_part_men),
                                part_men = total_part_men / (total_part_women + total_part_men))

# Arrange data, not particularly elegantly.
particiaption <- props[c(3,4)] %>% rename(women = part_women, men = part_men) %>% pivot_longer(c(1,2), names_to = 'gender', values_to = 'participation')
expenditure <- props[c(1,2)]  %>% rename(women = exp_women, men = exp_men) %>% pivot_longer(c(1,2), names_to = 'gender', values_to = 'expenditure')
props <- particiaption %>% inner_join(expenditure, by = c('gender'))%>% pivot_longer(cols =-1)

# Plot data.
props %>% ggplot(aes(x = as.factor(name), fill = as.factor(gender), y = value ))+
  geom_bar(stat = "identity")+xlab("")+ylab("Proportion")+
  labs(fill = 'Gender')+ scale_fill_viridis_d(option = "A",begin = 0.8, end = 0.5)+
  ggtitle("US Collegiate Sports Budgets:\nComparing expenditure with participation by gender.")

