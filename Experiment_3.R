#load packages

library(tidyverse)

#first, read in the data

settlement_counts_raw <- read_csv("settlement_data.csv")

#create a summary table for the average values

summary_settlement <- settlement_counts_raw %>% 
  select(-species) %>%
  group_by(tide_zone, substrate_type) %>%
  summarize(count_avg = mean(count),
            count_sd = sd(count),
            count_se = count_sd/sqrt(6))

summary_settlement

#create a graph for the average settlement as a function of tidal zone and substrate type

ggplot(summary_settlement, aes(x = tide_zone, y = count_avg, fill = substrate_type)) +
  geom_col(width = 0.5, position = "dodge") +
  geom_errorbar(aes(ymin = count_avg - count_se,
                    ymax = count_avg + count_se),
                position = position_dodge(0.5),
                width = 0.25) +
  labs(x = "Tide Zone",
       y = "Mean Settlement Count (+/- SE)") +
         scale_y_continuous(limits = c(0,6000), expand = c(0,0)) +
  theme_classic()

#run a two factor ANOVA to compare the average settlement as a function of tidal zone and substrate type

recruit_anova <- aov(count ~ tide_zone*substrate_type, data = settlement_counts_raw)

summary(recruit_anova)


#Percent Cover-------------------------- 

#read in data frame

percent_cover_raw <- read_csv("intertidal_percent_cover.csv")


# look at all of the species names

unique(percent_cover_raw$species)

# Let's sort the species by their functional groups

functional_groups <- percent_cover_raw %>%
  mutate(functional_group = case_when(species == "sand" ~ "Substrate",
                                      species == "bare" ~ "Substrate",
                                      species == "Ulva_spp" ~ "Algae",
                                      species == "Corallina_vancouveriensis" ~ "Algae",
                                      species == "Neorhodomela" ~ "Algae",
                                      TRUE ~ "Invertebrates"))

# Make a summary table for percent cover of functional groups at each tidal zone

summary_percent_cover <- functional_groups %>% 
  select(-species) %>%
  group_by(tidal_zone, functional_group) %>%
  summarize(perc_cover_avg = mean(percent_cover),
            perc_cover_sd = sd(percent_cover),
            perc_cover_se = perc_cover_sd/sqrt(25))

# Make a graph of functional cover by tidal zone

ggplot(summary_percent_cover, aes(x = functional_group, y = perc_cover_avg, fill = tidal_zone)) +
  geom_col(width = 0.5, position = "dodge") +
  geom_errorbar(aes(ymin = perc_cover_avg - perc_cover_se,
                    ymax = perc_cover_avg + perc_cover_se),
                position = position_dodge(0.5),
                width = 0.25) +
  labs(x = "Functional Group",
       y = "Percent Cover (+/- SE)") +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  theme_classic()

ggplot(summary_percent_cover, aes(x = tidal_zone, y = perc_cover_avg, fill = functional_group)) +
  geom_col(width = 0.5, position = "dodge") +
  geom_errorbar(aes(ymin = perc_cover_avg - perc_cover_se,
                    ymax = perc_cover_avg + perc_cover_se),
                position = position_dodge(0.5),
                width = 0.25) +
  labs(x = "Tidal Zone",
       y = "Percent Cover (+/- SE)") +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  theme_classic()



#Choose your own adventure! Take any of the functional groups and create a graph that breaks them into their species, separated by tidal zone

