### CHART CHALLENGE

# DAY 1 - Part-to-whole

rm(list = ls())

# Load libraries
library(openxlsx)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

# Load data
elections <- read.xlsx("data-raw/Gemeente uitslagen tweede kamer verkiezing 2021.xlsx")
covid<- read.csv("data-raw/COVID-19_aantallen_gemeente_per_dag.csv", sep=";")
popsize <- read.xlsx("data-raw/Regionale_kerncijfers_Nederland_01042022_140107.xlsx")


# Create percentage SGP voters for each municipality
elections <- elections %>% 
  select(RegioCode, GeldigeStemmen, "Staatkundig.Gereformeerde.Partij.(SGP)") %>% 
  rename(SGP="Staatkundig.Gereformeerde.Partij.(SGP)") %>%
  mutate(perc_sgp=SGP/GeldigeStemmen*100)


# Create monthly COVID incidents for each municipality
# I need: Municipality_code, Total_reported, Data_of_publication
covid <- covid %>% 
  mutate(monthyear=str_sub(Date_of_publication, start=1, end=7)) %>%
  group_by(monthyear, Municipality_code) %>%
  summarise(n_inf=sum(Total_reported))

# remove first months due to lack of testing capacity
covid <- covid %>% filter(monthyear!="2020-02", monthyear!="2020-03",
                          monthyear!="2020-04", monthyear!="2020-05",
                          monthyear!="2020-06")

# Now I can make percentages of Covid infections on the total population
covid <- covid %>% 
  left_join(popsize, by=c("Municipality_code"="code")) %>%
  filter(!is.na(aantal)) %>%
  mutate(perc_inf=n_inf/aantal*100)


# Now I only need to merge elections and covid together
covid <- covid %>% 
  mutate(id=str_sub(Municipality_code, start=3, end=6)) %>%
  select(id, perc_inf, monthyear)

elections <- elections %>% mutate(id=str_sub(RegioCode, start=2, end=5)) %>% select(id, perc_sgp)

# create final data
chart_data <- covid %>% 
  left_join(elections)


## Create gem percentage of infections over all municipalities 
chart_data2 <- chart_data %>%
  group_by(monthyear) %>% 
  summarise(covid_per_mean=mean(perc_inf),
            covid_per_25=quantile(perc_inf, 0.25),
            covid_per_75=quantile(perc_inf, 0.75)) 

chart_data2$id <- "overall"

chart_data4 <- chart_data2 %>% 
  pivot_longer(covid_per_mean:covid_per_75,names_to="value", values_to="percentage")

# And I want to make this graph for the municipality with the highest percentage of SGP voters
#chart_data3 <- chart_data %>% group_by(id) %>% summarise(SGP=first(perc_sgp))
#max(chart_data3$perc_sgp)
# municipalities: 0184 (Urk), 0703 (Reimerswaal), 0180 (Staphorst)
chart_data3 <- chart_data %>% 
  mutate(id=as.character(id)) %>% 
  filter(id=="0184"|id=="0703"|id=="0180")

# create one dataframe
# I need: id, monthyear percentage and value
chart_data3 <- chart_data3 %>% mutate(value=if_else(id=="0184", "Urk", ""),
                                      value=if_else(id=="0703", "Reimerswaal", value),
                                      value=if_else(id=="0180", "Staphorst", value),
                                      percentage=perc_inf) %>% select(id, monthyear, value, percentage)

chart_data_total <- chart_data4 %>% rbind(chart_data3)

chart_data_total %>%
  ggplot(aes(x = monthyear, y = percentage, colour = value, group = value)) +
  geom_point() +
  geom_line()

subset_mean <- chart_data_total[1:63,]
  
subset_mean <- subset_mean %>%
  pivot_wider(names_from = value,
              values_from = percentage) %>%
  rename(percentage = covid_per_mean,
         min = covid_per_25,
         max = covid_per_75) %>%
  mutate(value = "covid_overall")

subset_mean

subset2 <-  chart_data_total[64:126,] %>%
  mutate(min = NA, 
         max = NA)

data <- rbind(subset2, subset_mean)

data <- data %>%
  filter((value %in% c("Urk", "covid_overall")) &
           monthyear != "2022-03") %>%
  mutate(monthyear1=str_sub(monthyear, start=3, end=7),
         monthyear1=paste0("'", monthyear1)) 

plot <- data %>% ggplot(aes(x = monthyear1, y = percentage, group = value)) +
  geom_line(aes(colour = value)) +
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.4,  fill = "#A63737") +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab("Date (year-month)") +
  ylab("% Positive covid tests") +
  ggtitle("Percentages of Positive Covid Tests in the Netherlands: \nAll municipalities versus Urk") +
  labs(caption = "@JetWildeman \n @ThijmenJeroense") +
  theme(axis.text.x = element_text(angle = 45),
        axis.text = element_text(colour = "#E0E0E0"),
        axis.title = element_text(colour = "#E0E0E0"),
        plot.title = element_text(hjust = 0.5,
                                  colour = "#E0E0E0", 
                                  size = 18),
        plot.caption = element_text(colour = "#E0E0E0"),
        panel.background = element_rect(colour = "#202020",
                                        fill = "#202020"),
        panel.grid = element_line(colour = "#000000"),
        plot.background = element_rect(colour = "#202020",
                                       fill = "#202020"),
        legend.background = element_rect(colour = "#202020",
                                         fill = "#202020"),
        legend.position = "bottom",
        legend.key = element_rect(colour = "#202020",
                                  fill = "#202020"),
        legend.text = element_text(colour = "#E0E0E0"),
        legend.title = element_text(colour = "#E0E0E0"))


plot_day_1 <- plot + scale_color_discrete(name="Municipalities",
                         breaks=c("covid_overall", "Urk"),
                         labels=c("All", "Urk"))

ggsave(plot = plot_day_1, file = "plots/day_1/plot_day_1.png", height = 5, width = 7, dpi = 320)



  
