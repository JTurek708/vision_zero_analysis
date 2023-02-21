library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
library(formattable)
library(reshape2)
library(bbplot)
library(janitor)

# Combine all dataframes into one
satx_crash_df_new <- do.call("rbind", list(san_antonio_crash_data_2013, san_antonio_crash_data_2014, san_antonio_crash_data_2015_pt1,
                                       san_antonio_crash_data_2015_pt2, san_antonio_crash_data_2016_pt1, san_antonio_crash_data_2016_pt2,
                                       san_antonio_crash_data_2016_pt3, san_antonio_crash_data_2017_pt1, san_antonio_crash_data_2017_pt2,
                                       san_antonio_crash_data_2018_pt1, san_antonio_crash_data_2018_pt2, san_antonio_crash_data_2019_pt1,
                                       san_antonio_crash_data_2019_pt2, san_antonio_crash_data_2020, san_antonio_crash_data_2021,
                                       san_antonio_crash_data_2022))
head(satx_crash_df_new)
nrow(satx_crash_df_new)
View(satx_crash_df_new)
satx_crash_df %>% get_dupes(`Crash ID`) %>% tally()
df3 <- satx_crash_df_new %>%
  filter(`Crash ID` == 13051989)
df3 <- df3 %>%
  select(`Crash ID`, `Crash Date`, `Crash Non-Suspected Serious Injury Count`,
         `Crash Not Injured Count`, `Crash Total Injury Count`, `Person Total Injury Count`,
         `Person Type`)

View(df3)
df4 <- satx_crash_df_new %>%
  filter(`Crash ID` == 13051703)
View(df4)

df2 <- satx_crash_df[!duplicated(satx_crash_df), ]
nrow(df2)
df2 %>% get_dupes(`Crash ID`) %>%tally()
View(df2)
View(san_antonio_crash_data_2013)

satx_crash_df %>%
  group_by(`Person Type`) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

pedestrian_crashes <- satx_crash_df %>%
  filter(`Person Type` == "4 - PEDESTRIAN")
View(pedestrian_crashes)
pedestrian_crashes %>%
  group_by(`Person Death Count`) %>%
  summarize(total_deaths = n())

cyclist_crashes <- satx_crash_df %>%
  filter(`Person Type` == "3 - PEDALCYCLIST")
View(cyclist_crashes)
cyclist_crashes %>%
  group_by(`Person Death Count`) %>%
  summarize(n=n())

# Convert character date col to date type
cyclist_crashes$`Crash Date` <- mdy(cyclist_crashes$`Crash Date`)
pedestrian_crashes$`Crash Date` <- mdy(pedestrian_crashes$`Crash Date`)
satx_crash_df_new$`Crash Date` <- mdy(satx_crash_df_new$`Crash Date`)

# Parse date column to year
cyclist_crashes$Crash_Year <- format(cyclist_crashes$`Crash Date`, format = "%Y")
pedestrian_crashes$Crash_Year <- format(pedestrian_crashes$`Crash Date`, format = "%Y")
satx_crash_df_new$Crash_Year <- format(satx_crash_df_new$`Crash Date`, format = "%Y")

# Plot injury types over years
grouped_cyclist <- cyclist_crashes %>%
  select(Crash_Year, `Crash Severity`) %>%
  group_by(Crash_Year, `Crash Severity`) %>%
  summarise(n = n())
View(grouped_cyclist)

cyclist_crashes <- ggplot(grouped_cyclist, aes(x=Crash_Year, y = n, fill = `Crash Severity`)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = n)) +
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold",angle = 90)) +
  labs(x = "Crash Year", y = "Count", title = "Number of Car-Cyclist Involved Crashes",
       subtitle = "San Antonio, Texas, 2013 - 2022", caption = "Source: TxDOT Crash Query Tool")
cyclist_crashes

grouped_cyclist <- grouped_cyclist %>%
  pivot_wider(
    names_from = Crash_Year,
    values_from = n
  )

grouped_cyclist <- grouped_cyclist %>%
  adorn_totals("col")
cyclist_table <- formattable(grouped_cyclist,
            align = c("l", "c","c","c","c","c","c","c","c","c","c","r"))
cyclist_table
