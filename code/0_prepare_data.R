library(ggplot2)
library(readr)
library(splitstackshape)
library(dplyr)

df <- read_csv("data/data_30.06.2018.csv")

source("C:/Users/Alexey/Dropbox/R/table_output/table_output.R")

## Data cleaning

### Agencies ####
df$agency_crime[df$agency_crime %in% c("ОМВД","ОМОН")] <- "МВД"
df$agency_crime[df$agency_crime %in% c("УФСКН")] <- "ФСКН"

df$agency_catcher[df$agency_catcher %in% c("УСБ МВД","СБ МВД", "ОБНОН")] <- "МВД"
df$agency_catcher[df$agency_catcher %in% c("СКР")] <- "СК"
df$agency_catcher[df$agency_catcher %in% c("УФСКН")] <- "ФСКН"
df$agency_catcher[df$agency_catcher %in% c("УФСБ")] <- "ФСБ"

### Drugs ####
df$drugs_seized_1[df$drugs_seized_1 %in% c("марихуана","гашиш", "конопля","каннабис","мариухана",
                                           "каннабиноид", "каннабиноиды")] <- "каннабиоиды"

### Regions ####
df$agency_geo_region[df$agency_geo_region == "Кабардино-Балкарская республика"] <- "Кабардино-Балкарская Республика"
df$agency_geo_region[df$agency_geo_region == "Респбулика Башкортостан"] <- "Республика Башкортостан"
df$agency_geo_region[df$agency_geo_region %in% c("Респубика Дагестан","Респбулика Дагестан")] <- "Республика Дагестан"
df$agency_geo_region[df$agency_geo_region == "Удмурстская Республика"] <- "Удмуртская Республика"
df$agency_geo_region[df$agency_geo_region == "Томбовская область"] <- "Тамбовская область"

### Dates ####
df$date_publication_first <- substr(df$date_publication,1,10)
df$year_publication_first <- format(as.Date(df$date_publication_first, format="%d.%m.%Y"),"%Y")
df$month_publication_first <- format(as.Date(df$date_publication_first, format="%d.%m.%Y"),"%m")
df$yday_publication_first <- as.numeric(strftime(as.Date(df$date_publication_first, format="%d.%m.%Y"), format = "%j"))
df$season_publication_first <- cut(df$yday_publication_first, breaks = c(0, 60, 152, 244, 335, 366),
                                   labels = c("Зима", "Весна", "Лето", "Осень", "Зима"),
                                   include.lowest = TRUE)

### Publishers ####

publishers <- cSplit_e(df, "publisher", sep = ";", type = "character", fill = 0, drop = TRUE) %>% 
  select(starts_with("publisher")) %>% 
  colSums(.) %>% 
  as.data.frame(.)

publishers$publisher <- substr(rownames(publishers),11,999)
rownames(publishers) <- NULL
publishers <- publishers[c(2,1)]
names(publishers) <- c("publisher","count")



# Remove leading/trailing whitespaces
publishers$publisher <- trimws(publishers$publisher)

# Remove ",<,>
publishers$publisher <- gsub('"|<|>', '', publishers$publisher)

# Remove round brackets and everything inside
publishers$publisher <- gsub("\\([^()]*\\)", "", publishers$publisher)

# Be careful - these are powerful regexes
###TODO Clean some more publishers here, please ###

publishers$publisher <- gsub("АиФ.+", "АиФ", publishers$publisher)

publishers$publisher[grepl("Коммерсант",publishers$publisher)] <- "Коммерсант"
publishers$publisher[grepl("РБК|RBC|РосБизнесКонсалтинг",publishers$publisher)] <- "РБК"
publishers$publisher[grepl("Московский комсомолец|Московский Комсомолец|МК", publishers$publisher)] <- "МК"

### ###
# Check publishers_count to see what should be cleaned

publishers_count <- publishers %>% 
  group_by(publisher) %>% 
  summarise(count = sum(count)) %>% 
  arrange(desc(count))

### Articles from Criminal Code ####
# TODO

# Export data
write.csv2(df, "data/data_clean.csv",row.names = F)