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

library(stringr)
publishers$publisher[str_detect(publishers$publisher, "Вечерний Ставрополь")] <- "Вечерний Ставрополь"
publishers$publisher[str_detect(publishers$publisher, "Йошкар-Ола")] <- "Йошкар-Ола"
publishers$publisher[str_detect(publishers$publisher, "Невское время")] <- "Невское время"
publishers$publisher[str_detect(publishers$publisher, "ИНТЕР|Интер")] <- "Интер"
publishers$publisher[str_detect(publishers$publisher, "Слобода")] <- "Слобода"
publishers$publisher[str_detect(publishers$publisher, "Якутск вечерний|Якутск Вечерний")] <- "Якутск вечерний"
publishers$publisher[str_detect(publishers$publisher, "Трибуна")] <- "Трибуна"
publishers$publisher[str_detect(publishers$publisher, "Советская Молодежь|Советская молодежь")] <- "Советская молодежь"
publishers$publisher[str_detect(publishers$publisher, "Новая новгородская газета")] <- "Новая новгородская газета"
publishers$publisher[str_detect(publishers$publisher, "Нижегородские новости")] <- "Нижегородские новости"
publishers$publisher[str_detect(publishers$publisher, "Наше Время|Наше время")] <- "Наше время"
publishers$publisher[str_detect(publishers$publisher, "Молодой Коммунар|Молодой коммунар")] <- "Молодой коммунар"
publishers$publisher[str_detect(publishers$publisher, "Красный Север|Красный север")] <- "Красный север"
publishers$publisher[str_detect(publishers$publisher, "Краснодарские известия")] <- "Краснодарские известия"
publishers$publisher[str_detect(publishers$publisher, "Кабардино-Балкарская правда|Кабардино-балкарская правда")] <- "Кабардино-Балкарская правда"
publishers$publisher[str_detect(publishers$publisher, "Аресеньевские вести")] <- "Арсеньевские вести"
publishers$publisher[str_detect(publishers$publisher, "Вечерняя Казань")] <- "Вечерняя Казань"
publishers$publisher[str_detect(publishers$publisher, "Вятский наблюдатель")] <- "Вятский наблюдатель"
publishers$publisher[publishers$publisher == "Известия "] <- "Известия"
publishers$publisher[publishers$publisher == "Комсомольская правда "] <- "Комсомольская правда"
publishers$publisher[publishers$publisher == "Арсеньевские вести "] <- "Арсеньевские вести"

### ###
# Check publishers_count to see what should be cleaned

publishers_count <- publishers %>% 
  group_by(publisher) %>% 
  summarise(count = sum(count)) %>% 
  arrange(desc(count))

### Sanctions dummies

df$sanction[285] <- "уголовное дело"
df$sanction[286] <- "уголовное дело"
df$is_fired <- 0
df$is_wanted <- 0
df$is_case <- 0
df$is_seizure <- 0
df$is_justification <- 0
df$is_house_arrest <- 0
df$is_fired[str_detect(df$sanction, "увол|отстр")] <- 1
df$is_wanted[str_detect(df$sanction, "розыск")] <- 1
df$is_case[str_detect(df$sanction, "угол")] <- 1
df$is_seizure[str_detect(df$sanction, "пров|обыск")] <- 1
df$is_justification[str_detect(df$sanction, "оправд")] <- 1
df$is_house_arrest[str_detect(df$sanction, "подп")] <- 1
df$is_detention[str_detect(df$sanction, "страж|задер|арест")] <- 1

### Articles from Criminal Code ####
# TODO

# Export data
write.csv2(df, "data/data_clean.csv",row.names = F)