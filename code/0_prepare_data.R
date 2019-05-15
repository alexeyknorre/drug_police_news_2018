library(readr)
library(splitstackshape)
library(dplyr)
library(stringr)
library(tidyr)

df <- read_csv("data/data_30.06.2018.csv")

## Data cleaning

# There are some mistakes
df$articles[18] <- "300003; 1590003"
df$articles[19] <- "300003; 1590003"
df$articles[458] <- "2900003; 2860001"
df$articles[345] <- "2280001; 300001"
df$summary_crime[133] <- "Избиение потенциального наркопреступника"
df$summary_crime[134] <- "Подброс и хранение наркотиков"
df$summary_crime[135] <- "Фальсификация доказательств преступления"
df$summary_crime[136] <- "Фальсификация преступления"
df$drugs_seized_1[134] <- "амфетамин"
df$drugs_seized_weight_1[134] <- "5,5"
df$drugs_seized_2[134] <- "хлорфенилпиперазин"
df$drugs_seized_weight_2[134] <- "2,7"
df$drugs_seized_1[133] <- NA
df$drugs_seized_weight_1[133] <- NA
df$drugs_seized_2[133] <- NA

### Agencies ####
df$agency_crime[df$agency_crime %in% c("ОМВД","ОМОН")] <- "МВД"
df$agency_crime[df$agency_crime %in% c("УФСКН")] <- "ФСКН"

df$agency_catcher[df$agency_catcher %in% c("УСБ МВД","СБ МВД", "ОБНОН")] <- "МВД"
df$agency_catcher[df$agency_catcher %in% c("СКР")] <- "СК"
df$agency_catcher[df$agency_catcher %in% c("УФСКН")] <- "ФСКН"
df$agency_catcher[df$agency_catcher %in% c("УФСБ")] <- "ФСБ"

### Drugs ####
df$drugs_seized_1 <- gsub('"', '',df$drugs_seized_1)
df$drugs_seized_1 <- gsub('матамфетамин', 'метамфетамин',df$drugs_seized_1)
df$drugs_seized_1 <- gsub('героина', 'героин',df$drugs_seized_1)
df$drugs_seized_1 <- gsub('курительные смеси', 'курительная смесь',df$drugs_seized_1)
df$drugs_seized_2 <- gsub('"', '',df$drugs_seized_2)
df$drugs_seized_2 <- gsub('курительные смеси', 'курительная смесь',df$drugs_seized_2)
df$drugs_seized_1[df$drugs_seized_1 %in% c("марихуана","гашиш", "конопля","каннабис","мариухана",
                                           "каннабиноид", "каннабиноиды")] <- "каннабиноиды"
df$drugs_seized_2[df$drugs_seized_2 %in% c("марихуана","гашиш", "конопля","каннабис","мариухана",
                                           "каннабиноид", "каннабиноиды", "масло каннабиса")] <- "каннабиноиды"
df$drugs_seized_weight_1 <- as.numeric(format(df$drugs_seized_weight_1, decimal.mark=","))
df$drugs_seized_weight_2 <- as.numeric(format(df$drugs_seized_weight_2, decimal.mark=","))


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
# Counts publishers

publishers <- publishers %>% 
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

### Type of media ###

df_long_publishers <- unnest(df, publisher = strsplit(publisher, ";"))

df_long_publishers$publisher <- tolower(df_long_publishers$publisher)
df_long_publishers$agency_geo_city <- tolower(df_long_publishers$agency_geo_city)
df_long_publishers$agency_geo_region <- tolower(df_long_publishers$agency_geo_region)
df_long_publishers$publisher <- gsub(' (pdf-версия)', '', df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub('(московский выпуск, pdf)', '(Москва)', df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub('риа "росбизнесконсалтинг" казань и татарстан', 'рбк. татарстан (rt.rbc.ru)', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('риа "росбизнесконсалтинг" нижний новгород', 'рбк. нижний новгород (nn.rbc.ru)', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('риа "росбизнесконсалтинг" новосибирск и сибирь', 'рбк. новосибирск (nsk.rbc.ru)', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('известия (московский выпуск)', 'известия', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('вечерка (томск)', 'вечерка thebest (томск)', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('московский комсомолец', 'мк', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('коммерсант', 'коммерсантъ', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('rbc news', 'рбк. rbc news', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('"', '', df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub('<', '', df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub('>', '', df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub("г.", "", df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub("(", "", df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub(")", "", df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub(".", "", df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub("ъъ", "ъ", df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub("-", " ", df_long_publishers$publisher, fixed = TRUE)


allsources <- read_delim("data/allsources.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
allsources$name <- tolower(allsources$name)
allsources$name <- gsub(" (pdf-версия)", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(" (pdf версия)", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(" (архив)", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(" архив", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(" текст", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(" приложения", "", allsources$name, fixed = TRUE)
allsources$name <- gsub("г.", "", allsources$name, fixed = TRUE)
allsources$name <- gsub("(", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(")", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(".", "", allsources$name, fixed = TRUE)
allsources$name <- gsub("-", " ", allsources$name, fixed = TRUE)

df_long_publishers$type <- NA

for(i in 1:nrow(allsources)){
  df_long_publishers$type[str_detect(df_long_publishers$publisher,allsources$name[i])] <- allsources$type[i]
}
rm(i)
for(i in 1:nrow(df_long_publishers)){
  df_long_publishers$type[str_detect(df_long_publishers$publisher,df_long_publishers$agency_geo_city[i])] <- "region"
}
rm(i)
for(i in 1:nrow(df_long_publishers)){
  df_long_publishers$type[str_detect(df_long_publishers$publisher,df_long_publishers$agency_geo_region[i])] <- "region"
}

df_long_publishers$type[str_detect(df_long_publishers$publisher,"ru")] <- "region int"

# There are 6 types of media: federal print ("fed"), regional print ("region"), federal Internet ("fed int"), regional Internet ("region int"), federal archive ("fed arch") and regional archive ("region arch").
# Archive types refer to the media that is no longer publishing.

df_long_publishers$type[df_long_publishers$publisher == " северо запад санкт петербург"] <- "region arch"
df_long_publishers$type[df_long_publishers$publisher == "своими именами"] <- "fed arch"
df_long_publishers$type[df_long_publishers$publisher == "северо запад санкт петербург"] <- "region arch"
df_long_publishers$type[df_long_publishers$publisher == " экстра реклама чита"] <- "region arch"
df_long_publishers$type[df_long_publishers$publisher == "аиф   дон"] <- "region"
df_long_publishers$type[df_long_publishers$publisher == "аиф на оби"] <- "region"
df_long_publishers$type[df_long_publishers$publisher == "невское время"] <- "region arch"
df_long_publishers$type[df_long_publishers$publisher == "советская россия"] <- "fed"
df_long_publishers$type[df_long_publishers$publisher == "российская газета"] <- "fed"
df_long_publishers$type[df_long_publishers$publisher == "кабардино балкарская правда"] <- "region"
df_long_publishers$type[df_long_publishers$publisher == "российская газета неделя волга урал"] <- "region"
df_long_publishers$type[df_long_publishers$publisher == "столица плюс грозный"] <- "region"
df_long_publishers$type[df_long_publishers$publisher == "новая новгородская газета"] <- "region"
df_long_publishers$type[df_long_publishers$publisher == "комсомольская правда mskkpru"] <- "fed int"

df_long_publishers$fed_arch <- 0
df_long_publishers$fed <- 0
df_long_publishers$fed_int <- 0
df_long_publishers$region <- 0
df_long_publishers$region_arch <- 0
df_long_publishers$fed_arch[df_long_publishers$type == "fed arch"] <- 1
df_long_publishers$fed[df_long_publishers$type == "fed"] <- 1
df_long_publishers$fed_int[df_long_publishers$type == "fed int"] <- 1
df_long_publishers$region[df_long_publishers$type == "region"] <- 1
df_long_publishers$region_arch[df_long_publishers$type == "region arch"] <- 1
types <- select(df_long_publishers, N, fed, fed_int, fed_arch, region, region_arch)
types <- aggregate(. ~ N, types, sum)
df <- merge(df, types)

# AK: good practice is to delete unnecessary objects from memory:
rm(list = c("types","allsources", "i"))

### Articles from Criminal Code ####

df$articles <- gsub(',', ';', df$articles)
df_long_articles <- unnest(df, articles = strsplit(articles, ";"))
df_long_articles$articles <- gsub('\n| ', '', df_long_articles$articles)
df_long_articles$articles_short <- substr(df_long_articles$articles, 1,nchar(df_long_articles$articles)-4)


### Correcting more coding mistakes

df$other_crimes[is.na(df$other_crimes)] <- 0
df$is_detention[is.na(df$is_detention)] <- 0
df$is_trial[is.na(df$is_trial)] <- 0
l <- c(279,253,255,256,263,403, 404, 525:527)
for(i in 1:length(l)){
  df$is_detention[l[i]] <- 1
  df$is_trial[l[i]] <- 1
}
df$is_trial[527] <- 0

for(i in 1:nrow(df)){
  if(is.na(df$sanction[i]) == TRUE){
    if(df$is_detention[i] == 1){
      df$sanction <- "задержан"
    }else{
      print(i)
    }
  }
}
for(i in 1:nrow(df)){
  if(df$sanction[i] == "задержан"){
    df$is_detention <- 1
  }else{
    print(i)
  }
}
rm(list = c("l","i"))

#### function for deleting NAs of exact columns in dataframe
completeFun <- function(data, desiredCols){
  completeVec <- complete.cases(data[,desiredCols])
  return(data[completeVec,])
}

df$position[403] <- "участковый"
df$position[404] <- "участковый"
df$position <- gsub("заеститель","заместитель", df$position)

df$agency_crime[422] <- "МВД"
for(i in 17:19){
  df$agency_geo_region[i] <- "Москва"
}
rm(i)

df$summary_crime[521] <- "сбыт контрабанда"
df$summary_crime[526] <- "мошенничество транспортировка приобретение"
df$summary_crime[527] <- "мошенничество транспортировка приобретение"

### Creating dataframe with information about agencies

catcher_long <- select(df, N, agency_crime, agency_catcher)
catcher_long$agency_catcher <- gsub("[[:punct:]]"," ", catcher_long$agency_catcher)
library(tidytext)
catcher_long <- catcher_long %>%
  unnest_tokens(word, agency_catcher) # делим текст на токены
catcher_long <- dplyr::filter(catcher_long, word != "усб" & word != "гу" & word != "орч" & word != "сб" & word != "сбу" & word != "ур" & word != "гибдд" & word != "дпс" & word != "осб" & word != "cб" & word != "обнон")
catcher_long$word <- gsub("омон","мвд", catcher_long$word)
colnames(catcher_long)[3] <- "agency_catcher"

df1 <- select(df, N, is_same_agency_catcher)
df1 = df1 %>% filter(N %in% catcher_long$N)
df1 <- merge(df1, catcher_long)
df1$agency_crime <- tolower(df1$agency_crime)
df1 <- filter(df1, agency_crime == agency_catcher)
l <- df1$N
for(i in 1:length(l)){
  df$is_same_agency_catcher[df$N == l[i]] <- 1
}
rm(list = "i", "l", "df1")

### Creating dataframe with coded crime categories

crime_long <- dplyr::select(df, N, summary_crime)

crime_long$summary_crime <- tolower(crime_long$summary_crime)
crime_long$summary_crime <- gsub("[[:punct:]]"," ", crime_long$summary_crime)

library(tidytext)
crime_long <- crime_long %>%
  unnest_tokens(word, summary_crime) # делим текст на токены

library(qdap)
crime_long$word <- Trim(clean(crime_long$word)) # удаляем лишние пробелы
for(i in 1:length(crime_long)){ 
  crime_long$crime[str_detect(crime_long$word, "подброс|подкид|подбрас")] <- 1 
  crime_long$crime[str_detect(crime_long$word, "вымогат|взят|деньг|денеж|угрож|угроз")] <- 2 
  crime_long$crime[str_detect(crime_long$word, "вещдок|веществен")] <- 3 
  crime_long$crime[str_detect(crime_long$word, "подлог|подстав|фальсиф|показат|фабр|мошен|результат|признан")] <- 4 
  crime_long$crime[str_detect(crime_long$word, "пронес|пронос|СИЗО|заключ|колон")] <- 5 
  crime_long$crime[str_detect(crime_long$word, "транспорт|контраб|перевоз|распр|покуп|приобр|хран|личного|опьян|торговл|сбыт|оборот|продаж")] <- 6 
  crime_long$crime[str_detect(crime_long$word, "крыш|покров")] <- 7 
} 
crime_long <- completeFun(crime_long, 3)
crime_long <- crime_long[,-2]
crime_long <- unique(crime_long)

### Creating dataframe with coded position categories

position_long <- select(df, N, position)
for(i in 1:length(position_long)){ 
  position_long$positions[str_detect(position_long$position, "участк|дежурн|стаж|води|полиц")] <- 1 
  position_long$positions[str_detect(position_long$position, "оперу")] <- 2
  position_long$positions[str_detect(position_long$position, "старший оперу")] <- 3
  position_long$positions[str_detect(position_long$position, "следов|инспект")] <- 4 
  position_long$positions[str_detect(position_long$position, "начальн|команд")] <- 5 
}
position_long <- position_long[,-2]
position_long <- na.omit(position_long)
position_long <- unique(position_long)

### Creating dataframe with all seized drugs

drugs_long <- dplyr::select(df, N, drugs_seized_1, drugs_seized_2, drugs_seized_3)
drugs_long <-  drugs_long %>% 
  dplyr::group_by(N) %>% 
  dplyr::summarise(drugs = paste(drugs_seized_1, drugs_seized_2, drugs_seized_3, collapse =" "))

drugs_long$drugs <- gsub("и еще 12 видов наркотиков","другое", drugs_long$drugs)
drugs_long$drugs <- gsub("вещество растительного происхождения","другое", drugs_long$drugs)
drugs_long$drugs <- gsub("синтетическое вещество","другое", drugs_long$drugs)
drugs_long$drugs <- gsub("порошкообразное вещество","другое", drugs_long$drugs)
drugs_long$drugs <- gsub("курительная смесь","спайсы", drugs_long$drugs)
drugs_long$drugs <- gsub("психотропное вещество","другое", drugs_long$drugs)
drugs_long$drugs <- gsub("миксы","спайсы", drugs_long$drugs)
drugs_long$drugs <- gsub("афетамин","амфетамин", drugs_long$drugs)
library(tidytext)
drugs_long <- drugs_long %>%
  unnest_tokens(word, drugs) # делим текст на токены

library(qdap)
drugs_long$word <- Trim(clean(drugs_long$word)) # удаляем лишние пробелы
drugs_long <- dplyr::filter(drugs_long, word != "na")
drugs_long <- unique(drugs_long)
colnames(drugs_long)[2] <- "drugs"

### Creating dataframe dor CHAID analysis

drglng <- drugs_long
drglng$drugs <- as.numeric(as.factor(drglng$drugs))
crime_long1 = crime_long %>% filter(N %in% drugs_long$N)
chaid_drugs <- merge(crime_long1, drglng)
rm(list = "drglng", "crime_long1")

#### Drug type dummies

chaid_drugs$drugs <- as.factor(chaid_drugs$drugs)
futureVariables <- levels(chaid_drugs$drugs)
futureVariables <- purrr::map(futureVariables, function(x) {stringr::str_c("drugs", as.character(x))})

futVar <- c()
for (i in 1:length(futureVariables)){
  futVar <- c(futVar, futureVariables[[i]])
}

for (var in 1:length(futVar)){
  for (str in 1:nrow(chaid_drugs)){
    chaid_drugs[str, futVar[var]] <- ifelse(as.character(stringr::str_replace_all(futVar[var], "drugs", "")) == as.character(chaid_drugs$drugs[str]), 1, 0) 
  }
  print(stringr::str_c("Обработалась ", var, " переменная из ", length(futVar)))
}

#### Crime type dummies

chaid_drugs$crime <- as.factor(chaid_drugs$crime)
futureVariables <- levels(chaid_drugs$crime)
futureVariables <- purrr::map(futureVariables, function(x) {stringr::str_c("crime", as.character(x))})

futVar <- c()
for (i in 1:length(futureVariables)){
  futVar <- c(futVar, futureVariables[[i]])
}

for (var in 1:length(futVar)){
  for (str in 1:nrow(chaid_drugs)){
    chaid_drugs[str, futVar[var]] <- ifelse(as.character(stringr::str_replace_all(futVar[var], "crime", "")) == as.character(chaid_drugs$crime[str]), 1, 0) 
  }
  print(stringr::str_c("Обработалась ", var, " переменная из ", length(futVar)))
}

rm(list = "futureVariables", "futVar", "i", "str", "var")

# Export data
save(df_long_articles, df, publishers, df_long_publishers, catcher_long, crime_long, position_long, drugs_long, chaid_drugs, file = "data/data_clean.RData")
#write.csv2(df, "data/data_clean.csv", row.names = F)