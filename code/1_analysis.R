library(readr)

df <- read_csv("data/data_clean.csv")


# Week/season distribution of news publications
ggplot(df, aes(x = yday_publication_first))+
  geom_histogram(bins = 52) +
  geom_vline(xintercept = c(60,152,244,335))+
  theme_minimal() +
  scale_x_continuous(name="Сезон публикации новости",breaks = c(31,106,198,290,351), labels = c("Зима","Весна","Лето","Осень","Зима"))

table(df$season_publication_first)