#I. feladat

## Házi feladat 4
## Programozás I. (SST111)
## 2016/17. II. félév
## 2017.05.01.

# II. feladatsor.
#2.1 feladat: "twwets"-ként behívni a .csv fájlt.
tweets <-
  read.table(file = "data/clinton_trump_tweets.csv", sep = ";", header = T)

#2.2 feladat: Gyakoriság és ábrázolás

#Gyakoriság lekérése
summary(tweets$handle)

#Ábrázolás (Itt először beleraktam/megváltozattam mindent,
#majd kivettem,ami nem kellett.)

library(ggplot2)

ggplot(data = tweets, aes(handle, fill = handle)) +
  geom_bar(stat = "count") +
  labs(title = "Cantidate Tweets", y = "Tweet frequency") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(
    values = c("red", "blue"),
    name = "Candidate",
    breaks = c("HillaryClinton", "realDonaldTrump"),
    labels = c("Hillary Clinton", "Donald Trump")
  ) +
  theme(panel.background = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_blank())

# Exportálás.
ggsave("/Users/Adam/homework-04/fig/tweet1.png")


#2.3 feladat: Nyelvek és ábrázolás

#Milyen nyelveket használt a két jelölt?
table(tweets$lang, tweets$handle)

#Nézd meg, hogy a nem angol szövegek valóban nem angolok-e?
whatslangauge <- subset(tweets, lang != "en")
whatslangauge <- subset(whatslangauge, lang != "es")
print(whatslangauge$text)

#Szóval látszik, hogy a spanyolokon kívül minden másik nyelv valójában.
# Átírom.

for (i in 1:nrow(tweets)) {
  if (tweets$lang[i] == "da")
    tweets$lang[i] <- "en"
  else if (tweets$lang[i] == "et")
    tweets$lang[i] <- "en"
  else if (tweets$lang[i] == "fi")
    tweets$lang[i] <- "en"
  else if (tweets$lang[i] == "fr")
    tweets$lang[i] <- "en"
  else if (tweets$lang[i] == "tl")
    tweets$lang[i] <- "en"
}


#Megnézem, hogy megcsinálta-e.
table(tweets$lang, tweets$handle)

#Ábrázolás
ggplot(data = tweets, aes(handle, fill = lang)) +
  geom_bar(stat = "count", position = position_dodge()) +
  labs(title = "Langauge of Tweets", y = "Tweet frequency") +
  theme(plot.title = element_text(hjust = 0.4)) +
  scale_fill_manual(
    values = c("darkgrey", "cornflowerblue"),
    name = "Langauge",
    breaks = c("en", "es"),
    labels = c("English", "Spanish")
  ) +
  scale_x_discrete(
    breaks = c("HillaryClinton", "realDonaldTrump"),
    labels = c("Hillary Clinton", "Donald Trump")
  ) +
  theme(panel.background = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank())

ggsave("/Users/Adam/homework-04/fig/tweet2.png")

#Kitörlöm, ami nem kell
rm(whatslangauge)
rm(i)



#2.4 feladat: Függvény
source("src/homework-04-functions.R")

what_about_tweets("Hillary Clinton", 10)
what_about_tweets("Donald Trump", 15)

## Ellenőrzések:
what_about_tweets("Hillary Clinton", 10031)
what_about_tweets("Pinokkio", 11)


