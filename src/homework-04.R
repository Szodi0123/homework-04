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

#III. feladatsor.
#3.1 feladat: Ábrák elkészítése
require("fivethirtyeight")
data("hiphop_cand_lyrics")


#A feladatmegoldások során több package-t is használok. Valószínűleg van,
#amit feleslegesen.

library(reshape2)
library(data.table)

#Átalakítom az adataimat
abra_1 <-
  melt(
    hiphop_cand_lyrics,
    id.vars = c("album_release_date", "candidate","sentiment"),
    measure.vars = "candidate"
  )

setDT(abra_1)[,freq := .N, by = abra_1$candidate]


#Ide helytakarékosság miatt elmentettem a neveket/vez.neveket/színkódokat.
teljes_nevek <- c("Ben Carson",
                  "Bernie Sanders",
                  "Chris Christie",
                  "Donald Trump",
                  "Hillary Clinton",
                  "Jeb Bush",
                  "Mike Huckabee",
                  "Ted Cruz")

upcase_vez_nevek <- c("CARSON",
                      "SANDERS",
                      "CHRISTIE",
                      "TRUMP",
                      "CLINTON",
                      "BUSH",
                      "HUCKABEE",
                      "CRUZ")

szinkodok <- c("#a7d65d",
               "#6ac1a5",
               "#fd8977",
               "#fdbf7b",
               "#96d7f8",
               "#fed844",
               "#fbcee5",
               "#e58cc2")

#Ábrázolás: Ennyit tudtam belőle kihozni, de így is nagyon sok idő volt. 
#Nem tudom, hogy a clintonos tweetek miért másznak egymásra. :'(. Plusz
#Itt a nyers gyakoriság van, bár gondolom ez a kisebbik baj.

ggplot(abra_1, aes(x = album_release_date, y = freq)) +
  geom_point(
    aes(colour = abra_1$candidate),
    stat = "identity",
    size = 3,
    position = position_stack(vjust = 0, reverse = T)
  ) +
  scale_colour_manual(
    name = "",
    breaks = (teljes_nevek),
    labels = (upcase_vez_nevek),
    values = (szinkodok)) +
  theme(legend.position = "top") + guides(colour = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y  = element_text()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key  = element_blank()) +
  theme(axis.ticks.y  = element_blank()) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "darkgrey"),
    panel.grid.minor = element_line(colour = "darkgrey"),
    panel.ontop = F
  ) +
  labs(title = "Every mention of 2016 primary candidates in hip-hop songs") +
  theme(plot.title = element_text(
    colour = "black",
    face = "bold",
    hjust = 0.5
  )) +
  scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015))

# Exportálás.
ggsave("/Users/Adam/homework-04/fig/hiphop1.png")


#2. ábra: ugyanez a sentimentekre.
positive <- subset(hiphop_cand_lyrics,sentiment == "positive")
negative <- subset(hiphop_cand_lyrics,sentiment == "negative")
neutral <- subset(hiphop_cand_lyrics,sentiment == "neutral")


#Ezeknél fontos, hogy a színeket nem tudom a vektorból megadni,
#mert nem mindenkinek van sentimentes tweetje. 


# Pozitív sentiment
positive<-
  melt(
    positive,
    id.vars = c("album_release_date", "candidate"),
    measure.vars = "candidate"
  )


setDT(positive)[,freq := .N, by = positive$candidate]

pos <- ggplot(positive, aes(x = album_release_date, y =freq)) +
  geom_point(
    aes(colour = positive$candidate),
    stat = "identity",
    size = 3,
    position = position_stack(vjust = 0, reverse = T)
  ) +
  scale_colour_manual(
    name = "",
    breaks = (teljes_nevek),
    labels = (upcase_vez_nevek),
    values = ( values = c(
      "Ben Carson" = "#a7d65d",
      "Bernie Sanders" = "#6ac1a5",
      "Chris Christie" = "#6ac1a5",
      "Donald Trump" = "#fdbf7b",
      "Hillary Clinton" = "#96d7f8",
      "Jeb Bush" = "#fed844",
      "Mike Huckabee" = "#fbcee5",
      "Ted Cruz" = "#e58cc2"))) +
  theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y  = element_text()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key  = element_blank()) +
  theme(axis.ticks.y  = element_blank()) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "darkgrey"),
    panel.grid.minor = element_line(colour = "darkgrey"),
    panel.ontop = F
  ) +
  labs(title = "Positive") +
  theme(plot.title = element_text(
    colour = "black",
    face = "bold",
    hjust = 0.5
  )) +
  scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015))

# Negatív sentiment
negative<-
  melt(
    negative,
    id.vars = c("album_release_date", "candidate"),
    measure.vars = "candidate"
  )

setDT(negative)[,freq := .N, by = negative$candidate]


neg <- ggplot(negative, aes(x = album_release_date, y =freq)) +
  geom_point(
    aes(colour = negative$candidate),
    stat = "identity",
    size = 3,
    position = position_stack(vjust = 0, reverse = T)
  ) +
  scale_colour_manual(
    name = "",
    breaks = (teljes_nevek),
    labels = (upcase_vez_nevek),
    values = ( values = c(
      "Ben Carson" = "#a7d65d",
      "Bernie Sanders" = "#6ac1a5",
      "Chris Christie" = "#6ac1a5",
      "Donald Trump" = "#fdbf7b",
      "Hillary Clinton" = "#96d7f8",
      "Jeb Bush" = "#fed844",
      "Mike Huckabee" = "#fbcee5",
      "Ted Cruz" = "#e58cc2"))) +
  theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y  = element_text()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key  = element_blank()) +
  theme(axis.ticks.y  = element_blank()) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "darkgrey"),
    panel.grid.minor = element_line(colour = "darkgrey"),
    panel.ontop = F
  ) +
  labs(title = "Negative") +
  theme(plot.title = element_text(
    colour = "black",
    face = "bold",
    hjust = 0.5
  )) +
  scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015))


# Neutral sentiment
neutral<-
  melt(
    neutral,
    id.vars = c("album_release_date", "candidate"),
    measure.vars = "candidate"
  )

setDT(neutral)[,freq := .N, by = negative$neutral]

neut <- ggplot(neutral, aes(x = album_release_date, y =freq)) +
  geom_point(
    aes(colour = neutral$candidate),
    stat = "identity",
    size = 3,
    position = position_stack(vjust = 0, reverse = T)
  ) +
  scale_colour_manual(
    name = "",
    breaks = (teljes_nevek),
    labels = (upcase_vez_nevek),
    values = ( values = c(
      "Ben Carson" = "#a7d65d",
      "Bernie Sanders" = "#6ac1a5",
      "Chris Christie" = "#6ac1a5",
      "Donald Trump" = "#fdbf7b",
      "Hillary Clinton" = "#96d7f8",
      "Jeb Bush" = "#fed844",
      "Mike Huckabee" = "#fbcee5",
      "Ted Cruz" = "#e58cc2"))) +
  theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y  = element_text()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key  = element_blank()) +
  theme(axis.ticks.y  = element_blank()) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "darkgrey"),
    panel.grid.minor = element_line(colour = "darkgrey"),
    panel.ontop = F
  ) +
  labs(title = "Neutral") +
  theme(plot.title = element_text(
    colour = "black",
    face = "bold",
    hjust = 0.5
  )) +
  scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015))


library(grid)
library(gridExtra)
osszefuzott <- grid.arrange(pos,neg,neut,nrow=1,
                            top="Candidate mentions, by sentiment")

# Exportálás.
ggsave("/Users/Adam/homework-04/fig/hiphop2.png", width = 15.4, height = 5.82)


#3.2 Egyéni ábra elkészítése
#Itt arra gondoltam, hogy megnézem, hogy a két elnökjelöltnél 

sajat_abra <- subset(hiphop_cand_lyrics,theme == "money")

#Ábrázolás
library(dplyr)
sajat_abra2 <- sajat_abra %>% 
  group_by(candidate, sentiment) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))


#Ábrázolás
library(dplyr)
sajat_abra2 <- sajat_abra %>% 
  group_by(candidate, sentiment) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))


ggplot(data = sajat_abra2, aes(x=factor(candidate), y= perc*100, 
                               fill=sentiment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(plot.title = element_text(hjust = 0.6)) +
  labs(title = "A pénz témájú dalszövegek érzelmi töltete,
jelöltek szerint", y = "(%)", x="") +
  scale_fill_manual(name = "Érzelmi töltet",
                    breaks = c("negative", "neutral", "positive"),
                    labels = c("Negatív", "Semleges", "Pozitv"), 
                    values= c("aquamarine3", "bisque3", "coral3"))


ggsave("/Users/Adam/homework-04/fig/hiphop3.png")

