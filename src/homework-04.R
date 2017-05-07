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

#Behívom az adatokat.
require("fivethirtyeight")
data("hiphop_cand_lyrics")

#A feladatmegoldások során több package-t is használok. Valószínűleg van,
#amit feleslegesen.
library(reshape2)
library(data.table)
library(grid)
library(gridExtra)
library(dplyr)

#Átalakítom az adataimat
abra_1 <-
  melt(
    hiphop_cand_lyrics,
    id.vars = c("album_release_date", "candidate", "sentiment"),
    measure.vars = "candidate"
  )

#Elkészítem az y tengelyre kerülő változót, ami a freq lesz.
abra_1$freq <- 1

#Ide helytakarékosság miatt elmentettem a neveket/vez.neveket/színkódokat.
teljes_nevek <- c(
  "Ben Carson",
  "Bernie Sanders",
  "Chris Christie",
  "Donald Trump",
  "Hillary Clinton",
  "Jeb Bush",
  "Mike Huckabee",
  "Ted Cruz"
)


upcase_vez_nevek <- c("CARSON",
                      "SANDERS",
                      "CHRISTIE",
                      "TRUMP",
                      "CLINTON",
                      "BUSH",
                      "HUCKABEE",
                      "CRUZ")

szinkodok <- c(
  "#a7d65d",
  "#6ac1a5",
  "#fd8977",
  "#fdbf7b",
  "#96d7f8",
  "#fed844",
  "#fbcee5",
  "#e58cc2"
)


ggplot(abra_1, aes(x = album_release_date, y = freq)) +
  geom_point(
    aes(colour = abra_1$candidate),
    stat = "identity",
    size = 4,
    position = position_stack(reverse = T)
  ) +
  scale_colour_manual(
    name = "",
    breaks = (teljes_nevek),
    labels = (upcase_vez_nevek),
    values = (szinkodok)
  ) +
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
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015))


# Exportálás.
ggsave("/Users/Adam/homework-04/fig/hiphop1.png")


#2. ábra: ugyanez a sentimentekre.
positive <- subset(hiphop_cand_lyrics, sentiment == "positive")
negative <- subset(hiphop_cand_lyrics, sentiment == "negative")
neutral <- subset(hiphop_cand_lyrics, sentiment == "neutral")


#Ezeknél fontos, hogy a színeket nem tudom a vektorból megadni,
#mert nem mindenkinek van sentimentes tweetje.


# Pozitív sentiment táblakezelés
positive <-
  melt(
    positive,
    id.vars = c("album_release_date", "candidate", "sentiment"),
    measure.vars = "candidate"
  )

#Y tengelyhez
positive$freq <- 1

#Ábrázolás
pos <- ggplot(positive, aes(x = album_release_date, y = freq)) +
  geom_point(
    aes(colour = positive$candidate),
    stat = "identity",
    size = 4,
    position = position_stack(vjust = 0, reverse = T)
  ) +
  scale_colour_manual(
    name = "",
    breaks = (teljes_nevek),
    labels = (upcase_vez_nevek),
    values = c(
      "Ben Carson" = "#a7d65d",
      "Bernie Sanders" = "#6ac1a5",
      "Chris Christie" = "#6ac1a5",
      "Donald Trump" = "#fdbf7b",
      "Hillary Clinton" = "#96d7f8",
      "Jeb Bush" = "#fed844",
      "Mike Huckabee" = "#fbcee5",
      "Ted Cruz" = "#e58cc2"
    )
  ) +
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
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015))

# Negatív sentiment
negative <-
  melt(
    negative,
    id.vars = c("album_release_date", "candidate", "sentiment"),
    measure.vars = "candidate"
  )


#Y tengelyhez
negative$freq <- 1


neg <- ggplot(negative, aes(x = album_release_date, y = freq)) +
  geom_point(
    aes(colour = negative$candidate),
    stat = "identity",
    size = 4,
    position = position_stack(vjust = 0, reverse = T)
  ) +
  scale_colour_manual(
    name = "",
    breaks = (teljes_nevek),
    labels = (upcase_vez_nevek),
    values = c(
      "Ben Carson" = "#a7d65d",
      "Bernie Sanders" = "#6ac1a5",
      "Chris Christie" = "#6ac1a5",
      "Donald Trump" = "#fdbf7b",
      "Hillary Clinton" = "#96d7f8",
      "Jeb Bush" = "#fed844",
      "Mike Huckabee" = "#fbcee5",
      "Ted Cruz" = "#e58cc2"
    )
  ) +
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
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015))


# Neutral sentiment
neutral <-
  melt(
    neutral,
    id.vars = c("album_release_date", "candidate", "sentiment"),
    measure.vars = "candidate"
  )

neutral$freq <- 1

neut <- ggplot(neutral, aes(x = album_release_date, y = freq)) +
  geom_point(
    aes(colour = neutral$candidate),
    stat = "identity",
    size = 4,
    position = position_stack(vjust = 0, reverse = T)
  ) +
  scale_colour_manual(
    name = "",
    breaks = (teljes_nevek),
    labels = (upcase_vez_nevek),
    values = (
      values = c(
        "Ben Carson" = "#a7d65d",
        "Bernie Sanders" = "#6ac1a5",
        "Chris Christie" = "#6ac1a5",
        "Donald Trump" = "#fdbf7b",
        "Hillary Clinton" = "#96d7f8",
        "Jeb Bush" = "#fed844",
        "Mike Huckabee" = "#fbcee5",
        "Ted Cruz" = "#e58cc2"
      )
    )
  ) +
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
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015))


osszefuzott <- grid.arrange(pos, neg, neut, nrow = 1,
                            top = "Candidate mentions, by sentiment")

# Exportálás.
ggsave(
  "/Users/Adam/homework-04/fig/hiphop2.png",
  plot = osszefuzott,
  width = 15.4,
  height = 5.82
)


#3.2 Egyéni ábra elkészítése
#Itt arra gondoltam, hogy megnézem, hogy a két elnökjelöltnél a pénz témájú
#dalszövegeken belül hogyan oszlik meg a dalszövegek szentimentje.

sajat_abra <- subset(hiphop_cand_lyrics, theme == "money")


#Ábrázoláshoz előkészítés. Itt a sentiment és a candidate szerint csoportosítok
#majd leszámláltatom vele külön, hogy mennyi az összes és a perc oszlopba
#százalékot számoltatok. Így kerültem meg, hogy különböző mennyiségű
#money-s tweetjük van (Trumpnak sokkal több van).

sajat_abra <- sajat_abra %>%
  group_by(candidate, sentiment) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))

#Nem törődtem sokat a külalakkal, inkább az ábra a fontos
ggplot(data = sajat_abra, aes(
  x = factor(candidate),
  y = perc * 100,
  fill = sentiment
)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(plot.title = element_text(hjust = 0.59)) +
  labs(title = "A pénz témájú dalszövegek érzelmi töltete, jelöltenként",
       y = "(%)", x = "") +
  scale_fill_manual(
    name = "Érzelmi töltet",
    breaks = c("negative", "neutral", "positive"),
    labels = c("Negatív", "Semleges", "Pozitv"),
    values = c("aquamarine3", "bisque3", "coral3")
  )


#Innen pedig jól látszik, hogy valahogy a pozitívak vannak többségben.
#Bár ez egy kicsit azért fura.

ggsave("/Users/Adam/homework-04/fig/hiphop3.png")


#Letisztítom az environmentet, majd vi
rm(list = ls())


#IV.1 feladat I.rész: ábrázolások

#Először is tetszettek az alapszínek, szóval újra lefutattom őket.
szinkodok <- c(
  "#a7d65d",
  "#6ac1a5",
  "#fd8977",
  "#fdbf7b",
  "#96d7f8",
  "#fed844",
  "#fbcee5",
  "#e58cc2"
)

#Tulajdonképpen ugyanazt kell megcsinálnom a tweetsre,
#mint a saját ábránál, csak nem kell szűrnöm csoportra.
tweets <-
  read.table(file = "data/clinton_trump_tweets.csv", sep = ";", header = T)

#Százalékszámítások.
emo_tweets <- tweets %>%
  group_by(handle, text_emotion) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))

senti_tweets <- tweets %>%
  group_by(handle, text_sentiment) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))


#Ábrázolás: emotion_text
plot1 <-
  ggplot(data = emo_tweets, aes(
    x = factor(handle),
    y = perc * 100,
    fill = text_emotion
  )) +
  geom_bar(stat = "identity", position = position_stack()) +
  theme(plot.title = element_text(hjust = 0.59)) +
  labs(title = "A jelöltek tweetjei érzelmi besorolás alapján",
       y = "(%)", x = "") +
  scale_fill_manual(
    name = "Érzelmek",
    values = szinkodok,
    label = c(
      "harag",
      "undor",
      "félelem",
      "öröm",
      "szomorúság",
      "meglepettség",
      "nem felismerhető"
    )
  ) + scale_x_discrete(
    breaks = c("HillaryClinton", "realDonaldTrump"),
    labels = c("Hillary Clinton", "Donald Trump")
  )

#Ábrázolása: sentiment_text
plot2 <-
  ggplot(data = senti_tweets, aes(
    x = factor(handle),
    y = perc * 100,
    fill = text_sentiment
  )) +
  geom_bar(stat = "identity", position = position_stack()) +
  theme(plot.title = element_text(hjust = 0.59)) +
  labs(title = "A jelöltek tweetjei szentimentek alapján",
       y = "(%)", x = "") +
  scale_fill_manual(
    name = "Érzelmi töltet (sentiment)",
    values = szinkodok,
    label = c("Negatív", "Semleges", "Pozitív")
  ) + scale_x_discrete(
    breaks = c("HillaryClinton", "realDonaldTrump"),
    labels = c("Hillary Clinton", "Donald Trump")
  )

#Összevont ábra I.
grid.arrange(plot1, plot2, nrow = 1, top = "Összehasonlítás")




#IV.1 feladat II.rész: időSOROS ábrázolások
#Elmentem egy külön változóba a hónapokat: ezek kerülnek az x tengelyre.
tweets$months <- strftime(tweets$time, format = "%m")

#Mivel úgy szeretném, hogy legyen külön árbán legyen az emotion elemzés
#jelöltenként, meg kell csinálnom, egy sentiment/emotion szerinti táblát
#majd külön subseteket külön ábrákon ábrázolni. Ezután  majd összevonom a
#tipológia szerint.

#Idősoros-emotion
idosorhoz_emot <- tweets %>%
  group_by(handle, months, text_emotion) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))

idosor_emot_trump <-
  subset(idosorhoz_emot, handle == "realDonaldTrump")
idosor_emot_clinton <-
  subset(idosorhoz_emot, handle == "HillaryClinton")

p_idosor_emot_1 <-
  ggplot(data = idosor_emot_trump,
         aes(
           x = months,
           y = perc * 100,
           fill = text_emotion,
           show.legend
         )) +
  geom_bar(stat = "identity",
           position = position_stack(),
           show.legend = F) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Trump",
       y = "(%)", x = "") +
  scale_fill_manual(
    name = "Érzelmek",
    values = szinkodok,
    label = c(
      "harag",
      "undor",
      "félelem",
      "öröm",
      "szomorúság",
      "meglepettség",
      "nem felismerhető"
    )
  )


p_idosor_emot_2 <-
  ggplot(data = idosor_emot_clinton,
         aes(
           x = months,
           y = perc * 100,
           fill = text_emotion,
           show.legend
         )) +
  geom_bar(stat = "identity", position = position_stack()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Clinton",
       y = "(%)", x = "") +
  scale_fill_manual(
    name = "Érzelmek",
    values = szinkodok,
    label = c(
      "harag",
      "undor",
      "félelem",
      "öröm",
      "szomorúság",
      "meglepettség",
      "nem felismerhető"
    )
  )

#Összevont ábra II.1 emotion
p_idosor_emot <-
  grid.arrange(p_idosor_emot_1,
               p_idosor_emot_2,
               nrow = 1,
               top = "A jelöltek tweetjei érzelmi besorolás alapján")


#Idősoros: sentiment
idosorhoz_sent <- tweets %>%
  group_by(handle, months, text_sentiment) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))

idosor_sent_trump <-
  subset(idosorhoz_sent, handle == "realDonaldTrump")
idosor_sent_clinton <-
  subset(idosorhoz_sent, handle == "HillaryClinton")

p_idosor_sent_1 <-
  ggplot(data = idosor_sent_trump,
         aes(
           x = months,
           y = perc * 100,
           fill = text_sentiment,
           show.legend
         )) +
  geom_bar(stat = "identity",
           position = position_stack(),
           show.legend = F) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Trump",
       y = "(%)", x = "") +
  scale_fill_manual(
    name = "Érzelmi töltet (sentiment)",
    values = szinkodok,
    label = c("Negatív", "Semleges", "Pozitív")
  )

p_idosor_sent_2 <-
  ggplot(data = idosor_sent_clinton, aes(x = months, y = perc * 100,
                                         fill = text_sentiment)) +
  geom_bar(stat = "identity", position = position_stack()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Clinton",
       y = "(%)", x = "") +
  scale_fill_manual(
    name = "Érzelmi töltet (sentiment)",
    values = szinkodok,
    label = c("Negatív", "Semleges", "Pozitív")
  )


#Összevont ábra II.2 sentiment
p_idosor_sent <-
  grid.arrange(p_idosor_sent_1,
               p_idosor_sent_2,
               nrow = 1,
               top = "A jelöltek tweetjei sentiment besorolás alapján")


#IV.1 feladat: 3. rész: próba
chisq.test(tweets$handle, tweets$text_sentiment)
chisq.test(tweets$handle, tweets$text_emotion)

#Ezek szerint mind a szentiment, mind pedig az alapérzelmes
#beosztás mentén szig.különbség van.


#IV.2 feladat: Trump tweetjei ábrázolva
#Megnézem, hogy egyáltalán milyen lehetőségek vannak.
levels(as.factor(tweets$source_url))

#Kimentem egy subsetbe az adatokat, majd hozzárendelem a címkéket.
forras_vizsg <-
  subset(
    tweets,
    source_url == "http://twitter.com/download/iphone" |
      source_url == "http://twitter.com/download/android" &
      handle == "realDonaldTrump"
  )

#Definiálok egy változót, majd beleírom az eszköz nevét.

forras_vizsg$op_rendsz <- NA

forras_vizsg$op_rendsz <- NA
for (i in 1:nrow(forras_vizsg)) {
  if (forras_vizsg$source_url[i] == "http://twitter.com/download/iphone") {
    forras_vizsg$op_rendsz[i] <- "Iphone"
  }else{
    forras_vizsg$op_rendsz[i] <- "Android"
  }
}



#Ellenőrzöm.
levels(as.factor(forras_vizsg$op_rendsz))

#Mivel most nagyjából egyenlő számban vannak tweetek a két forrásból
#egyszerű countot készítek, halmozás nélkül <-  így összehasonlíthatóbb.

#Sentiment
source_sent <- ggplot(data = forras_vizsg,
                      aes(x = text_sentiment, fill = op_rendsz)) +
  geom_bar(stat = "count",
           position = position_dodge(),
           show.legend = T) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Sentiment kategóriák forrásonként",
       y = "Tweetek száma", x = "") +
  scale_fill_manual(name = "Operációs rendszer",
                    values = szinkodok) +
  scale_x_discrete(
    breaks = c("negative", "neutral", "positive"),
    labels = c("Negatív", "Semleges", "Pozitív")
  )


#Emotion
source_emo <- ggplot(data = forras_vizsg,
                     aes(x = text_emotion, fill = op_rendsz)) +
  geom_bar(stat = "count",
           position = position_dodge(),
           show.legend = T) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Érzelem-kategóriák forrásonként",
       y = "Tweetek száma", x = "") +
  scale_fill_manual(name = "Operációs rendszer",
                    values = szinkodok) +
  scale_x_discrete(
    breaks = c(
      "anger",
      "disgust",
      "fear",
      "joy",
      "sadness",
      "surprise",
      "unknown"
    ),
    labels = c(
      "harag",
      "undor",
      "félelem",
      "öröm",
      "szomorúság",
      "meglepettség",
      "nem felismerhető"
    )
  )

grid.arrange(source_emo, source_sent)


#IV.2 feladat: Trump tweetjei próba
chisq.test(forras_vizsg$op_rendsz, forras_vizsg$text_sentiment)
chisq.test(forras_vizsg$op_rendsz, forras_vizsg$text_emotion)

#A próba szerint különböznek a posztok mind szentiment, mind pedig emotion terén
# abban, hogy milyen készülékről küldtél el.

## Vége ##