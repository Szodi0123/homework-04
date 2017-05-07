# Házi feladat 4
* Kurzus: Programozás I. (SST111)
* Félév: 2016/17 II.
* Határidő: 2017-05-01

---

## I. feladat (1 pont)
1. Ha Coospace-re szeretnéd feltölteni a házit, töltsd le onnan és csomagold ki a ```homework-04.zip```-et! Mikor végeztél, tömörítsd vissza és úgy add be! Ha GitHub-on szeretnéd beadni, akkor kövesd az V. feladat pontjait!
2. Most nincs R projekt fájl a mappában, ezért indíts egy új projektet! 
3. Nyiss egy R szkriptet ```homework-04.R``` néven egy ```src``` nevű mappában! Ebbe írd a II., III., IV. feladat megoldásait. Nyiss egy R szkriptet ```homework-04-functions.R``` néven az ```src``` mappában! Ide kerüljön, ha írsz egy függvényt.
4. A ```homework-04.R``` fájlt kikommentelt fejléccel kezdd! A fejlécben a következőket add meg: 
    - fájl címe (*Házi feladat 4*)
    - kurzus neve (*Programozás I.*)
    - félév (*2016/17. II. félév*)
    - neved
    - dátum
5. Kommenteld folyamatosan, amit csinálsz és figyelj, hogy minél jobban betartsd az R kódírási stílusára vonatkozó irányelveket!

---

## II. feladat (8 pont)

1. A ```data``` mappában találsz egy ```clinton_trump_tweets.csv``` nevű fájlt. Ezt hívd be ```tweets``` néven. Az eredeti adatfájl [innen](https://www.kaggle.com/benhamner/clinton-trump-tweets) származik, és Hillary Clinton, valamint Donald Trump 6444 tweetjét tartalmazza 2016. januártól szeptemberig. Ez az adatsor bővítve lett pár új oszloppal. Az új oszlopok a tweetek szövegeinek különbözőképp feldolgozott verzióit tartalmazza, valamint a szövegek legjellemzőbb szentimentjét és emócióját. (Ezekről kicsit később.)
2. Kérd le, hány tweet származik Hillary Clintontól és hány Donald Trumptól! Ezt ábrázold is a ```fig/sample/tweet1.png``` alapján, majd mentsd ki a plotot a ```fig``` mappába ```tweet1.png``` néven!
3. Kérd le, milyen nyelveken írta a tweeteket a két jelölt külön-külön! Nézd meg, hogy a nem angol szövegek valóban nem angolok-e. Ha úgy gondolod, hogy rosszul azonosították a nyelvet, akkor írd át az általad helyesnek váltre. A spanyol nyelvű tweeteket nem kell végigböngészned, mert sok van, de a többit mindenképp nézd meg. Ábrázold a két jelölt által használt nyelvek gyakoriságát a ```fig/sample/tweet2.png``` szerint, majd mentsd ki a plotot a ```fig``` mappába ```tweet2.png``` néven! Az oszlopszínek legyenek "darkgrey" és "cornflowerblue". (A nyelvek gyakorisága eltérhet az előző lépés miatt.)
4. Írj egy függvényt, ami a "Hillary Clinton" bemenet esetében Hillary Clinton, "Donald Trump" bemenet esetében pedig Donald Trump tweetjeit adja ki a retweet_count és a favorite_count összesített értéke alapján csökkenő sorrendben. Legyen egy olyan argumentum is, amivel a kiadott tweetek számát lehet meghatározni. Az argumentumoknak legyen default értéke. A függvény írjon ki hibaüzenetet, ha rossz nevet kap. Ha a kiadott tweetek száma meghaladja a lehetséges tweetek számát, írja ki a tweetek max számát. Mikor a függvény kiadja a tweeteket, írja ki, hogy kinek a tweetjeit írja ki és mennyit. Kérd le a függvénnyel Hillary Clinton első 10, valamint Donald Trump első 15 legtöbbet retweetelt és kedvelt tweetjét.

## III. feladat (5 pont)
A ```fivethirtyeight``` package-ben van egy ```hiphop_cand_lyrics``` dataset. A fivethirtyeight-en [ezt](https://projects.fivethirtyeight.com/clinton-trump-hip-hop-lyrics/) az elemzést készítették az adatokhoz.
1. Készítsd el ```ggplot2```-vel az itt található első 2 ábrát úgy, hogy minél jobban megegyezzen a plotok kinézete. Természetesen nem kell mindennek ugyanolyannak lennie (pl. az oszlopoknak nem kell pontokból állnia), de törekedj a minél nagyobb fokú egyezőségre. Mentsd ki az ábrákat a ```fig``` mappába ```hiphop1.png``` és ```hiphop2.png``` néven!
2. Készíts egy általad értelmesnek tartott ábrát a datasetben található változókat felhasználva! Mentsd ki az ábrát a ```fig``` mappába ```hiphop3.png``` néven!

## IV. feladat (6 pont)
A ```clinton_trump_tweets.csv``` tartalmaz egy ```text_sentiment``` és egy ```text_emotion``` változót. Szentiment- és az emócióelemzés során a szövegek által közvetített érzelmeket, attitűdöket, véleményeket azonosítjuk. A szentiment egy szöveg polaritását jelzi, azaz, hogy a szöveg negatív, pozitív vagy semleges-e. Az emócióelemzés során pedig általában a hat emberi alapérzelmet (bánat, düh, öröm, undor, félelem, meglepődés) próbáljuk detektálni a szövegekben, ahogyan a tweetek esetében is történt. Bizonyos szentimentkategóriák sokszor együttjárnak valamelyik emóciókategóriával, de nem minden esetben és nem is feleltethetőek meg egymásnak.

1. Hasonlítsd össze Hillary Clinton és Donald Trump tweetjeinek szentimentjeit és emócióit. Vizsgáld meg oszlopdiagrammal és idősoros ábrával az eltéréseket. Nézd meg, hogy statisztikailag szignifikáns-e a különbség a két jelölt szentimentjeinek ill. emócióinak száma között.


2. Többen feltételezték, hogy Trump tweetjeit (legalább) két különböző ember írja, az androidosokat feltételezhetően Trump, az iphone-osokat pedig valaki más (a mi adatainkban a ```source_url``` oszlop tartalmaz erre vonatkozó információkat). Ennek a feltételezésnek a bizonyítására készült egy tök jó szövegelemzés: http://varianceexplained.org/r/trump-tweets/. A szerző itt is összehasonlítja a tweetek szentimentjét (bár más kategóriarendszert használva). Vizsgáld meg vizuálisan, hogy a mi adatunk szerint is különböznek-e a két forrásból származó tweetek szentimentjei ill. emóciói. Nézd meg, hogy statisztikailag szignifikáns-e a különbség.



3. Készíts az elemzéshez dokumentációt, ami tartalmazza az ábrákat és a tesztek eredményeit, valamint ezek szöveges értelmezéseit. A dokumentáció legyen pdf vagy html kiterjesztésű és mentsd egy ```doc``` nevű mappába. A dokumentáció készülhet szabadon választott eszközzel, bármilyen szövegszerkesztővel, de a legjobb, ha R markdown fájlt készítesz, és azt mented ki html-be vagy pdf-be. Utóbbi esetben az Rmd fájl is legyen a ```doc``` mappában. A ```markdownr``` és ```knitr``` package-ek használatáról [itt](https://www.r-bloggers.com/r-markdown-and-knitr-tutorial-part-1/) olvashatsz például, vagy az órai anyagok ```doc``` mappáit is veheted példának. Az ```markdownr``` és ```knitr``` package-ek használata egyáltalán nem kötelező, de elég jó cucc és hasznos skill, pl. ha riportot szeretne készíteni az ember.

---

## V. feladat (+2 pont)
A feladat megoldásához segítségedül lehet [ez a leírás](https://gist.github.com/Chaser324/ce0505fbed06b947d962) a Submitting a Pull Request részig.

1. Forkold le a házi feladat repóját! Ezt GitHub-on a Fork gombbal tudod megtenni. Ezután klónozd le a forkodból a repót!
2. Hozz létre egy új branchet ```my-solutions-githubazonosito``` néven.
3. A ```homework-04.R``` fájl írása közben ne felejts el commitokat írni, mikor úgy gondolod, hogy egy értelmes egység végéhez értél. A kimentett képeket, a ```homework-04-function.R``` fájlt és a dokumentációt se felejtsd ki.
4. A projekt indításakor létre kellett jönnie egy [.gitignore](https://github.com/github/gitignore/blob/master/R.gitignore) fájlnak, ha nem, akkor készíts egyet, mielőtt pusholnál, hogy ne töltsd fel pl. az RHistory fájlodat. Ezt se felejtsd el commitolni.
5. Ha végeztél, mindent commitoltál, pusholtad a változásokat GitHubra, küldj egy pull requestet!

---
 
