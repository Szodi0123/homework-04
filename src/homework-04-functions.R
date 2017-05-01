## Házi feladat 4
## Programozás I.
## 2016/17. II. félév
## 2017.04.01.

##### FUNCTIONS #####

### 1. Függvény:


what_about_tweets <- function(candidate, n = 20) {
  ### Rendezés:
  for (i in 1:nrow(tweets))
    tweets$ordering[i] <-
      sum(tweets$favorurite_count[i], tweets$retweet_count[i])
  
  tweets_sorted <- tweets[order(-tweets[, 33]), ]
  
  ### Az beírt névre reflektáló parancsok.
  if (candidate == "Hillary Clinton") {
    print(as.character(tweets_sorted$text[tweets_sorted$handle == 
                                            "HillaryClinton"][1:n]))
  } else if (candidate == "Donald Trump") {
    print(as.character(tweets_sorted$text[tweets_sorted$handle == 
                                            "realDonaldTrump"][1:n]))
  } else{
cat("Válassz 'Hillary Clinton' és 'Donald Trump' közül.
            \"", candidate, "\"-t, nem lehet!")
  }
  
  
  ### Summary a végén.
  if ((candidate == "Hillary Clinton") &
      n < length(tweets_sorted$text[tweets_sorted$handle == 
                                    "HillaryClinton"])) {
    cat("Név:\"", candidate, "\" Ennyit írtam ki:", n)
  }
  
  else if ((candidate == "Donald Trump") &
           n < length(tweets_sorted$text[tweets_sorted$handle == 
                                         "realDonaldTrump"])) {
    cat("Név:\"", candidate, "\" Ennyit írtam ki:", n)
  } else{
    
  }
  
  
  
  ### Limit meghatározása + üzenet.
  if (n > length(tweets_sorted$text[tweets_sorted$handle == 
                                    "HillaryClinton"])
      & candidate == "Hillary Clinton") {
    cat(
      "A legtöbb, amit választhatsz,\"",
      length(tweets_sorted$text[tweets_sorted$handle == 
                                  "HillaryClinton"]),
      "\", szóval válassz kevesebbet!"
    )
  } else if (n > length(tweets_sorted$text[tweets_sorted$handle == 
                                           "realDonaldTrump"])
             & candidate == "Donald Trump")
  {
    cat(
      "A legtöbb, amit választhatsz,\""
      ,
      length(tweets_sorted$text[tweets$handle == "realDonaldTrump"]),
      "\", szóval válassz kevesebbet!"
    )
  }
}