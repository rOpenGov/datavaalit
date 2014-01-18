# Kunnallisvaalit 2012

Seuraavat esimerkit visualisoivat R-ympäristössä kunnallisvaaliehdokkaiden aktiivisuutta sosiaalisessa mediassa 2012. Lähde: [Louhos-blogi](https://louhos.wordpress.com/2012/10/26/kunnallisvaaliehdokkaiden-aktiivisuus-sosiaalisessa-mediassa-puolueiden-vertailua/).

Aloitetaan hakemalla CSV-dumppi Datavaalit-palvelimelta ja muuta vaalidataa.


```r
# sorvi installation: http://louhos.github.com/sorvi/asennus.html
library(sorvi)
library(ggplot2)

# Download and unpack data
# download.file('http://www.datavaalit.fi/storage/some-updates-2013-01-22.csv.bz2',
# destfile = 'some-updates-2013-01-22.csv.bz2')
system("bunzip2 some-updates-2013-01-22.csv.bz2")

# Read table and name the columns
tsome <- read.csv("some-updates-2013-01-22.csv", sep = ";", header = FALSE)
names(tsome) <- c("nimi", "puolue", "kunta", "sukupuoli", "media", "aika")

# Pick just the 2012 updates
tsome <- tsome[grep("2012", tsome$aika), ]
tsome$media <- gsub("FB", "Facebook", tsome$media)
tsome$media <- gsub("TW", "Twitter", tsome$media)
tsome$media <- factor(tsome$media)

# Aktiivisimmat some-puolueet
isot <- names(which(table(tsome$puolue) > 1000))

# Hae 2012 kunnallisvaalien puoluetietoja Lähde: (C) Oikeusministeriö
# http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat Load
# party information (C) Oikeusministeriö 2012
parties.all <- ReadAllParties()

# Hae kunnallisvaalien ehdokastiedot Datavaalit-palvelimelta Lähde:
# Datavaalit ja Tilastokeskus ks.
# http://louhos.wordpress.com/2012/10/05/kunnallisvaalien-vertailukelpoiset-ehdokasdatat-csv-taulukkoina-2004-2008-2012/
cand <- read.csv2("http://www.datavaalit.fi/storage/avoindata/datavaalit-ehdokas-ja-tulostiedot/2012/municipal_elections_candidates_2012_finland.csv", 
    sep = ";", fileEncoding = "iso-8859-1")
cand.per.party <- sort(table(cand$Puolue_lyhenne_fi))
```


## Sosiaalisen median päivitystiheydet aikasarjana


```r
theme_set(theme_bw(20))
tsome$aika <- as.Date(tsome$aika)
tsome <- subset(tsome, puolue %in% isot)
tsome$puolue <- droplevels(tsome$puolue)

pics <- list()
for (me in c("Twitter", "Facebook")) {
    
    tsome.sub <- subset(tsome, media == me)
    p <- ggplot(tsome.sub, aes(x = aika))
    p <- p + geom_histogram(binaxis = "y", binwidth = 1, fill = "#3B5998") + 
        ylab("lkm") + facet_grid(puolue ~ .) + ggtitle(me) + scale_x_date() + 
        xlab("")
    pics[[me]] <- p
}
library(gridExtra)
grid.arrange(pics[[1]], pics[[2]], nrow = 1)
```

![plot of chunk datavaalit-8](figure/datavaalit-8.png) 



### Aktiivisimmat statuspäivittäjät


```r
theme_set(theme_bw(20))
pics <- list()

for (me in c("Twitter", "Facebook")) {
    dfs <- data.frame(list(count = table(subset(tsome, media == me)$nimi)))
    dfs <- dfs[rev(order(dfs$count.Freq)), ]
    dfs$ind <- 1:nrow(dfs)
    n <- 40
    pics[[me]] <- ggplot(data = dfs[1:n, ], aes(x = rev(ind), y = count.Freq)) + 
        geom_text(aes(label = count.Var1), size = 4) + ggtitle(me) + scale_x_continuous(limits = c(1, 
        n)) + scale_y_continuous(limits = c(0.6 * min(dfs[1:n, "count.Freq"]), 
        1.02 * max(dfs[1:n, "count.Freq"]) + 20)) + coord_flip() + ylab("Päivitystiheys") + 
        xlab("Nimi")
    
}

grid.arrange(pics[[1]], pics[[2]], nrow = 1)
```

![plot of chunk some2](figure/some2.png) 


### Puolueitten aktiivisuus sosiaalisessa mediassa (absoluuttinen lukumäärä)


```r

stsome <- subset(tsome, puolue %in% isot)
stsome$puolue <- factor(stsome$puolue, level = names(sort(table(stsome$puolue))))

p <- ggplot(stsome, aes(x = puolue, fill = media, group = media)) + geom_bar(stat = "bin", 
    position = "stack") + ylab("") + xlab("") + ggtitle(paste("Statuspäivitykset", 
    paste(sort(tsome$pvm)[[1]], "-", rev(sort(tsome$pvm))[[1]]), sep = "")) + 
    theme(axis.text.x = element_text(angle = 30), plot.title = element_text(size = 12))

print(p)
```

![plot of chunk some4](figure/some4.png) 


### Puolueitten aktiivisuus sosiaalisessa mediassa (edustajaa kohden)

Erot puolueiden ehdokasmäärissä on huomiotu laskemalla Facebook-viestien määrä ehdokasta kohden. Visualisointien selkeyttämiseksi poistetaan puolueet, joilla on vain yksi ehdokas.


```r
# Laske some-aktiivisuus ehdokasta kohden (= normalisoi data puolueiden
# ehdokasmäärillä)
library(reshape)
some <- melt(table(tsome[, c("puolue", "media")]))

pics <- list()
for (me in c("Twitter", "Facebook")) {
    msome <- subset(some, media == me)
    
    msome$normalized <- msome$value/cand.per.party[as.character(msome$puolue)]
    msome$candidates <- cand.per.party[as.character(msome$puolue)]
    
    # Poista yhden ehdokkaan puolueet
    msome <- subset(msome, candidates > 1)
    
    # Poista puolueet joilla ei mainittavaa aktiivisuutta
    msome <- subset(msome, normalized > 1)
    
    # puolueen lyhenne ja koko nimi
    msome$puolue.lyh <- droplevels(msome$puolue)
    msome$puolue <- parties.all$Nimi_fi[match(as.character(msome$puolue), parties.all$Puolue_lyhenne_fi)]
    
    # Aseta puolueet suuruusjärjestykseen some-aktiivisuuden nojalla
    msome <- msome[rev(order(msome$normalized)), ]
    msome$puolue <- factor(msome$puolue, levels = rev(as.character(msome$puolue)))
    
    # Visualisoi some-aktiisuus edustajaa kohden
    library(ggplot2)
    theme_set(theme_bw(15))
    msome$Paivitykset.edustajaa.kohden <- msome$normalized
    p <- ggplot(msome, aes(x = puolue, y = Paivitykset.edustajaa.kohden, group = media)) + 
        geom_bar(stat = "identity") + coord_flip() + ggtitle(paste("Aktiivisuus:", 
        me))
    pics[[me]] <- p
}

grid.arrange(pics[[1]], pics[[2]], nrow = 1)
```

![plot of chunk datavaalit-3](figure/datavaalit-3.png) 




### Miehet twiittaa, naiset facebookkaa


```r
p <- ggplot(tsome, aes(x = media, fill = sukupuoli, group = sukupuoli)) + geom_bar(stat = "bin", 
    position = "stack") + ylab("") + xlab("") + scale_fill_manual(name = "", 
    values = c("gray", "red", "blue")) + ggtitle(paste("Statuspäivitykset", 
    paste(sort(tsome$pvm)[[1]], "-", rev(sort(tsome$pvm))[[1]]), sep = ""))

print(p)
```

![plot of chunk some5](figure/some5.png) 






### Versiotiedot

Tämä esimerkki on toteutettu seuraavin versiotiedoin:
 

```r
sessionInfo()
```

```
## R version 2.15.1 (2012-06-22)
## Platform: x86_64-pc-linux-gnu (64-bit)
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=C                 LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] splines   grid      stats     graphics  grDevices utils     datasets 
## [8] methods   base     
## 
## other attached packages:
##  [1] reshape_0.8.4   plyr_1.8        gridExtra_0.9.1 ggplot2_0.9.3  
##  [5] sorvi_0.2.14    spdep_0.5-55    coda_0.16-1     deldir_0.0-21  
##  [9] maptools_0.8-21 foreign_0.8-50  nlme_3.1-106    MASS_7.3-23    
## [13] Matrix_1.0-10   lattice_0.20-6  boot_1.3-5      sp_1.0-5       
## [17] rjson_0.2.11    RCurl_1.95-3    bitops_1.0-5    pxR_0.28       
## [21] knitr_0.9      
## 
## loaded via a namespace (and not attached):
##  [1] colorspace_1.2-0   dichromat_1.2-4    digest_0.6.0      
##  [4] evaluate_0.4.3     formatR_0.7        gtable_0.1.2      
##  [7] labeling_0.1       LearnBayes_2.12    munsell_0.4       
## [10] proto_0.3-10       RColorBrewer_1.0-5 reshape2_1.2.2    
## [13] scales_0.2.3       stringr_0.6.2      tools_2.15.1
```

