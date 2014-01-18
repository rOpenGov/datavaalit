<!--
%\VignetteEngine{knitr}
%\VignetteIndexEntry{An R Markdown Vignette made with knitr}
-->

Finnish open government data toolkit for R
===========

This is an R package for Finnish open government data. New
contributions are [welcome!](http://louhos.github.com/contact.html).

This work is part of the [rOpenGov](http://ropengov.github.com)
project.


### Installing and loading the release version

For general users:


```r
install.packages("sorvi")
library(sorvi)
```


### Installing and loading the development version

For developers:


```r
install.packages("devtools")
library(devtools)
install_github("sorvi", "ropengov")
library(sorvi)
```


Further installation and development instructions can be found at the
project [home page](http://louhos.github.com/sorvi). 


### Using the package

For further usage
examples, see [Louhos-blog](http://louhos.wordpress.com) and
[Datawiki](https://github.com/louhos/sorvi/wiki/Data).


### Licensing and Citations

This work can be freely used, modified and distributed under the 
[Two-clause FreeBSD license](http://en.wikipedia.org/wiki/BSD\_licenses).

Kindly cite the work, if appropriate, as 'Leo Lahti, Juuso Parkkinen
ja Joona Lehtomäki (2011). sorvi - suomalainen avoimen datan
työkalupakki. URL: http://louhos.wordpress.com)'. 

A full list of authors and contributors and the relevant contact
information is [here](http://louhos.github.com/contact). The main
admins are Leo Lahti, Juuso Parkkinen and Joona Lehtomäki.

More information of the project is available at the project [home
page](http://louhos.github.com/sorvi).


### Session info


This vignette was created with


```r
sessionInfo()
```

```
## R version 3.0.1 (2013-05-16)
## Platform: x86_64-unknown-linux-gnu (64-bit)
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
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] knitr_1.2
## 
## loaded via a namespace (and not attached):
## [1] digest_0.6.3   evaluate_0.4.3 formatR_0.7    stringr_0.6.2 
## [5] tools_3.0.1
```





