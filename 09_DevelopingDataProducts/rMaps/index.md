---
title       : rMaps
subtitle    : Building Data Products
author      : Brian Caffo, Jeff Leek, Roger Peng
job         : Johns Hopkins Bloomberg School of Public Health
logo        : bloomberg_shield.png
framework   : io2012
highlighter : highlight.js  
hitheme     : tomorrow       
url:
    lib: ../../librariesNew #Remove new if using old slidify
    assets: ../../assets
widgets     : [mathjax, quiz, bootstrap]
mode        : selfcontained # {standalone, draft}
ext_widgets : {rCharts: ["libraries/highcharts","libraries/nvd3", "libraries/morris", "libraries/leaflet", "libraries/rickshaw"]}
---



---
## BONUS RMaps!!!
- There's a new github repository describing connections between more 
  mapping libraries and R
  - We already saw leaflet

```
library(devtools);
require(devtools)
install_github('ramnathv/rCharts@dev')
install_github('ramnathv/rMaps')
```

---
## Creating a map
```
library("devtools", lib.loc="d:/Program Files/R/R-3.1.2/library")
library(rjson); library(rMaps)
crosslet(
  x = "country", 
  y = c("web_index", "universal_access", "impact_empowerment", "freedom_openness"),
  data = web_index
)
```

---

