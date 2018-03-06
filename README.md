# Tech-Trend-Analysis

Gartner’s hype cycle is a popular method for visually showing an ongoing high-tech
innovation process. Fenn and Raskino noted that “the hype cycle’s particular contribution is in
highlighting the challenge of adopting an innovation during the early stages of the innovation’s
life cycle.” Executives and managers check new hype cycle reports as a means of trying to find
new technological trends.

This study explores 
1) the data linked to tech trends
2) an adjusted linear model time series to predict the curve trend
2) new approach for simulating hype cycle curves with mathematical functions

### Get started

Download and import automatically all the libraries of the project with: 

```r
pkg <- c("gtrendsR", "reshape2", "ggplot2", "dplyr", "leaflet", "timetk", "forecast")

# check to see if packages are installed. Install them if they are not, then load them into the R session.
install_all_pkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

install_all_pkg(pkg)
```


![1](https://github.com/jadedagher/Tech-Trend-Analysis/blob/master/www/1.jpeg?raw=true)
![2](https://github.com/jadedagher/Tech-Trend-Analysis/blob/master/www/2.jpeg?raw=true)
![3](https://github.com/jadedagher/Tech-Trend-Analysis/blob/master/www/3.jpeg?raw=true)
![4](https://github.com/jadedagher/Tech-Trend-Analysis/blob/master/www/4.jpeg?raw=true)
