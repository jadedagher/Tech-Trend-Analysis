#hello world 
pkg <- c("gtrendsR", "reshape2", "ggplot2", "dplyr", "leaflet", "timetk", "forecast")

# check to see if packages are installed. Install them if they are not, then load them into the R session.
install_all_pkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

install_all_pkg(pkg)


#hype cycle plot 
y <- c(0,0.5,3,25,75,100,75,50,40,30,35,60)
x <- c("2005-01-01","2007-01-01","2008-01-01","2009-01-01","2010-01-01","2011-06-01","2012-08-01","2013-04-01","2015-01-01","2017-01-01","2020-01-01","2025-01-01")

hype_cycle_data <- data_frame(x,y)
hype_cycle_data$x <- as.Date(x)
hype_cycle_data$y <- as.integer(y)

ggplot(hype_cycle_data, aes(x, y)) + 
  geom_line() + 
  theme_light()


set.seed(100)
#http://airccse.org/journal/ijmit/papers/7215ijmit01.pdf
#https://www.r-bloggers.com/analyzing-google-trends-data-in-r-2/
#https://www.r-bloggers.com/demo-week-time-series-machine-learning-with-timetk/ 

#cloud trend data
full_data_cloud <- gtrends("cloud computing", gprop = "web", time = "all", hl = "en-US")

#get interest over time data
google.trends_cloud <- full_data_cloud[[1]]
google.trends_cloud <- dcast(google.trends_cloud, date ~ keyword + geo, value.var = "hits")
colnames(google.trends_cloud) <- c("date", "cloud_computing_word")

sum(is.na(google.trends_cloud$cloud_computing_word))

#preprocessing (as cloud_computing_word is Characters)
google.trends_cloud$cloud_computing_word <- sapply(google.trends_cloud$cloud_computing_word, function(x) if(x == "<1"){x = "0"} else {x = x})
google.trends_cloud$cloud_computing_word <- as.integer(google.trends_cloud$cloud_computing_word)
#data viz Cloud dataset
ggplot(google.trends_cloud, aes(date, cloud_computing_word)) + 
  geom_line(color="red") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme_light()


#ML time series using timetk package

# test data 
google.trends_cloud_test <- google.trends_cloud[google.trends_cloud$date <= "2017-03-01",]

# We can quickly get a feel for the time series using tk_index() 
# to extract the index and tk_get_timeseries_summary() to retrieve summary information of the index
google.trends_cloud_test %>%
  tk_index() %>%
  tk_get_timeseries_summary() %>%
  glimpse()

# Augment (adds data frame columns)
# The tk_augment_timeseries_signature() function expands out the timestamp information column-wise 
# into a machine learning feature set, adding columns of time series information to the original data frame.
google.trends_cloud_tbl_test_aug <- tk_augment_timeseries_signature(google.trends_cloud_test)

# linear regression model used, but can use any model
fit_lm <- lm(cloud_computing_word ~ ., na.action = NULL, data = select(google.trends_cloud_tbl_test_aug, -c(date, diff)))
#fit_lm <- nls(cloud_computing_word ~ (year*a - month*b) -1, start=c(a=2004,b=1), na.action = NULL, data = select(google.trends_cloud_tbl_test_aug, -c(date, diff)))
#j <- ts(google.trends_cloud_test)
#fit_lm <- tslm(cloud_computing_word ~ trend, data = j)
coef(fit_lm)
# get correlation
paste0("Correlation: ",round(cor(google.trends_cloud_tbl_test_aug$cloud_computing_word,predict(fit_lm))*100,2),"%")

plot(fit_lm, las = 1)

# Retrieves the timestamp information
google.trends_cloud_test_idx <- tk_index(google.trends_cloud_test)

# Make a future index from the existing index with tk_make_future_timeseries. 
# The function internally checks the periodicity and returns the correct sequence
future_idx  <- tk_make_future_timeseries(google.trends_cloud_test_idx, n_future = 12)

future_idx2 <- tk_make_future_timeseries(google.trends_cloud_test_idx, n_future = 90)

# From the future index, use tk_get_timeseries_signature() to turn index into time signature data frame.
new_data_tbl  <- tk_get_timeseries_signature(future_idx)

new_data_tbl2 <- tk_get_timeseries_signature(future_idx2)

# Make predictions (actual)
pred <- predict(fit_lm, newdata = select(new_data_tbl, -c(index, diff)))

predictions_tbl_train <- tibble(
  date  = future_idx,
  value = pred/2
)
predictions_tbl_train

# Make predictions on 90 months 
pred2 <- predict(fit_lm, newdata = select(new_data_tbl2, -c(index, diff)))

predictions_tbl2_train <- tibble(
  date  = future_idx2,
  value = pred2/2
)
predictions_tbl2_train

actuals_tbl <- google.trends_cloud[google.trends_cloud$date > "2017-03-01" & google.trends_cloud$date <= "2018-03-01",]

# Plot Forecast
google.trends_cloud_test %>%
  ggplot(aes(x = date, y = cloud_computing_word)) +
  # Training data
  geom_point(aes(colour = "Past WEB trend")) +
  geom_point(color = "red") +
  # Predictions2
  geom_point(data = predictions_tbl2_train, aes(y = value, colour = "Trend prediction T+90 months")) +
  geom_point(data = predictions_tbl2_train, aes(y = value), color = "green") +
  # Predictions actual 
  geom_point(data = predictions_tbl_train, aes(y = value, colour = "Trend prediction T-oneYear")) +
  geom_point(data = predictions_tbl_train, aes(y = value), color = "blue") +
  # Real actuals
  geom_point(data = actuals_tbl, aes(colour = "Real trend T-oneYear")) +
  geom_point(data = actuals_tbl, color = "orange") +
  # Hype cycle modelling
  geom_line(data= hype_cycle_data, aes(x, y), color="black") +
  # Aesthetics
  #scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme_light() +
  scale_colour_manual("", breaks = c("Past WEB trend",
                                     "Real trend T-oneYear",
                                     "Trend prediction T-oneYear",
                                     "Trend prediction T+90 months"), 
                          values = c("red",
                                     "orange",
                                     "blue",
                                     "green")
                      ) +
  labs(title = "Cloud computing WEB trend Forecast: Time Series Machine Learning",
       subtitle = "Using basic multivariate linear regression")


error_tbl <- left_join(actuals_tbl, predictions_tbl_train) %>%
  rename(actual = cloud_computing_word, pred = value) %>%
  mutate(
    error = actual - pred,
    error_pct = error / actual
  ) 

# Calculating test error metrics
test_residuals <- error_tbl$error
test_error_pct <- error_tbl$error_pct * 100 # Percentage error

# https://people.duke.edu/~rnau/compare.htm
me   <- mean(test_residuals, na.rm=TRUE)
# Mean squared error regression loss
rmse <- mean(test_residuals^2, na.rm=TRUE)^0.5
# Mean absolute error
mae  <- mean(abs(test_residuals), na.rm=TRUE)
# Mean absolute percentage error
mape <- mean(abs(test_error_pct), na.rm=TRUE)
# mean error
mpe  <- mean(test_error_pct, na.rm=TRUE)

tibble(me, rmse, mae, mape, mpe) %>% glimpse()



# Coerce
google.trends_cloud_xts <- tk_xts(google.trends_cloud_test) 
head(google.trends_cloud_xts)

tk_tbl(google.trends_cloud_xts, rename_index = "date")

# Coerce to ts
google.trends_cloud_ts <- tk_ts(google.trends_cloud_test, start = 2004, freq = 12)

tk_tbl(google.trends_cloud_ts, rename_index = "date")

# Check for timetk index. 
has_timetk_idx(google.trends_cloud_ts)

# If timetk_idx is present, can get original dates back 
tk_tbl(google.trends_cloud_ts, timetk_idx = TRUE, rename_index = "date")


# -----------------


#AI trend data
full_data_AI <- gtrends("artificial intelligence", gprop = "web", time = "all", hl = "en-US")

#get interest over time data
google.trends_AI <- full_data_AI[[1]]
google.trends_AI <- dcast(google.trends_AI, date ~ keyword + geo, value.var = "hits")
colnames(google.trends_AI) <- c("date", "ai_word")

#data viz Ai dataset
ggplot(google.trends_AI, aes(date, ai_word)) + geom_line(color="tomato") + theme_light() 


#data viz AI & Cloud correlated
ggplot(NULL) + 
  geom_line(data = google.trends_cloud, aes(date, cloud_computing_word, colour="cloud_computing")) +
  geom_line(data = google.trends_AI, aes(date, ai_word, colour="artificial_intelligence")) +
  scale_colour_manual("", breaks = c("cloud_computing", "artificial_intelligence"), values = c("blue", "red")) + 
  ggtitle("AI vs Cloud_computing trends") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  xlab("Date") + 
  ylab("Trend") +
  theme_light() 

#diff_data 
diff_data <- data.frame(google.trends_cloud$date, google.trends_cloud$cloud_computing_word, google.trends_AI$ai_word)
colnames(diff_data) <- c("date", "cloud", "ai")

periodOfAnalysis <- function(p1, p2){
  diff_data <- diff_data[diff_data$date >= p1 & diff_data$date <= p2,]
  
  return(
    ggplot(NULL) + 
      geom_line(data = diff_data, aes(date, cloud, colour = 'tech1')) +
      geom_line(data = diff_data, aes(date, ai, colour = 'tech2')) +
      scale_colour_manual("", breaks = c("tech1", "tech2"), values = c("red", "blue")) +
      theme_light() + 
      ggtitle("Tech1(arg1) vs Tech2(arg2)") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      xlab("Date") + 
      ylab("Trend")
  )
}

# comparing the tech
howsTheTrendiestTech <- function(tech1, tech2){
  
  tech1_sum <- sum(tech1)
  tech2_sum <- sum(tech2)
  
  if(tech1_sum > tech2_sum){
    return(print("the first arg is the trendiest"))
  } else {
    return(print("the second arg is the trendiest"))
  }
}

periodOfAnalysis("2004-01-01", "2010-01-01")
howsTheTrendiestTech(diff_data$cloud, diff_data$ai)


mean(google.trends_AI$ai_word)
mean(google.trends_cloud$cloud_computing_word)


#cloud computing analysis by country
google.trends_cloud_country <-  full_data_cloud[[2]]
google.trends_cloud_country <- dcast(google.trends_cloud_country, location ~ keyword + geo, value.var = "hits")
colnames(google.trends_cloud_country) <- c("location", "hits")

ggplot(google.trends_cloud_country, aes(x=reorder(location, hits), hits)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Cloud Computing hits by country") + 
  xlab("Country") + 
  ylab("hits") + 
  coord_flip()


#cloud computing analysis by country
google.trends_AI_country <-  full_data_AI[[2]]
google.trends_AI_country <- dcast(google.trends_AI_country, location ~ keyword + geo, value.var = "hits")
colnames(google.trends_AI_country) <- c("location", "hits")

ggplot(google.trends_AI_country, aes(x=reorder(location, hits), hits)) + 
  geom_bar(stat = "identity") +
  ggtitle("AI hits by country") +
  xlab("Country") + 
  ylab("hits") + 
  coord_flip()


# on map
#google.trends_AI_country$ll.visited <- geocode(google.trends_AI_country$location)

#google.trends_AI_country$ll.visited[1]$lon <- as.numeric(google.trends_AI_country$ll.visited[1]$lon)
#google.trends_AI_country$ll.visited[2]$lat <- as.numeric(google.trends_AI_country$ll.visited[2]$lat)

#leaflet(google.trends_AI_country) %>% addTiles() %>%
#  addAwesomeMarkers(~google.trends_AI_country$ll.visited[1]$lon, google.trends_AI_country$ll.visited[2]$lat, popup= google.trends_AI_country$hits)



#Cloud computing data Web versus News 
full_data_cloud_WebvsNews <- gtrends("cloud computing", gprop = "news", time = "all", hl = "en-US")

google.trends_cloud_WebvsNews <- full_data_cloud_WebvsNews[[1]]
google.trends_cloud_WebvsNews <- dcast(google.trends_cloud_WebvsNews, date ~ keyword + geo, value.var = "hits")
colnames(google.trends_cloud_WebvsNews) <- c("date", "cloud_computing_word")

#preprocessing (as cloud_computing_word is Characters)
google.trends_cloud_WebvsNews$cloud_computing_word <- sapply(google.trends_cloud_WebvsNews$cloud_computing_word, function(x) if(x == "<1"){x = "0"} else {x = x})
google.trends_cloud_WebvsNews$cloud_computing_word <- as.integer(google.trends_cloud_WebvsNews$cloud_computing_word)

VSdata <- data.frame(google.trends_cloud_WebvsNews$date, google.trends_cloud_WebvsNews$cloud_computing_word, google.trends_cloud$cloud_computing_word)
colnames(VSdata) <- c("date","news","web")

for(i in 1:nrow(VSdata)){
  result <- print(mean(VSdata[i,2] + VSdata[i,3])/2)
  VSdata$mo[i] <- result
}

#data viz Cloud dataset
ggplot(NULL) + 
  geom_bar(data=VSdata, aes(date, news), stat = "identity", fill="slategray1") + 
  geom_line(data=VSdata, aes(date, web), color="slateblue4") +
  geom_line(data=VSdata, aes(date, mo), color="orange") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Cloud computing trend Forecast: WEB vs NEWS") + 
  theme_light()


