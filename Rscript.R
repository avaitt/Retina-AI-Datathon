#' ---
#' title: "Datathon Submission Template"
#' author: "[Retina AI](https://retina.ai)"
#' date: '`r format(Sys.time(), "%d %B, %Y")`'
#' output: 
#'    html_document:
#'      df_print: paged
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#if your machine doesn't have these libraries, use 'install.packages(<package name>)' to install first before loading
library(glue)
library(tidyverse)
library(dplyr)
library(comprehenr)
library(zoo)
library(reshape2)
knitr::opts_chunk$set(echo = FALSE)

#' 
#' 
#' ## Retina AI R Datathon - Submission Template
#' 
#' *Team Name*: Triple A\
#' *Team Members*: Adhvaith Vijay, Andrew Liu, Anurag Pamuru\
#' *Team Members Email*: avijay42@ucla.edu, andrewl7127@ucla.edu, anuragpamuru0129@gmail.com\
#' *Designated Slides*: 14, 16\
#' 
## ---- include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data14 <- read.csv("chart_14.csv")
data16 <- read.csv("chart_16.csv")
monthly <- read.table("monthly_retention_table.csv",sep=",",header=T)
ltv <- read.csv("ltv_table.csv")

#' 
#' *Directions*: Write your text and codes for Task 1, 2 and 3 in the provided space below.\
#' 
#' Task 1 insights to the visualization can be written as plain text in the field provided below. You can also add new bullet points to each slide's insights section.\
#' 
#' For Task 2 and 3, provide your codes in the code chunks below. If your team needs additional code chunks to run your code, you can add new code chunks.
#' 
#' 
#' ## Task 1 (Required)
#' 
## ----message=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
convert <- function(x) {
  vector <- as.numeric()
  vector <- c(vector, x[1])
  for (i in seq_along(x)) {
    if (i != 1) {
      x[i] = x[i] - sum(vector)
      vector <- c(vector, x[i])
    }
  }
  vector
}

rows_of_interest1 <- which(data14$ï....of.Customers %in% c(20.0, 40.1, 59.9, 80.1, 100.0))
revenues <- data14$Revenue[rows_of_interest1]
print(convert(revenues))


rows_of_interest2 <- which(data14$ï....of.Customers %in% c(20.1, 40.0, 60.1, 79.9, 99.9))
LTVs <- data14$LTV[rows_of_interest2]
print(convert(LTVs))

#Graph to see missing values
g <- ggplot(data14, aes(ï....of.Customers)) + 
     geom_line(aes(y=LTV), colour="blue") + 
     geom_line(aes(y=Revenue), colour="purple")
g

# Impute NA values using backwards fill to ensure continuous distribution
data14$ï....of.Customers <- na.locf(data14$ï....of.Customers, fromLast = TRUE)
data14$LTV <- c(na.locf(data14$LTV, fromLast = TRUE), 100.00)
data14$Revenue <- na.locf(data14$Revenue, fromLast = TRUE)

# Graph with imputed values
g2 <- ggplot(data14, aes(ï....of.Customers)) + 
      geom_line(aes(y=LTV), colour="blue") + 
      geom_line(aes(y=Revenue), colour="purple")
g2

#' 
#' 
#' 
#' **Slide Number**: 14
#' 
#' 
#' ***Key Insights***: 
#' 
#' 
#' - **Groomer should target high spenders**
#' Groomer should focus their efforts on targeting demographics related to their top spenders when acquiring new customers. About 17% of their customers construct about 60% and 50% of their 5-year Lifetime Revenue and revenue respectively [Graph 14]. Their top 1% of customers bring in about 15% and 10% of their 5-year Lifetime Revenue and revenue respectively. We also divided the data into 5 quantiles and calculated the revenue for each quantile as follows: quantile 1 generated 54.2% of the revenue, quantile 2 generates 18.0% of the revenue, quantile 3 generates 11.1% of the revenue, quantile 4 generates 9.5% of the revenue, and quantile 5 generates 7.2% of the revenue. Once again, we see that the upper quantile of spenders creates the majority of the revenue.
#' 
#' - **Expected Spending Index**
#' We then created an approximate copy of the 5-year Lifetime revenue and revenue curves from Graph 14. We split the graph into 5 quantiles based on the percentage of customers and calculated an approximation of the area between the curves in each quantile to see how much customers were expected to spend in those 5 years (the difference between LTR and revenue). This metric is the Expected Spend Index (ESI) and a higher ESI denotes a quantile of spenders with a higher potential spending in the next 5 years. We found that the first 3 quartiles had the largest ESI while the groups with smallest expected spend were the last two with the last quarter having by far the smallest ESI. The exact results were as follows: quantile 1 had an ESI of 156.5, quantile 2 had an ESI of 175.7, quantile 3 had an ESI of 146.9, quantile 4 had an ESI of 99.9, and quantile 5 had an ESI of 38. As a result, we suggest that Groomer incentivize/market toward those who fall in the top 60% (people who have historically spent more on Groomer) with a focus on quantile 2.
#' 
#' *Slide Number*: 16
#' 
#' ***Key Insights***: 
#' 
#' - **Lifetime Revenue Weighed against Customer Acquisition Cost**
#' The 50th percentile of the LTRs in Slide 16, i.e. the median, is ```$84```, which means that Groomer’s average customer acquisition cost of ```$30``` is a good investment. The first quartile predicted 10-Year LTR of a customer is about ```$50```, and the third quartile predicted 10-Year LTR of a customer is about ```$168```. The median of the predicted 10-Year LTR plot is fairly close to the average order value per customer of ```$79.7``` provided in Groomer’s executive summary. Almost 9% of customers have a predicted 10-Year Lifetime Revenue of less than ```$30``` (Groomer’s average customer acquisition cost). That specific customer base makes up just 1% of Groomer’s total predicted 10-Year Lifetime Revenue. Therefore, Groomer should stop targeting that demographic in their marketing campaigns for new customers as it’s not worth it considering their customer acquisition cost.
#' 
#' - **High value of top spenders**
#' The high value of top spenders is reinforced by the curve of the cumulative distribution function of the percentage of customers being heavily right skewed with many positive outliers, which is shown by the early plateau (if the x axis is scaled properly). Furthermore, the average lifetime value provided to us in the QoC is ```$130```. Since this figure is higher than the median of ```$84```, this further reinforces that the dataset is right skewed. Considering that Groomer is considering to further improve customer retention by the following strategies: to acquire higher lifetime value customers at the outset, launching a loyalty program, or creating a subscription offering, we recommend that Groomer tries acquiring higher lifetime value customers at the outset since they make up such a large portion of their total expected revenue. For that same reason, we recommend against strategies such as creating subscription offerings since it may cause them to lose profits when it comes to their biggest spenders.
#' 
#' 
#' ## Task 2 (Required)
#' 
## ---- include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(glue)
library(tidyverse)
knitr::opts_chunk$set(echo = FALSE)

#' 
#' 
#' 
#' **Slide Number*: 14
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Enter your codes to automize insights for the first slide here
calculate_differences <- function(x) {
  vector <- as.numeric()
  vector <- c(vector, x[1])
  for (i in seq_along(x)) {
    if (i != 1) {
      x[i] = x[i] - sum(vector)
      vector <- c(vector, x[i])
    }
  }
  vector
}

closest <- function(v, target) {
  which.min(abs(v-target))
}

find_quantile_contributions <- function(feature_col, pct_col, bin_cnt) {
  #ideal bins
  bin_size = 100/bin_cnt
  bins <- seq(from = bin_size, to = 100, by = bin_size)
  #find closest non-NA bins to ideal bins
  non_null_pct <- pct_col[which(!is.na(feature_col))]
  quantile_limits <- non_null_pct[to_vec(for(i in bins) closest(non_null_pct, i))]
  quantile_limits <- feature_col[which(pct_col %in% quantile_limits)]
  #calculate bin differences
  calculate_differences(quantile_limits)
}

calc_area_between_curves_integrate <- function(x_axis, curve_low, curve_high, x_min = 0, x_max = 100) {
  x <- na.locf(x_axis, fromLast = TRUE)
  c_l <- na.locf(curve_low, fromLast = TRUE)
  c_h <- na.locf(curve_high, fromLast = TRUE)
  f1 = approxfun(x, c_h)
  AUC1 = integrate(f1, x_min, x_max, subdivisions = 1000)
  f2 = approxfun(x, c_l)
  AUC2 = integrate(f2, x_min, x_max, subdivisions = 1000)
  AUC1$value - AUC2$value
}

find_quantile_area_between_curves <- function(curve_low, curve_high, x, bin_cnt) {
  #ideal bins
  bin = 100/bin_cnt
  bins <- seq(from = bin, to = 100, by = bin)
  #find closest non-NA bins to ideal bins
  non_null_pct <- x[which(!is.na(curve_low))]
  quantiles <- non_null_pct[to_vec(for(i in bins) closest(non_null_pct, i))]
  #calculate area between curve in quantile
  for( i in 1:length(quantiles) ) {
    x_min <- quantiles[i] - bin
    x_max <- quantiles[i]
    area <- calc_area_between_curves_integrate(x, curve_low, curve_high, x_min, x_max)
    cat("Quantile", i, "has an ESI of", area, "\n")
  }
}

contributions <- find_quantile_contributions(data14$Revenue, data14$ï....of.Customers, 5)

plot_area_between_curves <- function(x_axis, curve_low, curve_high, x_min = 0, x_max = 100) {
  #impute missing values
  x <- na.locf(x_axis, fromLast = TRUE)
  c_l <- na.locf(curve_low, fromLast = TRUE)
  c_h <- na.locf(curve_high, fromLast = TRUE)
  #imputed dataframe
  df <- data.frame(x, c_l, c_h)
  blue<-rgb(0.8, 0.8, 1, alpha=0.25)
  clear<-rgb(1, 0, 0, alpha=0.0001)
  ggplot(df, aes(x=x, y=c_l)) + 
      geom_line(aes(y = c_l)) + 
      geom_line(aes(y = c_h)) +
      geom_ribbon(data=subset(df, x_min <= x & x <= x_max), 
            aes(ymin=c_l,ymax=c_h), fill="blue", alpha=0.5) +
      scale_y_continuous(expand = c(0, 0), limits=c(0,100)) +
      scale_x_continuous(expand = c(0, 0), limits=c(0,100)) + 
      scale_fill_manual(values=c(clear,blue))
}  
plot_area_between_curves(data14$ï....of.Customers, data14$Revenue, data14$LTV, 20, 40)

# Print automated insights. Use original .csv data from task 1 visualization. 

for (i in 1:length(contributions)) {
  cat("Quantile", i, "generates", contributions[i], "% of Revenue\n")
}

print("")

find_quantile_area_between_curves(data14$Revenue, data14$LTV, data14$ï....of.Customers, 5)


#' 
#' 
#' *Slide Number*: 16
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Enter your codes to automize insights for the second slide here
calculate_median_from_percentile_col_and_feature_col <- function(percentile_col, feature_col) {
  feature_col[closest(percentile_col, 50)]
}

# Print automated insights. Use original .csv data from task 1 visualization. 
calculate_median_from_percentile_col_and_feature_col(data16$cumpct_cust, data16$ï..Revenue.in.USD)

#' 
#' 
#' ## Task 3 (Bonus/Optional)
#' 
#' ***Key Insights***: 
#' 
#' - **Relationship between Time and Churn Rate**
#' The first graph was derived from the monthly retention data in the additional files. We aggregated the data based off of years and plotted the retention rates against the time (periods). We noticed that as time went on, people were more likely to churn. This is also shown in graph 27.
#' 
#' - **Relationship between Activity and Predicted Future CLV**
#' We then plotted another graph using the LTV data from the additional files. In this graph, we plotted the probability that customers were active against both 1 year and 5 year predicted CLV. We noticed that the more likely the customer was active, the higher the predicted 1 year and 5 year CLV. This indicated to us that more active members were more likely to spend on Groomer and have increased CLV. This is also shown in Graph 28.
#' 
#' *How to read this chart*:
#' 
#' - For the Predicted Future CLV Graph the y-axis is the probability that a customer is alive/active. The x-axis is their predicted future CLV. The blue color represents the 5 year predicted future CLV while the purple color represents 1 year predicted future CLV.
#' 
#' - For the Retention Rate vs. Time Graph the y-axis represents the Retention Rate and the x-axis represents the time in periods which is 'Months since the Cohort made their First Purchase'. The different colors of line each represent a different year from 2016 - 2020.
#' 
## ---- include=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = ltv, aes(x = probability_alive)) +
  geom_line(aes(y = predicted_future_clv_05yr, colour = "5yr")) +
  geom_line(aes(y = predicted_future_clv_01yr, colour = "1yr")) +
  scale_colour_manual("",
                      breaks = c("5yr", "1yr"),
                      values = c("dodgerblue3", "darkorchid1")) +
  xlab("Probability Alive") +
  scale_y_continuous("Predicted Future CLV") +
  labs(title="Predicted Future CLV vs. Probability Alive")

monthly$cohort_yearmonth <- lapply(monthly$cohort_yearmonth, as.character)
monthly$cohort_yearmonth <- substr(monthly[,'cohort_yearmonth'], 1, nchar(monthly[,'cohort_yearmonth'])-3)
monthly <- subset(monthly, select = -c(num_new_customers))
monthly <- aggregate(. ~ cohort_yearmonth, monthly, mean)
monthly <- subset(monthly, select = -c(cohort_yearmonth))
monthly <- as.data.frame(t(as.matrix(monthly)))
colnames(monthly)<- c("2016","2017","2018", "2019", "2020")
monthly <- cbind(Period = rownames(monthly), monthly)
monthly$Period <- gsub('period', '', monthly$Period)
monthly$Period <- as.numeric(as.character(monthly$Period))
rownames(monthly) <- NULL
monthly <- melt(monthly, id.vars="Period")
ggplot(monthly, aes(Period, value, col=variable)) +
  geom_point() +
  stat_smooth() +
  xlab('Time (Period)') +
  ylab('Retention Rate') +
  labs(color='Year') +
  ggtitle('Retention Rate vs. Time (Period)')

#' 
