library(tidyverse)
library(data.table)
library(magrittr)
library(lubridate)
library(zoo)
library(scales)

# 0. loading package with data - from non-CRAN package;-------------------------
#    repo here: https://github.com/wegar-2/euribor.historical.data
library(euribor.historical.data)
dtData <- data.table::copy(dtEuribor)
View(head(dtData))
View(tail(dtData))
dtMeltedData <- data.table::melt.data.table(
  data = dtData, id.vars = "quote_date", measure.vars = colnames(dtData)[2:6],
  variable.name = "which_euribor", value.name = "euribor_value")
dtMeltedData$euribor_value <- dtMeltedData$euribor_value/100


# 1. start with line plot
plotHistory <- ggplot(data = dtMeltedData,
       mapping = aes(x = quote_date, y = euribor_value, color = which_euribor)) +
  geom_line() + theme_bw() +
  theme(legend.position = "bottom") +
  xlab(label = "Quote date") + ylab(label = "EURIBOR rate value") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle(label = "EURIBOR rates - historical values",
          subtitle = "EMMI data, 1998-2021") +
  scale_color_discrete(
    name = "EURIBOR rates: ",
    labels = c("1 week", "1 month", "3 months", "6 months", "12 months")
  ) + scale_x_date(breaks = "3 years") +
  theme
plotHistory

# 2. inspect more closely period before Financial Crisis 2007-2008 -------------
# definded as time before year 2007
dtMeltedDataBeforeCrisis <- dtMeltedData[lubridate::year(quote_date) < 2007, ]
plotHistoryBeforeCrisis <- ggplot(data = dtMeltedDataBeforeCrisis,
                      mapping = aes(x = quote_date, y = euribor_value, color = which_euribor)) +
  geom_line() + theme_bw() +
  theme(legend.position = "bottom") +
  xlab(label = "Quote date") + ylab(label = "EURIBOR rate value") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle(label = "EURIBOR rates - years 1998-2006 [before Financial Crisis]",
          subtitle = "EMMI data") +
  scale_color_discrete(
    name = "EURIBOR rates: ",
    labels = c("1 week", "1 month", "3 months", "6 months", "12 months")
  ) + scale_x_date(breaks = "1 year")
plotHistoryBeforeCrisis

# 3. inspect more closely period of Financial Crisis 2007-2009 -----------------
dtMeltedDataDuringCrisis <- dtMeltedData[lubridate::year(quote_date) %in% c(2007, 2008, 2009), ]
plotHistoryDuringCrisis <- ggplot(data = dtMeltedDataDuringCrisis,
                                  mapping = aes(x = quote_date, y = euribor_value, color = which_euribor)) +
  geom_line() + theme_bw() +
  theme(legend.position = "bottom") +
  xlab(label = "Quote date") + ylab(label = "EURIBOR rate value") +
  scale_y_continuous(labels = scales::percent, limits = c(0.00, 0.06),
                     breaks = seq(0.00, 0.06, 0.01)) +
  ggtitle(label = "EURIBOR rates - years 2007-2009 [during Financial Crisis]",
          subtitle = "EMMI data") +
  scale_color_discrete(
    name = "EURIBOR rates: ",
    labels = c("1 week", "1 month", "3 months", "6 months", "12 months")
  ) + scale_x_date(breaks = "6 months")
plotHistoryDuringCrisis

# 3. inspect more closely period of European Debt Crisis 2010-2013 -------------
dtMeltedDataDuringEuroDebtCrisis <-
  dtMeltedData[lubridate::year(quote_date) %in% c(2010, 2011, 2012, 2013), ]
plotHistoryDuringEuroDebtCrisis <- ggplot(
  data = dtMeltedDataDuringEuroDebtCrisis,
  mapping = aes(x = quote_date, y = euribor_value, color = which_euribor)) +
  geom_line() + theme_bw() +
  theme(legend.position = "bottom") +
  xlab(label = "Quote date") + ylab(label = "EURIBOR rate value") +
  scale_y_continuous(labels = scales::percent, limits = c(0.00, 0.0225),
                     breaks = seq(0.00, 0.0225, 0.005)) +
  ggtitle(label = "EURIBOR rates - years 2010-2013 [during European Debt Crisis]",
          subtitle = "EMMI data; rate 0.0% marked with dashed black line") +
  scale_color_discrete(
    name = "EURIBOR rates: ",
    labels = c("1 week", "1 month", "3 months", "6 months", "12 months")
  ) + scale_x_date(breaks = "8 months") +
  geom_hline(yintercept = 0.00, color = "black", linetype = "dashed", size = 1.2)
plotHistoryDuringEuroDebtCrisis

# 4. inspect more closely period since 2014 ------------------------------------
dtMeltedDataSince2014 <- dtMeltedData[lubridate::year(quote_date) >= 2014, ]
plotHistorySince2014 <- ggplot(
  data = dtMeltedDataSince2014,
  mapping = aes(x = quote_date, y = euribor_value, color = which_euribor)) +
  geom_line() + theme_bw() +
  theme(legend.position = "bottom") +
  xlab(label = "Quote date") + ylab(label = "EURIBOR rate value") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle(label = "EURIBOR rates - since 2014",
          subtitle = "EMMI data; rates 0.6%, 0.0%, -0.6% marked with dashed balck lines") +
  scale_color_discrete(
    name = "EURIBOR rates: ",
    labels = c("1 week", "1 month", "3 months", "6 months", "12 months")
  ) + scale_x_date(breaks = "1 year") +
  geom_hline(yintercept = c(-0.006, 0.00, 0.006), color = "black", linetype = "dashed", size = 1.0)
plotHistorySince2014

# 5. analysis of the World Financial Crisis (WFC) period - boxplots ------------
dtWfcData <- dtData[lubridate::year(quote_date) %in% c(2007, 2008, 2009), ]
dtMeltedWfcData <- data.table::melt.data.table(
  data = dtWfcData, id.vars = "quote_date", measure.vars = colnames(dtWfcData)[2:6],
  variable.name = "which_euribor", value.name = "euribor_value")
dtMeltedWfcData$euribor_value <- dtMeltedWfcData$euribor_value/100
plotBoxplotWfc <- ggplot(data = dtMeltedWfcData,
       mapping = aes(x = which_euribor, y = euribor_value, color = which_euribor)) +
  geom_boxplot() + theme_bw() +
  scale_x_discrete(
    name = "Which EURIBOR rate", labels = c("1 week", "1 month", "3 months", "6 months", "12 months")) +
  scale_y_continuous(labels = scales::percent, name = "EURIBOR value") +
  ggtitle(
    label = "Box plots of levels of EURIBOR rates in years 2007-2009",
    subtitle = "Note: this boxplot is NOT based on changes of rates but on their levels; crosshairs denote sample means"
  ) + stat_summary(fun = mean, geom = "point", shape = 10, size = 8) +
  theme(legend.position = "none")
plotBoxplotWfc

# 6. counts of observations by year --------------------------------------------
# 6.1. quick overview of obs counts by year
dtData[, which_year := lubridate::year(quote_date)]
dtObsByYear <- dtData[, .N, by = which_year]
View(dtObsByYear)
plotObsCounts <- ggplot(data = dtData, mapping = aes(y = which_year)) +
  geom_bar(color = "steelblue", fill = "white") + theme_bw() +
  scale_y_continuous(breaks = seq(1998, 2021, 1)) +
  ylab(label = "Which year") +
  xlab(label = "Number of observations") +
  ggtitle(label = "Count of observations in the data set by year, 1998-2021")
plotObsCounts
# dropping the data for years 1998, 2021


# 7. calculate and plot selected statistic over time ---------------------------
# 7.1. calculate summary stats in a long format
dtData <- data.table::copy(dtEuribor)
dtData[, which_year := lubridate::year(quote_date)]
dtData <- dtData[!(which_year %in% c(1998, 2021)), ]
cEuriborCols <- colnames(dtData)[2:6]
dtData[, (cEuriborCols) := lapply(X = .SD, function(x) x/100 ), .SD = cEuriborCols]
dtMaxSumm <- dtData[, c(lapply(X = .SD, FUN = max), list(which_stat = "max")),
  by = which_year, .SDcols = cEuriborCols]
dtMinSumm <- dtData[, c(lapply(X = .SD, FUN = min), list(which_stat = "min")),
                    by = which_year, .SDcols = cEuriborCols]
dtMedianSumm <- dtData[, c(lapply(X = .SD, FUN = median), list(which_stat = "median")),
                       by = "which_year", .SDcols = cEuriborCols]
dtSumm <- rbindlist(l = list(dtMaxSumm, dtMinSumm, dtMedianSumm))
# 7.2. plot the stats for 6M euribor by year
plotDescStats <-
  ggplot(data = dtSumm, mapping = aes(x = which_year, y = euribor_6m, color = which_stat)) +
  geom_line() + geom_point() + theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  xlab("Year") + ylab("EURIBOR rate value") +
  scale_y_continuous(labels = scales::percent, breaks = seq(-0.005, 0.05, 0.005)) +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  ggtitle(
    label = "Descriptive stats of 6M EURIBOR by year"
  ) + scale_color_discrete(
    name = "Which statistic: ", labels = c("maximum", "median", "minimum")) +
  geom_hline(yintercept = 0.00, linetype = "dashed", color = "black", size = 1)
plotDescStats

# 8. analysis of changes of the rates ------------------------------------------
# 8.1. calculate daily increments of the rates
View(head(dtData))
dtDeltasData <- dtData[, c(list(quote_date = dtData$quote_date), stats::setNames(
  object = lapply(X = .SD, FUN = function(x) {
    c(NA, diff(x))
  }), nm = paste0("delta_", cEuriborCols)))
, .SD = cEuriborCols]
cEuriborDeltasCols <- paste0("delta_", cEuriborCols)
# 8.2. split data set into short-term and mid-term data
dtShortRatesDeltasData <-
  dtDeltasData[, .(quote_date, delta_euribor_1w, delta_euribor_1m,
                   delta_euribor_3m)]
dtMidRatesDeltasData <-
  dtDeltasData[, .(quote_date, delta_euribor_6m, delta_euribor_12m)]
# 8.3. histogram for mid-term rates deltas
dtMeltedMidRatesDeltasData <- data.table::melt.data.table(
  data = dtMidRatesDeltasData, id.vars = "quote_date",
  measure.vars = c("delta_euribor_6m", "delta_euribor_12m"),
  variable.name = "which_euribor", value.name = "euribor_value"
)
View(head(dtMeltedMidRatesDeltasData, 20))
ggplot(data = dtMeltedMidRatesDeltasData, mapping = aes(x = euribor_value,
                                                        fill = which_euribor)) +
  geom_histogram(mapping = aes(y = ..count../sum(..count..)),
    bins = 50, alpha = 0.25, color = "black", size = 0.2) + theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0.00, 0.6, 0.1)) +
  theme(legend.position = "bottom") +
  ggtitle(
    label = "",
    subtitle = ""
  ) + xlab("EURIBOR rate DtD change") + ylab(label = "share of the sample")



# 9. measures of variability of changes of absolute changes --------------------
#    of 12M EURIBOR rates over the years
# a) standard deviation of changes
# b) mean absolute deviation of changes
# c) median of absolute changes
# d) interquartile range
dtYearlyRates <- dtDeltasData[, list(quote_date, delta_euribor_12m)]
dtYearlyRates[, which_year := lubridate::year(quote_date)]
dtYearlyRatesSumm <- dtYearlyRates[, list(
  deltas_avg_abs_dev_around_zero = mean(abs(delta_euribor_12m), na.rm = T) * 1e4,
  deltas_std = sd(delta_euribor_12m, na.rm = T) * 1e4,
  deltas_abs_median = median(abs(x = delta_euribor_12m), na.rm = T)* 1e4,
  deltas_iqr = IQR(delta_euribor_12m, na.rm = T)  * 1e4
), by = which_year]

# 10. visual check for autocorrelation of daily changes ------------------------
#
