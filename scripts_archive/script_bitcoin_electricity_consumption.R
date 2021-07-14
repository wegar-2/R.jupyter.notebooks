library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)
library(scales)
library(hrbrthemes)
library(TTR)
library(zoo)

options(repr.plot.width=4, repr.plot.height=3)

# 1. load the data on annualized bitcoin electricity consumption
dtBitcoin <- data.table::fread(
  input = "data/bitcoin_electricity_consumption_index_by_digiconomist.csv",
  colClasses = c("Date", "double", "double"))
data.table::setnames(x = dtBitcoin, old = "Estimated TWh per Year", 
                     new = "estimated_annualized_bitcoin_pwr_cons_twh")
dtBitcoin <- dtBitcoin[, list(Date, estimated_annualized_bitcoin_pwr_cons_twh)]
dtBitcoin[, date_year := lubridate::year(x = Date)]

# 2. load the data on world electricity generation based on BP 
dtWorldElectGen <- data.table::fread(
  file = "data/bp_electricity_generation_world_terawatthours.csv",
  colClasses = c("integer", "double"))

# 3. join the data
dtBitcoinShare <- data.table::merge.data.table(
  x = dtBitcoin, y = dtWorldElectGen,
  by.x = "date_year", by.y = "data_year",
  all.x = T, all.y = F)
# use yearly 2020 power generation for Y2021 dates
dtBitcoinShare[, electricity_generation_total_world_terawatthours := 
                 zoo::na.locf0(electricity_generation_total_world_terawatthours, fromLast = F)]
dtBitcoinShare <- dtBitcoinShare[, date_year := NULL]
dtBitcoinShare[, bitcoin_share := estimated_annualized_bitcoin_pwr_cons_twh / 
                 electricity_generation_total_world_terawatthours]

# 4. plot the bitcoin share of electricity usage
ggplot(data = dtBitcoinShare, mapping = aes(x = Date, y = bitcoin_share)) +
  geom_line() + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  xlab("Date") +
  ylab("Percent") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month") +
  ggtitle(
    label = "Approx. Bitcoin mining's usage of World's electricity generation", 
    subtitle = "Time series is calculated as ratio of Digiconomist's BECI index to yearly electricity generation in BP's Statistical Review 2021"
  ) +
  theme(plot.title = element_text(size = 14), plot.subtitle = element_text(size = 8))

# 5. world electricity generation - a tiny bit more of analysis
dtWorldElectGen[
  , electricity_generation_total_world_terawatthours_ma7y := 
    TTR::SMA(
      x = dtWorldElectGen$electricity_generation_total_world_terawatthours, n = 7)]
data.table::setnames(
  x = dtWorldElectGen,
  old = c("electricity_generation_total_world_terawatthours", "electricity_generation_total_world_terawatthours_ma7y"),
  new = c("generation", "generation_7y_ma")
)
dtMeltedWorldElectGen <- data.table::melt.data.table(
  data = dtWorldElectGen,
  id.vars = "data_year",
  measure.vars = c("generation", "generation_7y_ma"),
  variable.name = "which_variable",
  value.name = "variable_value"
)
# 5.2. plot
ggplot(data = dtMeltedWorldElectGen, 
       mapping = aes(x = data_year, y = variable_value, color = which_variable)) +
  geom_line() + geom_point() +
  theme_ipsum() + 
  ggtitle(
    label = "World's yearly electricity generation in TWH",
    subtitle = "Based on BP's Statistical Review 2021 [spreadsheet data sheet 'Electricity Generation']"
  ) +
  xlab("Year") + 
  ylab("Electricity generation in TWh") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) + 
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(
    breaks = seq(1990, 2021, 5)
  ) + scale_color_discrete(
    name = "Legend: ",
    labels = c("yearly generation", "7-year average of yearly generation")
  )

# 6. include Bitcoin price data in the analysis
# 6.1. load the prices and get a glance of their evolution since 2015
dtBitcoinPrices <- data.table::fread(file = "data/btcusd_d.csv", select = c("Date", "Close"))
dtBitcoinPrices <- dtBitcoinPrices[lubridate::year(Date) >= 2015, ]
colnames(dtBitcoinPrices) <- c("quote_date", "bitcoin_price_USD")
dtBitcoinPrices <- dtBitcoinPrices[
  , `:=`(
    logret_bitcoin_price = c(NA, diff(log(dtBitcoinPrices$bitcoin_price_USD))),
    sq_logret_bitcoin_price = logret_bitcoin_price^2
  )
]
ggplot(data = dtBitcoinPrices, mapping = aes(x = quote_date, y = logret_bitcoin_price)) +
  geom_line() + theme_bw() + 
  xlab("Quote date") + 
  ylab("Daily log-return") +
  scale_x_date(
    date_labels = "M%mY%Y",
    date_breaks = "12 months"
  ) + 
  ggtitle(
    label = "Daily log-returns of Bitcoin close price, 2015-2021",
    subtitle = "Based on Stooq.com data"
  ) + scale_y_continuous(labels = scales::percent)
ggplot(data = dtBitcoinPrices, mapping = aes(x = quote_date, y = sq_logret_bitcoin_price)) +
  geom_line() + theme_bw() + 
  xlab("Quote date") + 
  ylab("Squared daily logreturn") +
  scale_x_date(
    date_labels = "M%mY%Y",
    date_breaks = "12 months"
  ) + 
  ggtitle(
    label = "Daily squared log-returns of Bitcoin close price, 2015-2021",
    subtitle = "Based on Stooq.com data"
  )
ggplot(data = dtBitcoinPrices, mapping = aes(x = logret_bitcoin_price)) +
  geom_histogram(
    bins = 100, 
    aes(y = ..count../sum(..count..)),
    color="steelblue", 
    fill = "white") + 
                   theme_bw() +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(
    labels = scales::percent,
    breaks = seq(-0.3, 0.2, 0.05)
  ) + 
  xlab(label = "") + 
  ylab(label = "")

# 6.3. compare the monthly avg Bitcoin prices and BECI index
# 6.3.1. prepare the data
dtBitcoinPricesMonthly <- dtBitcoinPrices[
  lubridate::year(quote_date) >= 2017, 
  list(avg_monthly_price = mean(bitcoin_price_USD)),
  by = zoo::as.yearmon(x = quote_date)]
dtPricesMining <- data.table::merge.data.table(
  x = , 
  y = ,
  by.x = ,
  by.y = ,
  all.x = 
)
# 6.3.2. make the comparative plot
