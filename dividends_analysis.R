library(tidyquant)
library(riingo)
library(data.table)

# Get all stock symbols in a stock index
dow <- data.table(tq_index("DOW"))
nyse <- data.table(tq_exchange("NYSE"))
nasdaq <- data.table(tq_exchange("NASDAQ"))
ticks <- data.table(supported_tickers())

dow_t <- ticks[ticker %in% dow$symbol, ticker]
dow_p <- data.table(riingo_prices(ticker = dow_t, start_date = "2016-01-01", end_date = "2020-12-31"))
dow_p[, year := year(date)]
dow_p[, div_total := sum(divCash), by = c("ticker", "year")]
dow_p[, div_yield_percent := round(100*(div_total/close), digits = 2)]
dow_p[, div_max := max(div_yield_percent, na.rm = TRUE), by = c("ticker", "year")]
dow_p[divCash>0, ]


library(ggplot2)

ggplot(data = dow_p[ticker != "DOW" & div_max > 5], aes(x = date, y = div_yield_percent, colour = ticker)) +
  geom_line()

dow_p[div_max>5, ]



nyse_t <- ticks[ticker %in% nyse$symbol, ticker]
nyse_p <- data.table(riingo_prices(ticker = nyse_t, start_date = "2020-01-01", end_date = "2020-12-31"))
nyse_p[, year := year(date)]
nyse_p[, div_total := sum(divCash), by = c("ticker", "year")]
nyse_p[, div_yield_percent := round(100*(div_total/close), digits = 2)]
nyse_p[, div_max := max(div_yield_percent, na.rm = TRUE), by = c("ticker", "year")]
nyse_p[divCash>0, ]


library(ggplot2)

ggplot(data = nyse_p[ticker != "DOW" & div_max > 5], aes(x = date, y = div_yield_percent, colour = ticker)) +
  geom_line()

nyse_p[div_max>5, ]