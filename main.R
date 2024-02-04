library(quantmod)
library(ggplot2)
library(systemfit)
library(AER)
library(tseries)


# Function which downloads OHLC data from Yahoo Finance
download_yahoo <- function(symbol, from, to = Sys.Date(), filepath) {
  if (file.exists(filepath)) {
    return(read.csv(filepath))
  }
  # Package Check
  if (!requireNamespace("quantmod", quietly = TRUE)) {
    stop("Package 'quantmod' must be installed. Use install.packages('quantmod').")
  }
  
  # Download data from yahoo finance
  tryCatch({
    getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = TRUE)
    if (grepl("^", symbol)){
      data <- get(sub("^.", "", symbol))
    }
    else{
      data <- get(symbol)
    }
    # save data to csv
    write.zoo(data, file = filepath, sep = ",")
    message("Data were save to: ", filepath)
    return(data)
  }, error = function(e) {
    stop("The error occured: ", e$message)
  })
}


garman_klass <- function(high, low, close, open) {
  log_hl <- (log(high / low))^2
  log_co <- (log(close / open))^2
  
  return(sqrt(0.5 * log_hl - (2 * log(2) - 1) * log_co))
}

add_non_continuous <- function(data, other, name) {
  merged_data <- merge(data, other, by = "date", all = TRUE)
  merged_data[name] <- na.locf(merged_data$adjusted.y, na.rm = FALSE)
  stopifnot(dim(data)[1] == dim(merged_data)[1])
  data[name] <- merged_data[name]
  # Since 2020-01-01 is weekend we should replace na in vix, with value from 
  # 2020-01-02.
  data[1, name] = data[2, name]
  return(data)
}

### Pipeline ###

# Daily bitcoin data
btc_data <- download_yahoo("BTC-USD", "2020-01-01", "2023-12-31", "data/btc_data.csv")
# Rename dataframe column names
names(btc_data) = c("date", "open", "high", "low", "close", "volume", "adjusted")
# Download googletrends searches about the keyword 'bitcoin'.
if (!file.exists("data/btc_googletrends.csv")){
  system2("python", args = "google_queries.py")
}
google_searches = read.csv("data/btc_googletrends.csv")
stopifnot(dim(google_searches)[1] == dim(btc_data)[1])
btc_data$google_searches =  google_searches$bitcoin
vix_data = download_yahoo("^VIX", "2020-01-01", "2023-12-31", "data/vix.csv")
names(vix_data) = c("date", "open", "high", "low", "close", "volume", "adjusted")
sp500_data = download_yahoo("^GSPC", "2020-01-01", "2023-12-31", "data/sp500.csv")
names(sp500_data) = c("date","open", "high", "low", "close", "volume", "adjusted")
btc_data$date <- as.Date(btc_data$date)
vix_data$date <- as.Date(vix_data$date)
btc_data = add_non_continuous(btc_data, vix_data, "vix")
btc_data = add_non_continuous(btc_data, sp500_data, "sp500")
btc_data$sigma =  apply(btc_data[, c("high", "low", "close", "open")], 1,
                        function(x) garman_klass(x[1], x[2], x[3], x[4]))
btc_data$volume_btc = btc_data$volume / btc_data$close

head(btc_data)
ggplot(btc_data, aes(date, log(close))) + 
  geom_line() +
  labs(x="Date", y="log(close)", title="BTC Closing Prices Over Time")
ggplot(btc_data, aes(date, log(sigma))) + 
  geom_line() +
  labs(x="Date", y="log(sigma)", title="BTC Volatility Over Time")

ggplot(btc_data, aes(date, sigma)) + 
  geom_line() +
  labs(x="Date", y="sigma", title="BTC Volatility Over Time")

plot(btc_data$close)

### Modelling ###
model_ols = lm(log(sigma) ~ log(close) + log(vix) + log(google_searches) + log(volume),
                     data=btc_data)

## Check relevancy of instruments
cor(log(btc_data$close), log(btc_data$sp500))

model_2sls = ivreg(log(sigma) ~ log(close) + log(vix) + log(google_searches) + log(volume) | 
                     log(vix) + log(google_searches) + log(volume) +log(sp500), data = btc_data)

# Hausman
hausman.systemfit(model_2sls,model_ols) # Reject the NULL hypothesis -> 
  # Reject the exogeneity of close
model = model_2sls
summary(model, diagnostic=TRUE)

#H_0 = unit_root
adf.test(model$residuals)

# KPSS test
kpss.test(model$residuals)

## Test heteroscedasticity

# Breusch-Pagan test
bptest(model, data = btc_data)
# heteroscedasticity detected - we should use heteroscedasticity robust errors.

## Test Autocorelation

# Durbin-Watson test
dwtest(model, data = btc_data)

# Test normality of residuals
shapiro.test(model$residuals)

# heteroscedasticity and autocorrelation errors.
coeftest(model, vcov = vcovHAC(model, type="HC3"))
