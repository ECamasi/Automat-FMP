library(readr) 
library(dplyr) 
library(estimatr) 
library(ggplot2) 
library(lmtest)
library(Ecdat) 
library(plm) 
library(readxl) 
library(openxlsx)
library(tinytex)
library(sandwich)
library(gridExtra)
library(writexl)
library(stargazer)
library(httr)
library(jsonlite)
library(scatterplot3d)
library(rgl)
library(plotly)
library(plot3D)
library(knitr)



ask_api_key <- function() 
  {
  key <- Sys.getenv("FMP_API_KEY")
  if (key == "") 
    {
    if (!exists("global_api_key")) 
      {
      global_api_key <<- readline(" Inserisci la tua API Key: ")
      Sys.setenv(FMP_API_KEY = global_api_key)
    }
    return(global_api_key)
  }
  return(key)
}

## Questa funzione mi permette di generare un key globale nel caso in cui non esista
## La richiesta key verifica se esiste un valore "key", altrimenti genera un nuovo global key
## chiamando un readline che apparirà sulla console dell'usuario, richiedendo di inserire la sua key


FMP_CALL <- function(endpoint) 
  {
  api_key <- ask_api_key()
  url <- paste0("https://financialmodelingprep.com/api/v3/", endpoint, "?apikey=", api_key)
  res <- httr::GET(url)
  if (res$status_code == 200) 
    {
    content <- httr::content(res, as = "text")
    data <- fromJSON(content)
    return(data)
  } 
  else 
    {
    message(" Errore API: ", res$status_code)
    return(NULL)
  }
}

FMP_CALL("analyst-stock-recommendations/GOOGL")


## Avendo settato la key dell'usuario, questa funzione richiede di dare come input cosa si vuole
## avere dalla chiamata (es: key-metrics/META , profile/AAPL). Il condizionale if serve alla gestione
## di eventuali errori. Se la chiamata provoca una situazione diversa a code: 200, la funzione
## ritornerà errore nella chiamata


##Applicazione 1: FX Exchanges
head(FMP_CALL("fx"),10)

##Applicazione 2: Sorprese di GOOGL

GOOGL_SURPRISES<- FMP_CALL("earnings-surprises/GOOGL")
head(GOOGL_SURPRISES,10)

df<- as.data.frame(GOOGL_SURPRISES)
df$date <- as.numeric(as.POSIXct(df$date))

scatter3D(x = df$actualEarningResult, 
          y = df$estimatedEarning, 
          z = df$date, 
          pch = 16, 
          colvar = df$actualEarningResult,
          col = terrain.colors(100), 
          xlab = "Actual", 
          ylab = "Estimated", 
          zlab = "Date",
          main = "GOOGL SURPRISES",
          theta = 40, phi = 20, bty = "g")

##Applicazione 3: Discounted Cash Flow

CFS_data <- FMP_CALL("cash-flow-statement/MSFT")
fcf <- CFS_data$freeCashFlow
print(fcf)

key_metrics <- FMP_CALL("key-metrics/MSFT")
Fin_stat<- FMP_CALL("financial-statement-full-as-reported/MSFT")
sheets<-FMP_CALL("balance-sheet-statement/MSFT")
df<- cbind(key_metrics,Fin_stat,sheets)

beta <- 0.94
Rf <- 0.04   
Rm_minus_Rf <- 0.05 
tax_rate <- 0.21

Re <-c(Rf + beta * Rm_minus_Rf,Rf + beta * Rm_minus_Rf,Rf + beta * Rm_minus_Rf,Rf + beta * Rm_minus_Rf,Rf + beta * Rm_minus_Rf)
Rd <- df$interestexpense / df$totalDebt  
interest_expense <- df$interestexpense
total_debt <- df$totalDebt  
cash <- df$cashAndCashEquivalents  

D <- total_debt - cash  
E <- key_metrics$marketCap 

Ve <- E + D  
We <- E / Ve  
Wd <- D / Ve  

MSFT_WACC <- We * Re + Wd * Rd * (1 - tax_rate)

Anno <- c(2024, 2023, 2022, 2021, 2020)

df_MSFT_WACC <- data.frame(
  Anno = Anno,
  Costo_Capitale_Proprio = Re,
  Costo_Debito = Rd,
  Debito_Netto = D,
  Peso_Equity = We,
  Peso_Debito = Wd,
  WACC = MSFT_WACC
)

print(df_MSFT_WACC)

g<- mean(diff(log(rev(fcf))))

print(MSFT_WACC)
print(g)

g<- 0.06

calculate_terminal_value <- function(df, g) {
  FCF_n <- fcf[1]
  WACC_n <- MSFT_WACC[1]
  
  if (WACC_n <= g) {
    warning(" WACC value is lower than g")
    return(NA)
  }
  
  TV <- (FCF_n * (1 + g)) / (WACC_n - g)
  return(TV)
}

MSFT_TV<- calculate_terminal_value(df,g)
print(MSFT_TV)


calculate_dcf <- function(fcf, WACC, TV) {
  n <- length(fcf)
  discounted_fcf <- fcf / (1 + WACC)^(1:n)
  discounted_TV <- TV / (1 + WACC)^n
  dcf_value <- sum(discounted_fcf) + discounted_TV
  return(dcf_value)
}

MSFT_DCF <- calculate_dcf(fcf, MSFT_WACC, MSFT_TV)
MSFT_DCF <- as.numeric(MSFT_DCF)
MSFT_MK <- as.numeric(key_metrics$marketCap)

if (length(MSFT_DCF) == length(MSFT_MK)) {
  for (i in 1:length(MSFT_DCF)) {
    if (MSFT_DCF[i] > MSFT_MK[i]) {
      cat("Anno", i, "→ Microsoft è SOTTOVALUTATA secondo il DCF!\n")
    } else {
      cat("Anno", i, "→ Microsoft è SOPRAVVALUTATA secondo il DCF!\n")
    }
  }
} else {
  cat(" Errore: I vettori non hanno la stessa lunghezza!\n")
}


df_MSFT <- data.frame(
  Anno = seq(from = 2024, by = -1, length.out = length(MSFT_DCF)),  
  MSFT_DCF = round(MSFT_DCF, 2),  
  MSFT_Market_Cap = round(MSFT_MK, 2),
  Valutazione = ifelse(MSFT_DCF > MSFT_MK, "SOTTOVALUTATA", "SOPRAVVALUTATA")  
)


print(df_MSFT)


DCF_MS<- FMP_CALL("discounted-cash-flow/MSFT")

print(DCF_MS)

api_commands <- data.frame(
  Endpoint = c(
    "profile/****",
    "stock-screener",
    "cik-search/****",
    "market-capitalization/****",
    "grade/****",
    "key-executives/****",
    "analyst-estimates/****",
    "analyst-stock-recommendations/****",
    "is-the-market-open-all",
    "sectors-list",
    "industries-list",
    "quote/****",
    "quote-short/****",
    "quotes/****",
    "stock-price-change/****",
    "stock/full/real-time-price/****",
    "fx",
    "fx/******",
    "income-statement/****",
    "balance-sheet-statement/****",
    "cash-flow-statement/****",
    "financial-statement-full-as-reported/****",
    "key-metrics/****",
    "key-metrics-ttm/****",
    "ratios/****",
    "ratios-ttm/****",
    "cash-flow-statement-growth/****",
    "income-statement-growth/****",
    "balance-sheet-statement-growth/****",
    "financial-growth/****",
    "enterprise-values/****",
    "discounted-cash-flow/****",
    "rating/****",
    "historical-rating/****",
    "press-releases/****",
    "earning_call_transcript/****",
    "rss_feed",
    "sec_filings/****",
    "earnings-surprises/****",
    "historical-chart/*min/****",
    "etf-holder/****",
    "stock_market/gainers",
    "stock_market/losers",
    "institutional-holder/****",
    "symbol/available-commodities",
    "symbol/available-forex-currency-pairs",
    "quote/******",
    "symbol/available-cryptocurrencies",
    "quote/BTCUSD",
    "quote/****,****,****"
  ),
  Descrizione = c(
    "Company profile by ticker",
    "Top Companies screening by revenues, dividends, volume, industry, sector...",
    "Search by company name or part of it. Return cik number and name",
    "Total Market Capitalization by ticker",
    "Last rating by ticker. Premium needed",
    "Relevant executives of a company, by ticker. Premium needed",
    "Analyst estimates, by ticker. Premium needed",
    "Firms recommendations summary by ticker. Premium needed",
    "Opening and Closing hours for Stocks Exchanges.",
    "Sectors.",
    "Industries.",
    "Full stock quotes and info in real time, by ticker.",
    "Last price and volume of trading, by ticker.",
    "All stocks quotes and info in real time, by stock exchange tag. Premium needed",
    "Price change over 1D,5D,1M,3M,YTD,1Y,3Y,5Y,10Y,MAX. By ticker",
    "Real time, last bid-ask-price and volumes. By ticker",
    "Real time, all foreign exchange prices.",
    "Last bid-ask-price for a currency exchange. (EURUSD,USDEUR format)",
    "Last 5 official income statements, by ticker or cik.",
    "Last 5 official balance sheet statements, by ticker or cik.",
    "Last 5 official cash flow statements, by ticker or cik.",
    "Last 5 reported financial statements (income, balance and cash flow), by ticker or cik.",
    "Financial metrics for a single company, by ticker.",
    "Financial metrics for a single company TTM, by ticker.",
    "Financial ratios for a single company, by ticker.",
    "Financial ratios for a single company TTM, by ticker.",
    "Cash growth rate for a single company, by ticker.",
    "Income growth rate for a single company, by ticker.",
    "Balance sheet growth rate for a single company, by ticker.",
    "Financial growth rate for a single company, by ticker.",
    "Full total value of a company, by ticker.",
    "Last DCF valuation of a company, by ticker.",
    "Company rating based of financial ratios and indicators, by ticker.",
    "Day by Day ratings, by ticker.",
    "Latest press releases from a company, by ticker. Premium needed",
    "Latest earning call transcript for a specific company, by ticker. Premium needed",
    "Latest SEC filings from public companies.",
    "Latest SEC filings from a specific company, by ticker.",
    "Latest earnings surprises from a specific company, by ticker.",
    "Intraday prices for 1-5-15-60 minutes for a specific company, by ticker.",
    "ETF composition, by ticker. Premium needed",
    "Top 50 gainers",
    "Top 50 losers",
    "Most relevant holders, by ticker. Premium needed",
    "Commodities traded around the stock markets.",
    "Full list of all currency pairs traded",
    "Real time quote for a specific pair of currencies (EURUSD,USDEUR format)",
    "Full list of all cryptocurrencies traded.",
    "Real time quote for a specific pair of crypto-fiat (ETHUSD format only)",
    "Real time multiple companies quotes and ratios, by ticker."
  )
)




kable(api_commands, booktabs = TRUE, caption = "Lista dei comandi v. 1.0")


