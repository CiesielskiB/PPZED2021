prepareLibs <- function(){
  library(tidyverse)
  library(readxl)
}

replaceNaWithClosest <- function(df, col){
  col_values <- df[col]
  na_cols <- which(is.na(col_values))
  for(i in na_cols){
    right_not_nan_index <- min(which(!is.na(col_values[i:nrow(col_values),]))) + i-1
    if(i == 1){
      col_values[i, ] = col_values[right_not_nan_index,]
    }else{
      left_not_nan_index <- max(which(!is.na(col_values[1:i,])))
      if(right_not_nan_index != Inf){
        col_values[i, ] = round((col_values[right_not_nan_index,] + col_values[left_not_nan_index,])/2, digits = 4)
      }else{
        col_values[i, ] = col_values[left_not_nan_index,]
      }
    }
  }
  col_values
}



replaceNaInGoldPrices <- function(df){
  
  
  
  na_cols_am <- which(is.na(df["USD_AM"]))
  na_cols_pm <- which(is.na(df["USD_PM"]))
  for(i in na_cols_am){
    
    right_not_nan_index <- min(which(!is.na(df["USD_PM"][i:nrow(col_values),]))) + i-1
    
    if(i == 1){
      col_values[i, ] = col_values[right_not_nan_index,]
    }else{
      left_not_nan_index <- max(which(!is.na(col_values[1:i,])))
      if(right_not_nan_index != Inf){
        col_values[i, ] = round((col_values[right_not_nan_index,] + col_values[left_not_nan_index,])/2, digits = 4)
      }else{
        col_values[i, ] = col_values[left_not_nan_index,]
      }
    }
  }
  col_values
}

loadCurrencyExchangeRates <- function(csvFilePath){
  
  currency <- tbl_df(read.csv(csvFilePath, 
                          header = T, 
                          sep=",", 
                          colClasses = c(c("character"), rep("numeric", 51))
                          )
                 )
  currency <- currency %>% mutate(Date =  as.Date(Date,format="%Y-%m-%d"))

  currency
}

loadGoldPrices <- function(csvFilePath){
  
  data <- tbl_df(read.csv(csvFilePath, 
                          skip = 1, 
                          col.names = c("Date", "USD_AM", "USD_PM", "GBP_AM", "GBP_PM", "EURO_AM", "EURO_PM"), 
                          sep=",", 
                          colClasses = c(c("character"), rep("numeric", 6))
                          )
                 )

  fixedData <- data %>% mutate(Date =  as.Date(Date,format="%Y-%m-%d")) 

  fixedData
}

loadSPComposite <- function(csvFilePath){
  
  data <- tbl_df(read.csv(csvFilePath, 
                          header = TRUE, 
                          sep=",", 
                          colClasses = c(c("character"), rep("numeric", 9))))
  fixedData <- data %>%
    rename(Date = Year ) %>%
    mutate(Date = as.Date(Date,format="%Y-%m-%d"))
  
  fixedData
}

loadWorldDevelopmentIndicators <- function(excelFilePath){
  
  data <- read_excel(excelFilePath)
  fixedData <- rename_with(data,  .cols = 5:ncol(data), .fn = substr, 1, 4) %>%
    gather("year", "World Development Indicator", 5:ncol(data)) %>%
    mutate(`World Development Indicator` = as.numeric(`World Development Indicator`)) %>%
    mutate(year = as.integer(year))
  fixedData
}

loadBitcoinData <- function(csvBaseFilePath, DiffFileName, HRateFileName, MKPRUFileName, TRVOUFileName){
  
  Diff <- tbl_df(read.csv(paste(csvBaseFilePath, DiffFileName, sep="/"))) %>% rename( Difficulty = Value)
  HRate <- tbl_df(read.csv(paste(csvBaseFilePath, HRateFileName, sep="/"))) %>% rename(HashRate = Value)
  MKPRUF <- tbl_df(read.csv(paste(csvBaseFilePath, MKPRUFileName, sep="/"))) %>% rename(MarketPrice = Value)
  TRVOU <- tbl_df(read.csv(paste(csvBaseFilePath, TRVOUFileName, sep="/"))) %>% rename(TradeVolume = Value)
  
  bitcointData <- Diff %>% inner_join(HRate, by="Date") %>% 
    inner_join(MKPRUF, by="Date") %>% 
    inner_join(TRVOU, by="Date") %>%
    mutate(Date = as.Date(Date,format="%Y-%m-%d"))
  
  write.csv(bitcointData, "../data/prepared/Bitcoin.csv", row.names = F)
  bitcointData
}

readRawFiles <- function(currencyExchangeRatesPath = "../data/raw/CurrencyExchangeRates.csv", 
                         goldPricesPath = "../data/raw/Gold prices.csv", 
                         spCompositePath = "../data/raw/S&P Composite.csv", 
                         worldDevelopmentIndicatorsPath = "../data/raw/World_Development_Indicators.xlsx",
                         bitcoinBasePath = "../data/raw/Bitcoin",
                         DiffFileName = "BCHAIN-DIFF.csv",
                         HRateFileName = "BCHAIN-HRATE.csv",
                         MKPRUFileName = "BCHAIN-MKPRU.csv",
                         TRVOUFileName = "BCHAIN-TRVOU.csv"){
  prepareLibs()
  list(currency = loadCurrencyExchangeRates(currencyExchangeRatesPath),
       gold = loadGoldPrices(goldPricesPath),
       sp = loadSPComposite(spCompositePath),
       wdi = loadWorldDevelopmentIndicators(worldDevelopmentIndicatorsPath),
       bitcoin = loadBitcoinData(bitcoinBasePath, DiffFileName, HRateFileName, MKPRUFileName, TRVOUFileName))
}

runFilePreparation <- function(data){
  #remove cols with too many NA values and replace NA in currency dataset
  data$currency <- data$currency %>% 
    filter(as.numeric(format(Date ,'%Y')) >= as.numeric(format(min(data$bitcoin$Date),'%Y')))
  
  number_of_rows = nrow(data$currency)
  passing_cols = c()
  for(col in colnames(select(data$currency, -Date))){
    if(number_of_rows*0.35 > sum(is.na(data$currency[col]))){
      passing_cols <- append(passing_cols, col) 
    }else {
      data$currency[col] <- replaceNaWithClosest(data$currency, col)
    }
  }
  data$currency <- select(data$currency, Date, passing_cols)

  #fix gold prices to have daily mean and remove NA
  data$gold <- data$gold %>%
    mutate("USD_Mean" = (USD_AM + USD_PM)/2 ) %>%
    mutate("GBP_Mean" = (GBP_AM + GBP_PM)/2 ) %>%
    select(Date, USD_Mean, GBP_Mean)
  data$gold["USD_Mean"] <- replaceNaWithClosest(data$gold, "USD_Mean")
  data$gold["GBP_Mean"] <- replaceNaWithClosest(data$gold, "GBP_Mean")
  
  #repace NA in sp
  data$sp["Dividend"] <- replaceNaWithClosest(data$sp, "Dividend")
  data$sp["Earnings"] <- replaceNaWithClosest(data$sp, "Earnings")
  data$sp["Real.Dividend"] <- replaceNaWithClosest(data$sp, "Real.Dividend")
  data$sp["Real.Earnings"] <- replaceNaWithClosest(data$sp, "Real.Earnings")
  data$sp["Cyclically.Adjusted.PE.Ratio"] <- replaceNaWithClosest(data$sp, "Cyclically.Adjusted.PE.Ratio")
  
  #custom wdi clean method

  data$seriesMapping <- distinct(data$wdi[3:4])
  
  data$wdi <- data$wdi %>% 
    mutate(joinedName = paste(`Country Code`,`Series Code`, sep="_")) %>%
    group_by(joinedName) %>%
    mutate(fixedWdi = zoo::na.approx(`World Development Indicator`, na.rm = FALSE))%>%
    mutate(fixedWdi = zoo::na.locf(fixedWdi, na.rm = FALSE)) %>%
    ungroup() %>%
    select(-`Country Code`, -`Series Name`, -`World Development Indicator`, -joinedName) %>% 
    pivot_wider(names_from = `Series Code`, values_from = `fixedWdi`) %>% 
    select(-`NA`)
  data$wdi <- unnest(data$wdi)
  data
}










