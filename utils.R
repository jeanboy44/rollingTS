loadData <- function(dataset=1:5){
  # 1: monthly-car-sales in quebec-1960
  # 2: monthly-gasoline-demand ontario
  # 3: monthly-milk-production-pounds
  # 4: monthly sales of company x japan
  # 5: monthly unit sales winnebago
  if(dataset==1){
    df <- read.csv("data/monthly-car-sales-in-quebec-1960.csv")
  }else if(dataset==2){
    df <- read.csv("data/monthly-gasoline-demand-ontario-.csv")
  }else if(dataset==3){
    df <- read.csv("data/monthly-milk-production-pounds-p.csv")
  }else if(dataset==4){
    df <- read.csv("data/monthly-sales-of-company-x-jan-6.csv")
  }else if(dataset==5){
    df <- read.csv("data/monthly-unit-sales-winnebago-ind.csv")
  }else{
    return(NULL)
  }
  Sys.setlocale("LC_TIME", "C")
  df$Month <- as.Date(toupper(paste0(df$Month, "-01")), format="%b-%y-%d")
  df$Month <- as.Date(gsub("^..", "19", df$Month))
  names(df) <- c("Month", "Qty")
  Sys.setlocale()
  return(df)
}
# ts=df_train$Qty;len_wdw = 4
generateFeatures <- function(ts, len_wdw, prefix="TS", seasonal=c(3,6), dataFrameOutput=FALSE){
  ## the len_wdw is the number of rows used to create a row in a new data.
  ## regression based feature can use only len_wdw-1 rows to fit regressor.
  ## For example, 4 rows(t-1, t-2, t-3, t-4) are used to create a row and it corresponds to the t'th row of target(y) value.
  
  library(dplyr)
  library(zoo)
  if(min(seasonal)<2|max(seasonal)>12){
    print('The seasonal must be >=2 or <=12')
    stop()
  }
  ## Define functions #################################################################
  
  ## lag features
  f_lags <- list()
  for(i in 1:len_wdw){
    f_lags[[i]] <- eval(substitute(function(x) dplyr::lag(x, n=n), list(n=i)))
  }

  ## rolling window features
  
  # statistic
  # mean
  
  # regression
  # linear regression
  f_lm <- function(ts){
    y <- ts[2:length(ts)]
    x <- ts[1:(length(ts)-1)]
    
    return(predict(lm(y~x), data.frame(x=ts[length(ts)])))
  }
  # polynomial regression
  f_poly2 <- function(ts){
    if(length(unique(ts))<=3){
      return(ts[length(ts)])
    }
    y <- ts[2:length(ts)]
    x <- ts[1:(length(ts)-1)]
    
    return(predict(lm(y~poly(x, 2)), data.frame(x=ts[length(ts)])))
  }
  # polynomial regression
  f_poly3 <- function(ts){
    if(length(unique(ts))<=4){
      return(ts[length(ts)])
    }
    y <- ts[2:length(ts)]
    x <- ts[1:(length(ts)-1)]
    
    return(predict(lm(y~poly(x, 3)), data.frame(x=ts[length(ts)])))
  }
  
  f_rolling <- list(f_mean = mean, f_lm=f_lm, f_poly2=f_poly2, f_poly3=f_poly3)
  
  ## seasonal features
  f_seasonals <- list()
  for(i in 1:length(seasonal)){
    s <- seasonal[i]-1
    f_seasonal <- list()
    tmp <- rep(c(1, rep(0,s)), ceiling(length(ts)/(s+1)))[1:(length(ts)-len_wdw)]
    for(j in 1:s){
      f_seasonal[[j]] <- dplyr::lag(tmp, n=(j-1), default=0)
    }
    f_seasonals[[i]] <- do.call(cbind,f_seasonal)
    colnames(f_seasonals[[i]]) <- paste0(prefix,"_SS", sprintf("%02d",seasonal[i]),"_",sprintf("%02d",1:s))
  }
  
  ## Generate features #################################################################
  ## lag features
  features_lag <- sapply(f_lags, function(x) x(ts))
  colnames(features_lag) <- paste0(prefix, "_LAG_", sprintf("%02d",1:length(f_lags)))
  features_lag <- na.omit(features_lag)
  ## rolling window features
  features_rw <- sapply(f_rolling, function(x) rollapply(ts[-length(ts)], width=len_wdw, align="left", FUN=x))
  if(is.vector(features_rw)){
    fnm <- names(features_rw)
    features_rw <- matrix(features_rw, nrow=1)
    colnames(features_rw) <- gsub("f_", paste0(prefix,"_RW_"), fnm)
  }else{
    colnames(features_rw) <- gsub("f_", paste0(prefix,"_RW_"), colnames(features_rw))
  }
  ## seasonal features
  features_ss <- do.call(cbind,f_seasonals)
  
  ## Combine features #################################################################
  X <- cbind(features_lag, features_rw, features_ss)
  
  ## Get target value  #################################################################
  y <- ts[(len_wdw+1):length(ts)]
  
  if(dataFrameOutput){
    return(list(X=X, y=y, data=as.data.frame(cbind(X,y))))
  }else{
    return(list(X=X,y=y))
  }
}

# ts=df_train$Qty;ts_test=df_test$Qty;fcst_h=nrow(df_test);model=fit
rollingPredict <- function(ts, fcst_h, len_wdw, seasonal=c(4,6), model, outputDataFrame){
  ## 초기 예측
  ts_train <- ts
  ts_train_f <- generateFeatures(ts_train, len_wdw=len_wdw, seasonal=c(4,6))
  pred <- predict(model, ts_train_f$X)
  ts_train <- c(ts_train, pred[length(pred)])
  ts_train <- ts_train[(length(ts_train)-len_wdw):length(ts_train)]
  for(t in 2:length(fcst_h)){
    ## 한 시점씩 추가적으로 예측
    # feature 생성
    ts_train_f <- generateFeatures(ts_train, len_wdw=len_wdw, seasonal=c(4,6))
    # 예측 및 예측값 저장
    pred_tmp <- predict(model, ts_train_f$X)
    pred <- c(pred, pred_tmp)
    # 예측값을 train set에 추가
    ts_train <- c(ts_train, pred_tmp)
    ts_train <- ts_train[(length(ts_train)-len_wdw):length(ts_train)]
  }
  
  return(pred[(length(pred)-fcst_h+1):length(pred)])
}


mape <- function(p, y){
  100/length(y)*sum(abs((y-p)/y))
}
