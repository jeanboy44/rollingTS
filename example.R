library(glmnet)
library(dplyr)
library(ggplot2)
library(forecast)
source("utils.R")

## 데이터 불러오기
df <- loadData(2)

## train / test 셋 나누기
train_id <- 1:floor(nrow(df)*0.7)
df_train <- df[train_id,]
df_test <- df[-train_id,]

## 모델 생성 -------------------------------------------------------------------------

## LASSO
df_train_f <- generateFeatures(ts=df_train$Qty, len_wdw = 4, seasonal=c(4,6))
cv.fit <- cv.glmnet(df_train_f$X, df_train_f$y, alpha = 1)
fit <-glmnet(df_train_f$X, df_train_f$y, alpha = 1, lambda=cv.fit$lambda.min)

## STL
ts_train <- ts(df_train$Qty, start=min(df_train$Month), end=max(df_train$Month), frequency=12)
fit_stl <- stl(ts_train, s.window="periodic")

## HW
ts_train <- ts(df_train$Qty, start=min(df_train$Month), end=max(df_train$Month), frequency=12)
fit_hw <- hw(ts_train)


## 예측 -------------------------------------------------------------------------
pred_lasso <- rollingPredict(ts = df_train$Qty, fcst_h = nrow(df_test), model = fit, len_wdw = 4, seasonal=c(4,6))
pred_stl <- as.numeric(forecast(fit_stl, h=nrow(df_test))$mean)
pred_hw <- as.numeric(forecast(fit_hw, h=nrow(df_test))$mean)


## 성능 확인 -------------------------------------------------------------------------
mape(pred_lasso, df_test$Qty)
mape(pred_stl, df_test$Qty)
# mape(pred_hw, df_test$Qty)

## 시각화 -------------------------------------------------------------------------
df_pred_lasso <- data.frame(Month=df_test$Month, Qty=pred_lasso)
df_pred_stl <- data.frame(Month=df_test$Month, Qty=pred_stl)
# df_pred_hw <- data.frame(Month=df_test$Month, Qty=pred_hw)
df%>%
  mutate(TYPE="REAL")%>%
  union_all(
    df_pred_lasso%>%
      mutate(TYPE="LASSO")
  )%>%
  union_all(
    df_pred_stl%>%
      mutate(TYPE="STL")
  )%>%
  # union_all(
  #   df_pred_hw%>%
  #     mutate(TYPE="HW")
  # )%>%
  ggplot(., aes(x=Month, y=Qty, color=TYPE))+
  geom_line()+
  theme_bw()
