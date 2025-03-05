library(quantmod)
library(abind)
library(reticulate)
library(xts)
library(keras)
library(tensorflow)

#use_condaenv(condaenv = "base", conda = "auto", required = TRUE)
#use_python('/Users/clarkkong/Library/r-miniconda-arm64/envs/r-reticulate/bin/python', required = TRUE)
#use_condaenv("r-reticulate", required = TRUE)
#use_python_version()

# download tickers data 
ticker_names = c("AAPL", "AMZN", "GOOGL", "MSFT", "TSLA","300418.SZ","000001.SZ","600547.SS","601857.SS","000807.SZ","000858.SZ","601519.SS","000799.SZ")
ticker_data= lapply(ticker_names, function(x) { getSymbols(x, src = "yahoo", auto.assign = FALSE)})
names(ticker_data) <- ticker_names

 ## read data from RDS files
#setwd("/Users/clarkkong/R Projects/konglr/stock_prediction")
setwd("/Users/clarkkong/R Projects/konglr/stock_prediction/Stock_Price_Prediction")
saveRDS(ticker_data, file = "ticker_data.rds")
ticker_data <- readRDS(file = "ticker_data.rds")
PAYH<- readRDS("000001_yahoo.rds")

PAYH.A <- adjustOHLC(`PAYH`, use.Adjusted = TRUE)
chartSeries(AAPL, subset = "last 1 year",theme = "white")
addSMA(n=5, with.col= Cl, col = "purple")
addSMA(n=10, col = "orange")
addSMA(n=20, col = "red")
add_MACD()

chartSeries(to.monthly(PAYH.A), theme = "white")
chartSeries(to.quarterly(PAYH.A), theme = "white")


# Set seed value for reproducibility
set.seed(1234)
tensorflow::set_random_seed(1234)


## Data Preparation
days_back <-5
n_factors <- 4
n_steps <- 5
ticker <- adjustOHLC(ticker_data$AAPL, use.Adjusted = TRUE)
ticker <- na.omit(ticker)
start_row <- 1
train_percent <- 0.99
train_data <- ceiling(((nrow(ticker)*train_percent)-start_row)/days_back)
train_data_end <- train_data*days_back-days_back+1
val_percent <- 0.08
val_data <- ceiling(nrow(ticker)*val_percent/days_back)
val_data_end <- train_data_end + val_data* days_back
test_percent <- 0.01
test_data <-ceiling(nrow(ticker)* test_percent-days_back
                    
# TTR index
mySMA <- lapply(c(5,10,20), function(n) {SMA(ticker[, 4], n = n)})
mySMA_matrix <- do.call(cbind, mySMA)

# 均线
SMA10 <- SMA(Cl(ticker), n = 10)
SMA20 <- SMA(Cl(ticker), n = 20)
mySMA <- lapply(c(5,10,20), function(n) {SMA(ticker[, 4], n = n)})
mySMA_matrix <- do.call(cbind, mySMA)

# 指数移动平均线（EMA）
EMA10 <- EMA(Cl(ticker), n = 10)
EMA20 <- EMA(Cl(ticker), n = 20)

# 相对强弱指数（RSI）
RSI14 <- RSI(Cl(ticker), n = 14)

# 移动平均收敛/发散指标（MACD）
MACD <- MACD(Cl(ticker))

# 布林带
BOLL <- BBands(Cl(ticker))

# 威廉指标
WILLR <- WPR(HLC(ticker), n = 14)

# 交易量指标
OBV <- OBV(Cl(ticker), Vo(ticker))
#train_x <- array_reshape(x=as.matrix(ticker[(start_row+1) :train_data_end , 1:n_factors]), dim = c(train_data, days_back, n_factors))
#x_test <- array_reshape(x=as.matrix(ticker[7609:7668, 1:n_factors]), dim = c(12, days_back , n_factors))
#train_y <- as.matrix(ticker[seq((start_row+days_back+1), (train_data_end+1), days_back),1:4])
#train_y<- array_reshape(train_y, dim = c(dim(train_y)[1], 4))


train_x_daily <- array(0, dim = c((train_data*days_back-days_back+1), days_back, n_factors))
dimnames(train_x_daily)[[1]] <- c( rep("", (train_data*days_back-days_back+1)))
train_y_daily <- array(0, dim = c((train_data*days_back-days_back+1), n_factors))
dimnames(train_y_daily)[[1]] <- c( rep("", (train_data*days_back-days_back+1)))
train_y_daily_5 <- array(0, dim = c((train_data*n_steps-days_back+1), days_back, n_factors))
dimnames(train_y_daily_5)[[1]] <- c( rep("", (train_data*days_back-days_back+1)))
SMA_51020 <- array(0, dim = c((train_data*n_steps-days_back+1), days_back, 3))

for (l in (start_row):(start_row+train_data*days_back- days_back)) {
  train_x_daily[(l-start_row+1),,] <- as.matrix(ticker[l:(l+days_back-1), 1:n_factors])
  dimnames(train_x_daily)[[1]][l-start_row+1] <- c(as.character(time(ticker[(l+days_back-1),])))
  train_y_daily[l-start_row+1,] <- ticker[l + days_back, 1:n_factors]
  dimnames(train_y_daily)[[1]][l-start_row+1] <- c(as.character(time(ticker[(l+days_back-1+1),])))
  train_y_daily_5[l- start_row + 1,,] <-as.matrix(ticker[(l+1):(l+days_back-1+1), 1:n_factors])
  dimnames(train_y_daily_5)[[1]][l-start_row+1] <- c(as.character(time(ticker[(l+days_back-1+1),])))
  
  SMA_51020[(l-start_row+1),,] <- as.matrix(mySMA_matrix[l:(l+days_back-1), 1:3])
}
   train_x_merged <- abind(train_x_daily,SMA_51020,along = 3) 

## Test data preparation
x_test_daily <- array(0, dim = c((test_data-days_back+1), days_back, n_factors))
dimnames(x_test_daily)[[1]] <- c( rep("", (test_data-days_back+1)))
y_actual_daily<- array(0, dim = c((test_data-days_back+1), n_factors))
dimnames(y_actual_daily)[[1]] <- c( rep("", (test_data-days_back+1)))
x_test_SMA<- array(0, dim = c((test_data-days_back+1), days_back, 3))
dimnames(x_test_SMA)[[1]] <- c( rep("", (test_data-days_back+1)))

for (i in (nrow(ticker)-test_data+1):nrow(ticker)-days_back+1) {
  
  x_test_daily[i-(nrow(ticker)-test_data+1)+1,,]<-c(as.matrix(ticker[i:(i+days_back-1), 1:n_factors]))
  dimnames(x_test_daily)[[1]][i-(nrow(ticker)-test_data+1)+1] <- c(as.character(time(ticker[(i+days_back-1),])))

  x_test_SMA[i-(nrow(ticker)-test_data+1)+1,,]<-c(as.matrix(mySMA_matrix[i:(i+days_back-1),1:3]))
  dimnames(x_test_SMA)[[1]][i-(nrow(ticker)-test_data+1)+1] <- c(as.character(time(mySMA_matrix[(i+days_back-1),])))
 
  x_test_merged <- abind(x_test_daily,x_test_SMA,along = 3) 
  
   if(i== (nrow(ticker)-days_back+1)){ 
      y_actual_daily[i-(nrow(ticker)-test_data+1)+1,]<-as.matrix(ticker[(i+days_back-1), 1:n_factors])}
      else{
   y_actual_daily[i-(nrow(ticker)-test_data+1)+1,]<-as.matrix(ticker[(i+days_back), 1:n_factors])
   dimnames(y_actual_daily)[[1]][i-(nrow(ticker)-test_data+1)+1] <- as.character(time(ticker[(i+days_back),])) }
   }


#val_x <- array_reshape(x=as.matrix(ticker[(train_data_end+1):(train_data_end + val_data* days_back), 1:n_factors]), dim = c(val_data, days_back , n_factors))
#val_y <- as.matrix(ticker[seq((train_data_end+days_back+1), (train_data_end + val_data* days_back+1),days_back), 1:4])
#val_y<- array_reshape(val_y, dim = c(dim(val_y)[1], 4))

#scaled_train_x<- scale(train_x_daily)
#scaled_train_y<- scale(train_y_daily)
#scaled_x_test<-scale(x_test_daily)

# Define the model
model <- keras_model_sequential()
model %>%

  bidirectional(layer_lstm(units = 64, return_sequences = TRUE, input_shape = c(days_back, 4))) %>%
  layer_lstm(units = 128, return_sequences = FALSE) %>%
  layer_dense(units = 64, activation = "relu")  %>%
  layer_dense(units = 4)


# Compile the model
model %>% compile(
  #loss = "mean_absolute_percentage_error",
  loss = "MSE",
  optimizer = optimizer_rmsprop(learning_rate = 0.001),
  #optimizer = optimizer_adam(learning_rate = 0.001),
  metrics = c("accuracy")
)


history <- model %>% fit(
 # x = train_x_merged[100:train_data_end,,],
  x = train_x_daily[1000:train_data_end,,],
  y = train_y_daily[1000:train_data_end,],
  batch_size = 1,
  epochs = 10,
  #validation_data = list(val_x, val_y),
  validation_split = 0.2
)


#Save models
save_model_hdf5(model,'model_PAYH_5_days.h5')
model_5_days<-load_model_hdf5('model_PAYH_5_days.h5')
#y_pred <- model_5_days %>% predict(x_test_daily,reduce_retracing=True)

# Predict on test data
y_pred <- model %>% predict(x_test_daily)
y_pred <- model %>% predict(x_test_merged)


#y_pred <- y_pred * attr(scaled_x_test, "scaled:scale") + attr(scaled_x_test, "scaled:center")
print(y_pred)

pred_row <- ticker[nrow(ticker),1:4]
index(pred_row) <- as.Date(index(pred_row))+1
comparingplot_cl <- ticker[(nrow(ticker)-nrow(y_pred)+2):nrow(ticker),1:4]
comparingplot_cl <- rbind(comparingplot_cl,pred_row)
comparingplot_cl[,1:4] <- y_actual_daily[,1:4]
comparingplot_cl <- cbind(comparingplot_cl, y_pred[,1:4])

par(mfrow=c(4,2))
plot(comparingplot_cl[,1], type = "l", col = "blue")
lines(comparingplot_cl[,5], type = "l", col = "red")

plot(comparingplot_cl[,2], type = "l", col = "blue")
lines(comparingplot_cl[,6], type = "l", col = "red")

plot(comparingplot_cl[,3], type = "l", col = "blue")
lines(comparingplot_cl[,7], type = "l", col = "red")

plot(comparingplot_cl[,4], type = "l", col = "blue")
lines(comparingplot_cl[,8], type = "l", col = "red")

diff_pred <- comparingplot_cl[,8]-comparingplot_cl[,4]
print(diff_pred)
summary(diff_pred)


 



# Save models
# save_model_hdf5(model,'model_PAYH_5_days_5.h5')
save_model_hdf5(model,'model_300418_5_days_SMA.h5')
# load_model_hdf5('model_PAYH_5_days_5.h5')

# Define the SMA input layer
sma_input <- layer_input(shape = c(days_back, 3), name = "sma_input")

# Define the main input layer for the existing data
main_input <- layer_input(shape = c(days_back, n_factors), name = "main_input")

# Merge the two inputs using keras_layer
merged_input <- layer_concatenate( inputs = list(main_input, sma_input), axis = -1, name = "merged_input")

outputs(4)

# Predict on test data
y_pred_7_4 <- model_7_4 %>% predict(x_test_merged, batch_size = 1)

print(y_pred_7_4)

