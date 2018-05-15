library(forecast)
library(ggplot2)
library(tidyr)
library(plyr)

#temp <- c("Hourly.csv")
temp <- list.files(path="../../cut/10000/train/", pattern="*.csv")

time_type <- c("Yearly", "Quarterly", "Monthly", "Weekly", "Daily", "Hourly")
horizon_length <- c(6, 8, 18, 13, 14, 48)

horizons <- data.frame(time_type, horizon_length)
rownames(horizons) <- horizons[, "time_type"]
horizons$time_type <- NULL

get_predictions <- function(datarow, horizon.length) {
  datarow <- unname(datarow)
  
  fit <- forecast::auto.arima(y=datarow[!is.na(datarow)], max.order=8, stepwise=FALSE, approximation=FALSE, start.p=0, start.q=0)
  
  resluts <- forecast::forecast(fit, h=horizon.length)$mean
  
  if (length(resluts) == 0) {
    print("Fuckidy fuck")
  }
  
  return_list <- list(fit$residuals, resluts)
  
  return(return_list)
}

transform.data <- function(basepath, filename, horizon_length) {
  print(paste("Dealing with ", filename, collapse="", sep=""))
  print(horizon_length)
  data <- read.csv(file=paste(basepath, filename, collapse="", sep=""))  # Read in the data
  
  print(paste("Number of rows: ", nrow(data), collapse="", sep=""))
  reduced_data <- data[,3:ncol(data)]  # Discard the first 2 columns
  
  stuff <- apply(reduced_data, 1, function(x) get_predictions(x, horizon.length=horizon_length))  # Crunch the numbers, reduced_data[1:5,] for testing shorter ones
  
  residuals <- lapply(stuff, function(x) x[[1]])
  residuals <- plyr::ldply(residuals, rbind)
  residuals <- cbind(id=data$V1[1:nrow(residuals)], residuals)
  residuals$.id <- NULL
  
  write.csv(residuals, file=paste("./data/opt-arima-2/residuals-10000-", filename, collapse="", sep=""), row.names=FALSE)
  
  arima_pred <- lapply(stuff, function(x) x[[2]])
  arima_pred <- plyr::ldply(arima_pred, rbind)  # Assemble the data into a data frame
  
  arima_pred <- cbind(id=data$V1[1:nrow(arima_pred)], arima_pred)
  arima_pred$.id <- NULL
  
  write.csv(arima_pred, file=paste("./data/opt-arima-2/arima-10000-", filename, collapse="", sep=""), row.names=FALSE)  # Output as CSV
}

for (val in temp) {
  type.name <- strsplit(val, "[.]")[[1]][1]
  transform.data("../../cut/10000/train/", val, horizons[type.name, ])
}
