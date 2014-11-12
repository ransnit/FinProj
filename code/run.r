source("train_model.r")

rf_model <- process.data.and.train.model(data)
saveRDS(rf_model, "rf_model.rds")
print(rf_model)
