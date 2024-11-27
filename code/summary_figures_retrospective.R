library(ggplot2)


model_name = "UT-Osiris"
reference_dates <- c("2024-09-28", "2024-10-19", "2024-10-26")
reference_dates <- c("2023-10-07", "2023-11-18", "2023-12-23", "2024-02-03", "2024-03-16")

for(i in 1:length(reference_dates)){
  assign(paste0("data", i), read.csv(paste0("model-output/",reference_dates[i],"-",model_name,".csv")))
}
  


for(i in 1:length(reference_dates)){
  assign(paste0("forecast_ribbon",i), get(paste0("data", i)) %>%
    filter(output_type_id %in% c(0.025, 0.5, 0.975)) %>%
    select(target_end_date, state, value, output_type_id) %>%
    pivot_wider(names_from = output_type_id, values_from = value) %>%
    rename(lower = `0.025`, upper = `0.975`, median = `0.5`, date = target_end_date) %>%
    mutate(date = as.Date(date)))
}

all_data <- nhsn %>%
  filter(date > '2022-07-01') %>%
  select(date, jurisdiction, total_flu_new_adm) %>%
  rename(value = total_flu_new_adm,
         state = jurisdiction) %>%
  #bind_rows(forecast_ribbon)
  left_join(forecast_ribbon1, by = c("date", "state")) %>%
  left_join(forecast_ribbon2, by = c("date", "state")) %>%
  left_join(forecast_ribbon3, by = c("date", "state")) %>%
  left_join(forecast_ribbon4, by = c("date", "state")) %>%
  left_join(forecast_ribbon5, by = c("date", "state"))


pdf(paste0("res_figures/retrospective.pdf"), width = 15, height = 10)
all_data %>%
  filter(date > '2023-08-01', date < '2024-05-01') %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(color = "black", size = 1)+
  geom_line(aes(y =median), color = "red", size = 1)+
  geom_ribbon(aes( ymin = lower, ymax = upper), fill = "pink", alpha = 0.8) + 
  #geom_vline(xintercept = as.Date(forecast_date), linetype = "dashed", color = "blue") +
  geom_line(aes(y =median.x), color = "blue", size = 1)+
  geom_ribbon(aes( ymin = lower.x, ymax = upper.x), fill = "skyblue", alpha = 0.8) + 
  geom_line(aes(y =median.x.x), color = "green", size = 1)+
  geom_ribbon(aes( ymin = lower.x.x, ymax = upper.x.x), fill = "lightgreen", alpha = 0.8) + 
  geom_line(aes(y =median.y), color = "orange", size = 1)+
  geom_ribbon(aes( ymin = lower.y, ymax = upper.y), fill = "orange", alpha = 0.5) + 
  geom_line(aes(y =median.y.y), color = "brown", size = 1)+
  geom_ribbon(aes( ymin = lower.y.y, ymax = upper.y.y), fill = "brown", alpha = 0.5) +
  facet_wrap(~state, scales = "free_y") +
  labs(title = "Actual vs Forecast with Prediction Intervals",
       x = "Date", y = "Flu Nwe Hospitalizations") +
  theme_minimal()

dev.off()



WIS_ftn <- function(quantiles, true_data, forecast_data){
  
  res <- c()
  forecast_data1 <- forecast_data %>%
    select(state, target_end_date, output_type_id, value) %>%
    mutate(target_end_date = as.Date(target_end_date) )
  true_data1 <- true_data %>%
    filter(date %in% unique(forecast_data$target_end_date)) %>%
    select(date, jurisdiction, total_flu_new_adm) %>%
    rename(state = jurisdiction,
           target_end_date = date,
           value = total_flu_new_adm) %>%
    mutate(output_type_id = 0)
  
  all_data <- true_data1 %>%
    bind_rows(forecast_data1)
  
  all_data_wide <- all_data %>%
    pivot_wider(
      names_from = output_type_id,  # 새 변수 이름으로 사용할 열
      values_from = value          # 해당 열의 값을 채움
    )
  quantile_cols <- paste0(quantiles)
  
  calculate_wis <- function(row) {
    true <- as.numeric(row[["0"]])
    predicted <- as.numeric(row[quantile_cols]) 
    n <- length(quantiles)
    wis <- 1/n*sum(2 * ((true <= predicted) - quantiles) * (predicted - true))
    return(wis)
  }
  
  for(i in 1:dim(all_data_wide)[1]){
    res[i] <- calculate_wis(all_data_wide[i,])
  }
  
  
  all_data_wide$WIS <- apply(all_data_wide, 1, calculate_wis)
  
  return(all_data_wide)
}


wis1 <- WIS_ftn(quantiles, true_data = nhsn, forecast_data = data1)
wis2 <- WIS_ftn(quantiles, true_data = nhsn, forecast_data = data2)
wis3 <- WIS_ftn(quantiles, true_data = nhsn, forecast_data = data3)
wis4 <- WIS_ftn(quantiles, true_data = nhsn, forecast_data = data4)
wis5 <- WIS_ftn(quantiles, true_data = nhsn, forecast_data = data5)


wis1 %>% group_by(target_end_date) %>%
  summarise(mean(WIS))

wis2 %>% group_by(target_end_date) %>%
  summarise(mean(WIS))

wis3 %>% group_by(target_end_date) %>%
  summarise(mean(WIS))

wis4 %>% group_by(target_end_date) %>%
  summarise(mean(WIS))

wis5 %>% group_by(target_end_date) %>%
  summarise(mean(WIS))


