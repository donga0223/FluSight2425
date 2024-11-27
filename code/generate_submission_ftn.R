source ("code/Casey_ensemble_ftn.R")


gen_submission_file <- function(forecast_date, nhsn, n_horizon = 4, model_name = "UT-Osiris", write.csv = TRUE){
  ## load data 
  fips <- read.csv("data/fips_mappings.csv")
  
  # reference date
  reference_date = forecast_date - ((wday(forecast_date, week_start = 1) + 1) %% 7 - 7)
  
  forecast_table <- forecast_mean_table <- c()
  mystate <- unique(nhsn$jurisdiction)
  
  for (i in 1:length(mystate)){
    statename = mystate[i]
    state_data <- nhsn %>% filter(jurisdiction == statename,
                                  date <= forecast_date)
    fit_result <- fit_ensemble(state_data$total_flu_new_adm, forecast_horizon = n_horizon)
    forecast_result <- predict_ensemble(state_data$total_flu_new_adm, fit_result, forecast_horizon = n_horizon)
    forecast_mean <- data.frame(val = forecast_result$mean, 
                                horizon = 0:(n_horizon-1), 
                                state = statename,
                                reference_date = reference_date) 
    forecast_mean <- forecast_mean %>%
      mutate(target_end_date = reference_date + 7*horizon)
    forecast_quantile <- forecast_result$quantiles %>% 
      mutate(state = statename,
             target_end_date = reference_date + 7*horizon,
             reference_date = reference_date) 
    
    col_order <- c("state",	"horizon",	"target_end_date",	
                   "reference_date",	"target",	"output_type",	
                   "output_type_id",	"value")
    forecast_table <- rbind(forecast_table, forecast_quantile[, col_order])
    forecast_mean_table <- rbind(forecast_mean_table, forecast_mean)
  }
  
  forecast_table2 <- forecast_table %>%
    left_join(fips, by = c("state" = "abbreviation")) %>%
    select(-location_name, -hhs_region, -state) 
  
  if(write.csv){
    write.csv(forecast_table2, file = paste0("model-output/", reference_date,"-",model_name,".csv"))
  }
  return(list(point_est = forecast_mean_table, quantile_est = forecast_table2))
  
}







