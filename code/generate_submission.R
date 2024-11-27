source("code/generate_submission_ftn.R")
source ("code/get_nhsn.R")


nhsn <- get_nhsn()
nhsn <- nhsn %>% 
  mutate(total_flu_new_adm = replace_na(total_flu_new_adm, 0))

forecast_date <- as.Date(max(nhsn$date))

forecast_res <- gen_submission_file(forecast_date, nhsn, n_horizon = 4, model_name = "UT-Osiris", write.csv = FALSE)
