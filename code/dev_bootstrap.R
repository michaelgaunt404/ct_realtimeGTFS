library(purrr)
library(modeldata)
data(wa_churn)

set.seed(13)
resample1 <- rsample::bootstraps(wa_churn, times = 3)
rsample::bootstraps(temp_pro, times = 2)
mean(mtcars$drat == "yes")




temp_pro_boot = rsample::bootstraps(temp_pro, times = 1000)
# resample2 <- bootstraps(wa_churn, strata = churn, times = 1000)
ttt = map_df(
  temp_pro_boot$splits,
  function(x) {
    dat <- as.data.frame(x) %>%
      group_by(flag_peak_time, direction_id) %>%
      summarise(mean = mean(datetime_diff_m)) %>%
      ungroup()
    # mean(dat$datetime_diff_m)
    # print(dat)
  }
)

ttt %>%
  ggplot() +
  geom_boxplot(aes(mean, color = flag_peak_time)) +
  facet_grid(rows = vars(direction_id))
