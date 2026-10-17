library(sabRmetrics)
library(tidyverse)

cluster <- parallel::makeCluster(parallel::detectCores())
data_20 <- sabRmetrics::download_baseballsavant(
  start_date = "2020-01-01",
  end_date = "2020-12-31",
  cl = cluster
  )
data_19 <- sabRmetrics::download_baseballsavant(
  start_date = "2019-01-01",
  end_date = "2019-12-31",
  cl = cluster
)
data_18 <- sabRmetrics::download_baseballsavant(
  start_date = "2018-01-01",
  end_date = "2018-12-31",
  cl = cluster
)
data_17 <- sabRmetrics::download_baseballsavant(
  start_date = "2017-01-01",
  end_date = "2017-12-31",
  cl = cluster
)
data_16 <- sabRmetrics::download_baseballsavant(
  start_date = "2016-01-01",
  end_date = "2016-12-31",
  cl = cluster
)
data_15 <- sabRmetrics::download_baseballsavant(
  start_date = "2015-01-01",
  end_date = "2015-12-31",
  cl = cluster
)
parallel::stopCluster(cluster)


write.csv(data_20, "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data/savant_data_2020.csv")
write.csv(data_19, "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data/savant_data_2019.csv")
write.csv(data_18, "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data/savant_data_2018.csv")
write.csv(data_17, "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data/savant_data_2017.csv")
write.csv(data_16, "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data/savant_data_2016.csv")
write.csv(data_15, "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data/savant_data_2015.csv")


data_dir <- "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data"

savant_data <- list.files(data_dir, pattern = "savant_data.*\\.csv$", full.names = TRUE) |>
  set_names(~ str_extract(.x, "\\d{4}(?=\\.csv)")) |>
  map(read_csv, show_col_types = FALSE) |>
  bind_rows(.id = "season")


savant_data <- savant_data |>
  arrange(season, game_id, game_date, at_bat_number, pitch_number)

write.csv(savant_data, "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data/savant_data.csv")
