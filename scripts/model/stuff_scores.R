library(tidyverse)
library(data.table)
library(xgboost)
library(caret)

# ── Paths ──────────────────────────────────────────────────────────────────────
model_dir    <- "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2025-26 Fall/Analytics Classes Fall'25/Baseball/Final Project/models"
smgt_data    <- "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2025-26 Fall/Analytics Classes Fall'25/Baseball/Final Project"
stat490_data <- "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data"

# ── 1. Load data ───────────────────────────────────────────────────────────────
savant_data <- fread(file.path(stat490_data, "savant_data.csv"))

# ── 2. Build count_trans_wide ──────────────────────────────────────────────────
count_values    <- fread("~/Google Drive/My Drive/Rice-University/2025-26 Fall/Analytics Classes Fall'25/Baseball/Final Project/count_values.csv")
count_transitions <- fread("~/Google Drive/My Drive/Rice-University/2025-26 Fall/Analytics Classes Fall'25/Baseball/Final Project/count_transitions.csv")

count_trans_clean <- count_transitions %>%
  mutate(
    PitchResult = case_when(
      PitchResult %in% c("BallCalled", "Ball")                          ~ "ball",
      PitchResult %in% c("StrikeCalled")                                ~ "called_strike",
      PitchResult %in% c("StrikeSwinging", "SwingingStrike")            ~ "swinging_strike",
      PitchResult %in% c("FoulBallFieldable", "FoulBallNotFieldable", "Foul") ~ "foul",
      PitchResult %in% c("FieldOut", "Out")                             ~ "field_out",
      PitchResult %in% c("Single")                                      ~ "single",
      PitchResult %in% c("Double")                                      ~ "double",
      PitchResult %in% c("Triple")                                      ~ "triple",
      PitchResult %in% c("HomeRun")                                     ~ "home_run",
      PitchResult %in% c("HitByPitch")                                  ~ "hit_by_pitch",
      PitchResult %in% c("Error", "FieldError")                         ~ "field_error",
      PitchResult %in% c("FieldersChoice")                              ~ "fielders_choice",
      PitchResult %in% c("Sacrifice")                                   ~ "sac_fly",
      TRUE ~ PitchResult
    )
  ) %>%
  group_by(balls, strikes, PitchResult) %>%
  summarise(Pitch_Value = mean(Pitch_Value, na.rm = TRUE), .groups = "drop")

count_trans_wide <- count_trans_clean %>%
  mutate(
    PitchResult_Category = case_when(
      PitchResult == "ball"                                  ~ "Ball_Value",
      PitchResult %in% c("called_strike", "swinging_strike") ~ "Strike_Value",
      PitchResult == "foul"                                  ~ "Foul_Value",
      PitchResult == "hit_by_pitch"                          ~ "HBP_Value",
      PitchResult == "walk"                                  ~ "Ball_Value",
      PitchResult == "strikeout"                             ~ "Strike_Value",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(PitchResult_Category)) %>%
  group_by(balls, strikes, PitchResult_Category) %>%
  summarize(Pitch_Value = mean(Pitch_Value), .groups = "drop") %>%
  pivot_wider(names_from = PitchResult_Category, values_from = Pitch_Value) %>%
  select(balls, strikes, Ball_Value, Foul_Value, HBP_Value, Strike_Value) %>%
  arrange(balls, strikes) 

# ── 3. Feature engineering ─────────────────────────────────────────────────────

g<- 32.5 
full_data <- savant_data |>
  select(-V1) |>
  mutate(
    pitch_type = case_when(
      pitch_type %in% c("ST", "SV")       ~ "SL",
      pitch_type %in% c("KC")             ~ "CU",
      pitch_type %in% c("SI", "FT")       ~ "SI",
      pitch_type %in% c("FC", "CT")       ~ "FC",
      pitch_type %in% c("FO", "EP", "KN") ~ NA_character_,
      TRUE ~ pitch_type
    )
  ) |>
  filter(!is.na(pitch_type)) |>
  mutate(
    ivb = pfx_z * 12,
    hb  = case_when(
      pitch_hand == "R" ~ -(pfx_x * 12),
      pitch_hand == "L" ~   pfx_x * 12,
      TRUE ~ NA_real_
    ),
    arm_angle      = if_else(is.na(arm_angle), mean(arm_angle, na.rm = TRUE), arm_angle),
    bauer_units    = release_spin_rate / release_speed,
    spin_axis_rad  = spin_axis * pi / 180,
    ssw_vertical   = ivb - bauer_units * cos(spin_axis_rad),
    ssw_horizontal = hb  - bauer_units * sin(spin_axis_rad),
    t_flight       = (60.5 - extension) / abs(vy0),
    vaa            = atan((vz0 + az * t_flight) / abs(vy0)) * (180 / pi),
    haa            = atan((vx0 + ax * t_flight) / abs(vy0)) * (180 / pi),
    ax_spin   = ax,
    az_spin   = az - g,
    accel_angle   = atan2(az_spin, ax_spin) * 180 / pi,
    accel_arm_dev = abs(((accel_angle - arm_angle + 180) %% 360) - 180),
    same_hand      = as.integer(pitch_hand == bat_side),
    pitch_category = case_when(
      pitch_type %in% c("FF", "SI", "FC") ~ "Fastball",
      pitch_type %in% c("SL", "CU")       ~ "Breaking",
      pitch_type %in% c("CH", "FS")       ~ "Offspeed"
    )
  ) |>
  filter(!is.na(pitch_category))

# VAA/HAA residuals
vaa_mod <- lm(vaa ~ plate_z + release_pos_z + pitch_type, data = full_data)
haa_mod <- lm(haa ~ plate_x + release_pos_x + pitch_type, data = full_data)
full_data <- full_data |>
  mutate(
    vaa_vs_exp = vaa - predict(vaa_mod, newdata = full_data),
    haa_vs_exp = haa - predict(haa_mod, newdata = full_data)
  )

# Pitcher fastball means for secondary pitch normalization
pitcher_mean_fb <- full_data |>
  filter(pitch_category == "Fastball") |>
  group_by(pitcher_id) |>
  summarize(
    FB_speed = mean(release_speed, na.rm = TRUE),
    FB_ivb   = mean(ivb,           na.rm = TRUE),
    FB_hb    = mean(hb,            na.rm = TRUE),
    .groups  = "drop"
  )

full_data <- full_data |>
  left_join(pitcher_mean_fb, by = "pitcher_id") |>
  mutate(
    norm_speed = release_speed - FB_speed,
    norm_ivb   = ivb - FB_ivb,
    norm_hb    = hb  - FB_hb
  )

# ── 4. Feature sets ────────────────────────────────────────────────────────────
features_fb <- c(
  "balls", "strikes", "release_speed", "release_spin_rate",
  "release_pos_x", "release_pos_y", "extension",
  "ivb", "hb", "same_hand", "vaa_vs_exp", "haa_vs_exp",
  "arm_angle", "pitch_hand"
)

features_sec <- c(
  "balls", "strikes", "norm_speed", "release_spin_rate",
  "release_pos_x", "release_pos_y", "extension",
  "norm_ivb", "norm_hb", "same_hand", "vaa_vs_exp", "haa_vs_exp",
  "arm_angle", "pitch_hand", "accel_arm_dev"
)

# ── 5. Load models ─────────────────────────────────────────────────────────────
sd_model_fb  <- readRDS(file.path(model_dir, "fb_sd_model"))
sr_model_fb  <- readRDS(file.path(model_dir, "fb_sr_model"))
tr_model_fb  <- readRDS(file.path(model_dir, "fb_tr_model"))
bip_model_fb <- readRDS(file.path(model_dir, "fb_bip_model"))

sd_model_bb  <- readRDS(file.path(model_dir, "bb_sd_model"))
sr_model_bb  <- readRDS(file.path(model_dir, "bb_sr_model"))
tr_model_bb  <- readRDS(file.path(model_dir, "bb_tr_model"))
bip_model_bb <- readRDS(file.path(model_dir, "bb_bip_model"))

sd_model_os  <- readRDS(file.path(model_dir, "os_sd_model"))
sr_model_os  <- readRDS(file.path(model_dir, "os_sr_model"))
tr_model_os  <- readRDS(file.path(model_dir, "os_tr_model"))
bip_model_os <- readRDS(file.path(model_dir, "os_bip_model"))

# ── 6. Helpers ─────────────────────────────────────────────────────────────────
impute_medians <- function(data, features) {
  for (col in features) {
    if (is.numeric(data[[col]])) {
      med <- median(data[[col]], na.rm = TRUE)
      data[[col]][is.na(data[[col]])] <- med
    }
  }
  data
}

compute_stuff_rv <- function(data, sd_mod, sr_mod, tr_mod, bip_mod, features) {
  data <- impute_medians(data, features)
  
  sd_pred  <- predict(sd_mod,  newdata = data, type = "prob")
  sr_pred  <- predict(sr_mod,  newdata = data, type = "prob")
  tr_pred  <- predict(tr_mod,  newdata = data, type = "prob")
  bip_pred <- predict(bip_mod, newdata = data)
  
  cbind(data, sd_pred, sr_pred, tr_pred, bip_pred = bip_pred) |>
    left_join(count_trans_wide, by = c("balls", "strikes")) |>
    mutate(
      EV_Swing         = swinging_strike * Strike_Value + foul * Foul_Value + hit_into_play * bip_pred,
      EV_Take          = ball * Ball_Value + hit_by_pitch * HBP_Value + called_strike * Strike_Value,
      Pred_Value_Stuff = Swing * EV_Swing + Take * EV_Take
    )
}

rm(savant_data)

# ── 7. Predict by pitch category ───────────────────────────────────────────────
cat("Predicting fastballs...\n")
fb_rv <- compute_stuff_rv(filter(full_data, pitch_category == "Fastball"),
                          sd_model_fb, sr_model_fb, tr_model_fb, bip_model_fb, features_fb)

# Slim down immediately — keep only join keys + stuff scores
fb_rv <- fb_rv |>
  select(season, game_id, inning_topbot, balls, strikes, at_bat_number, pitch_number,
         pitcher_id, pitch_category, Pred_Value_Stuff)

rm(sd_model_fb, sr_model_fb, tr_model_fb, bip_model_fb)
gc()

cat("Predicting breaking balls...\n")
bb_rv <- compute_stuff_rv(filter(full_data, pitch_category == "Breaking"),
                          sd_model_bb, sr_model_bb, tr_model_bb, bip_model_bb, features_sec)

bb_rv <- bb_rv |>
  select(season, game_id, inning_topbot, balls, strikes, at_bat_number, pitch_number,
         pitcher_id, pitch_category, Pred_Value_Stuff)

rm(sd_model_bb, sr_model_bb, tr_model_bb, bip_model_bb)

gc()

cat("Predicting offspeed...\n")
os_rv <- compute_stuff_rv(filter(full_data, pitch_category == "Offspeed"),
                          sd_model_os, sr_model_os, tr_model_os, bip_model_os, features_sec)

os_rv <- os_rv |>
  select(season, game_id, inning_topbot, balls, strikes, at_bat_number, pitch_number,
         pitcher_id, pitch_category, Pred_Value_Stuff)

rm(sd_model_os, sr_model_os, tr_model_os, bip_model_os)

gc()

rm(full_data)

all_rv <- bind_rows(fb_rv, bb_rv, os_rv)

rm(fb_rv, bb_rv, os_rv)


# ── 8a. Build scale charts (count-cell mean/SD per pitch category) ─────────────
fb_scale_chart <- all_rv |>
  filter(pitch_category == "Fastball") |>
  filter(balls <= 3 & strikes <= 2) |>
  group_by(pitch_category, balls, strikes) |>
  summarize(
    meanPV = mean(Pred_Value_Stuff, na.rm = TRUE),
    sdPV   = sd(Pred_Value_Stuff,   na.rm = TRUE),
    .groups = "drop"
  )

bb_scale_chart <- all_rv |>
  filter(pitch_category == "Breaking") |>
  filter(balls <= 3 & strikes <= 2) |>
  group_by(pitch_category, balls, strikes) |>
  summarize(
    meanPV = mean(Pred_Value_Stuff, na.rm = TRUE),
    sdPV   = sd(Pred_Value_Stuff,   na.rm = TRUE),
    .groups = "drop"
  )

os_scale_chart <- all_rv |>
  filter(pitch_category == "Offspeed") |>
  filter(balls <= 3 & strikes <= 2) |>
  group_by(pitch_category, balls, strikes) |>
  summarize(
    meanPV = mean(Pred_Value_Stuff, na.rm = TRUE),
    sdPV   = sd(Pred_Value_Stuff,   na.rm = TRUE),
    .groups = "drop"
  )

full_rv <- all_rv |>
  left_join(
    bind_rows(fb_scale_chart, bb_scale_chart, os_scale_chart) |>
      rename(cell_mean = meanPV, cell_sd = sdPV),
    by = c("pitch_category", "balls", "strikes")
  ) |>
  mutate(
    stuff = 100 + 50 * (Pred_Value_Stuff - cell_mean) / cell_sd
  ) |>
  select(-cell_mean, -cell_sd)

# ── 9. Save stuff scores only ──────────────────────────────────────────────────
# Join key: season + game_id + inning_topbot + at_bat_number + pitch_number
stuff_scores <- full_rv |>
  select(season, game_id, inning_topbot, at_bat_number, pitch_number,
         pitcher_id, pitch_category, Pred_Value_Stuff, stuff)

winsorize <- function(x, low = 0.01, high = 0.99) {
  q <- quantile(x, c(low, high), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

stuff_scores <- stuff_scores |>
  group_by(pitch_category) |>
  mutate(stuff = winsorize(stuff)) |>
  ungroup()

stuff_scores <- stuff_scores |>
  mutate(stuff = pmin(pmax(stuff, 0), 200))

fwrite(stuff_scores, file.path(stat490_data, "stuff_scores.csv"))
cat("Saved stuff_scores.csv —", nrow(stuff_scores), "pitches\n")
