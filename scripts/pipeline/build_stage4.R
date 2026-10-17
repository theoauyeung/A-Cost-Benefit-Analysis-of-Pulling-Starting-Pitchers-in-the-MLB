# build_stage4.R
#
# Builds starter_pa_full.rds in a memory-conscious way:
# 1. Load upstream checkpoints
# 2. Run build_starter_pa_full()
# 3. rm() all large inputs + gc() BEFORE saveRDS so gzip has headroom
# 4. Build workload_features.rds (lightweight, reads raw CSV columns only)
#
# Run this instead of build_all() when Stage 4 is stale.
# After it finishes, build_all() will cache-hit Stage 4 and continue.

source("scripts/pipeline/build_features.R")

spf_path <- file.path(DATA_DIR, "starter_pa_full.rds")

if (file.exists(spf_path)) {
  message("[cache] starter_pa_full.rds already exists — skipping Stage 4 build")
} else {
  message("[load] reading upstream checkpoints …")
  clean_data       <- readRDS(file.path(DATA_DIR, "clean_data.rds"))
  starter_pa_est   <- readRDS(file.path(DATA_DIR, "starter_pa_est.rds"))
  reliever_rolling <- readRDS(file.path(DATA_DIR, "reliever_rolling.rds"))
  prior_n_sp       <- readRDS(file.path(DATA_DIR, "prior_n_sp.rds"))

  message("[build] running build_starter_pa_full() …")
  result <- build_starter_pa_full(clean_data, starter_pa_est, reliever_rolling, prior_n_sp)

  # Free everything except result before gzip serialisation
  rm(clean_data, starter_pa_est, reliever_rolling, prior_n_sp)
  gc(); gc()

  message("[save] writing starter_pa_full.rds …")
  saveRDS(result, spf_path)
  message("[done] starter_pa_full.rds  (",
          round(file.size(spf_path) / 1024 / 1024, 1), " MB)")
  rm(result); gc()
}

# Stage 4b: reliever_pa_curves (lightweight — only needs starter_pa_full + reliever_rolling)
curves_path <- file.path(DATA_DIR, "reliever_pa_curves.rds")
if (file.exists(curves_path)) {
  message("[cache] reliever_pa_curves.rds")
} else {
  starter_pa_full  <- readRDS(spf_path)
  reliever_rolling <- readRDS(file.path(DATA_DIR, "reliever_rolling.rds"))
  build_reliever_pa_curves(starter_pa_full, reliever_rolling)
  rm(starter_pa_full, reliever_rolling); gc()
}

build_workload_features()
message("All done.")
