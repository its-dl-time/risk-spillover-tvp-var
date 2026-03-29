# ============================================================
# robustness.R
# R1 — Lag sensitivity         (lag 1 vs 2 vs 3)
# R2 — Cutpoint sensitivity    (COVID cut ±1 tháng)
# R3 — QVAR tau sensitivity     (bad: 0.05/0.10 | good: 0.90/0.95)
# R4 — TVP-VAR kappa sensitivity (0.97 vs 0.99 vs 0.995)
# Input:  returns_clean.rds | var_results.rds
# Output: figures/R*.png
# ============================================================

library(ConnectednessApproach)
library(ggplot2)
library(reshape2)
library(zoo)
library(xts)
library(openxlsx)

data_dir <- "D:/SCHOOL/HK5 (25-26)/GOI_2/final_term/data/processed/"
out_04   <- "D:/SCHOOL/HK5 (25-26)/GOI_2/final_term/output/04_robustness/"
fig_dir  <- paste0(out_04, "figures/")
tab_dir  <- paste0(out_04, "tables/")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

save_table <- function(df, name) {
  base <- paste0(tab_dir, name)
  saveRDS(df, paste0(base, ".rds"))
  write.xlsx(as.data.frame(df), paste0(base, ".xlsx"), rowNames = FALSE)
  message("  Saved table: ", name, " (.rds + .xlsx)")
}

returns_clean <- readRDS(paste0(data_dir, "returns_clean.rds"))
baseline      <- readRDS(paste0(data_dir, "var_results.rds"))

markets <- c("US", "Japan", "China", "Korea", "Singapore", "Vietnam")
H       <- 10

save_gg <- function(filename, p, width = 10, height = 6) {
  ggsave(paste0(fig_dir, filename), plot = p, width = width, height = height, dpi = 150)
  message("  Saved: ", filename)
}

# Helper: chạy static GFEVD và trả về Net + TCI
run_gfevd <- function(data_xts, lag) {
  dca <- ConnectednessApproach(as.zoo(data_xts),
                               nlag        = lag,
                               nfore       = H,
                               window.size = NULL,
                               corrected   = FALSE)
  list(Net   = setNames(as.numeric(dca$NET), markets),
       Total = as.numeric(dca$TCI))
}

# ============================================================
# R1 — LAG SENSITIVITY
# So sánh Net spillover và TCI với lag = 1, 2, 3
# ============================================================
cat("\n================================================================\n")
cat("  R1: LAG SENSITIVITY (lag 1 / 2 / 3)\n")
cat("================================================================\n")

lag_results <- list()
for (p in 1:3) {
  cat(sprintf("\nChạy GFEVD với lag = %d...\n", p))
  res <- run_gfevd(returns_clean, lag = p)
  lag_results[[paste0("Lag ", p)]] <- res
  cat(sprintf("  TCI = %.2f%%\n", res$Total))
  cat(sprintf("  Net: %s\n",
      paste(sprintf("%s=%.1f", markets, res$Net), collapse = " | ")))
}

# Bảng so sánh Net
net_lag_df <- do.call(rbind, lapply(names(lag_results), function(nm) {
  data.frame(Market = markets,
             Net    = lag_results[[nm]]$Net,
             Lag    = nm)
}))
net_lag_df$Lag <- factor(net_lag_df$Lag, levels = c("Lag 1", "Lag 2", "Lag 3"))

cat("\nBảng Net Spillover theo Lag:\n")
print(dcast(net_lag_df, Market ~ Lag, value.var = "Net", fun.aggregate = mean))

tci_lag <- data.frame(
  Lag = names(lag_results),
  TCI = sapply(lag_results, `[[`, "Total")
)
cat("\nTCI theo Lag:\n")
print(tci_lag)

p_r1 <- ggplot(net_lag_df, aes(x = Market, y = Net, fill = Lag)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Lag 1" = "steelblue",
                               "Lag 2" = "tomato",
                               "Lag 3" = "seagreen")) +
  labs(title    = "R1: Robustness — Lag Sensitivity",
       subtitle = paste0("TCI: ",
                         paste(sprintf("Lag%d=%.1f%%", 1:3,
                                       sapply(lag_results, `[[`, "Total")),
                               collapse = " | ")),
       x = NULL, y = "Net Spillover (%)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
print(p_r1)
save_gg("R1_lag_sensitivity.png", p_r1)

# Lưu bảng R1
T11 <- dcast(net_lag_df, Market ~ Lag, value.var = "Net", fun.aggregate = mean)
T11$TCI_note <- NA
for (i in seq_along(names(lag_results))) {
  nm <- names(lag_results)[i]
  T11$TCI_note[i] <- sprintf("%s: TCI=%.1f%%", nm, lag_results[[nm]]$Total)
}
save_table(T11, "T11_robustness_lag")
save_table(tci_lag, "T11b_tci_by_lag")

# ============================================================
# R2 — CUTPOINT SENSITIVITY
# Dịch COVID cutpoint: Early (2020-02-01), Base (2020-03-11), Late (2020-04-30)
# So sánh Net spillover của Pre period
# ============================================================
cat("\n================================================================\n")
cat("  R2: CUTPOINT SENSITIVITY\n")
cat("================================================================\n")

cutpoints <- list(
  "Early (Feb 2020)"  = as.Date("2020-02-01"),
  "Base (Mar 2020)"   = as.Date("2020-03-11"),
  "Late (Apr 2020)"   = as.Date("2020-04-30")
)

cut_results <- list()
for (nm in names(cutpoints)) {
  cut <- cutpoints[[nm]]
  pre  <- returns_clean[index(returns_clean) <  cut]
  post <- returns_clean[index(returns_clean) >= cut]

  cat(sprintf("\nCutpoint: %s (%s)\n", nm, cut))
  cat(sprintf("  Pre:  %d obs | Post: %d obs\n", nrow(pre), nrow(post)))

  res_pre  <- run_gfevd(pre,  lag = 1)
  res_post <- run_gfevd(post, lag = 1)

  cut_results[[nm]] <- list(pre = res_pre, post = res_post)
  cat(sprintf("  Pre  TCI=%.2f%% | VN Net=%.2f%%\n",
              res_pre$Total,  res_pre$Net["Vietnam"]))
  cat(sprintf("  Post TCI=%.2f%% | VN Net=%.2f%%\n",
              res_post$Total, res_post$Net["Vietnam"]))
}

# Plot: Vietnam Net spillover pre/post theo từng cutpoint
cut_vn_df <- do.call(rbind, lapply(names(cut_results), function(nm) {
  data.frame(
    Cutpoint = nm,
    Period   = c("Pre", "Post"),
    VN_Net   = c(cut_results[[nm]]$pre$Net["Vietnam"],
                 cut_results[[nm]]$post$Net["Vietnam"]),
    US_Net   = c(cut_results[[nm]]$pre$Net["US"],
                 cut_results[[nm]]$post$Net["US"]),
    TCI      = c(cut_results[[nm]]$pre$Total,
                 cut_results[[nm]]$post$Total)
  )
}))

cat("\nBảng VN Net & TCI theo Cutpoint:\n")
print(cut_vn_df)

p_r2 <- ggplot(cut_vn_df, aes(x = Cutpoint, y = VN_Net, fill = Period)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Pre" = "steelblue", "Post" = "tomato")) +
  labs(title    = "R2: Robustness — COVID Cutpoint Sensitivity",
       subtitle = "Vietnam Net Spillover trước/sau các mốc cutpoint khác nhau",
       x = "Cutpoint", y = "Vietnam Net Spillover (%)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
print(p_r2)
save_gg("R2_cutpoint_sensitivity.png", p_r2)

# Lưu bảng R2
save_table(cut_vn_df, "T12_robustness_cutpoint")

# ============================================================
# R3 — QVAR TAU SENSITIVITY
# Kiểm tra asymmetry có robust khi đổi tail threshold
# Bad news: tau = 0.05 vs 0.10
# Good news: tau = 0.90 vs 0.95
# ============================================================
cat("\n================================================================\n")
cat("  R3: QVAR TAU SENSITIVITY\n")
cat("================================================================\n")

tau_configs <- list(
  "Bad (tau=0.05)"  = 0.05,
  "Bad (tau=0.10)"  = 0.10,
  "Good (tau=0.90)" = 0.90,
  "Good (tau=0.95)" = 0.95
)

tau_results <- list()
for (label in names(tau_configs)) {
  tau_val <- tau_configs[[label]]
  cat(sprintf("\nChạy QVAR tau = %.2f...\n", tau_val))
  dca_q <- ConnectednessApproach(
    as.zoo(returns_clean),
    nlag          = 1,
    nfore         = H,
    window.size   = NULL,
    model         = "QVAR",
    corrected     = FALSE,
    connectedness = "Time",
    VAR_config    = list(QVAR = list(tau = tau_val, method = "fn"))
  )
  net_q <- setNames(as.numeric(dca_q$NET), markets)
  tci_q <- as.numeric(dca_q$TCI)
  tau_results[[label]] <- list(Net = net_q, Total = tci_q)
  cat(sprintf("  TCI=%.2f%% | VN Net=%.2f%% | US Net=%.2f%%\n",
              tci_q, net_q["Vietnam"], net_q["US"]))
}

# So sánh Net VN và US theo tau
tau_df <- do.call(rbind, lapply(names(tau_results), function(nm) {
  data.frame(Scenario = nm,
             Market   = markets,
             Net      = tau_results[[nm]]$Net,
             TCI      = tau_results[[nm]]$Total,
             Type     = ifelse(grepl("Bad", nm), "Bad News", "Good News"))
}))

vn_tau_df <- tau_df[tau_df$Market %in% c("Vietnam", "US"), ]
vn_tau_df$Scenario <- factor(vn_tau_df$Scenario,
                              levels = names(tau_configs))

cat("\nNet Spillover VN & US theo tau:\n")
print(dcast(vn_tau_df[, c("Scenario","Market","Net")],
            Scenario ~ Market, value.var = "Net", fun.aggregate = mean))

p_r3 <- ggplot(vn_tau_df, aes(x = Scenario, y = Net, fill = Type)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~Market, scales = "free_y") +
  scale_fill_manual(values = c("Bad News" = "tomato", "Good News" = "steelblue")) +
  labs(title    = "R3: Robustness — QVAR Tau Sensitivity",
       subtitle = "VN và US Net Spillover với các ngưỡng tau khác nhau",
       x = NULL, y = "Net Spillover (%)", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 15, hjust = 1))
print(p_r3)
save_gg("R3_qvar_tau_sensitivity.png", p_r3)
save_table(dcast(tau_df[, c("Scenario","Market","Net")],
                 Scenario ~ Market, value.var = "Net", fun.aggregate = mean),
           "T12_robustness_qvar_tau")

# ============================================================
# R4 — TVP-VAR KAPPA SENSITIVITY
# Kiểm tra TCI có robust với forgetting factor khác nhau
# kappa = 0.97 (fast-adapting) vs 0.99 (baseline) vs 0.995 (slow-adapting)
# ============================================================
cat("\n================================================================\n")
cat("  R4: TVP-VAR KAPPA SENSITIVITY (0.97 / 0.99 / 0.995)\n")
cat("================================================================\n")

kappas     <- c(0.97, 0.99, 0.995)
kappa_tcis <- list()

for (k in kappas) {
  cat(sprintf("Chạy TVP-VAR kappa = %.3f...\n", k))
  dca_k <- ConnectednessApproach(
    as.zoo(returns_clean),
    nlag          = 1,
    nfore         = H,
    window.size   = NULL,
    model         = "TVP-VAR",
    corrected     = FALSE,
    connectedness = "Time",
    VAR_config    = list(TVPVAR = list(
      kappa1 = k, kappa2 = k,
      prior  = "BayesPrior", gamma = 0.01
    ))
  )
  tci_k   <- as.numeric(dca_k$TCI)
  dates_k <- index(returns_clean)[1:length(tci_k)]
  # Trim warm-up
  trim_i  <- 61
  kappa_tcis[[paste0("k=", k)]] <- data.frame(
    Date  = dates_k[trim_i:length(dates_k)],
    TCI   = tci_k[trim_i:length(tci_k)],
    Kappa = paste0("κ=", k)
  )
  cat(sprintf("  Mean TCI=%.2f%% | Max TCI=%.2f%%\n",
              mean(tci_k[trim_i:length(tci_k)], na.rm = TRUE),
              max(tci_k[trim_i:length(tci_k)], na.rm = TRUE)))
}

tci_kappa_df <- do.call(rbind, kappa_tcis)
tci_kappa_df$Kappa <- factor(tci_kappa_df$Kappa,
                              levels = c("κ=0.97", "κ=0.99", "κ=0.995"))

events_r4 <- data.frame(
  Date  = as.Date(c("2020-03-11", "2020-12-31", "2022-02-24")),
  Label = c("COVID-19", "Vaccine", "Russia-Ukraine")
)

p_r4 <- ggplot(tci_kappa_df, aes(x = Date, y = TCI, color = Kappa)) +
  geom_line(linewidth = 0.6, alpha = 0.85) +
  geom_vline(data = events_r4,
             aes(xintercept = Date),
             linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_text(data = events_r4,
            aes(x = Date, y = Inf, label = Label),
            inherit.aes = FALSE,
            angle = 90, vjust = 1.3, hjust = 1.1, size = 3, color = "gray40") +
  scale_color_manual(values = c("κ=0.97"  = "tomato",
                                "κ=0.99"  = "steelblue",
                                "κ=0.995" = "seagreen")) +
  labs(title    = "R4: Robustness — TVP-VAR Kappa Sensitivity",
       subtitle = "TCI theo thời gian với 3 forgetting factors khác nhau",
       x = NULL, y = "TCI (%)", color = "Kappa") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
print(p_r4)
save_gg("R4_tvpvar_kappa_sensitivity.png", p_r4, width = 12, height = 5)

T13 <- do.call(rbind, lapply(names(kappa_tcis), function(nm) {
  df <- kappa_tcis[[nm]]
  data.frame(Kappa = nm,
             Mean  = round(mean(df$TCI, na.rm = TRUE), 2),
             Min   = round(min(df$TCI,  na.rm = TRUE), 2),
             Max   = round(max(df$TCI,  na.rm = TRUE), 2),
             SD    = round(sd(df$TCI,   na.rm = TRUE), 2))
}))
save_table(T13, "T13_robustness_kappa")

# ============================================================
# TỔNG KẾT ROBUSTNESS
# ============================================================
cat("\n================================================================\n")
cat("  TỔNG KẾT ROBUSTNESS\n")
cat("================================================================\n")

cat("\nR1 — Lag Sensitivity:\n")
cat("  Kết quả robust nếu: direction Net (dương/âm) không đổi theo lag\n")
for (nm in names(lag_results)) {
  vn  <- round(lag_results[[nm]]$Net["Vietnam"], 2)
  us  <- round(lag_results[[nm]]$Net["US"], 2)
  tci <- round(lag_results[[nm]]$Total, 2)
  cat(sprintf("  %-8s TCI=%.1f%% | US Net=%+.1f%% | VN Net=%+.1f%%  %s\n",
              nm, tci, us, vn,
              ifelse(us > 0 & vn < 0, "✅ Consistent", "⚠️  Check")))
}

cat("\nR2 — Cutpoint Sensitivity:\n")
cat("  Kết quả robust nếu: VN luôn net receiver ở cả 3 cutpoints\n")
for (nm in names(cut_results)) {
  pre_vn  <- round(cut_results[[nm]]$pre$Net["Vietnam"],  2)
  post_vn <- round(cut_results[[nm]]$post$Net["Vietnam"], 2)
  cat(sprintf("  %-20s Pre VN=%+.1f%% | Post VN=%+.1f%%  %s\n",
              nm, pre_vn, post_vn,
              ifelse(pre_vn < 0 & post_vn < 0, "✅ VN net receiver cả 2", "⚠️  Check")))
}

cat("\nR3 — QVAR Tau Sensitivity:\n")
cat("  Kết quả robust nếu: VN net receiver (âm) và US net transmitter (dương) ở mọi tau\n")
for (nm in names(tau_results)) {
  vn <- round(tau_results[[nm]]$Net["Vietnam"], 2)
  us <- round(tau_results[[nm]]$Net["US"], 2)
  cat(sprintf("  %-22s VN=%+.1f%% | US=%+.1f%%  %s\n",
              nm, vn, us,
              ifelse(vn < 0 & us > 0, "✅ Consistent", "⚠️  Check")))
}

cat("\nR4 — TVP-VAR Kappa Sensitivity:\n")
cat("  Kết quả robust nếu: pattern TCI (spike COVID) nhất quán qua các kappa\n")
for (nm in names(kappa_tcis)) {
  df <- kappa_tcis[[nm]]
  cat(sprintf("  %-12s Mean=%.1f%% | Range=[%.1f%%, %.1f%%]\n",
              nm,
              mean(df$TCI, na.rm = TRUE),
              min(df$TCI,  na.rm = TRUE),
              max(df$TCI,  na.rm = TRUE)))
}

message("\n✅ robustness.R hoàn tất.")
message("   Figures : ", fig_dir)
message("   Tables  : ", tab_dir)