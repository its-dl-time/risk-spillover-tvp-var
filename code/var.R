# ============================================================
# var_model.R
# Mục đích: VAR(1) đầy đủ kiểm định cho 3 periods
# Pipeline:
#   A. Stability (roots, CUSUM)
#   B. Residual diagnostics (LM, ARCH-LM, JB)
#   C. Cointegration note (I(0) confirmed)
#   D. Granger, IRF, Static GFEVD Spillover
#   E. TVP-VAR Time Connectedness
#   F. Frequency Connectedness (Short vs Long run)
#   G. QVAR Asymmetric (Bad vs Good news)
# ============================================================

library(vars)
library(ConnectednessApproach)
library(urca)
library(ggplot2)
library(reshape2)
library(zoo)
library(xts)
library(openxlsx)

data_dir <- "D:/SCHOOL/HK5 (25-26)/GOI_2/final_term/data/processed/"
out_03   <- "D:/SCHOOL/HK5 (25-26)/GOI_2/final_term/output/03_var_model/"
fig_dir  <- paste0(out_03, "figures/")
tab_dir  <- paste0(out_03, "tables/")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

save_table <- function(df, name) {
  base <- paste0(tab_dir, name)
  saveRDS(df, paste0(base, ".rds"))
  write.xlsx(as.data.frame(df), paste0(base, ".xlsx"), rowNames = FALSE)
  message("  Saved table: ", name, " (.rds + .xlsx)")
}

returns_clean <- readRDS(paste0(data_dir, "returns_clean.rds"))
returns_pre   <- readRDS(paste0(data_dir, "returns_pre.rds"))
returns_post  <- readRDS(paste0(data_dir, "returns_post.rds"))

markets <- c("US", "Japan", "China", "Korea", "Singapore", "Vietnam")
LAG     <- 1
H       <- 10

save_gg <- function(filename, plot_obj, width = 10, height = 6) {
  ggsave(paste0(fig_dir, filename), plot = plot_obj,
         width = width, height = height, dpi = 150)
  message("  Saved: ", filename)
}

datasets <- list(
  "Full Sample" = returns_clean,
  "Pre-COVID"   = returns_pre,
  "Post-COVID"  = returns_post
)

# Storage
var_models    <- list()
diag_results  <- list()
coint_results <- list()
spillover_res <- list()
granger_all   <- data.frame()

# ============================================================
# HELPER: GFEVD Spillover via ConnectednessApproach
# ============================================================
compute_spillover <- function(data_xts, label) {
  dca <- ConnectednessApproach(as.zoo(data_xts),
                                nlag          = LAG,
                                nfore         = H,
                                window.size   = NULL,
                                corrected     = FALSE)
  sp <- list(
    TABLE = dca$TABLE,
    To    = as.numeric(dca$TO),
    From  = as.numeric(dca$FROM),
    Net   = as.numeric(dca$NET),
    Total = as.numeric(dca$TCI)
  )
  names(sp$To)   <- markets
  names(sp$From) <- markets
  names(sp$Net)  <- markets
  sp
}

# ============================================================
# LOOP QUA 3 PERIODS
# ============================================================
for (nm in names(datasets)) {
  cat(sprintf("\n\n================================================================\n"))
  cat(sprintf("  PERIOD: %s\n", nm))
  cat(sprintf("================================================================\n"))

  dat  <- datasets[[nm]]
  mat  <- coredata(dat)
  slug <- gsub(" |-", "_", nm)

  # Ước lượng VAR
  vm <- vars::VAR(mat, p = LAG, type = "const")
  var_models[[nm]] <- vm

  # ============================================================
  # BLOCK A: STABILITY
  # ============================================================
  cat(sprintf("\n--- A. STABILITY [%s] ---\n", nm))

  rv <- roots(vm)
  cat("Roots of characteristic polynomial (modulus):\n")
  print(round(rv, 4))
  cat(sprintf("Max root: %.4f  %s\n", max(rv),
              ifelse(max(rv) < 1, "✅ Stable", "❌ Unstable")))

  # Root plot
  png(paste0(fig_dir, "A1_roots_", slug, ".png"), width = 600, height = 600)
  plot(stability(vm))
  dev.off()
  message("  Saved: A1_roots_", slug, ".png")

  # CUSUM plot (trên residuals Vietnam equation)
  vn_resid <- residuals(vm)[, "Vietnam"]
  cusum_df <- data.frame(
    t     = seq_along(vn_resid),
    cusum = cumsum(vn_resid) / sd(vn_resid)
  )
  n_obs <- nrow(cusum_df)
  bound <- 0.948 * sqrt(n_obs)   # 5% boundary (Brownian motion)

  p_cusum <- ggplot(cusum_df, aes(x = t, y = cusum)) +
    geom_line(color = "steelblue", linewidth = 0.7) +
    geom_hline(yintercept = c(-bound, bound),
               linetype = "dashed", color = "red") +
    labs(title    = paste("CUSUM — Vietnam Equation —", nm),
         subtitle = "Đường đỏ: 5% significance boundary",
         x = "Observation", y = "Cumulative Sum") +
    theme_minimal(base_size = 11)
  print(p_cusum)
  save_gg(paste0("A2_cusum_", slug, ".png"), p_cusum)

  # ============================================================
  # BLOCK B: RESIDUAL DIAGNOSTICS
  # ============================================================
  cat(sprintf("\n--- B. RESIDUAL DIAGNOSTICS [%s] ---\n", nm))

  ## B1. Serial correlation — Portmanteau LM test
  pt <- serial.test(vm, lags.pt = 10, type = "PT.asymptotic")
  cat(sprintf("  LM Serial (Portmanteau) : p = %.4f  %s\n",
              pt$serial$p.value,
              ifelse(pt$serial$p.value > 0.05,
                     "✅ No autocorrelation", "⚠️  Autocorrelation present")))

  ## B2. ARCH-LM — Box-Ljung test trên residuals bình phương
  cat("  ARCH-LM test per equation (Ljung-Box on r^2):\n")
  arch_pvals <- sapply(markets, function(m) {
    r <- residuals(vm)[, m]
    Box.test(r^2, lag = 5, type = "Ljung-Box")$p.value
  })
  for (m in markets) {
    cat(sprintf("    %-10s p = %.4f  %s\n", m, arch_pvals[m],
                ifelse(arch_pvals[m] > 0.05,
                       "✅ No ARCH", "⚠️  ARCH effect")))
  }

  ## B3. Normality — JB test on multivariate residuals
  norm <- normality.test(vm, multivariate.only = FALSE)
  jb_p <- norm$jb.mul$JB$p.value
  cat(sprintf("  Normality (JB multivar) : p = %.4f  %s\n", jb_p,
              ifelse(jb_p > 0.05,
                     "✅ Normal", "⚠️  Non-normal (common in daily data)")))

  diag_results[[nm]] <- list(
    portmanteau = pt$serial$p.value,
    arch        = arch_pvals,
    jb          = jb_p,
    max_root    = max(rv)
  )

  # ============================================================
  # BLOCK C: COINTEGRATION NOTE
  # ============================================================
  # Log returns đã là I(0) (confirmed bởi ADF trong diagnostics.R)
  # → Johansen test không áp dụng (chỉ dùng cho I(1) price levels)
  # → VAR in returns là specification đúng
  # → VECM không cần thiết
  cat(sprintf("\n--- C. COINTEGRATION [%s] ---\n", nm))
  cat("  Variables are I(0) log returns (ADF confirmed in diagnostics.R)\n")
  cat("  → Johansen test not applicable. VAR in returns is correct.\n")
  coint_results[[nm]] <- "I(0) confirmed — VAR in returns appropriate"

  # ============================================================
  # BLOCK D: ỨNG DỤNG
  # ============================================================

  ## D1. Granger Causality → Vietnam
  cat(sprintf("\n--- D1. GRANGER CAUSALITY → Vietnam [%s] ---\n", nm))
  senders <- c("US", "Japan", "China", "Korea", "Singapore")
  gc_df   <- data.frame(Sender  = senders,
                        F_stat  = NA_real_,
                        p_value = NA_real_,
                        Period  = nm)

  for (i in seq_along(senders)) {
    gc <- causality(vm, cause = senders[i])$Granger
    gc_df$F_stat[i]  <- round(gc$statistic, 3)
    gc_df$p_value[i] <- round(gc$p.value,   4)
    cat(sprintf("  %-10s F = %.3f, p = %.4f  %s\n",
                senders[i], gc$statistic, gc$p.value,
                ifelse(gc$p.value < 0.05, "✅ Granger-causes VN", "❌ No causality")))
  }

  # Lưu để so sánh sau
  granger_all <- rbind(granger_all, gc_df)

  ## D2. IRF → Vietnam
  cat(sprintf("\n--- D2. IRF → Vietnam [%s] ---\n", nm))
  irf_res <- irf(vm,
                 impulse  = senders,
                 response = "Vietnam",
                 n.ahead  = H,
                 boot     = TRUE,
                 ci       = 0.95,
                 runs     = 300)

  irf_df <- do.call(rbind, lapply(senders, function(s) {
    data.frame(Horizon = 0:H,
               IRF     = irf_res$irf[[s]][, "Vietnam"],
               Lower   = irf_res$Lower[[s]][, "Vietnam"],
               Upper   = irf_res$Upper[[s]][, "Vietnam"],
               Impulse = s)
  }))

  p_irf <- ggplot(irf_df, aes(x = Horizon)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper),
                fill = "steelblue", alpha = 0.2) +
    geom_line(aes(y = IRF), color = "steelblue", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    facet_wrap(~Impulse, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = 0:H) +
    labs(title    = paste("IRF: Shock → Vietnam —", nm),
         subtitle = "Vùng bóng: 95% bootstrap CI | Horizon: ngày giao dịch",
         x = "Horizon", y = "Response") +
    theme_minimal(base_size = 11)
  print(p_irf)
  save_gg(paste0("D2_irf_", slug, ".png"), p_irf, width = 10, height = 7)

  ## D3. GFEVD Spillover Index
  cat(sprintf("\n--- D3. GFEVD SPILLOVER INDEX [%s] ---\n", nm))
  sp <- compute_spillover(dat, nm)
  spillover_res[[nm]] <- sp
  cat(sprintf("Total Connectedness Index: %.2f%%\n", sp$Total))
  print(sp$TABLE)
  save_table(as.data.frame(sp$TABLE), paste0("T5_spillover_", slug))

  # Spillover bar chart
  sp_df <- data.frame(Market = markets,
                      To     = sp$To,
                      From   = sp$From,
                      Net    = sp$Net)
  sp_long <- melt(sp_df, id.vars = "Market",
                  variable.name = "Type", value.name = "Value")

  p_sp <- ggplot(sp_long, aes(x = Market, y = Value, fill = Type)) +
    geom_col(position = "dodge", alpha = 0.85) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    scale_fill_manual(values = c(To = "steelblue", From = "tomato",
                                 Net = "seagreen")) +
    labs(title    = paste("Spillover Index —", nm),
         subtitle = paste0("TCI = ", round(sp$Total, 1), "%"),
         x = NULL, y = "Spillover (%)") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")
  print(p_sp)
  save_gg(paste0("D3_spillover_", slug, ".png"), p_sp)
}

# ============================================================
# TỔNG HỢP: So sánh Net Spillover + TCI qua 3 periods
# ============================================================
cat("\n\n================================================================\n")
cat("  TỔNG HỢP SO SÁNH 3 PERIODS\n")
cat("================================================================\n")

# Granger so sánh
cat("\nGranger Causality → Vietnam (so sánh 3 periods):\n")
granger_all$Period <- factor(granger_all$Period,
                             levels = c("Pre-COVID", "Full Sample", "Post-COVID"))
print(dcast(granger_all, Sender ~ Period, value.var = "p_value", fun.aggregate = mean))

p_gc <- ggplot(granger_all, aes(x = Sender, y = p_value, fill = Period)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("Pre-COVID"   = "steelblue",
                               "Full Sample" = "gray60",
                               "Post-COVID"  = "tomato")) +
  labs(title    = "Granger Causality → Vietnam: so sánh 3 periods",
       subtitle = "Đường đỏ: p = 0.05",
       x = NULL, y = "p-value") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
print(p_gc)
save_gg("D1_granger_comparison.png", p_gc)

net_df <- data.frame(
  Market = rep(markets, 3),
  Period = rep(names(datasets), each = length(markets)),
  Net    = c(spillover_res[["Full Sample"]]$Net,
             spillover_res[["Pre-COVID"]]$Net,
             spillover_res[["Post-COVID"]]$Net)
)
net_df$Period <- factor(net_df$Period,
                        levels = c("Pre-COVID", "Full Sample", "Post-COVID"))

cat("\nNet Spillover by Period:\n")
print(dcast(net_df, Market ~ Period, value.var = "Net", fun.aggregate = mean))

cat("\nTotal Connectedness Index:\n")
for (nm in names(spillover_res)) {
  cat(sprintf("  %-15s %.2f%%\n", nm, spillover_res[[nm]]$Total))
}

p_net <- ggplot(net_df, aes(x = Market, y = Net, fill = Period)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Pre-COVID"   = "steelblue",
                               "Full Sample" = "gray60",
                               "Post-COVID"  = "tomato")) +
  labs(title    = "Net Spillover so sánh qua các giai đoạn",
       subtitle = "Dương = Net Transmitter | Âm = Net Receiver",
       x = NULL, y = "Net Spillover (%)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
print(p_net)
save_gg("D4_net_comparison.png", p_net)

# Lưu bảng tổng hợp
T8 <- dcast(net_df, Market ~ Period, value.var = "Net", fun.aggregate = mean)
T9 <- dcast(granger_all, Sender ~ Period, value.var = "p_value", fun.aggregate = mean)
T10 <- do.call(rbind, lapply(names(diag_results), function(nm) {
  d <- diag_results[[nm]]
  data.frame(Period = nm, Portmanteau_p = round(d$portmanteau, 4),
             JB_p = round(d$jb, 4), Max_Root = round(d$max_root, 4))
}))
save_table(T8,  "T8_net_spillover_comparison")
save_table(T9,  "T9_granger_comparison")
save_table(T10, "T10_residual_diagnostics")



# Helper: extract NET time series từ ConnectednessApproach object
extract_net_df <- function(dca_obj, dates_full, window_size = 0) {
  net_raw <- dca_obj$NET
  start_i <- if (window_size > 0) window_size + 1 else 1
  dates_ts <- dates_full[start_i:length(dates_full)]

  if (inherits(net_raw, c("xts", "zoo"))) {
    df        <- data.frame(coredata(net_raw))
    df$Date   <- as.Date(index(net_raw))
  } else if (is.matrix(net_raw) || is.data.frame(net_raw)) {
    df        <- as.data.frame(net_raw)
    df$Date   <- dates_ts[1:nrow(df)]
  } else {
    df        <- data.frame(matrix(as.numeric(net_raw),
                                   ncol = length(markets)))
    df$Date   <- dates_ts[1:nrow(df)]
  }
  net_cols <- setdiff(colnames(df), "Date")
  # Trong extract_net_df, thêm:
  df <- df[df$Date > as.Date("2018-06-01"), ]
  
  if (length(net_cols) == length(markets))
    colnames(df)[colnames(df) != "Date"] <- markets
  return(df)
}

# Helper: extract TCI time series
extract_tci_df <- function(dca_obj, dates_full, window_size = 0) {
  tci_raw <- as.numeric(dca_obj$TCI)
  start_i <- if (window_size > 0) window_size + 1 else 1
  dates_ts <- dates_full[start_i:length(dates_full)]
  min_len  <- min(length(tci_raw), length(dates_ts))
  data.frame(Date = dates_ts[1:min_len], TCI = tci_raw[1:min_len])
}

# ============================================================
# BLOCK E: TVP-VAR TIME CONNECTEDNESS
# Thay thế Rolling Window — time-varying, không cần chọn window size
# Ref: Antonakakis & Gabauer (2020)
# ============================================================
cat("\n\n================================================================\n")
cat("  BLOCK E: TVP-VAR TIME CONNECTEDNESS\n")
cat("================================================================\n")

dca_tvp <- ConnectednessApproach(
  as.zoo(returns_clean),
  nlag          = LAG,
  nfore         = H,
  window.size   = NULL,
  model         = "TVP-VAR",
  corrected     = FALSE,
  connectedness = "Time",
  VAR_config    = list(TVPVAR = list(
    kappa1 = 0.99,
    kappa2 = 0.99,
    prior  = "BayesPrior",
    gamma  = 0.01
  ))
)

# E1: TCI theo thời gian
tci_tvp_df <- extract_tci_df(dca_tvp, index(returns_clean))

# Mốc sự kiện COVID-focused
events_covid <- data.frame(
  Date  = as.Date(c("2020-03-11", "2020-12-31",
                    "2022-01-01", "2022-02-24",
                    "2023-03-10", "2024-09-18")),
  Label = c("COVID-19", "Vaccine",
            "RCEP", "Russia-Ukraine",
            "SVB", "Fed Cut"),
  Color = c("red", "darkgreen",
            "steelblue", "darkred",
            "brown", "forestgreen"),
  stringsAsFactors = FALSE
)

p_tvp_tci <- ggplot(tci_tvp_df, aes(x = Date, y = TCI)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_vline(data = events_covid,
             aes(xintercept = Date, color = Label),
             linetype = "dashed", linewidth = 0.6) +
  scale_color_manual(values = setNames(events_covid$Color, events_covid$Label),
                     name = "Sự kiện") +
  labs(title    = "Total Connectedness Index — TVP-VAR",
       subtitle = "Antonakakis & Gabauer (2020) | kappa=0.99",
       x = NULL, y = "TCI (%)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))
print(p_tvp_tci)
save_gg("E1_tvpvar_TCI.png", p_tvp_tci, width = 12, height = 5)

# E2: Net spillover từng thị trường theo thời gian
net_tvp_df   <- extract_net_df(dca_tvp, index(returns_clean))
net_tvp_long <- melt(net_tvp_df, id.vars = "Date",
                     variable.name = "Market", value.name = "Net")

p_tvp_net <- ggplot(net_tvp_long, aes(x = Date, y = Net, color = Market)) +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(data = events_covid,
             aes(xintercept = Date),
             linetype = "dashed", color = "gray70", linewidth = 0.4) +
  facet_wrap(~Market, scales = "free_y", ncol = 2) +
  labs(title    = "Net Spillover theo thời gian — TVP-VAR",
       subtitle = "Dương = Net Transmitter | Âm = Net Receiver",
       x = NULL, y = "Net (%)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")
print(p_tvp_net)
save_gg("E2_tvpvar_net.png", p_tvp_net, width = 12, height = 10)
message("  Saved: E1_tvpvar_TCI.png | E2_tvpvar_net.png")

# ============================================================
# BLOCK F: FREQUENCY CONNECTEDNESS (Short vs Long run)
# Short: 1-5 ngày | Long: >5 ngày
# Ref: Barunik & Krehlik (2018)
# ============================================================
cat("\n\n================================================================\n")
cat("  BLOCK F: FREQUENCY CONNECTEDNESS (Short vs Long run)\n")
cat("================================================================\n")

# partition = c(pi, pi/5, 0):
#   Band 1 [pi/5, pi]   = freq > pi/5 = cycles < 5 ngày  → SHORT run
#   Band 2 [0,   pi/5]  = freq < pi/5 = cycles > 5 ngày  → LONG run
freq_results <- list()

for (nm in names(datasets)) {
  cat(sprintf("\n--- Frequency Connectedness [%s] ---\n", nm))

  dca_freq <- ConnectednessApproach(
    as.zoo(datasets[[nm]]),
    nlag          = LAG,
    nfore         = H,
    window.size   = NULL,
    model         = "VAR",
    corrected     = FALSE,
    connectedness = "Frequency",
    Connectedness_config = list(
      FrequencyConnectedness = list(
        partition   = c(pi, pi/5, 0),
        generalized = TRUE,
        scenario    = "ABS"
      )
    )
  )

  # Extract TO/FROM/NET cho từng band
  # Band 1 = Short run, Band 2 = Long run
  freq_results[[nm]] <- dca_freq

  # Print TCI per band
  tci_bands <- dca_freq$TCI
  cat(sprintf("  Short-run TCI : %.2f%%\n", as.numeric(tci_bands)[1]))
  cat(sprintf("  Long-run  TCI : %.2f%%\n", as.numeric(tci_bands)[2]))
}

# Thay toàn bộ đoạn từ "freq_vn_df <- do.call..." đến "save_table(freq_vn_df...)"
# bằng đoạn này:

freq_vn_df <- do.call(rbind, lapply(names(freq_results), function(nm) {
  net_arr   <- freq_results[[nm]]$NET  # array [1, 6, 3]
  net_short <- net_arr[1, , "1-5"]    # band Short: 1-5 ngày
  net_long  <- net_arr[1, , "5-Inf"]  # band Long: >5 ngày
  data.frame(
    Period    = nm,
    Market    = markets,
    Net_Short = as.numeric(net_short),
    Net_Long  = as.numeric(net_long)
  )
}))

cat("\nNet Spillover VN — Short vs Long run:\n")
vn_freq <- freq_vn_df[freq_vn_df$Market == "Vietnam", ]
print(vn_freq[, c("Period", "Net_Short", "Net_Long")])
save_table(freq_vn_df, "T_freq_net_spillover")

# Plot: VN Net Short vs Long, 3 periods
vn_freq_long <- melt(vn_freq, id.vars = c("Period", "Market"),
                     variable.name = "Horizon", value.name = "Net")
vn_freq_long$Horizon <- ifelse(vn_freq_long$Horizon == "Net_Short",
                               "Short run (1-5 days)", "Long run (>5 days)")
vn_freq_long$Period  <- factor(vn_freq_long$Period,
                               levels = c("Pre-COVID", "Full Sample", "Post-COVID"))

p_freq <- ggplot(vn_freq_long, aes(x = Period, y = Net, fill = Horizon)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Short run (1-5 days)" = "tomato",
                               "Long run (>5 days)"   = "steelblue")) +
  labs(title    = "F: Vietnam Net Spillover — Short vs Long Run",
       subtitle = "Barunik & Krehlik (2018) Frequency Decomposition",
       x = NULL, y = "Net Spillover (%)", fill = "Horizon") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
print(p_freq)
save_gg("F1_frequency_VN_net.png", p_freq)
message("  Saved: F1_frequency_VN_net.png")

# ============================================================
# BLOCK G: QVAR ASYMMETRIC CONNECTEDNESS
# Bad news: tau = 0.05 (lower tail)
# Good news: tau = 0.95 (upper tail)
# Ref: Ando et al. (2022)
# ============================================================
cat("\n\n================================================================\n")
cat("  BLOCK G: QVAR ASYMMETRIC CONNECTEDNESS\n")
cat("================================================================\n")

qvar_results <- list()

for (tau_val in c(0.05, 0.95)) {
  label <- ifelse(tau_val == 0.05, "Bad News (tau=0.05)", "Good News (tau=0.95)")
  cat(sprintf("\n--- QVAR [%s] ---\n", label))

  dca_q <- ConnectednessApproach(
    as.zoo(returns_clean),
    nlag          = LAG,
    nfore         = H,
    window.size   = NULL,
    model         = "QVAR",
    corrected     = FALSE,
    connectedness = "Time",
    VAR_config    = list(QVAR = list(
      tau    = tau_val,
      method = "fn"
    ))
  )

  net_q <- setNames(as.numeric(dca_q$NET), markets)
  tci_q <- as.numeric(dca_q$TCI)

  qvar_results[[label]] <- list(Net = net_q, Total = tci_q)

  cat(sprintf("  TCI = %.2f%%\n", tci_q))
  cat(sprintf("  VN Net = %.2f%%  US Net = %.2f%%\n",
              net_q["Vietnam"], net_q["US"]))
}

# So sánh Bad vs Good news
qvar_df <- do.call(rbind, lapply(names(qvar_results), function(nm) {
  data.frame(Market   = markets,
             Net      = qvar_results[[nm]]$Net,
             Scenario = nm)
}))
qvar_df$Scenario <- factor(qvar_df$Scenario,
                            levels = c("Bad News (tau=0.05)", "Good News (tau=0.95)"))

cat("\nNet Spillover: Bad vs Good News:\n")
print(dcast(qvar_df, Market ~ Scenario, value.var = "Net", fun.aggregate = mean))
save_table(dcast(qvar_df, Market ~ Scenario, value.var = "Net", fun.aggregate = mean),
           "T_qvar_asymmetric")

p_qvar <- ggplot(qvar_df, aes(x = Market, y = Net, fill = Scenario)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Bad News (tau=0.05)"  = "tomato",
                               "Good News (tau=0.95)" = "steelblue")) +
  labs(title    = "G: Asymmetric Spillover — Bad vs Good News",
       subtitle = "QVAR Connectedness | Ando et al. (2022)",
       x = NULL, y = "Net Spillover (%)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
print(p_qvar)
save_gg("G1_qvar_asymmetric.png", p_qvar)
message("  Saved: G1_qvar_asymmetric.png")


# ============================================================
# LƯU KẾT QUẢ
# ============================================================
saveRDS(list(
  var_models    = var_models,
  diagnostics   = diag_results,
  cointegration = coint_results,
  spillover     = spillover_res,
  freq_results  = freq_results,
  qvar_results  = qvar_results
), paste0(data_dir, "var_results.rds"))

message("\n✅ var_model.R hoàn tất. Figures: ", fig_dir)
