# ============================================================
# diagnostics.R
# Mục đích: Kiểm định tiền mô hình
# Input:    returns_clean.rds | returns_pre.rds | returns_post.rds
# Output:   output/02_diagnostics/figures/ | tables/
# ============================================================

library(vars)
library(tseries)
library(ggplot2)
library(reshape2)
library(moments)
library(xts)
library(openxlsx)

# ============================================================
# 0. PATHS & HELPERS
# ============================================================
data_dir <- "D:/SCHOOL/HK5 (25-26)/GOI_2/final_term/data/processed/"
out_02   <- "D:/SCHOOL/HK5 (25-26)/GOI_2/final_term/output/02_diagnostics/"
fig_dir  <- paste0(out_02, "figures/")
tab_dir  <- paste0(out_02, "tables/")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

save_fig <- function(filename, plot_obj, width = 10, height = 6) {
  ggsave(paste0(fig_dir, filename), plot = plot_obj,
         width = width, height = height, dpi = 150)
  message("  Saved fig: ", filename)
}

save_table <- function(df, name) {
  base <- paste0(tab_dir, name)
  saveRDS(df, paste0(base, ".rds"))
  write.xlsx(as.data.frame(df), paste0(base, ".xlsx"), rowNames = TRUE)
  message("  Saved table: ", name, " (.rds + .xlsx)")
}

# ============================================================
# LOAD DATA
# ============================================================
returns_clean <- readRDS(paste0(data_dir, "returns_clean.rds"))
returns_pre   <- readRDS(paste0(data_dir, "returns_pre.rds"))
returns_post  <- readRDS(paste0(data_dir, "returns_post.rds"))

markets <- colnames(coredata(returns_clean))  # tự động lấy từ data
to_df   <- function(x) data.frame(Date = index(x), coredata(x))

# ============================================================
# 1. THỐNG KÊ MÔ TẢ
# ============================================================
cat("\n========== THỐNG KÊ MÔ TẢ (FULL SAMPLE) ==========\n")

desc_stats <- function(x) {
  df <- coredata(x)
  t(apply(df, 2, function(v) c(
    Mean = round(mean(v),     6),
    SD   = round(sd(v),       6),
    Min  = round(min(v),      6),
    Max  = round(max(v),      6),
    Skew = round(skewness(v), 4),
    Kurt = round(kurtosis(v), 4),
    JB_p = round(jarque.test(v)$p.value, 4)
  )))
}

T1 <- as.data.frame(desc_stats(returns_clean))
print(T1)
save_table(T1, "T1_descriptive_stats")

# ============================================================
# 2. EDA — TRỰC QUAN HÓA
# ============================================================

## 2.1 Return series
df_long <- melt(to_df(returns_clean), id.vars = "Date",
                variable.name = "Market", value.name = "Return")

p1 <- ggplot(df_long, aes(x = Date, y = Return)) +
  geom_line(alpha = 0.6, linewidth = 0.35, color = "steelblue") +
  geom_vline(xintercept = as.Date("2020-03-11"),
             linetype = "dashed", color = "red", linewidth = 0.5) +
  facet_wrap(~Market, scales = "free_y", ncol = 1) +
  labs(title    = "Log Returns các thị trường (2018–2025)",
       subtitle = "Đường đỏ: COVID cutpoint (11/03/2020)",
       x = NULL, y = "Log Return") +
  theme_minimal(base_size = 11)
print(p1)
save_fig("01_return_series.png", p1, width = 10, height = 14)

## 2.2 Correlation heatmap
cor_mat  <- cor(coredata(returns_clean))
cor_melt <- melt(cor_mat)

p2 <- ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3.5) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "tomato",
                       midpoint = 0, limits = c(-1, 1), name = "Corr") +
  labs(title = "Ma trận tương quan — Full Sample", x = NULL, y = NULL) +
  theme_minimal(base_size = 11)
print(p2)
save_fig("02_correlation_heatmap.png", p2)
save_table(as.data.frame(cor_mat), "T1b_correlation_matrix")

## 2.3 Boxplot
p3 <- ggplot(melt(to_df(returns_clean), id.vars = "Date",
                  variable.name = "Market", value.name = "Return"),
             aes(x = Market, y = Return, fill = Market)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
  labs(title = "Phân phối Return và Outliers", y = "Log Return") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")
print(p3)
save_fig("03_boxplot_returns.png", p3)

# ============================================================
# 3. KIỂM ĐỊNH TÍNH DỪNG — ADF TEST
# ============================================================
cat("\n========== ADF TEST ==========\n")

run_adf <- function(data, label) {
  cat("\n---", label, "---\n")
  res_df <- data.frame(
    Market  = markets,
    p_value = NA_real_,
    Result  = NA_character_,
    Period  = label,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(markets)) {
    p <- adf.test(coredata(data)[, markets[i]],
                  alternative = "stationary")$p.value
    res_df$p_value[i] <- round(p, 4)
    res_df$Result[i]  <- ifelse(p < 0.05, "I(0) Stationary", "I(1) Non-stationary")
  }
  print(res_df[, c("Market", "p_value", "Result")])
  return(res_df)
}

adf_full <- run_adf(returns_clean, "Full Sample")
adf_pre  <- run_adf(returns_pre,   "Pre-COVID")
adf_post <- run_adf(returns_post,  "Post-COVID")

T2 <- rbind(adf_full, adf_pre, adf_post)
save_table(T2, "T2_adf_test")

# ============================================================
# 4. LAG SELECTION — ĐẦY ĐỦ
# ============================================================
# Decision framework:
#   Bước 1 — Bảng AIC/HQ/SC/FPE đầy đủ tại từng lag 1→10
#   Bước 2 — Plot criterion values (chuẩn hóa) theo lag
#   Bước 3 — Portmanteau test tại TỪNG lag, plot p-value theo lag
#   Bước 4 — Quyết định:
#             (a) SC và AIC đồng thuận (chênh ≤ 2) → chọn SC
#             (b) Lệch > 2 → chọn lag NHỎ NHẤT mà Portmanteau p > 0.05
#             (c) Nhất quán giữa sub-periods được ưu tiên khi viết báo cáo

cat("\n========== LAG SELECTION ==========\n")

all_crit_dfs <- list()

run_lag_full <- function(data, label, lag_max = 10) {
  cat("\n===", label, "===\n")
  mat <- coredata(data)
  sel <- VARselect(mat, lag.max = lag_max, type = "const")

  # Bước 1: Bảng criterion đầy đủ
  crit_df         <- as.data.frame(t(sel$criteria))
  crit_df$Lag     <- 1:lag_max
  crit_df$Period  <- label
  cat("\nBảng tiêu chí theo lag:\n")
  print(round(crit_df[, c("Lag", "AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")], 4))

  # Bước 2: Plot criterion (chuẩn hóa)
  crit_long <- melt(crit_df[, c("Lag", "AIC(n)", "HQ(n)", "SC(n)")],
                    id.vars = "Lag", variable.name = "Criterion", value.name = "Value")
  crit_long <- do.call(rbind, lapply(split(crit_long, crit_long$Criterion), function(d) {
    rng <- max(d$Value) - min(d$Value)
    d$Value_norm <- if (rng < 1e-10) 0 else (d$Value - min(d$Value)) / rng
    d
  }))

  p_crit <- ggplot(crit_long, aes(x = Lag, y = Value_norm, color = Criterion)) +
    geom_line(linewidth = 0.8) + geom_point(size = 2.5) +
    scale_x_continuous(breaks = 1:lag_max) +
    labs(title    = paste("Criterion Values theo Lag —", label),
         subtitle = "Giá trị chuẩn hóa [0,1]",
         x = "Lag", y = "Normalized Value") +
    theme_minimal(base_size = 11)
  print(p_crit)
  save_fig(paste0("04_criterion_", gsub(" |-", "_", label), ".png"), p_crit)

  # Bước 3: Portmanteau tại từng lag
  cat("\nPortmanteau test theo từng lag:\n")
  pt_df <- data.frame(Lag = 1:lag_max, PT_pvalue = NA_real_)
  for (p in 1:lag_max) {
    tryCatch({
      m <- vars::VAR(mat, p = p, type = "const")
      
      pt_df$PT_pvalue[p] <- serial.test(m, lags.pt = 10,
                                         type = "PT.asymptotic")$serial$p.value
    }, error = function(e) {})
  }
  print(round(pt_df, 4))

  p_pt <- ggplot(pt_df, aes(x = Lag, y = PT_pvalue)) +
    geom_col(aes(fill = PT_pvalue > 0.05), alpha = 0.8) +
    geom_hline(yintercept = 0.05, linetype = "dashed",
               color = "red", linewidth = 0.8) +
    scale_x_continuous(breaks = 1:lag_max) +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                      labels = c("Còn autocorrelation", "Residual sạch"), name = "") +
    labs(title    = paste("Portmanteau p-value theo Lag —", label),
         subtitle = "Xanh = residual sạch (p > 0.05)",
         x = "Lag", y = "p-value") +
    theme_minimal(base_size = 11)
  print(p_pt)
  save_fig(paste0("05_portmanteau_", gsub(" |-", "_", label), ".png"), p_pt)

  # Bước 4: Quyết định
  sc_lag     <- sel$selection["SC(n)"]
  aic_lag    <- sel$selection["AIC(n)"]
  clean_lags <- pt_df$Lag[!is.na(pt_df$PT_pvalue) & pt_df$PT_pvalue > 0.05]
  min_clean  <- ifelse(length(clean_lags) > 0, min(clean_lags), aic_lag)

  if (abs(aic_lag - sc_lag) <= 2) {
    chosen <- sc_lag
    cat(sprintf("  → SC=%d AIC=%d đồng thuận → chọn SC=%d\n", sc_lag, aic_lag, chosen))
  } else {
    chosen <- min_clean
    cat(sprintf("  → SC=%d AIC=%d lệch >2 → chọn lag sạch nhỏ nhất=%d\n",
                sc_lag, aic_lag, chosen))
  }
  cat(sprintf("  ✅ Lag cuối cùng [%s]: %d\n", label, chosen))

  # Lưu crit_df để export
  all_crit_dfs[[label]] <<- crit_df

  return(chosen)
}

lag_full <- run_lag_full(returns_clean, "Full Sample")
lag_pre  <- run_lag_full(returns_pre,   "Pre-COVID")
lag_post <- run_lag_full(returns_post,  "Post-COVID")

cat("\n>>> Tóm tắt lag:\n")
cat("  Full:", lag_full, "| Pre-COVID:", lag_pre, "| Post-COVID:", lag_post, "\n")

# Lưu lag choices
saveRDS(list(full = lag_full, pre = lag_pre, post = lag_post),
        paste0(data_dir, "chosen_lags.rds"))

# Lưu bảng criterion đầy đủ cho cả 3 periods
T3 <- do.call(rbind, all_crit_dfs)
save_table(T3, "T3_lag_selection_all")

# Bảng tóm tắt lag được chọn
T3b <- data.frame(
  Period     = c("Full Sample", "Pre-COVID", "Post-COVID"),
  Chosen_Lag = c(lag_full, lag_pre, lag_post)
)
save_table(T3b, "T3b_lag_chosen")

# ============================================================
# 5. GRANGER CAUSALITY → VIETNAM (Full Sample)
# ============================================================
cat("\n========== GRANGER CAUSALITY → VIETNAM ==========\n")

senders  <- setdiff(markets, "Vietnam")
var_full <- vars::VAR(coredata(returns_clean), p = lag_full, type = "const")
gc_res <- data.frame(Sender  = senders,
                     F_stat  = NA_real_,
                     p_value = NA_real_,
                     stringsAsFactors = FALSE)
for (i in seq_along(senders)) {
  gc <- causality(var_full, cause = senders[i])$Granger
  gc_res$F_stat[i]  <- round(gc$statistic, 3)
  gc_res$p_value[i] <- round(gc$p.value,   4)
  cat(sprintf("  %-12s F=%.3f, p=%.4f  %s\n",
              senders[i], gc$statistic, gc$p.value,
              ifelse(gc$p.value < 0.05, "✅ Granger-causes VN", "❌ No causality")))
}

p_gc <- ggplot(gc_res, aes(x = Sender, y = p_value, fill = p_value < 0.05)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  geom_text(aes(label = round(p_value, 3)), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                    labels = c("Không nhân quả", "Có nhân quả"), name = "") +
  labs(title = "Granger Causality → Vietnam (Full Sample)",
       x = "Thị trường", y = "p-value") +
  theme_minimal(base_size = 11)
print(p_gc)
save_fig("06_granger_causality.png", p_gc)
save_table(gc_res, "T4_granger_full")

# ============================================================
# 6. RESIDUAL DIAGNOSTICS (Full Sample)
# ============================================================
cat("\n========== RESIDUAL DIAGNOSTICS (Full Sample) ==========\n")

pt   <- serial.test(var_full, lags.pt = 10, type = "PT.asymptotic")
norm <- normality.test(var_full)
ev   <- max(roots(var_full))

diag_df <- data.frame(
  Test       = c("Portmanteau (serial)", "Normality (JB)", "Max Eigenvalue"),
  Statistic  = c(round(pt$serial$statistic, 4),
                 round(norm$jb.mul$JB$statistic, 4),
                 round(ev, 4)),
  p_value    = c(round(pt$serial$p.value, 4),
                 round(norm$jb.mul$JB$p.value, 4),
                 NA),
  Result     = c(ifelse(pt$serial$p.value   > 0.05, "✅ No autocorrelation", "⚠️ Autocorrelation"),
                 ifelse(norm$jb.mul$JB$p.value > 0.05, "✅ Normal", "⚠️ Non-normal"),
                 ifelse(ev < 1, "✅ Stable", "❌ Unstable"))
)
print(diag_df)
save_table(diag_df, "T5_residual_diagnostics_full")

message("\n✅ diagnostics.R hoàn tất.")
message("   Figures : ", fig_dir)
message("   Tables  : ", tab_dir)