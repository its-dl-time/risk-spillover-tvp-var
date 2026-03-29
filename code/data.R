# ============================================================
# data.R
# Mục đích: Thu thập, xử lý dữ liệu và tách sub-period
# Output:   returns_clean.rds | returns_pre.rds | returns_post.rds
# ============================================================

# install.packages(c("quantmod", "PerformanceAnalytics", "zoo", "readxl"))
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(readxl)

### HELPER FUNCTION
# install.packages("openxlsx")
library(openxlsx)
out_01 <- "D:\\SCHOOL\\HK5 (25-26)\\GOI_2\\final_term\\output\\figures\\01_descriptive"

save_table <- function(df, name, folder) {
  dir.create(paste0(folder, "tables/"), recursive = TRUE, showWarnings = FALSE)
  base <- paste0(folder, "tables/", name)
  saveRDS(df,   paste0(base, ".rds"))
  write.xlsx(df, paste0(base, ".xlsx"), rowNames = FALSE)
  message("  Saved: ", name, " (.rds + .xlsx)")
}

# ============================================================
# 1. THU THẬP DỮ LIỆU
# ============================================================
start_date <- "2018-01-01"
end_date   <- "2025-12-31"

message("Đang tải dữ liệu quốc tế từ Yahoo Finance...")
getSymbols(c("^GSPC", "^N225", "000001.SS", "^KS11","^STI"),
           src = "yahoo", from = start_date, to = end_date,
           warnings = FALSE, auto.assign = TRUE)

# --- VNINDEX từ file Excel (chỉnh path nếu cần) ---
vn_path  <- "D:/SCHOOL/HK5 (25-26)/GOI_2/final_term/data/raw/vni.xlsx"
vn_data  <- read_excel(vn_path)
vn_price <- xts(as.numeric(gsub(",", ".", vn_data$GiaDongCua)),
                order.by = as.Date(vn_data$Ngay, format = "%d/%m/%Y"))
colnames(vn_price) <- "Close"

# Gộp giá đóng cửa
prices <- merge(Cl(GSPC), Cl(N225), Cl(`000001.SS`), Cl(KS11), Cl(STI), vn_price$Close)
colnames(prices) <- c("US", "Japan", "China", "Korea", "Singapore", "Vietnam")

# ============================================================
# 2. XỬ LÝ DỮ LIỆU & TÍNH LOG RETURN
# ============================================================
message("Đang xử lý dữ liệu...")

prices        <- na.locf(prices, na.rm = FALSE)   # forward-fill ngày nghỉ lễ
prices        <- na.omit(prices)

returns       <- diff(log(prices))
# Không lag US — same-day returns, following Aziz et al. (2022)
returns_clean <- na.omit(returns)

message("✅ Kích thước dữ liệu: ", nrow(returns_clean), " x ", ncol(returns_clean))

# ============================================================
# 3. TÁCH SUB-PERIOD
# ============================================================
# Cutpoint: 2020-03-11 — ngày WHO công bố COVID-19 là đại dịch toàn cầu
covid_cut <- as.Date("2020-03-11")

returns_pre  <- returns_clean[index(returns_clean) <  covid_cut]  # Pre-COVID
returns_post <- returns_clean[index(returns_clean) >= covid_cut]  # Post-COVID (incl. COVID + sau)

message("Full sample : ", nrow(returns_clean), " ngày")
message("Pre-COVID   : ", nrow(returns_pre),   " ngày  (", start(returns_pre),  " – ", end(returns_pre),  ")")
message("Post-COVID  : ", nrow(returns_post),  " ngày  (", start(returns_post), " – ", end(returns_post), ")")

# ============================================================
# 4. LƯU OUTPUT
# ============================================================
out_dir <- "D:/SCHOOL/HK5 (25-26)/GOI_2/final_term/data/processed/"
print(returns_clean)

stats_table <- table.Stats(returns_clean)
save_table(as.data.frame(stats_table), "T1_descriptive_stats", out_01)
saveRDS(returns_clean, paste0(out_dir, "returns_clean.rds"))
saveRDS(returns_pre,   paste0(out_dir, "returns_pre.rds"))
saveRDS(returns_post,  paste0(out_dir, "returns_post.rds"))

message("✅ Đã lưu: returns_clean.rds | returns_pre.rds | returns_post.rds")