# ============================================================
# run_all.R
# Mục đích: Chạy toàn bộ pipeline phân tích một cách tự động
# Thứ tự thực thi:
# 1. data.R          - Thu thập và xử lý dữ liệu
# 2. diagnostic.R    - Kiểm định tiền mô hình
# 3. var.R           - Mô hình VAR và connectedness
# 4. robustness.R    - Phân tích độ bền vững
# 5. [EX]transfer_entropy.R - Phân tích Transfer Entropy (tùy chọn)
# ============================================================

# Đặt working directory về root của project
setwd("D:/SCHOOL/HK5 (25-26)/GOI_2/final_term")

# Hàm helper để source file và báo cáo
run_script <- function(filename, description) {
  message("\n=========================================")
  message("🚀 Đang chạy: ", description)
  message("📁 File: ", filename)
  message("=========================================")

  start_time <- Sys.time()
  tryCatch({
    source(file.path("code", filename), encoding = "UTF-8")
    end_time <- Sys.time()
    duration <- round(difftime(end_time, start_time, units = "secs"), 2)
    message("✅ Hoàn thành: ", description, " (", duration, " giây)")
  }, error = function(e) {
    message("❌ Lỗi khi chạy ", filename, ": ", e$message)
    stop(e$message)
  })
}

# ============================================================
# 1. THU THẬP VÀ XỬ LÝ DỮ LIỆU
# ============================================================
run_script("data.R", "Thu thập và xử lý dữ liệu")

# ============================================================
# 2. KIỂM ĐỊNH TIỀN MÔ HÌNH
# ============================================================
run_script("diagnostic.R", "Kiểm định tiền mô hình (descriptive, stationarity, lag selection, Granger)")

# ============================================================
# 3. MÔ HÌNH VAR VÀ CONNECTEDNESS
# ============================================================
run_script("var.R", "Mô hình VAR và phân tích connectedness (static, TVP, frequency, QVAR)")

# ============================================================
# 4. PHÂN TÍCH ĐỘ BỀN VỮNG
# ============================================================
run_script("robustness.R", "Phân tích độ bền vững (lag, cutpoint, tau, kappa)")

# ============================================================
# 5. PHÂN TÍCH TRANSFER ENTROPY (TÙY CHỌN - MỞ RỘNG)
# ============================================================
# Uncomment dòng dưới nếu muốn chạy transfer entropy
# run_script("[EX]transfer_entropy.R", "Phân tích Transfer Entropy")

# ============================================================
# HOÀN THÀNH
# ============================================================
message("\n🎉 Pipeline hoàn thành! Tất cả output đã được lưu trong thư mục output/")

# Tóm tắt thời gian chạy
total_time <- round(difftime(Sys.time(), Sys.time() - as.numeric(Sys.time() - Sys.time()), units = "mins"), 2)
message("⏱️ Tổng thời gian: ", total_time, " phút")