# ==========================================
# FILE: model.R
# ==========================================
library(xts)

# 1. IMPORT DỮ LIỆU TỪ BƯỚC 1
returns_clean <- readRDS("D:/SCHOOL/HK5 (25-26)/GOI_2/final_term/data/processed/returns_clean.rds")
nodes <- colnames(returns_clean)
num_nodes <- length(nodes)

# ==========================================
# 2. GLOBAL PARAMETERS & GRID SEARCH (Mở rộng phủ rộng hơn)
# ==========================================
# -- Tham số cho mô hình TE --
LAG_VALS    <- c(1)     # Độ trễ (1-3 ngày)
N_BINS_VALS <- c(5)     # Trạng thái (Giảm, Đi ngang, Tăng, Sập, Tăng tốc...)
N_PERM      <- 200            # Lặp hoán vị (Để 200 cho Grid search nhanh hơn)
ALPHA       <- 0.05           # Mức ý nghĩa thống kê

# -- Tham số cho Rolling Window --
# Về kinh tế: 250 ngày ~ 1 năm giao dịch. Step 21 ~ 1 tháng.
ROLL_WINDOW <- 250            
ROLL_STEP   <- 21             
ROLL_PERM   <- 100            # Số lần lặp cho Rolling (Giảm xuống để tránh chạy quá lâu)

SEED <- 42

# ==========================================
# 3. CORE FUNCTIONS (Sử dụng RTransferEntropy - Lấy ETE & Chống Lỗi)
# ==========================================
library(RTransferEntropy)

transfer_entropy_discrete <- function(x_s, y_s, lag = 1, n_bins = 3, n_perm = 200, rng_seed = NULL) {
  
  # Dùng tryCatch để bọc lại, nếu RTransferEntropy gặp lỗi nội bộ cũng không làm chết vòng lặp
  te_obj <- tryCatch({
    transfer_entropy(x = x_s, 
                     y = y_s, 
                     lx = lag, 
                     ly = lag, 
                     type = "bins", 
                     bins = n_bins,
                     shuffles = 100,      # Bắt buộc > 0 để tính Effective TE (ETE)
                     nboot = n_perm,      # Số lần bootstrap tính p-value
                     quiet = TRUE, 
                     seed = rng_seed)
  }, error = function(e) {
    warning(paste("Lỗi RTransferEntropy:", e$message))
    return(NULL)
  })
  
  # Nếu hàm bị lỗi nội bộ, trả về mặc định để bỏ qua cặp này
  if (is.null(te_obj)) return(list(TE_obs = 0.0, p_val = 1.0))
  
  # Trích xuất và ép kiểu về data.frame để dễ xử lý
  df_coef <- as.data.frame(coef(te_obj))
  
  # [PHẦN DEBUG & TÌM CỘT TỰ ĐỘNG]
  # grep sẽ dò tìm cột bất chấp viết hoa/thường hay có dấu gạch ngang
  col_ete <- grep("(?i)^ete$", colnames(df_coef)) 
  col_pval <- grep("(?i)p[\\.-]?value", colnames(df_coef))
  
  # Nếu không tìm thấy cột, in ra để debug
  if (length(col_ete) == 0 || length(col_pval) == 0) {
    message("\n--- ⚠️ DEBUG INFO: CÁC CỘT THỰC TẾ TRẢ VỀ TỪ THƯ VIỆN ---")
    print(colnames(df_coef))
    stop("Thư viện không trả về cột ETE hoặc p-value. Hãy xem danh sách cột ở trên!")
  }
  
  # Lấy ETE và p-value của chiều X -> Y (dòng số 1)
  ETE_obs <- as.numeric(df_coef[1, col_ete[1]])
  p_val   <- as.numeric(df_coef[1, col_pval[1]])
  
  # Đảm bảo không dính NA làm hỏng logic mảng 3D phía sau
  if(is.na(p_val)) p_val <- 1.0 
  if(is.na(ETE_obs)) ETE_obs <- 0.0
  if(ETE_obs < 0) ETE_obs <- 0.0 # ETE đôi khi ra âm do nhiễu, chuẩn hóa về 0
  
  # Vẫn giữ tên biến trả về là "TE_obs" để KHÔNG làm hỏng các block Grid Search và Rolling bên dưới
  # Nhưng giá trị thực chất bạn đang chạy là Effective TE.
  return(list(TE_obs = ETE_obs, p_val = p_val))
}
# ==========================================
# 4. TÌM GLOBAL PARAMETERS (Chạy TE tĩnh trên toàn bộ các cặp)
# ==========================================
message("\n🚀 [BƯỚC 1] TÌM BỘ THAM SỐ CHUNG TỐT NHẤT CHO TOÀN BỘ MẠNG LƯỚI...")
param_grid <- expand.grid(lag = LAG_VALS, n_bins = N_BINS_VALS)
grid_scores <- numeric(nrow(param_grid))

# Lặp qua tất cả cấu hình
for(p in 1:nrow(param_grid)) {
  lag_i <- param_grid$lag[p]
  bins_i <- param_grid$n_bins[p]
  robust_count <- 0
  
  # Lặp qua tất cả các cặp i -> j
  for(i in 1:num_nodes) {
    for(j in 1:num_nodes) {
      if(i != j) {
        res <- transfer_entropy_discrete(as.numeric(returns_clean[,i]), 
                                         as.numeric(returns_clean[,j]), 
                                         lag_i, bins_i, N_PERM)
        if(res$p_val < ALPHA) robust_count <- robust_count + 1
      }
    }
  }
  grid_scores[p] <- robust_count
  message(sprintf("Cấu hình Lag=%d, Bins=%d -> Có %d cạnh ý nghĩa (Robust Edges)", lag_i, bins_i, robust_count))
}

param_grid$robust_edges <- grid_scores
best_params <- param_grid[which.max(param_grid$robust_edges), ]
OPT_LAG <- best_params$lag
OPT_BINS <- best_params$n_bins

message("🏆 ĐÃ CHỌN ĐƯỢC THAM SỐ TOÀN CỤC TỐI ƯU:")
message(sprintf("-> Lag: %d | Số Bins: %d (Sẽ dùng để chạy Rolling Window)", OPT_LAG, OPT_BINS))

# ==========================================
# 5. ROLLING WINDOW -> TẠO MA TRẬN KỀ 3D
# ==========================================
message("\n⏳ [BƯỚC 2] KHỞI CHẠY ROLLING WINDOW (Tạo Ma trận 3D)...")

total_rows <- nrow(returns_clean)
start_indices <- seq(1, total_rows - ROLL_WINDOW + 1, by = ROLL_STEP)
num_windows <- length(start_indices)

# Khởi tạo Ma trận 3D: [Node_From, Node_To, Time_Window]
adj_matrix_3d <- array(0, dim = c(num_nodes, num_nodes, num_windows),
                       dimnames = list(From = nodes, To = nodes, Window = 1:num_windows))
# Mảng lưu nhãn thời gian của từng cửa sổ (Lấy ngày kết thúc cửa sổ)
window_dates <- as.Date(rep(NA, num_windows))

pb <- txtProgressBar(min = 0, max = num_windows, style = 3) # Thanh tiến trình

for(w in 1:num_windows) {
  s_idx <- start_indices[w]
  e_idx <- s_idx + ROLL_WINDOW - 1
  window_data <- returns_clean[s_idx:e_idx, ]
  window_dates[w] <- index(window_data)[nrow(window_data)] # Ngày cuối của window
  
  for(i in 1:num_nodes) {
    for(j in 1:num_nodes) {
      if(i != j) {
        res <- transfer_entropy_discrete(as.numeric(window_data[,i]), 
                                         as.numeric(window_data[,j]), 
                                         OPT_LAG, OPT_BINS, ROLL_PERM)
        # Lọc Robust: Nếu có ý nghĩa thì ghi nhận TE, nếu là nhiễu thì gán = 0
        if(res$p_val < ALPHA) {
          adj_matrix_3d[i, j, w] <- res$TE_obs
        }
      }
    }
  }
  setTxtProgressBar(pb, w)
}
close(pb)

message("\n✅ Đã tạo xong Ma trận kề 3D. Kích thước (Từ x Đến x Thời gian): ", 
        dim(adj_matrix_3d)[1], "x", dim(adj_matrix_3d)[2], "x", dim(adj_matrix_3d)[3])
# ==========================================
# 6. TRỰC QUAN HÓA ROLLING WINDOW CHO 5 MÃ & LƯU FILE (ĐÃ SỬA LỖI)
# ==========================================
message("\nĐang tính toán Out-degree (Tổng lượng TE phát ra) cho 5 thị trường...")

# 6.1. Tính Out-degree cho từng node ở mỗi cửa sổ thời gian
out_degree_matrix <- matrix(0, nrow = num_windows, ncol = num_nodes)
colnames(out_degree_matrix) <- nodes

for(w in 1:num_windows) {
  out_degree_matrix[w, ] <- rowSums(adj_matrix_3d[, , w], na.rm = TRUE)
}

# Chuyển thành định dạng xts
out_degree_xts <- xts(out_degree_matrix, order.by = window_dates)

# 6.2. Khởi tạo thư mục lưu trữ ảnh
out_dir <- "D:/SCHOOL/HK5 (25-26)/GOI_2/final_term/output/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 6.3. Vẽ và lưu Biểu đồ Rolling cho 5 mã (Dùng ggplot2 để tránh lỗi đồ họa)
library(ggplot2)
library(tidyr)
library(reshape2)

# Chuyển xts sang dataframe long format cho ggplot
df_out <- data.frame(Date = index(out_degree_xts), coredata(out_degree_xts))
df_out_long <- melt(df_out, id.vars = "Date", variable.name = "Country", value.name = "OutDegree")

p_5tickers <- ggplot(df_out_long, aes(x = Date, y = OutDegree, color = Country)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "Động lực tràn thông tin (Tổng ETE phát ra - Out-Degree)",
       x = "Thời gian", y = "Out-Degree (Effective TE)", color = "Thị trường") +
  theme(legend.position = "bottom")

chart_5tickers_path <- paste0(out_dir, "/rolling_TE_outdegree_5tickers.png")
ggsave(filename = chart_5tickers_path, plot = p_5tickers, width = 12, height = 7, dpi = 150)


# 6.4. Vẽ và lưu Biểu đồ cặp đơn lẻ (Ví dụ: US -> Vietnam)
us_to_vn_te <- adj_matrix_3d["US", "Vietnam", ]
df_us_vn <- data.frame(Date = window_dates, TE = us_to_vn_te)

p_us_vn <- ggplot(df_us_vn, aes(x = Date, y = TE)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = paste("Rolling Effective Transfer Entropy: US -> Vietnam\n",
                     "(Window: ", ROLL_WINDOW, " | Step: ", ROLL_STEP, ")", sep=""),
       x = "Thời gian", y = "Effective TE")

chart_us_vn_path <- paste0(out_dir, "/rolling_TE_US_to_VN.png")
ggsave(filename = chart_us_vn_path, plot = p_us_vn, width = 10, height = 6, dpi = 150)

message("✅ Đã lưu tất cả biểu đồ thành công vào thư mục: ", out_dir)