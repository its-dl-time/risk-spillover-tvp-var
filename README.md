# Phân tích hiệu ứng tràn thông tin (Information Spillover) vào Thị trường Chứng khoán Việt Nam: Tiếp cận bằng Transfer Entropy và Mô hình VAR

**Đề tài Tiểu luận cuối kỳ:** Môn Gói phần mềm ứng dụng cho tài chính (R) - Trường Đại học Kinh tế - Luật (UEL).

## 📖 Tổng quan dự án (Project Overview)

Dự án này nghiên cứu hiệu ứng tràn thông tin từ các thị trường chứng khoán lớn (Mỹ, Nhật Bản, Trung Quốc, Hàn Quốc) sang Việt Nam. Nghiên cứu sử dụng phương pháp kết hợp:

1. **Effective Transfer Entropy (ETE):** Đo lường dòng chảy thông tin phi tuyến và có tính hướng (Directional).

2. **Vector Autoregression (VAR):** Phân tích động lực học cú sốc (IRF) và phân rã phương sai (FEVD) dựa trên các chỉ số spillover rút trích từ mạng lưới ETE.

Kết quả nghiên cứu giúp xác định vị thế của Việt Nam trong mạng lưới tài chính (Net Receiver) và mức độ tổn thương trước các cú sốc ngoại sinh (đặc biệt từ thị trường Mỹ).

## 📂 Cấu trúc thư mục (Directory Structure)

```text

📦 project_root/

 ┣ 📂 data/

 ┃ ┣ 📂 raw/                # Dữ liệu thô (giá đóng cửa các chỉ số chứng khoán)

 ┃ ┗ 📂 processed/          # Dữ liệu đã làm sạch và các file .rds (te_matrix_3d.rds, returns_clean.rds)

 ┣ 📂 code/

 ┃ ┣ 📜 data.R              # Tiền xử lý dữ liệu, xử lý missing values, tính Log Returns

 ┃ ┣ 📜 model.R             # Chạy Grid Search tham số tối ưu và Rolling Window ETE (tạo ma trận 3D)

 ┃ ┣ 📜 analysis.R          # Tính toán Inflow, Outflow, Netflow và trực quan hóa mạng lưới

 ┃ ┗ 📜 var.R               # Khớp dữ liệu ETE với Returns, kiểm định tính dừng, chạy mô hình VAR & IRF/FEVD

 ┣ 📂 output/

 ┃ ┗ 📂 figures/            # Nơi lưu các biểu đồ xuất ra từ ggplot2 (.png)

 ┣ 📜 .gitignore            # Bỏ qua các file rác và data nặng khi push lên Git

 ┗ 📜 README.md             # Tài liệu dự án

```

## 🛠️ Yêu cầu hệ thống & Thư viện (Prerequisites)

Ngôn ngữ sử dụng: R (>= 4.0.0).

Các package cần thiết để chạy dự án:

```
install.packages(c("dplyr", "tidyr", "zoo", "xts", "vars", "tseries", "ggplot2", "reshape2"))
```

## 🚀 Luồng thực thi Code (Pipeline Execution)
 
Để tái lập toàn bộ kết quả nghiên cứu, vui lòng chạy các script theo thứ tự sau:

Bước 1: Tiền xử lý dữ liệu (code/data.R)

- Nhập dữ liệu chuỗi thời gian của 5 thị trường.

- Xử lý giá trị thiếu (missing values) bằng phương pháp na.locf (forward-fill) để đồng bộ ngày giao dịch.

- Tính toán Log Return và lưu ra file data/processed/returns_clean.rds.

Bước 2: Ước lượng Transfer Entropy (code/model.R)

- Thực hiện Grid Search để tìm tham số tối ưu (Lag, Bins) giảm thiểu nhiễu và tránh bảng trạng thái thưa (Sparse State Space).

- Chạy thuật toán Rolling Window để ước lượng ETE theo thời gian.

Lưu ý: Bước này mất nhiều thời gian. Kết quả cuối cùng được cache lại tại data/processed/te_matrix_3d.rds.

Bước 3: Phân tích mạng lưới ETE (code/analysis.R)

- Load ma trận ETE 3D.

- Rút trích các chỉ số dòng chảy cho Việt Nam: Inflow, Outflow, Netflow, Share_US_to_VN.

- Xuất các biểu đồ phân phối và chuỗi thời gian (Time-series) vào thư mục output/figures/.

Bước 4: Ước lượng mô hình VAR (code/var.R)

- Đồng bộ chỉ số Inflow_VN với chuỗi lợi suất và độ biến động (Volatility) của Việt Nam.

- Thực hiện kiểm định nghiệm đơn vị (ADF Test).

- Lựa chọn độ trễ tối ưu (theo tiêu chí SC/BIC).

- Ước lượng mô hình VAR chuẩn (Cholesky Ordering: US_Return -> Inflow_VN -> VN_Volatility).

- Vẽ biểu đồ Phản ứng xung (IRF) và Phân rã phương sai (FEVD).


## 📊 Kết quả chính (Key Findings)

1. Cấu trúc mạng lưới: Việt Nam thể hiện rõ vai trò là "Net Receiver" (Nơi hứng chịu thông tin rủi ro), với giá trị Netflow luôn âm trong suốt chu kỳ quan sát.

2. Tính phi tuyến: ETE bắt được những đỉnh lây lan rủi ro (spillover spikes) vào các giai đoạn thị trường biến động mạnh mà các hệ số tương quan tuyến tính (Pearson) thông thường bỏ sót.

3. Động lực học (VAR): Cú sốc từ thị trường Mỹ (US_Return) và lượng thông tin tràn vào (Inflow_VN) có tác động thống kê đáng kể đến sự biến động của VN-Index trong ngắn hạn (thể hiện rõ qua đồ thị IRF và FEVD).

Tác giả:
Phạm Mạnh Quyền
