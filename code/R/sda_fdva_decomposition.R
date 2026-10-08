# =============================================================================
#  sda_fdva_decomposition.R
#
#  Phân rã cấu trúc (Structural Decomposition Analysis – SDA) của
#  Functional Domestic Value Added (FDVA_ik) thành 3 hiệu ứng:
#     (1) Reallocation effect : do thay đổi tỷ trọng chức năng k trong LI (sh_ik)
#     (2) Intensity effect    : do thay đổi tỷ trọng lao động trong VA (c_i)
#     (3) Scale effect        : do thay đổi giá trị gia tăng nội địa (DVA_i)
#
#  Theo Lahr & Dietzenbacher (2018) – tích hợp shift-share (SSA) vào SDA –
#  và Miller & Blair (2022). Công thức là trung bình của 2 phân rã "polar"
#  (Dietzenbacher & Los, 1998), nên tổng 3 hiệu ứng = ΔFDVA_ik chính xác.
#
#  Ký hiệu (i = ngành, k = chức năng, t = 0: kỳ gốc, t = 1: kỳ cuối):
#     LI_ik   : labor income của chức năng k trong ngành i
#     VA_i    : value added của ngành i
#     DVA_i   : domestic value added của ngành i
#     sh_ik   = LI_ik / sum_k LI_ik     (share of function k in labor income of i)
#     c_i     = sum_k LI_ik / VA_i      (labor share of value added in i)
#     W_ik    = LI_ik / VA_i = sh_ik * c_i
#     FDVA_ik = sh_ik * c_i * DVA_i   (= W_ik * DVA_i)
#
#  ΔFDVA_ik = ½[(Δsh ⊙ c⁰)DVA⁰ + (Δsh ⊙ c¹)DVA¹]      <- Reallocation effect
#           + ½[(sh¹ ⊙ Δc)DVA⁰ + (sh⁰ ⊙ Δc)DVA¹]      <- Intensity effect
#           + ½[(sh¹ ⊙ c¹)ΔDVA + (sh⁰ ⊙ c⁰)ΔDVA]      <- Scale effect
#
#  "⊙" là phép nhân từng phần tử (Hadamard): hàng i của ma trận sh (i x k)
#  được nhân với vô hướng c_i và DVA_i của ngành i.
#
#  Cách dùng:
#     1. Điền đường dẫn / link dữ liệu ở MỤC 1 (đang để trống – xem TODO).
#     2. Chạy toàn bộ file. Kết quả nằm trong `res` và (tuỳ chọn) được ghi ra
#        thư mục `out_dir` dưới dạng CSV.
#     Khi chưa điền link, script tự tạo DỮ LIỆU MẪU để chạy thử.
# =============================================================================


# -----------------------------------------------------------------------------
# MỤC 0. THIẾT LẬP
# -----------------------------------------------------------------------------
# Script chỉ dùng base R. `readxl` chỉ cần nếu dữ liệu là file Excel.
# install.packages("readxl")

options(stringsAsFactors = FALSE)


# -----------------------------------------------------------------------------
# MỤC 1. IMPORT DỮ LIỆU  ====>  TODO: ĐIỀN LINK / ĐƯỜNG DẪN DỮ LIỆU TẠI ĐÂY <====
# -----------------------------------------------------------------------------
# Để trống ("") thì script sẽ dùng dữ liệu mẫu (MỤC 5) để chạy thử.
# Chấp nhận: đường dẫn file local hoặc URL (http/https); đuôi .csv / .xlsx / .xls

path_LI <- ""   # TODO: file labor income LI_ik theo năm
                #       Dạng dài (long), các cột: industry | func | year | LI
path_VA <- ""   # TODO: file value added VA_i và DVA_i theo năm
                #       Dạng dài (long), các cột: industry | year | VA | DVA

year0 <- NA     # TODO: năm gốc  (t = 0), ví dụ 2010
year1 <- NA     # TODO: năm cuối (t = 1), ví dụ 2020

# Nếu tên cột trong file khác, sửa lại ở đây (không cần sửa code bên dưới)
col_industry <- "industry"   # mã / tên ngành i
col_func     <- "func"       # mã / tên chức năng k (ví dụ: RD, Management, Marketing, Fabrication)
col_year     <- "year"       # năm
col_LI       <- "LI"         # labor income LI_ik
col_VA       <- "VA"         # value added VA_i
col_DVA      <- "DVA"        # domestic value added DVA_i

# Ghi kết quả ra CSV?
write_output <- TRUE
out_dir      <- "output_sda"   # TODO (tuỳ chọn): thư mục lưu kết quả


# -----------------------------------------------------------------------------
# MỤC 2. HÀM HỖ TRỢ ĐỌC VÀ CHUYỂN DẠNG DỮ LIỆU
# -----------------------------------------------------------------------------

# Đọc bảng từ csv / xlsx / xls, file local hoặc URL
read_table_any <- function(path, sheet = 1) {
  ext <- tolower(tools::file_ext(sub("[?#].*$", "", path)))
  if (ext == "csv") {
    return(read.csv(path, check.names = FALSE))
  }
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Cần cài gói 'readxl' để đọc file Excel: install.packages('readxl')")
    if (grepl("^https?://", path)) {              # readxl không đọc trực tiếp URL
      tmp <- tempfile(fileext = paste0(".", ext))
      download.file(path, tmp, mode = "wb", quiet = TRUE)
      path <- tmp
    }
    return(as.data.frame(readxl::read_excel(path, sheet = sheet)))
  }
  stop("Không nhận dạng được định dạng file: ", path, " (chỉ hỗ trợ csv/xlsx/xls)")
}

# Chuyển bảng dạng dài -> ma trận (hàng = `row`, cột = `col`, giá trị = `value`).
# Giữ nguyên thứ tự xuất hiện của ngành / chức năng; tổ hợp thiếu được gán 0.
long_to_matrix <- function(df, row, col, value) {
  for (v in c(row, col, value))
    if (!v %in% names(df)) stop("Không tìm thấy cột '", v, "' trong dữ liệu")
  r <- factor(df[[row]], levels = unique(df[[row]]))
  k <- factor(df[[col]], levels = unique(df[[col]]))
  m <- tapply(as.numeric(df[[value]]), list(r, k), sum)
  m[is.na(m)] <- 0
  m <- unclass(m)                       # bỏ class "table" của tapply -> ma trận thường
  storage.mode(m) <- "double"
  m
}

# Chuyển bảng dạng dài -> vector có tên (tên = `name`, giá trị = `value`)
long_to_vector <- function(df, name, value) {
  for (v in c(name, value))
    if (!v %in% names(df)) stop("Không tìm thấy cột '", v, "' trong dữ liệu")
  if (anyDuplicated(df[[name]]))
    stop("Cột '", name, "' bị trùng trong cùng một năm – mỗi ngành chỉ được 1 dòng/năm")
  setNames(as.numeric(df[[value]]), as.character(df[[name]]))
}

# Lọc đúng năm
filter_year <- function(df, year) {
  out <- df[df[[col_year]] == year, , drop = FALSE]
  if (nrow(out) == 0) stop("Không có dữ liệu cho năm ", year)
  out
}


# -----------------------------------------------------------------------------
# MỤC 3. HÀM TÍNH TOÁN (phần lõi của phương pháp)
# -----------------------------------------------------------------------------

# Nhân hàng i của ma trận M với phần tử thứ i của vector v  (M ⊙ v, broadcast theo hàng)
row_scale <- function(M, v) {
  stopifnot(is.matrix(M), length(v) == nrow(M))
  sweep(M, 1, v, "*")
}

# sh_ik = LI_ik / sum_k LI_ik   (ma trận i x k, mỗi hàng cộng lại = 1)
compute_sh <- function(LI) {
  tot <- rowSums(LI)
  if (any(tot == 0, na.rm = TRUE))
    warning("Có ngành với tổng labor income = 0; sh_ik của ngành đó được gán 0")
  sh <- row_scale(LI, ifelse(tot == 0, 0, 1 / tot))
  sh
}

# c_i = sum_k LI_ik / VA_i   (vector theo ngành)
compute_c <- function(LI, VA) {
  stopifnot(length(VA) == nrow(LI))
  if (any(VA == 0, na.rm = TRUE))
    warning("Có ngành với VA = 0; c_i của ngành đó sẽ là NA/Inf")
  rowSums(LI) / VA
}

# Sắp xếp / kiểm tra để 2 kỳ có cùng tập ngành (hàng) và chức năng (cột),
# cùng thứ tự với VA và DVA. Ngành / chức năng chỉ có ở 1 kỳ sẽ bị báo lỗi.
align_inputs <- function(LI0, LI1, VA0, VA1, DVA0, DVA1) {
  ind  <- rownames(LI0); fun <- colnames(LI0)
  chk <- function(x, ref, what) {
    if (!setequal(x, ref))
      stop("Tập ", what, " không khớp giữa các đầu vào. Chỉ có ở một bên: ",
           paste(c(setdiff(x, ref), setdiff(ref, x)), collapse = ", "))
  }
  chk(rownames(LI1), ind, "ngành (LI0 vs LI1)")
  chk(colnames(LI1), fun, "chức năng (LI0 vs LI1)")
  chk(names(VA0),  ind, "ngành (VA0)");  chk(names(VA1),  ind, "ngành (VA1)")
  chk(names(DVA0), ind, "ngành (DVA0)"); chk(names(DVA1), ind, "ngành (DVA1)")
  list(LI0 = LI0,
       LI1 = LI1[ind, fun, drop = FALSE],
       VA0 = VA0[ind],  VA1 = VA1[ind],
       DVA0 = DVA0[ind], DVA1 = DVA1[ind])
}

# ---- Phân rã SDA theo công thức trên slide -----------------------------------
# Đầu vào: sh0, sh1 (ma trận i x k); c0, c1, DVA0, DVA1 (vector theo ngành i)
# Đầu ra : danh sách các ma trận i x k
sda_fdva <- function(sh0, sh1, c0, c1, DVA0, DVA1) {
  stopifnot(identical(dim(sh0), dim(sh1)),
            length(c0) == nrow(sh0), length(c1) == nrow(sh0),
            length(DVA0) == nrow(sh0), length(DVA1) == nrow(sh0))

  d_sh  <- sh1  - sh0          # Δsh_ik
  d_c   <- c1   - c0           # Δc_i
  d_DVA <- DVA1 - DVA0         # ΔDVA_i

  # (1) Reallocation effect: ½[(Δsh ⊙ c⁰)DVA⁰ + (Δsh ⊙ c¹)DVA¹]
  reallocation <- 0.5 * ( row_scale(d_sh, c0 * DVA0) + row_scale(d_sh, c1 * DVA1) )

  # (2) Intensity effect:    ½[(sh¹ ⊙ Δc)DVA⁰ + (sh⁰ ⊙ Δc)DVA¹]
  intensity    <- 0.5 * ( row_scale(sh1, d_c * DVA0) + row_scale(sh0, d_c * DVA1) )

  # (3) Scale effect:        ½[(sh¹ ⊙ c¹)ΔDVA + (sh⁰ ⊙ c⁰)ΔDVA]
  scale        <- 0.5 * ( row_scale(sh1, c1 * d_DVA) + row_scale(sh0, c0 * d_DVA) )

  # Mức FDVA từng kỳ và thay đổi thực tế
  FDVA0 <- row_scale(sh0, c0 * DVA0)
  FDVA1 <- row_scale(sh1, c1 * DVA1)
  delta <- FDVA1 - FDVA0

  # Kiểm tra: tổng 3 hiệu ứng phải bằng ΔFDVA (sai số chỉ do làm tròn số)
  residual <- delta - (reallocation + intensity + scale)
  if (max(abs(residual), na.rm = TRUE) > 1e-8 * max(1, max(abs(delta), na.rm = TRUE)))
    warning("Tổng 3 hiệu ứng KHÔNG bằng ΔFDVA – kiểm tra lại dữ liệu đầu vào (NA/Inf?)")

  list(FDVA0 = FDVA0, FDVA1 = FDVA1, delta_FDVA = delta,
       reallocation = reallocation, intensity = intensity, scale = scale,
       residual = residual)
}

# ---- Chạy toàn bộ: từ LI / VA / DVA -> sh, c -> phân rã -> bảng kết quả ------
run_sda <- function(LI0, LI1, VA0, VA1, DVA0, DVA1) {
  a   <- align_inputs(LI0, LI1, VA0, VA1, DVA0, DVA1)

  sh0 <- compute_sh(a$LI0);        sh1 <- compute_sh(a$LI1)
  c0  <- compute_c(a$LI0, a$VA0);  c1  <- compute_c(a$LI1, a$VA1)

  dec <- sda_fdva(sh0, sh1, c0, c1, a$DVA0, a$DVA1)

  ind <- rownames(a$LI0); fun <- colnames(a$LI0)

  # Bảng dài: 1 dòng cho mỗi cặp (ngành i, chức năng k)
  tab <- data.frame(
    industry     = rep(ind, times = length(fun)),
    func         = rep(fun, each  = length(ind)),
    sh0          = as.vector(sh0),
    sh1          = as.vector(sh1),
    c0           = rep(c0,  times = length(fun)),
    c1           = rep(c1,  times = length(fun)),
    DVA0         = rep(a$DVA0, times = length(fun)),
    DVA1         = rep(a$DVA1, times = length(fun)),
    FDVA0        = as.vector(dec$FDVA0),
    FDVA1        = as.vector(dec$FDVA1),
    delta_FDVA   = as.vector(dec$delta_FDVA),
    reallocation = as.vector(dec$reallocation),
    intensity    = as.vector(dec$intensity),
    scale        = as.vector(dec$scale)
  )

  # Tổng hợp theo chức năng k (cộng theo ngành) và theo ngành i (cộng theo chức năng)
  agg <- function(f) {
    out <- data.frame(
      delta_FDVA   = f(dec$delta_FDVA),
      reallocation = f(dec$reallocation),
      intensity    = f(dec$intensity),
      scale        = f(dec$scale)
    )
    # % đóng góp của từng hiệu ứng vào ΔFDVA (NA nếu ΔFDVA = 0)
    pct <- function(x) ifelse(out$delta_FDVA == 0, NA, 100 * x / out$delta_FDVA)
    out$reallocation_pct <- pct(out$reallocation)
    out$intensity_pct    <- pct(out$intensity)
    out$scale_pct        <- pct(out$scale)
    out
  }
  by_function <- agg(colSums);  by_function <- cbind(func = fun, by_function)
  by_industry <- agg(rowSums);  by_industry <- cbind(industry = ind, by_industry)
  total       <- agg(function(M) sum(M))
  rownames(by_function) <- rownames(by_industry) <- NULL

  list(inputs = list(sh0 = sh0, sh1 = sh1, c0 = c0, c1 = c1,
                     DVA0 = a$DVA0, DVA1 = a$DVA1),
       matrices = dec, table = tab,
       by_function = by_function, by_industry = by_industry, total = total)
}


# -----------------------------------------------------------------------------
# MỤC 4. ĐỌC DỮ LIỆU THẬT (khi đã điền link ở MỤC 1)
# -----------------------------------------------------------------------------
load_inputs <- function() {
  LI_long <- read_table_any(path_LI)
  VA_long <- read_table_any(path_VA)

  LI0 <- long_to_matrix(filter_year(LI_long, year0), col_industry, col_func, col_LI)
  LI1 <- long_to_matrix(filter_year(LI_long, year1), col_industry, col_func, col_LI)

  VA0  <- long_to_vector(filter_year(VA_long, year0), col_industry, col_VA)
  VA1  <- long_to_vector(filter_year(VA_long, year1), col_industry, col_VA)
  DVA0 <- long_to_vector(filter_year(VA_long, year0), col_industry, col_DVA)
  DVA1 <- long_to_vector(filter_year(VA_long, year1), col_industry, col_DVA)

  list(LI0 = LI0, LI1 = LI1, VA0 = VA0, VA1 = VA1, DVA0 = DVA0, DVA1 = DVA1)
}


# -----------------------------------------------------------------------------
# MỤC 5. DỮ LIỆU MẪU (chỉ dùng khi chưa điền link dữ liệu)
# -----------------------------------------------------------------------------
make_example_inputs <- function(n_ind = 5, seed = 123) {
  set.seed(seed)
  ind <- paste0("IND", seq_len(n_ind))
  fun <- c("RD", "Management", "Marketing", "Fabrication")   # business functions

  LI0 <- matrix(runif(n_ind * length(fun), 10, 100), n_ind, length(fun),
                dimnames = list(ind, fun))
  LI1 <- LI0 * matrix(runif(n_ind * length(fun), 0.8, 1.5), n_ind, length(fun))

  VA0 <- setNames(rowSums(LI0) * runif(n_ind, 1.5, 2.5), ind)   # để c_i < 1
  VA1 <- setNames(rowSums(LI1) * runif(n_ind, 1.5, 2.5), ind)

  DVA0 <- setNames(VA0 * runif(n_ind, 0.2, 0.6), ind)           # DVA_i (ví dụ: DVA trong xuất khẩu)
  DVA1 <- setNames(VA1 * runif(n_ind, 0.2, 0.6), ind)

  list(LI0 = LI0, LI1 = LI1, VA0 = VA0, VA1 = VA1, DVA0 = DVA0, DVA1 = DVA1)
}


# -----------------------------------------------------------------------------
# MỤC 6. CHẠY
# -----------------------------------------------------------------------------
if (nzchar(path_LI) && nzchar(path_VA)) {
  message("Đọc dữ liệu từ:\n  LI : ", path_LI, "\n  VA : ", path_VA)
  inputs <- load_inputs()
} else {
  message("*** CHƯA ĐIỀN LINK DỮ LIỆU (path_LI / path_VA ở MỤC 1) -> dùng DỮ LIỆU MẪU ***")
  inputs <- make_example_inputs()
}

res <- with(inputs, run_sda(LI0, LI1, VA0, VA1, DVA0, DVA1))

cat("\n===== Kiểm tra: max |ΔFDVA - (reallocation + intensity + scale)| =",
    format(max(abs(res$matrices$residual)), digits = 3), "=====\n")

cat("\n----- Tổng toàn nền kinh tế -----\n");      print(res$total)
cat("\n----- Theo chức năng k -----\n");           print(res$by_function)
cat("\n----- Theo ngành i -----\n");               print(res$by_industry)
cat("\n----- Chi tiết theo (ngành i, chức năng k) – 10 dòng đầu -----\n")
print(head(res$table, 10))

# Ghi kết quả
if (write_output) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(res$table,       file.path(out_dir, "sda_fdva_detail.csv"),      row.names = FALSE)
  write.csv(res$by_function, file.path(out_dir, "sda_fdva_by_function.csv"), row.names = FALSE)
  write.csv(res$by_industry, file.path(out_dir, "sda_fdva_by_industry.csv"), row.names = FALSE)
  write.csv(res$total,       file.path(out_dir, "sda_fdva_total.csv"),       row.names = FALSE)
  message("Đã ghi kết quả vào thư mục: ", normalizePath(out_dir))
}

# -----------------------------------------------------------------------------
# GHI CHÚ
# - Nếu có nhiều quốc gia: tách dữ liệu theo quốc gia rồi gọi run_sda() cho
#   từng quốc gia, ví dụ:
#     res_by_country <- lapply(split(LI_long, LI_long$country), function(d) { ... })
# - Nếu dữ liệu đã có sẵn sh_ik và c_i (không có LI/VA), gọi trực tiếp
#   sda_fdva(sh0, sh1, c0, c1, DVA0, DVA1).
# - Tổng 3 hiệu ứng luôn bằng ΔFDVA_ik (trung bình 2 phân rã polar), nên không
#   có phần dư (interaction term).
# -----------------------------------------------------------------------------
