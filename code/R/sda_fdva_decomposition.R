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
#  Lưu ý diễn giải: vì sum_k sh_ik = 1 ở cả hai kỳ nên sum_k Δsh_ik = 0, do đó
#  Reallocation effect cộng theo k LUÔN BẰNG 0 cho từng ngành i (và cho toàn
#  nền kinh tế). Hiệu ứng này chỉ phân bổ lại FDVA giữa các chức năng trong
#  cùng một ngành, nên chỉ nhìn thấy ở mức (i, k) và khi tổng hợp theo chức
#  năng k; ở mức ngành i / tổng, ΔFDVA = Intensity + Scale.
#
#  Cách dùng:
#     1. Điền đường dẫn / link dữ liệu và 2 năm ở MỤC 1 (đang để trống – xem TODO).
#     2. Chạy toàn bộ file. Kết quả nằm trong `res` và (tuỳ chọn) được ghi ra
#        thư mục `out_dir` dưới dạng CSV.
#     Khi chưa điền link, script tự tạo DỮ LIỆU MẪU (MỤC 5) để chạy thử.
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
# Để trống ("") cả hai thì script dùng dữ liệu mẫu (MỤC 5) để chạy thử.
# Chấp nhận: đường dẫn file local hoặc URL (http/https); đuôi .csv / .xlsx / .xls
# (Google Sheets: dùng link .../export?format=csv và đặt format_* = "csv" bên dưới)

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

# Tuỳ chọn (thường không cần sửa)
col_country  <- ""           # TODO (tuỳ chọn): tên cột quốc gia nếu file có NHIỀU nước, ví dụ "country"
country      <- ""           # TODO (tuỳ chọn): mã nước cần tính, ví dụ "VNM"  (để "" nếu file chỉ có 1 nước)
format_LI    <- ""           # "" = nhận dạng theo đuôi file; hoặc "csv" / "xlsx" (dùng khi link không có đuôi file)
format_VA    <- ""
sheet_LI     <- 1            # sheet (số thứ tự hoặc tên) nếu là file Excel
sheet_VA     <- 1
csv_sep      <- ""           # "" = tự phát hiện dấu phân cách CSV ("," hoặc ";" hoặc tab); hoặc điền trực tiếp
csv_dec      <- ""           # "" = tự chọn dấu thập phân ("," nếu file phân cách bằng ";", còn lại "."); hoặc điền "." / ","

# Ghi kết quả ra CSV?
write_output <- TRUE
out_dir      <- "output_sda"   # TODO (tuỳ chọn): thư mục lưu kết quả


# -----------------------------------------------------------------------------
# MỤC 2. HÀM HỖ TRỢ ĐỌC VÀ KIỂM TRA DỮ LIỆU
# -----------------------------------------------------------------------------

# Báo lỗi rõ ràng nếu thiếu cột (liệt kê các cột có trong file để dễ sửa col_* ở MỤC 1)
check_cols <- function(df, cols, what = "") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0)
    stop("Không tìm thấy cột ", paste(sprintf("'%s'", missing), collapse = ", "),
         if (nzchar(what)) paste0(" trong file ", what) else "",
         ". Các cột có trong file: ", paste(names(df), collapse = " | "),
         ". Sửa tên cột (col_*) ở MỤC 1 hoặc kiểm tra dấu phân cách CSV (csv_sep).")
  invisible(TRUE)
}

# Ép sang số; DỪNG nếu có ô trống / không phải số (tránh lặng lẽ biến thành 0 hay NA)
parse_numeric <- function(x, what) {
  num <- suppressWarnings(as.numeric(as.character(x)))
  bad <- is.na(num)
  if (any(bad))
    stop("Cột '", what, "' có ", sum(bad), " giá trị trống hoặc không phải số, ví dụ: ",
         paste(sprintf("'%s'", head(unique(as.character(x[bad])), 3)), collapse = ", "),
         ". Hãy bỏ dấu phẩy ngăn cách hàng nghìn / ký hiệu n.a. / ô trống rồi chạy lại.")
  num
}

# Đọc bảng từ csv / xlsx / xls, file local hoặc URL
read_table_any <- function(path, format = "", sheet = 1) {
  is_url <- grepl("^https?://", path)
  ext <- tolower(if (nzchar(format)) format else tools::file_ext(sub("[?#].*$", "", path)))
  if (ext == "" && is_url) {
    message("Link không có đuôi file -> giả định là CSV (đặt format_* ở MỤC 1 nếu khác): ", path)
    ext <- "csv"
  }

  if (ext == "csv") {
    sep <- csv_sep
    if (!nzchar(sep)) {                         # tự phát hiện dấu phân cách từ dòng tiêu đề
      first <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
      sep <- if (grepl(";", first) && !grepl(",", first)) ";"
             else if (grepl("\t", first) && !grepl(",", first)) "\t"
             else ","
    }
    dec <- if (nzchar(csv_dec)) csv_dec else if (sep == ";") "," else "."
    if (sep != ",")
      message("CSV phân cách bằng '", if (sep == "\t") "tab" else sep, "', dấu thập phân '", dec, "': ", path)
    return(read.csv(path, sep = sep, dec = dec, check.names = FALSE,
                    fileEncoding = "UTF-8-BOM", strip.white = TRUE))
  }

  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Cần cài gói 'readxl' để đọc file Excel: install.packages('readxl')")
    if (is_url) {                               # readxl không đọc trực tiếp URL
      tmp <- tempfile(fileext = paste0(".", ext))
      download.file(path, tmp, mode = "wb", quiet = TRUE)
      path <- tmp
    }
    return(as.data.frame(readxl::read_excel(path, sheet = sheet)))
  }

  stop("Không nhận dạng được định dạng file: ", path,
       " (chỉ hỗ trợ csv/xlsx/xls; có thể đặt format_* = \"csv\" hoặc \"xlsx\" ở MỤC 1)")
}

# Lọc đúng năm (so sánh dưới dạng chuỗi nên 2010 và "2010" đều được)
filter_year <- function(df, year, what = "") {
  if (length(year) != 1 || is.na(year)) stop("Chưa điền year0 / year1 ở MỤC 1")
  check_cols(df, col_year, what)
  yrs <- df[[col_year]]
  out <- df[!is.na(yrs) & as.character(yrs) == as.character(year), , drop = FALSE]
  if (nrow(out) == 0)
    stop("Không có dữ liệu cho năm ", year, if (nzchar(what)) paste0(" trong file ", what) else "",
         ". Các năm có trong file: ", paste(sort(unique(yrs[!is.na(yrs)])), collapse = ", "))
  out
}

# Lọc theo quốc gia (chỉ khi col_country / country được điền ở MỤC 1)
filter_country <- function(df, what = "") {
  if (!nzchar(col_country) && !nzchar(country)) return(df)
  if (!nzchar(col_country) || !nzchar(country))
    stop("Phải điền cả col_country và country ở MỤC 1 (hoặc để trống cả hai)")
  check_cols(df, col_country, what)
  ct  <- df[[col_country]]
  out <- df[!is.na(ct) & as.character(ct) == country, , drop = FALSE]
  if (nrow(out) == 0)
    stop("Không có dữ liệu cho quốc gia '", country, "'", if (nzchar(what)) paste0(" trong file ", what) else "",
         ". Các quốc gia có trong file: ", paste(head(sort(unique(ct[!is.na(ct)])), 30), collapse = ", "))
  out
}

# Chuyển bảng dạng dài -> ma trận (hàng = `row`, cột = `col`, giá trị = `value`).
# - Mỗi cặp (row, col) chỉ được 1 dòng; có dòng trùng -> DỪNG (không tự cộng dồn).
# - Giữ nguyên thứ tự xuất hiện của ngành / chức năng.
# - Tổ hợp (ngành, chức năng) không có trong dữ liệu được gán 0 và được in ra để kiểm tra.
long_to_matrix <- function(df, row, col, value) {
  check_cols(df, c(row, col, value))
  if (anyDuplicated(df[c(row, col)]) > 0)
    stop("Có nhiều hơn 1 dòng cho cùng cặp (", row, ", ", col, ") trong một năm. ",
         "Nếu file có nhiều quốc gia, điền col_country / country ở MỤC 1; ",
         "nếu không, kiểm tra dữ liệu bị lặp.")
  x <- parse_numeric(df[[value]], value)
  r <- factor(df[[row]], levels = unique(df[[row]]))
  k <- factor(df[[col]], levels = unique(df[[col]]))
  m <- tapply(x, list(r, k), sum)
  miss <- which(is.na(m), arr.ind = TRUE)
  if (nrow(miss) > 0)
    message(nrow(miss), " tổ hợp (ngành, chức năng) không có trong dữ liệu, được gán ",
            value, " = 0: ",
            paste(rownames(m)[miss[, 1]], colnames(m)[miss[, 2]], sep = "/", collapse = ", "))
  m[is.na(m)] <- 0
  m <- unclass(m)                       # bỏ class "table" của tapply -> ma trận thường
  storage.mode(m) <- "double"
  m
}

# Chuyển bảng dạng dài -> vector có tên (tên = `name`, giá trị = `value`)
long_to_vector <- function(df, name, value) {
  check_cols(df, c(name, value))
  if (anyDuplicated(df[[name]]) > 0)
    stop("Cột '", name, "' bị trùng trong cùng một năm – mỗi ngành chỉ được 1 dòng/năm ",
         "(nếu file có nhiều quốc gia, điền col_country / country ở MỤC 1)")
  setNames(parse_numeric(df[[value]], value), as.character(df[[name]]))
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
  if (any(tot == 0))
    warning("Ngành có tổng labor income = 0 (", paste(names(tot)[tot == 0], collapse = ", "),
            "): sh_ik của ngành đó được gán 0 (quy ước; FDVA của ngành đó = 0 trong kỳ này)")
  row_scale(LI, ifelse(tot == 0, 0, 1 / tot))
}

# c_i = sum_k LI_ik / VA_i   (vector theo ngành)
compute_c <- function(LI, VA) {
  stopifnot(length(VA) == nrow(LI))
  if (any(is.na(VA) | VA == 0))
    warning("Có ngành với VA = 0 hoặc NA; c_i của ngành đó sẽ là NA/Inf")
  rowSums(LI) / VA
}

# Sắp xếp / kiểm tra để 2 kỳ có cùng tập ngành (hàng) và chức năng (cột),
# cùng thứ tự với VA và DVA. Ngành / chức năng chỉ có ở 1 kỳ sẽ bị báo lỗi.
align_inputs <- function(LI0, LI1, VA0, VA1, DVA0, DVA1) {
  for (nm in c("LI0", "LI1", "VA0", "VA1", "DVA0", "DVA1")) {
    x <- get(nm)
    if (!is.numeric(x) || anyNA(x)) stop("Đầu vào ", nm, " phải là số và không chứa NA")
  }
  ind  <- rownames(LI0); fun <- colnames(LI0)
  if (is.null(ind) || is.null(fun)) stop("LI0 phải có tên hàng (ngành) và tên cột (chức năng)")
  chk <- function(x, ref, what) {
    if (is.null(x) || !setequal(x, ref))
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
  if (anyNA(residual))
    warning("Kết quả có NA/NaN – kiểm tra VA = 0 hoặc dữ liệu thiếu")
  else if (max(abs(residual)) > 1e-8 * max(1, max(abs(delta))))
    warning("Tổng 3 hiệu ứng KHÔNG bằng ΔFDVA – kiểm tra lại dữ liệu đầu vào")

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

  # Tổng hợp theo chức năng k (cộng theo ngành) và theo ngành i (cộng theo chức năng).
  # Sai số làm tròn (cỡ 1e-15) được đưa về 0 để các tổng bằng 0 về mặt lý thuyết
  # (Reallocation theo ngành / tổng) không hiển thị thành số rất nhỏ gây hiểu nhầm.
  tol <- 1e-10 * max(1, max(abs(dec$delta_FDVA), na.rm = TRUE))
  zap <- function(x) { x[!is.na(x) & abs(x) < tol] <- 0; x }
  agg <- function(f) {
    out <- data.frame(
      delta_FDVA   = zap(f(dec$delta_FDVA)),
      reallocation = zap(f(dec$reallocation)),
      intensity    = zap(f(dec$intensity)),
      scale        = zap(f(dec$scale))
    )
    # % đóng góp của từng hiệu ứng vào ΔFDVA (NA nếu ΔFDVA = 0)
    pct <- function(x) ifelse(out$delta_FDVA == 0, NA, 100 * x / out$delta_FDVA)
    out$reallocation_pct <- pct(out$reallocation)
    out$intensity_pct    <- pct(out$intensity)
    out$scale_pct        <- pct(out$scale)
    out
  }
  by_function <- cbind(func = fun,     agg(colSums))
  by_industry <- cbind(industry = ind, agg(rowSums))
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
  if (!nzchar(path_LI) || !nzchar(path_VA)) stop("Phải điền cả path_LI và path_VA ở MỤC 1")
  if (is.na(year0) || is.na(year1))         stop("Chưa điền year0 / year1 ở MỤC 1")

  LI_long <- read_table_any(path_LI, format_LI, sheet_LI)
  VA_long <- read_table_any(path_VA, format_VA, sheet_VA)
  check_cols(LI_long, c(col_industry, col_func, col_year, col_LI), "LI")
  check_cols(VA_long, c(col_industry, col_year, col_VA, col_DVA),  "VA")
  LI_long <- filter_country(LI_long, "LI")
  VA_long <- filter_country(VA_long, "VA")

  LI0 <- long_to_matrix(filter_year(LI_long, year0, "LI"), col_industry, col_func, col_LI)
  LI1 <- long_to_matrix(filter_year(LI_long, year1, "LI"), col_industry, col_func, col_LI)

  VA0  <- long_to_vector(filter_year(VA_long, year0, "VA"), col_industry, col_VA)
  VA1  <- long_to_vector(filter_year(VA_long, year1, "VA"), col_industry, col_VA)
  DVA0 <- long_to_vector(filter_year(VA_long, year0, "VA"), col_industry, col_DVA)
  DVA1 <- long_to_vector(filter_year(VA_long, year1, "VA"), col_industry, col_DVA)

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
if (nzchar(path_LI) || nzchar(path_VA)) {
  message("Đọc dữ liệu từ:\n  LI : ", path_LI, "\n  VA : ", path_VA,
          if (nzchar(country)) paste0("\n  Quốc gia: ", country) else "",
          "\n  Năm: ", year0, " -> ", year1)
  inputs <- load_inputs()
} else {
  message("*** CHƯA ĐIỀN LINK DỮ LIỆU (path_LI / path_VA ở MỤC 1) -> dùng DỮ LIỆU MẪU ***")
  inputs <- make_example_inputs()
}

res <- with(inputs, run_sda(LI0, LI1, VA0, VA1, DVA0, DVA1))

cat("\n===== Kiểm tra: max |ΔFDVA - (reallocation + intensity + scale)| =",
    format(max(abs(res$matrices$residual), na.rm = TRUE), digits = 3), "=====\n")

cat("\n----- Tổng toàn nền kinh tế (Reallocation = 0 theo lý thuyết) -----\n"); print(res$total)
cat("\n----- Theo chức năng k -----\n");                                         print(res$by_function)
cat("\n----- Theo ngành i (Reallocation = 0 theo lý thuyết) -----\n");           print(res$by_industry)
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
# - Nhiều quốc gia: điền col_country và country ở MỤC 1 để tính cho một nước.
#   Để tính lần lượt nhiều nước (sau khi đã điền path_LI, path_VA, year0, year1):
#     res_by_country <- list()
#     for (ct in c("VNM", "THA", "IDN")) {
#       country <- ct
#       inputs  <- load_inputs()
#       res_by_country[[ct]] <- with(inputs, run_sda(LI0, LI1, VA0, VA1, DVA0, DVA1))
#     }
# - Dữ liệu dạng rộng (hàng = ngành, cột = chức năng, 1 file/năm): đọc thẳng
#   thành ma trận rồi gọi run_sda(), ví dụ:
#     LI0 <- as.matrix(read.csv("LI_2010.csv", row.names = 1, check.names = FALSE))
# - Nếu đã có sẵn sh_ik và c_i (không có LI/VA), gọi trực tiếp
#   sda_fdva(sh0, sh1, c0, c1, DVA0, DVA1).
# - Tổng 3 hiệu ứng luôn bằng ΔFDVA_ik (trung bình 2 phân rã polar), không có
#   phần dư (interaction term). Reallocation cộng theo chức năng k = 0 cho từng
#   ngành (xem lưu ý ở đầu file).
# - Ngành có tổng LI = 0 ở một kỳ: sh_ik kỳ đó được quy ước = 0 (công thức
#   không xác định 0/0); FDVA kỳ đó = 0 và cách chia 3 hiệu ứng cho ngành đó
#   chỉ mang tính quy ước.
# -----------------------------------------------------------------------------
