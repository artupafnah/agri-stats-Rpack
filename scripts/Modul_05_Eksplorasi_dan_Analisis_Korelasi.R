# ==============================================================================
# MODUL 05: EKSPLORASI HUBUNGAN DUA PEUBAH & ANALISIS KORELASI
# Deskripsi   : 1. Scatter Plot & Korelasi Linier Pearson + Uji-t (H0: rho = 0)
#               2. Uji Hipotesis Korelasi Umum (H0: rho = rho_0) via Transformasi Z Fisher
#               3. Korelasi Peringkat Spearman (Data Ber-Outlier / Skala Ordinal)
#               4. Tabel Kontingensi & Uji Khi-Kuadrat (Dua Peubah Kategorik)
# ==============================================================================

if (!dir.exists("data") && dir.exists("../data")) setwd("..")

df_hc       <- read.csv("data/10_emisi_hc_korelasi.csv")
df_jangkrik <- read.csv("data/11_jangkrik_suhu.csv")

# ==============================================================================
# ANALISIS 5.1: SCATTER PLOT & KORELASI LINIER PEARSON + UJI-t (H0: rho = 0)
# ==============================================================================

cat("=== 5.1A. KORELASI PEARSON JARAK TEMPUH VS EMISI HC (TANPA OUTLIER) ===\n")
uji_pearson_hc <- cor.test(df_hc$Jarak_Tempuh_X, df_hc$Emisi_HC_Normal, method = "pearson")
print(uji_pearson_hc)

cat("\n=== 5.1B. KORELASI PEARSON DERIK JANGKRIK VS SUHU UDARA ===\n")
uji_pearson_jangkrik <- cor.test(df_jangkrik$Derik_Per_Detik, df_jangkrik$Suhu_Fahrenheit,
                                 method = "pearson")
print(uji_pearson_jangkrik)

# Verifikasi rumus manual t_hitung = r * sqrt(n - 2) / sqrt(1 - r^2)
n_j   <- nrow(df_jangkrik)
r_j   <- cor(df_jangkrik$Derik_Per_Detik, df_jangkrik$Suhu_Fahrenheit)
t_man <- r_j * sqrt(n_j - 2) / sqrt(1 - r_j^2)
cat(sprintf("Verifikasi Manual: r = %.4f | t_hitung = %.4f (db = %d)\n", r_j, t_man, n_j - 2))

# Visualisasi Scatter Plot kedua kasus berdampingan
par(mfrow = c(1, 2))
plot(df_hc$Jarak_Tempuh_X, df_hc$Emisi_HC_Normal,
     pch = 19, col = "#2980b9", cex = 1.3,
     xlab = "Jarak Tempuh Mobil (ribu km)", ylab = "Emisi Gas HC (gram/km)",
     main = "Jarak Tempuh vs Emisi HC (r = 0.9978)")
abline(lm(Emisi_HC_Normal ~ Jarak_Tempuh_X, data = df_hc), col = "#e74c3c", lwd = 2)

plot(df_jangkrik$Derik_Per_Detik, df_jangkrik$Suhu_Fahrenheit,
     pch = 19, col = "#27ae60", cex = 1.3,
     xlab = "Frekuensi Derik Jangkrik per Detik", ylab = "Suhu Udara (Fahrenheit)",
     main = "Derik Jangkrik vs Suhu (r = 0.8351)")
abline(lm(Suhu_Fahrenheit ~ Derik_Per_Detik, data = df_jangkrik), col = "#e74c3c", lwd = 2)
par(mfrow = c(1, 1))

# ==============================================================================
# ANALISIS 5.2: UJI HIPOTESIS KORELASI UMUM (H0: rho = rho_0) — TRANSFORMASI Z FISHER
# Kasus: Menguji apakah korelasi Derik Jangkrik vs Suhu sama dengan rho_0 = 0.70
# ==============================================================================

cat("\n=== 5.2. UJI TRANSFORMASI Z FISHER UNTUK H0: rho = 0.70 ===\n")

uji_fisher_z <- function(x, y, rho_0 = 0.70, alpha = 0.05) {
  n     <- length(x)
  r     <- cor(x, y, method = "pearson")
  z_r   <- atanh(r)                 # 0.5 * log((1 + r) / (1 - r))
  z_0   <- atanh(rho_0)             # 0.5 * log((1 + rho_0) / (1 - rho_0))
  se_z  <- sqrt(1 / (n - 3))        # Simpangan baku sebaran Z Fisher
  z_hit <- (z_r - z_0) / se_z
  p_2arah <- 2 * (1 - pnorm(abs(z_hit)))
  p_kanan <- 1 - pnorm(z_hit)       # Untuk H1 satu arah: rho > rho_0

  # Selang kepercayaan (1 - alpha) melalui transformasi balik tanh()
  z_kritis <- qnorm(1 - alpha / 2)
  ci_bawah <- tanh(z_r - z_kritis * se_z)
  ci_atas  <- tanh(z_r + z_kritis * se_z)

  data.frame(
    n             = n,
    r_sampel      = round(r, 4),
    rho_0         = rho_0,
    Z_r           = round(z_r, 4),
    Z_rho0        = round(z_0, 4),
    Z_hitung      = round(z_hit, 4),
    p_value_2arah = round(p_2arah, 4),
    p_value_kanan = round(p_kanan, 4),
    SK95_Bawah    = round(ci_bawah, 4),
    SK95_Atas     = round(ci_atas, 4)
  )
}

hasil_fisher <- uji_fisher_z(df_jangkrik$Derik_Per_Detik, df_jangkrik$Suhu_Fahrenheit, rho_0 = 0.70)
print(hasil_fisher)

# ==============================================================================
# ANALISIS 5.3: KORELASI PERINGKAT SPEARMAN (DATA BER-OUTLIER / SKALA ORDINAL)
# Studi Kasus: Dampak Pencilan Ekstrem pada Amatan Mobil ke-10
# ==============================================================================

cat("\n=== 5.3. PERBANDINGAN PEARSON VS SPEARMAN SAAT ADA OUTLIER ===\n")
r_pearson_normal   <- cor.test(df_hc$Jarak_Tempuh_X, df_hc$Emisi_HC_Normal, method = "pearson")
r_spearman_normal  <- cor.test(df_hc$Jarak_Tempuh_X, df_hc$Emisi_HC_Normal, method = "spearman", exact = FALSE)
r_pearson_outlier  <- cor.test(df_hc$Jarak_Tempuh_X, df_hc$Emisi_HC_Outlier, method = "pearson")
r_spearman_outlier <- cor.test(df_hc$Jarak_Tempuh_X, df_hc$Emisi_HC_Outlier, method = "spearman", exact = FALSE)

ringkasan_outlier <- data.frame(
  Kondisi_Data = c("Normal (Tanpa Outlier)", "Dengan Outlier (Mobil ke-10 = 2.35)"),
  r_Pearson    = round(c(r_pearson_normal$estimate, r_pearson_outlier$estimate), 4),
  p_Pearson    = signif(c(r_pearson_normal$p.value, r_pearson_outlier$p.value), 4),
  r_Spearman   = round(c(r_spearman_normal$estimate, r_spearman_outlier$estimate), 4),
  p_Spearman   = signif(c(r_spearman_normal$p.value, r_spearman_outlier$p.value), 4)
)
print(ringkasan_outlier)

# Visualisasi efek tarikan pencilan terhadap garis tren linier
par(mfrow = c(1, 2))
plot(df_hc$Jarak_Tempuh_X, df_hc$Emisi_HC_Normal, pch = 19, col = "#2980b9", cex = 1.3,
     ylim = c(0.3, 2.5), xlab = "Jarak Tempuh (ribu km)", ylab = "Emisi HC (g/km)",
     main = "Tanpa Outlier (r = 0.9978, rs = 1.00)")
abline(lm(Emisi_HC_Normal ~ Jarak_Tempuh_X, data = df_hc), col = "#2980b9", lwd = 2)

warna_titik <- ifelse(df_hc$Mobil == 10, "#e74c3c", "#2c3e50")
plot(df_hc$Jarak_Tempuh_X, df_hc$Emisi_HC_Outlier, pch = 19, col = warna_titik, cex = 1.4,
     ylim = c(0.3, 2.5), xlab = "Jarak Tempuh (ribu km)", ylab = "Emisi HC (g/km)",
     main = "Dengan 1 Outlier (r = 0.7811, rs = 1.00)")
abline(lm(Emisi_HC_Outlier ~ Jarak_Tempuh_X, data = df_hc), col = "#e74c3c", lwd = 2, lty = 2)
text(df_hc$Jarak_Tempuh_X[10] - 32, df_hc$Emisi_HC_Outlier[10], "Outlier!", col = "#e74c3c", font = 2)
par(mfrow = c(1, 1))

# ==============================================================================
# ANALISIS 5.4: TABEL KONTINGENSI & UJI KHI-KUADRAT (DUA PEUBAH KATEGORIK)
# Kasus: Asosiasi Status Penguasaan Lahan Petani vs Tingkat Adopsi Benih Unggul
# ==============================================================================

cat("\n=== 5.4. TABEL KONTINGENSI & UJI KHI-KUADRAT (CHI-SQUARE) ===\n")
tabel_adopsi <- matrix(
  c(28, 15,  7,
    12, 25, 23),
  nrow = 2, byrow = TRUE,
  dimnames = list(
    Status_Lahan = c("Sewa/Sakap", "Milik_Sendiri"),
    Adopsi_Benih = c("Rendah", "Sedang", "Tinggi")
  )
)
cat("Frekuensi Pengamatan (O_ij):\n")
print(addmargins(tabel_adopsi))

uji_chi <- chisq.test(tabel_adopsi)
print(uji_chi)

cat("Frekuensi Harapan di Bawah H0 (E_ij — wajib >= 5):\n")
print(round(uji_chi$expected, 2))

# Menghitung Koefisien Kontingensi (C) dan Cramer's V sebagai ukuran kekuatan asosiasi
N_tot     <- sum(tabel_adopsi)
koef_C    <- sqrt(uji_chi$statistic / (uji_chi$statistic + N_tot))
cramers_V <- sqrt(uji_chi$statistic / (N_tot * (min(dim(tabel_adopsi)) - 1)))
cat(sprintf("Koefisien Kontingensi C = %.4f | Cramer's V = %.4f\n", koef_C, cramers_V))

# Visualisasi Proporsi Barplot Bertumpuk
prop_adopsi <- prop.table(tabel_adopsi, margin = 1) * 100
barplot(t(prop_adopsi), beside = TRUE,
        col = c("#e74c3c", "#f39c12", "#27ae60"),
        ylim = c(0, 65),
        xlab = "Status Penguasaan Lahan Petani",
        ylab = "Persentase Petani (%)",
        main = "Distribusi Tingkat Adopsi Benih Unggul Berdasarkan Status Lahan")
legend("topright", legend = colnames(tabel_adopsi),
       fill = c("#e74c3c", "#f39c12", "#27ae60"), title = "Tingkat Adopsi", bty = "n")
