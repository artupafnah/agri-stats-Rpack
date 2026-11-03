# ==============================================================================
# MODUL 04: RANCANGAN PETAK TERPISAH (SPLIT-PLOT) & PETAK BERJALUR (STRIP-PLOT)
# Deskripsi   : 1. Pengacakan Rancangan Split-Plot dan Strip-Plot (agricolae)
#               2. ANOVA Split-Plot dalam RAL (Serapan Debu 2 Lokasi x 10 Tanaman)
#               3. ANOVA Split-Plot dalam RAK & Uji Lanjut LSMeans (Varietas Gandum)
#               4. ANOVA Strip-Plot (Split-Block) dalam RAK & Perhitungan KKa, KKb, KKc
# ==============================================================================

if (!dir.exists("data") && dir.exists("../data")) setwd("..")

library(agricolae)
library(emmeans)
library(multcomp)

# ==============================================================================
# ANALISIS 4.1: PENGACAKAN RANCANGAN SPLIT-PLOT DAN STRIP-PLOT
# ==============================================================================

A <- c("A1", "A2", "A3")       # 3 taraf Faktor A (Petak Utama / Jalur Mendatar)
B <- c("B1", "B2", "B3", "B4") # 4 taraf Faktor B (Anak Petak / Jalur Tegak)

cat("=== 4.1A. PENGACAKAN SPLIT-PLOT DALAM RAL (CRD) ===\n")
desain_split_ral <- design.split(trt1 = A, trt2 = B, r = 3, design = "crd", seed = 512, serie = 2)
print(head(desain_split_ral$book, 12))

cat("\n=== 4.1B. PENGACAKAN SPLIT-PLOT DALAM RAK (RCBD) ===\n")
desain_split_rak <- design.split(trt1 = A, trt2 = B, r = 3, design = "rcbd", seed = 512, serie = 2)
print(head(desain_split_rak$book, 12))

cat("\n=== 4.1C. PENGACAKAN STRIP-PLOT (SPLIT-BLOCK) DALAM RAK ===\n")
desain_strip_rak <- design.strip(trt1 = A, trt2 = B, r = 3, seed = 512, serie = 2)
print(head(desain_strip_rak$book, 12))

# ==============================================================================
# ANALISIS 4.2: ANOVA SPLIT-PLOT DALAM RAL
# Kasus: Petak Utama = 2 Lokasi (Pabrik vs Kampus) x 3 Ulangan;
#        Anak Petak  = 10 Jenis Tanaman Pelindung (T1..T10) pada Serapan Debu
# ==============================================================================

cat("\n=== 4.2. ANOVA SPLIT-PLOT DALAM RAL (SERAPAN DEBU TANAMAN) ===\n")
df_debu <- read.csv("data/07_debu_splitplot_ral.csv")
df_debu$Lokasi      <- factor(df_debu$Lokasi, levels = c("Kampus", "Pabrik"))
df_debu$Tanaman     <- factor(df_debu$Tanaman, levels = paste0("T", 1:10))
df_debu$Ulangan     <- factor(df_debu$Ulangan)
df_debu$Petak_Utama <- interaction(df_debu$Lokasi, df_debu$Ulangan) # 6 satuan petak utama

# Menjalankan ANOVA Split-Plot RAL dengan Galat (a) = Petak_Utama (Lokasi:Ulangan)
model_split_ral <- aov(Serapan_Debu ~ Lokasi * Tanaman + Error(Petak_Utama), data = df_debu)
ringkasan_split_ral <- summary(model_split_ral)
print(ringkasan_split_ral)

# Perhitungan Koefisien Keragaman Galat Petak Utama (KKa) & Anak Petak (KKb)
ktg_a_debu  <- ringkasan_split_ral[["Error: Petak_Utama"]][[1]]["Residuals", "Mean Sq"]
ktg_b_debu  <- ringkasan_split_ral[["Error: Within"]][[1]]["Residuals", "Mean Sq"]
rata_debu   <- mean(df_debu$Serapan_Debu)
kka_debu    <- sqrt(ktg_a_debu) / rata_debu * 100
kkb_debu    <- sqrt(ktg_b_debu) / rata_debu * 100
cat(sprintf("Rata-rata = %.2f | KTG(a) = %.4f (KKa = %.2f%%) | KTG(b) = %.4f (KKb = %.2f%%)\n",
            rata_debu, ktg_a_debu, kka_debu, ktg_b_debu, kkb_debu))

# Visualisasi profil serapan debu 10 jenis tanaman pada kedua lokasi
interaction.plot(
  x.factor     = df_debu$Tanaman,
  trace.factor = df_debu$Lokasi,
  response     = df_debu$Serapan_Debu,
  type = "b", pch = c(19, 17), col = c("#27ae60", "#c0392b"), lwd = 2,
  xlab = "Jenis Tanaman Pelindung (Anak Petak)",
  ylab = "Rata-rata Serapan Debu (g/m2)",
  trace.label = "Lokasi (Petak Utama)",
  main = "Profil Serapan Debu 10 Jenis Tanaman di Lokasi Kampus vs Pabrik"
)

# ==============================================================================
# ANALISIS 4.3: ANOVA SPLIT-PLOT DALAM RAK & UJI LANJUT LSMEANS
# Kasus: Petak Utama = 4 Varietas Gandum (A1..A4)
#        Anak Petak  = 4 Perlakuan Kimiawi Benih (B1..B4) dalam 4 Kelompok
# ==============================================================================

cat("\n=== 4.3. ANOVA SPLIT-PLOT DALAM RAK (HASIL GANDUM) ===\n")
df_gandum <- read.csv("data/08_gandum_splitplot_rak.csv")
df_gandum$Varietas_A  <- factor(df_gandum$Varietas_A)
df_gandum$Perlakuan_B <- factor(df_gandum$Perlakuan_B)
df_gandum$Kelompok    <- factor(df_gandum$Kelompok, levels = c("I", "II", "III", "IV"))

# Cara 1: Menggunakan aov() bawaan R dengan suku Error(Kelompok:Varietas_A)
model_split_rak <- aov(
  Hasil ~ Kelompok + Varietas_A * Perlakuan_B + Error(Kelompok:Varietas_A),
  data = df_gandum
)
cat("--- Output Cara 1: aov() dengan Error(Kelompok:Varietas_A) ---\n")
print(summary(model_split_rak))

# Cara 2: Menggunakan agricolae::sp.plot() (otomatis menampilkan Ea, Eb, cv(a), cv(b))
cat("\n--- Output Cara 2: agricolae::sp.plot() ---\n")
hasil_sp <- with(df_gandum, sp.plot(block = Kelompok, pplot = Varietas_A, splot = Perlakuan_B, Y = Hasil))

# Uji Lanjut LSMeans (emmeans) menggunakan strata galat yang tepat secara otomatis:
# 1) Karena interaksi A:B tidak nyata (p = 0.9675), uji pengaruh utama Varietas_A (df = 9) & Perlakuan_B (df = 36)
cat("\n--- Uji Lanjut LSMeans Petak Utama (Varietas_A, memakai Galat a ber-db = 9) ---\n")
emm_var_a <- emmeans(model_split_rak, ~ Varietas_A)
print(cld(emm_var_a, alpha = 0.05, Letters = letters, adjust = "tukey"))

cat("\n--- Uji Lanjut LSMeans Anak Petak (Perlakuan_B, memakai Galat b ber-db = 36) ---\n")
emm_perl_b <- emmeans(model_split_rak, ~ Perlakuan_B)
print(cld(emm_perl_b, alpha = 0.05, Letters = letters, adjust = "tukey"))

# 2) Demonstrasi sintaks Pengaruh Sederhana Anak Petak di dalam Petak Utama (~ B | A)
cat("\n--- Demonstrasi LSMeans Pengaruh Sederhana (~ Perlakuan_B | Varietas_A) ---\n")
emm_gandum_simple <- emmeans(model_split_rak, ~ Perlakuan_B | Varietas_A)
print(pairs(emm_gandum_simple, adjust = "tukey"))

# ==============================================================================
# ANALISIS 4.4: ANOVA STRIP-PLOT (SPLIT-BLOCK) DALAM RAK & KKa, KKb, KKc
# Kasus: Jalur Mendatar = 6 Varietas Padi (V1..V6)
#        Jalur Tegak    = 3 Dosis Pupuk Nitrogen (N0, N60, N120) dalam 3 Kelompok
# ==============================================================================

cat("\n=== 4.4. ANOVA STRIP-PLOT DALAM RAK (HASIL GABAH PADI) ===\n")
df_strip <- read.csv("data/09_padi_stripplot_rak.csv")
df_strip$Varietas <- factor(df_strip$Varietas, levels = paste0("V", 1:6))
df_strip$Nitrogen <- factor(df_strip$Nitrogen, levels = c("N0", "N60", "N120"))
df_strip$Kelompok <- factor(df_strip$Kelompok, levels = c("I", "II", "III"))

# Cara 1: Menggunakan agricolae::strip.plot()
cat("--- Output Cara 1: agricolae::strip.plot() ---\n")
hasil_strip <- with(
  df_strip,
  strip.plot(BLOCK = Kelompok, COL = Varietas, ROW = Nitrogen, Y = Hasil_Gabah)
)

# Cara 2: Menggunakan aov() multi-galat Error(Kelompok:Varietas + Kelompok:Nitrogen)
cat("\n--- Output Cara 2: aov() dengan 3 Lapisan Galat ---\n")
model_strip <- aov(
  Hasil_Gabah ~ Kelompok + Varietas * Nitrogen +
    Error(Kelompok:Varietas + Kelompok:Nitrogen),
  data = df_strip
)
ringkasan_strip <- summary(model_strip)
print(ringkasan_strip)

# Perhitungan Manual Eksplisit KKa, KKb, dan KKc dari Ketiga Lapisan Galat
ktg_a_strip <- ringkasan_strip[["Error: Kelompok:Varietas"]][[1]]["Residuals", "Mean Sq"]
ktg_b_strip <- ringkasan_strip[["Error: Kelompok:Nitrogen"]][[1]]["Residuals", "Mean Sq"]
ktg_c_strip <- ringkasan_strip[["Error: Within"]][[1]]["Residuals", "Mean Sq"]
rata_strip  <- mean(df_strip$Hasil_Gabah)

KKa <- sqrt(ktg_a_strip) / rata_strip * 100
KKb <- sqrt(ktg_b_strip) / rata_strip * 100
KKc <- sqrt(ktg_c_strip) / rata_strip * 100

cat(sprintf(
  "\nVerifikasi Manual Koefisien Keragaman 3 Lapisan Galat Strip-Plot:\nRata-rata Umum = %.2f ton/ha\nKTG(a) = %.4f -> KKa = %.2f%%\nKTG(b) = %.4f -> KKb = %.2f%%\nKTG(c) = %.4f -> KKc = %.2f%%\n",
  rata_strip, ktg_a_strip, KKa, ktg_b_strip, KKb, ktg_c_strip, KKc
))

# Visualisasi respons 6 Varietas Padi pada 3 jalur Nitrogen
interaction.plot(
  x.factor     = df_strip$Varietas,
  trace.factor = df_strip$Nitrogen,
  response     = df_strip$Hasil_Gabah,
  type = "b", pch = c(19, 17, 15), col = c("#7f8c8d", "#2980b9", "#27ae60"), lwd = 2.2,
  xlab = "Varietas Padi (Jalur Mendatar)",
  ylab = "Rata-rata Hasil Gabah (ton/ha)",
  trace.label = "Dosis Nitrogen",
  main = "Profil Hasil Gabah 6 Varietas Padi x 3 Taraf Nitrogen (Strip-Plot RAK)"
)
