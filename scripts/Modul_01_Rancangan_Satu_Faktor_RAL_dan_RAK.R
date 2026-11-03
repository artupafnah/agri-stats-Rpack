# ==============================================================================
# MODUL 01: RANCANGAN PERCOBAAN SATU FAKTOR — RAL DAN RAK
# Deskripsi   : 1. Pengacakan denah lapangan RAL (CRD) dan RAK (RCBD)
#               2. ANOVA Satu Arah — Rancangan Acak Lengkap (RAL)
#               3. ANOVA Dua Arah Tanpa Interaksi — Rancangan Acak Kelompok (RAK)
# ==============================================================================

# Mengatur direktori kerja otomatis ke root proyek jika dijalankan dari folder scripts/
if (!dir.exists("data") && dir.exists("../data")) setwd("..")

library(agricolae)

# ==============================================================================
# ANALISIS 1.1: PENGACAKAN LAYOUT RAL (CRD) DAN RAK (RCBD)
# Kasus: Penempatan 4 perlakuan pupuk (A, B, C, D) dengan 3 ulangan/kelompok
# ==============================================================================

perlakuan <- c("A", "B", "C", "D")

# 1. Pengacakan Rancangan Acak Lengkap (RAL / CRD): 4 perlakuan x 3 ulangan = 12 petak
desain_ral <- design.crd(trt = perlakuan, r = 3, seed = 512, serie = 2)
cat("=== BUKU LAPANGAN PENGACAKAN RAL (12 PETAK) ===\n")
print(desain_ral$book)

# Menampilkan susunan RAL dalam matriks denah 3 baris x 4 kolom
denah_ral <- matrix(
  paste0(desain_ral$book$plots, ":", desain_ral$book$perlakuan),
  nrow = 3, ncol = 4, byrow = TRUE
)
cat("\n=== DENAH TATA LETAK PETAK RAL ===\n")
print(denah_ral)

# 2. Pengacakan Rancangan Acak Kelompok (RAK / RCBD): 4 perlakuan x 3 kelompok = 12 petak
desain_rak <- design.rcbd(trt = perlakuan, r = 3, seed = 512, serie = 2)
cat("\n=== DENAH LAPANGAN PER KELOMPOK RAK (SKETCH) ===\n")
print(desain_rak$sketch)

cat("\n=== BUKU LAPANGAN PENGACAKAN RAK (BOOK) ===\n")
print(desain_rak$book)

# ==============================================================================
# ANALISIS 1.2: ANOVA SATU ARAH — RANCANGAN ACAK LENGKAP (RAL / CRD)
# Studi Kasus: Pengaruh 6 Strain Bakteri Rhizobium terhadap Kadar Nitrogen (mg)
#              Tanaman Red Clover dengan 5 Ulangan di Rumah Kaca
# ==============================================================================

# 1. Membaca dataset dan mengatur urutan faktor perlakuan
df_clover <- read.csv("data/02_red_clover_ral.csv")
df_clover$Perlakuan <- factor(
  df_clover$Perlakuan,
  levels = c("3DOK1", "3DOK5", "3DOK4", "3DOK7", "3DOK13", "Komposit")
)

str(df_clover)

# 2. Eksplorasi rata-rata, simpangan baku, dan jumlah ulangan per strain
ringkasan_clover <- do.call(
  data.frame,
  aggregate(Kadar_N ~ Perlakuan, data = df_clover,
            FUN = function(x) c(n = length(x), Mean = round(mean(x), 2), SD = round(sd(x), 2)))
)
cat("\n=== RINGKASAN STATISTIK KADAR NITROGEN RED CLOVER (RAL) ===\n")
print(ringkasan_clover)

# 3. Menjalankan Analisis Ragam (ANOVA) Satu Arah RAL
model_ral <- aov(Kadar_N ~ Perlakuan, data = df_clover)
cat("\n=== TABEL ANOVA RAL RED CLOVER ===\n")
print(summary(model_ral))

# 4. Menghitung Kuadrat Tengah Galat (KTG), Rata-rata Umum, dan Koefisien Keragaman (KK %)
ktg_ral <- summary(model_ral)[[1]]["Residuals", "Mean Sq"]
rata_ral <- mean(df_clover$Kadar_N)
kk_ral  <- sqrt(ktg_ral) / rata_ral * 100
cat(sprintf("Rata-rata Umum = %.2f mg | KTG = %.2f | KK = %.2f%%\n",
            rata_ral, ktg_ral, kk_ral))

# 5. Menampilkan nilai kritis F-tabel pada alpha = 5% dan alpha = 1%
f_tab_05_ral <- qf(0.95, df1 = 5, df2 = 24)
f_tab_01_ral <- qf(0.99, df1 = 5, df2 = 24)
cat(sprintf("F_tabel(0.05; 5, 24) = %.2f | F_tabel(0.01; 5, 24) = %.2f\n",
            f_tab_05_ral, f_tab_01_ral))

# 6. Visualisasi Boxplot & Titik Rata-rata Kadar Nitrogen per Strain Rhizobium
boxplot(Kadar_N ~ Perlakuan, data = df_clover,
        col  = "#aed6f1", border = "#1b4f72",
        main = "Sebaran Kadar Nitrogen Red Clover per Strain Rhizobium (RAL)",
        xlab = "Perlakuan Strain Bakteri Rhizobium",
        ylab = "Kadar Nitrogen (mg)")
rata_strain <- tapply(df_clover$Kadar_N, df_clover$Perlakuan, mean)
points(1:length(rata_strain), rata_strain, pch = 19, col = "#c0392b", cex = 1.3)
legend("topright", legend = "Rata-rata Perlakuan", pch = 19, col = "#c0392b", bty = "n")

# ==============================================================================
# ANALISIS 1.3: ANOVA DUA ARAH TANPA INTERAKSI — RANCANGAN ACAK KELOMPOK (RAK)
# Studi Kasus: Pengaruh 6 Kombinasi Pupuk NPK pada Padi Genotif S-969
#              di 4 Kelompok Petak Sawah
# ==============================================================================

# 1. Membaca dataset dan mengubah Perlakuan serta Kelompok menjadi factor
df_padi <- read.csv("data/01_padi_rak_s969.csv")
df_padi$Perlakuan <- factor(
  df_padi$Perlakuan,
  levels = c("Kontrol", "PK", "N", "NP", "NK", "NPK")
)
df_padi$Kelompok <- factor(
  df_padi$Kelompok,
  levels = c("I", "II", "III", "IV")
)

# 2. Menyusun tabel dua arah (Perlakuan x Kelompok) beserta rata-rata baris dan kolom
tabel_dua_arah <- xtabs(Hasil_Gabah ~ Perlakuan + Kelompok, data = df_padi)
cat("\n=== TABEL DATA HASIL GABAH (TON/HA) PADI S-969 (PERLAKUAN x KELOMPOK) ===\n")
print(addmargins(tabel_dua_arah, FUN = mean, quiet = TRUE))

# 3. Menjalankan ANOVA RAK (Kelompok + Perlakuan tanpa interaksi)
model_rak <- aov(Hasil_Gabah ~ Kelompok + Perlakuan, data = df_padi)
cat("\n=== TABEL ANOVA RAK PADI S-969 ===\n")
print(summary(model_rak))

# 4. Menghitung KTG, KK (%), dan Efisiensi Relatif (ER) Pengelompokan terhadap RAL
tabel_anova_rak <- summary(model_rak)[[1]]
ktk_rak <- tabel_anova_rak["Kelompok", "Mean Sq"]
ktg_rak <- tabel_anova_rak["Residuals", "Mean Sq"]
kk_rak  <- sqrt(ktg_rak) / mean(df_padi$Hasil_Gabah) * 100

r_blok <- nlevels(df_padi$Kelompok)
t_perl <- nlevels(df_padi$Perlakuan)
ktg_uga_ral <- ((r_blok - 1) * ktk_rak + r_blok * (t_perl - 1) * ktg_rak) / (r_blok * t_perl - 1)
efisiensi_relatif <- (ktg_uga_ral / ktg_rak) * 100

cat(sprintf("KTG = %.4f | KK = %.2f%% | Efisiensi Relatif RAK vs RAL = %.1f%%\n",
            ktg_rak, kk_rak, efisiensi_relatif))

# 5. Visualisasi Profil Hasil Gabah per Perlakuan di Setiap Kelompok Lahan
par(mfrow = c(1, 2))
boxplot(Hasil_Gabah ~ Perlakuan, data = df_padi,
        col  = "#a9dfbf", border = "#1e8449",
        main = "Hasil Gabah per Perlakuan Pupuk",
        xlab = "Kombinasi Pupuk NPK", ylab = "Hasil Gabah (ton/ha)")

boxplot(Hasil_Gabah ~ Kelompok, data = df_padi,
        col  = "#fad7a0", border = "#b9770e",
        main = "Keragaman Hasil Antar-Kelompok Sawah",
        xlab = "Kelompok (Blok)", ylab = "Hasil Gabah (ton/ha)")
par(mfrow = c(1, 1))
