# ==============================================================================
# MODUL 03: PERCOBAAN FAKTORIAL (MULTIFAKTOR)
# Deskripsi   : 1. Pengacakan Faktorial dalam RAL, RAK, dan RBSL (design.ab)
#               2. ANOVA Faktorial 2 Faktor dalam RAL (Daya Kecambah Tomat)
#               3. ANOVA Faktorial 2 Faktor dalam RAK (Pertambahan Bobot Sapi)
#               4. ANOVA Faktorial 2 Faktor dalam RBSL (Jagung 2x2 Latin Square)
#               5. Plot Interaksi & Uji Lanjut Pengaruh Sederhana (Simple Effects)
# ==============================================================================

if (!dir.exists("data") && dir.exists("../data")) setwd("..")

library(agricolae)
library(emmeans)
library(multcomp)

# ==============================================================================
# ANALISIS 3.1: PENGACAKAN FAKTORIAL DALAM RAL, RAK, DAN RBSL
# ==============================================================================

cat("=== 3.1A. PENGACAKAN FAKTORIAL 3 x 4 DALAM RAL (CRD) ===\n")
fak_ral <- design.ab(trt = c(3, 4), r = 3, design = "crd", seed = 512, serie = 2)
print(head(fak_ral$book, 10))

cat("\n=== 3.1B. PENGACAKAN FAKTORIAL 3 x 4 DALAM RAK (RCBD) ===\n")
fak_rak <- design.ab(trt = c(3, 4), r = 3, design = "rcbd", seed = 512, serie = 2)
print(head(fak_rak$book, 12))

cat("\n=== 3.1C. PENGACAKAN FAKTORIAL 2 x 2 DALAM RBSL / LATIN SQUARE (4x4) ===\n")
fak_rbsl <- design.ab(trt = c(2, 2), design = "lsd", seed = 512, serie = 2)
print(fak_rbsl$book)

# ==============================================================================
# ANALISIS 3.2: ANOVA FAKTORIAL 2 FAKTOR DALAM RAL (FAKTORIAL RAL)
# Studi Kasus: 5 Dosis Fumigan (0,16,32,48,64) x 2 Lama Fumigasi (2,4 jam) x 3 Ulangan
#              terhadap Daya Kecambah Benih Tomat (%)
# ==============================================================================

cat("\n=== 3.2. ANOVA FAKTORIAL RAL DAYA KECAMBAH BENIH TOMAT ===\n")
df_tomat <- read.csv("data/05_tomat_faktorial_ral.csv")
df_tomat$Dosis <- factor(df_tomat$Dosis, levels = c(0, 16, 32, 48, 64))
df_tomat$Lama  <- factor(df_tomat$Lama, levels = c(2, 4), labels = c("2_Jam", "4_Jam"))

# Tabel rata-rata kombinasi perlakuan Dosis x Lama (Tabel Dwitaraf AB)
cat("Tabel Rata-rata Daya Kecambah (%) Kombinasi Dosis x Lama:\n")
tabel_tomat <- with(df_tomat, tapply(Daya_Kecambah, list(Dosis = Dosis, Lama = Lama), mean))
print(addmargins(tabel_tomat, FUN = mean, quiet = TRUE))

# Operator '*' otomatis menguraikan: Dosis + Lama + Dosis:Lama
model_fak_ral <- aov(Daya_Kecambah ~ Dosis * Lama, data = df_tomat)
print(summary(model_fak_ral))

ktg_tomat <- summary(model_fak_ral)[[1]]["Residuals", "Mean Sq"]
kk_tomat  <- sqrt(ktg_tomat) / mean(df_tomat$Daya_Kecambah) * 100
cat(sprintf("KTG = %.2f | KK = %.2f%%\n", ktg_tomat, kk_tomat))

# ==============================================================================
# ANALISIS 3.3: ANOVA FAKTORIAL 2 FAKTOR DALAM RAK (FAKTORIAL RAK)
# Studi Kasus: 4 Suplemen Seng/SM (0,25,50,75) x 3 Minyak Ikan/SS (0, 1.5, 3.0)
#              dalam 3 Blok Bobot Awal Sapi
# ==============================================================================

cat("\n=== 3.3. ANOVA FAKTORIAL RAK PERTAMBAHAN BOBOT SAPI ===\n")
df_sapi <- read.csv("data/06_sapi_faktorial_rak.csv")
df_sapi$SM   <- factor(df_sapi$SM, levels = c(0, 25, 50, 75), labels = c("0", "25", "50", "75"))
df_sapi$SS   <- factor(df_sapi$SS, levels = c(0, 1.5, 3), labels = c("0", "1.5", "3.0"))
df_sapi$Blok <- factor(df_sapi$Blok, levels = c("I", "II", "III"))

# Tabel rata-rata kombinasi SM x SS
cat("Tabel Rata-rata Pertambahan Bobot Harian Sapi (kg/hari) Kombinasi SM x SS:\n")
tabel_sapi <- with(df_sapi, tapply(Bobot, list(SM = SM, SS = SS), mean))
print(round(addmargins(tabel_sapi, FUN = mean, quiet = TRUE), 3))

model_fak_rak <- aov(Bobot ~ Blok + SM * SS, data = df_sapi)
print(summary(model_fak_rak))

ktg_sapi <- summary(model_fak_rak)[[1]]["Residuals", "Mean Sq"]
kk_sapi  <- sqrt(ktg_sapi) / mean(df_sapi$Bobot) * 100
cat(sprintf("KTG = %.6f | KK = %.2f%%\n", ktg_sapi, kk_sapi))

# ==============================================================================
# ANALISIS 3.4: ANOVA FAKTORIAL DALAM RANCANGAN BUJUR SANGKAR LATIN (FAKTORIAL RBSL)
# Kasus: Faktorial 2 Varietas Jagung (V1, V2) x 2 Olah Tanah (OT_Min, OT_Int)
#        pada Lahan Bergradasi Ganda (4 Baris Irigasi x 4 Kolom Lereng = 16 Petak)
# ==============================================================================

cat("\n=== 3.4. ANOVA FAKTORIAL 2x2 DALAM RBSL (LATIN SQUARE 4x4) ===\n")
# Susunan Bujur Sangkar Latin 4x4 untuk 4 kombinasi (A=V1:Min, B=V1:Int, C=V2:Min, D=V2:Int)
df_fak_rbsl <- data.frame(
  Baris      = factor(rep(1:4, each = 4)),
  Kolom      = factor(rep(1:4, times = 4)),
  Kombinasi  = c("A", "B", "C", "D",
                 "B", "A", "D", "C",
                 "C", "D", "A", "B",
                 "D", "C", "B", "A"),
  Hasil_Jagung = c(6.2, 7.1, 6.8, 8.4,
                   6.9, 5.8, 8.1, 6.5,
                   6.4, 8.0, 5.6, 6.6,
                   7.9, 6.3, 6.5, 5.5)
)
df_fak_rbsl$Varietas   <- factor(ifelse(df_fak_rbsl$Kombinasi %in% c("A", "B"), "V1", "V2"))
df_fak_rbsl$Olah_Tanah <- factor(ifelse(df_fak_rbsl$Kombinasi %in% c("A", "C"), "Minimum", "Intensif"),
                                 levels = c("Minimum", "Intensif"))

# Verifikasi denah Bujur Sangkar Latin 4x4 (tiap kombinasi muncul tepat 1x per baris & per kolom)
print(xtabs(~ Baris + Kolom, data = df_fak_rbsl))
cat("Denah Kombinasi Faktorial pada RBSL 4x4:\n")
print(matrix(paste0(df_fak_rbsl$Varietas, "-", df_fak_rbsl$Olah_Tanah), nrow = 4, byrow = TRUE))

model_fak_rbsl <- aov(Hasil_Jagung ~ Baris + Kolom + Varietas * Olah_Tanah, data = df_fak_rbsl)
print(summary(model_fak_rbsl))

# ==============================================================================
# ANALISIS 3.5: PLOT INTERAKSI & UJI LANJUT PENGARUH SEDERHANA (SIMPLE EFFECTS)
# ==============================================================================

cat("\n=== 3.5. PLOT INTERAKSI & UJI PENGARUH SEDERHANA (EMMEANS) ===\n")

# 1. Grafik Interaksi Kasus Tomat (RAL) & Kasus Sapi (RAK)
par(mfrow = c(1, 2))
interaction.plot(
  x.factor     = df_tomat$Dosis,
  trace.factor = df_tomat$Lama,
  response     = df_tomat$Daya_Kecambah,
  type = "b", pch = c(19, 17), col = c("#2980b9", "#c0392b"), lwd = 2,
  xlab = "Dosis Fumigan (g/m3)",
  ylab = "Rata-rata Daya Kecambah (%)",
  trace.label = "Lama Fumigasi",
  main = "Interaksi Dosis x Lama (Benih Tomat)"
)

interaction.plot(
  x.factor     = df_sapi$SM,
  trace.factor = df_sapi$SS,
  response     = df_sapi$Bobot,
  type = "b", pch = c(19, 17, 15), col = c("#2980b9", "#e67e22", "#27ae60"), lwd = 2,
  xlab = "Taraf Suplemen Seng / SM (ppm)",
  ylab = "Rata-rata Pertambahan Bobot (kg/hari)",
  trace.label = "Minyak Ikan (SS)",
  main = "Interaksi SM x SS (Bobot Sapi)"
)
par(mfrow = c(1, 1))

# 2. Uji Lanjut Pengaruh Sederhana (Simple Effects): Membandingkan SM pada tiap taraf SS
emm_sm_per_ss <- emmeans(model_fak_rak, ~ SM | SS)
cat("\n--- Perbandingan Berpasangan Tukey Taraf SM di Dalam Tiap Taraf SS ---\n")
print(pairs(emm_sm_per_ss, adjust = "tukey"))

cat("\n--- Pengelompokan Huruf (CLD) Pengaruh Sederhana SM pada Tiap Taraf SS ---\n")
print(cld(emm_sm_per_ss, alpha = 0.05, Letters = letters, adjust = "tukey"))

# 3. Arah sebaliknya: Membandingkan taraf SS pada tiap taraf SM
emm_ss_per_sm <- emmeans(model_fak_rak, ~ SS | SM)
cat("\n--- Pengelompokan Huruf (CLD) Pengaruh Sederhana SS pada Tiap Taraf SM ---\n")
print(cld(emm_ss_per_sm, alpha = 0.05, Letters = letters, adjust = "tukey"))
