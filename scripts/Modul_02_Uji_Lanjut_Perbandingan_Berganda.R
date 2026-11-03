# ==============================================================================
# MODUL 02: PERBANDINGAN NILAI TENGAH BERGANDA (UJI LANJUT)
# Deskripsi   : A. Uji Lanjut Tak Terencana (Post-Hoc):
#                  1. BNT / Fisher's LSD (Ulangan Sama & Tidak Sama)
#                  2. BNJ / Tukey's HSD
#                  3. Uji Jarak Berganda Duncan (DMRT)
#                  4. SNK (Student-Newman-Keuls), Scheffe, dan Dunnett
#               B. Uji Lanjut Terencana (A Priori):
#                  5. Kontras Ortogonal (Ulangan Sama & Tidak Sama)
#                  6. Polinomial Ortogonal (Linier, Kuadratik, Kubik, Kuartik)
# ==============================================================================

if (!dir.exists("data") && dir.exists("../data")) setwd("..")

library(agricolae)
library(multcomp)

# Memuat dataset Red Clover (RAL: 6 Strain Rhizobium x 5 Ulangan) untuk Analisis 2.1-2.4
df_clover <- read.csv("data/02_red_clover_ral.csv")
df_clover$Perlakuan <- factor(
  df_clover$Perlakuan,
  levels = c("3DOK1", "3DOK5", "3DOK4", "3DOK7", "3DOK13", "Komposit")
)
model_ral <- aov(Kadar_N ~ Perlakuan, data = df_clover)

# ==============================================================================
# ANALISIS 2.1: UJI BEDA NYATA TERKECIL (BNT / FISHER'S LSD)
# ==============================================================================

# 1. Uji BNT (LSD) Ulangan Sama (r = 5 pada setiap strain)
cat("=== 2.1A. UJI BNT (LSD) ULANGAN SAMA ===\n")
uji_bnt <- LSD.test(model_ral, "Perlakuan", alpha = 0.05, console = TRUE)
plot(uji_bnt, variation = "SD", main = "Pengelompokan Huruf Uji BNT (LSD) Kadar Nitrogen Red Clover")

# 2. Uji BNT (LSD) Ulangan Tidak Sama (simulasi 2 pot rusak/hilang pada baris ke-1 dan ke-7)
cat("\n=== 2.1B. UJI BNT (LSD) ULANGAN TIDAK SAMA ===\n")
df_clover_unequal <- df_clover[-c(1, 7), ]
print(table(df_clover_unequal$Perlakuan)) # 3DOK1 (r=4), 3DOK5 (r=4), lainnya (r=5)

model_ral_unequal <- aov(Kadar_N ~ Perlakuan, data = df_clover_unequal)
uji_bnt_unequal   <- LSD.test(model_ral_unequal, "Perlakuan", alpha = 0.05, console = TRUE)

# Perhitungan manual nilai kritis BNT untuk pasangan r=4 vs r=5 dan r=4 vs r=4
ktg_un <- summary(model_ral_unequal)[[1]]["Residuals", "Mean Sq"]
dbg_un <- df.residual(model_ral_unequal)
t_kris <- qt(0.975, df = dbg_un)
bnt_4_5 <- t_kris * sqrt(ktg_un * (1/4 + 1/5))
bnt_4_4 <- t_kris * sqrt(ktg_un * (1/4 + 1/4))
bnt_5_5 <- t_kris * sqrt(ktg_un * (1/5 + 1/5))
cat(sprintf("BNT(r=4 vs r=4) = %.2f | BNT(r=4 vs r=5) = %.2f | BNT(r=5 vs r=5) = %.2f\n",
            bnt_4_4, bnt_4_5, bnt_5_5))

# ==============================================================================
# ANALISIS 2.2: UJI BEDA NYATA JUJUR (BNJ / TUKEY'S HSD)
# ==============================================================================

cat("\n=== 2.2. UJI BEDA NYATA JUJUR (BNJ / TUKEY HSD) ===\n")
# Cara 1: Selang kepercayaan 95% selisih seluruh pasangan rata-rata (fungsi bawaan R)
tukey_ci <- TukeyHSD(model_ral, "Perlakuan", conf.level = 0.95)
print(tukey_ci)
plot(tukey_ci, las = 1, col = "#2980b9")

# Cara 2: Pengelompokan huruf notasi & nilai kritis tunggal HSD (agricolae)
uji_bnj <- HSD.test(model_ral, "Perlakuan", alpha = 0.05, console = TRUE)

# ==============================================================================
# ANALISIS 2.3: UJI JARAK BERGANDA DUNCAN (DMRT)
# ==============================================================================

cat("\n=== 2.3. UJI JARAK BERGANDA DUNCAN (DMRT) ===\n")
uji_duncan <- duncan.test(model_ral, "Perlakuan", alpha = 0.05, console = TRUE)
plot(uji_duncan, variation = "SD", main = "Pengelompokan Huruf Uji Duncan (DMRT) Red Clover")

# ==============================================================================
# ANALISIS 2.4: UJI SNK, UJI SCHEFFE, DAN UJI DUNNETT (VS KONTROL)
# ==============================================================================

cat("\n=== 2.4A. UJI STUDENT-NEWMAN-KEULS (SNK) ===\n")
uji_snk <- SNK.test(model_ral, "Perlakuan", alpha = 0.05, console = TRUE)

cat("\n=== 2.4B. UJI SCHEFFE ===\n")
uji_scheffe <- scheffe.test(model_ral, "Perlakuan", alpha = 0.05, console = TRUE)

cat("\n=== 2.4C. UJI DUNNETT (MEMBANDINGKAN TIAP STRAIN TERHADAP KONTROL 'KOMPOSIT') ===\n")
df_clover$Perlakuan_Dunnett <- relevel(df_clover$Perlakuan, ref = "Komposit")
model_dunnett <- aov(Kadar_N ~ Perlakuan_Dunnett, data = df_clover)
uji_dunnett   <- glht(model_dunnett, linfct = mcp(Perlakuan_Dunnett = "Dunnett"))
print(summary(uji_dunnett))
print(confint(uji_dunnett, level = 0.95))

# ==============================================================================
# ANALISIS 2.5: UJI TERENCANA — METODE KONTRAS ORTOGONAL
# Kasus: 6 Pabrik Feed Supplement Ayam (A,B = Lokal; C,D = Eropa; E,F = AS)
# ==============================================================================

df_ayam <- read.csv("data/03_ayam_kontras_ral.csv")
df_ayam$Pabrik <- factor(df_ayam$Pabrik, levels = c("A", "B", "C", "D", "E", "F"))

# --- 2.5A. Kontras Ortogonal Ulangan Sama (r = 4 untuk semua pabrik) ---
cat("\n=== 2.5A. KONTRAS ORTOGONAL ULANGAN SAMA (r = 4) ===\n")
mat_kontras <- cbind(
  "Lokal vs Impor" = c( 2,  2, -1, -1, -1, -1),
  "Eropa vs AS"    = c( 0,  0,  1,  1, -1, -1),
  "Pabrik A vs B"  = c( 1, -1,  0,  0,  0,  0),
  "Pabrik C vs D"  = c( 0,  0,  1, -1,  0,  0),
  "Pabrik E vs F"  = c( 0,  0,  0,  0,  1, -1)
)

# Verifikasi syarat jumlah koefisien = 0 dan perkalian silang = 0 (matriks diagonal)
cat("Jumlah koefisien tiap kontras:\n")
print(colSums(mat_kontras))
cat("Matriks t(C) %*% C (luar diagonal wajib 0):\n")
print(t(mat_kontras) %*% mat_kontras)

# Memasang kontras ke faktor Pabrik & memecah JK Perlakuan menjadi 5 komponen ber-db 1
contrasts(df_ayam$Pabrik) <- mat_kontras
model_ayam <- aov(Bobot_Akhir ~ Pabrik, data = df_ayam)

ringkasan_kontras <- summary.aov(model_ayam, split = list(Pabrik = list(
  "C1: Lokal (A,B) vs Impor (C,D,E,F)" = 1,
  "C2: Impor Eropa (C,D) vs AS (E,F)"  = 2,
  "C3: Lokal Pabrik A vs Pabrik B"     = 3,
  "C4: Eropa Pabrik C vs Pabrik D"     = 4,
  "C5: AS Pabrik E vs Pabrik F"        = 5
)))
print(ringkasan_kontras)

# --- 2.5B. Kontras Ortogonal Ulangan Tidak Sama (r_i berbeda) ---
cat("\n=== 2.5B. KONTRAS ORTOGONAL ULANGAN TIDAK SAMA ===\n")
# Misalkan 1 ayam pada Pabrik A (baris 1) dan 1 ayam pada Pabrik D (baris 13) mati/hilang
df_ayam_un <- df_ayam[-c(1, 13), ]
df_ayam_un$Pabrik <- factor(df_ayam_un$Pabrik, levels = c("A", "B", "C", "D", "E", "F"))
r_vec <- as.numeric(table(df_ayam_un$Pabrik)) # r = (3, 4, 4, 3, 4, 4)
cat("Jumlah ulangan per pabrik (A..F):", r_vec, "\n")

# Menyusun matriks bobot k_i yang memenuhi sum(r_i * k_i) = 0 dan sum(r_i * k_1i * k_2i) = 0
# Total ulangan: Lokal (A,B) = 3+4 = 7; Impor (C,D,E,F) = 4+3+4+4 = 15
# Total ulangan: Eropa (C,D) = 4+3 = 7; AS (E,F) = 4+4 = 8
mat_kontras_un <- cbind(
  "Lokal vs Impor" = c(15, 15, -7, -7, -7, -7),
  "Eropa vs AS"    = c( 0,  0,  8,  8, -7, -7),
  "Pabrik A vs B"  = c( 4, -3,  0,  0,  0,  0),
  "Pabrik C vs D"  = c( 0,  0,  3, -4,  0,  0),
  "Pabrik E vs F"  = c( 0,  0,  0,  0,  1, -1)
)

cat("Cek syarat sum(r_i * k_i) = 0:\n")
print(colSums(r_vec * mat_kontras_un))
cat("Cek keortogonalan berbobot t(K) %*% diag(r) %*% K:\n")
print(t(mat_kontras_un) %*% diag(r_vec) %*% mat_kontras_un)

contrasts(df_ayam_un$Pabrik) <- mat_kontras_un
model_ayam_un <- aov(Bobot_Akhir ~ Pabrik, data = df_ayam_un)
print(summary.aov(model_ayam_un, split = list(Pabrik = list(
  "C1: Lokal vs Impor (Tak Seimbang)" = 1,
  "C2: Eropa vs AS (Tak Seimbang)"    = 2,
  "C3: Pabrik A vs B (r=3 vs r=4)"    = 3,
  "C4: Pabrik C vs D (r=4 vs r=3)"    = 4,
  "C5: Pabrik E vs F (r=4 vs r=4)"    = 5
))))

# ==============================================================================
# ANALISIS 2.6: UJI TERENCANA — METODE POLINOMIAL ORTOGONAL
# Kasus: 5 Jarak Tanam Kedelai Ottawa Mandarin (18, 24, 30, 36, 42 inci) x 6 Blok
# ==============================================================================

cat("\n=== 2.6. POLINOMIAL ORTOGONAL JARAK TANAM KEDELAI (RAK) ===\n")
df_kedelai <- read.csv("data/04_kedelai_polinomial_rak.csv")
df_kedelai$Jarak_Faktor <- factor(df_kedelai$Jarak_Inci)
df_kedelai$Kelompok     <- factor(df_kedelai$Kelompok)

# Menampilkan matriks koefisien polinomial ortogonal untuk 5 taraf berjarak sama
print(round(contr.poly(5), 4))

# Memasang polinomial ortogonal dan memecah JK Jarak Tanam menjadi 4 derajat tren
contrasts(df_kedelai$Jarak_Faktor) <- contr.poly(5)
model_poly <- aov(Hasil ~ Kelompok + Jarak_Faktor, data = df_kedelai)

print(summary.aov(model_poly, split = list(Jarak_Faktor = list(
  "Linier"    = 1,
  "Kuadratik" = 2,
  "Kubik"     = 3,
  "Kuartik"   = 4
))))

# Estimasi persamaan respons linier (karena tren Linier sangat nyata)
mod_tren_linier <- lm(Hasil ~ Jarak_Inci, data = df_kedelai)
cat("\nPersamaan Respons Linier Hasil Kedelai terhadap Jarak Tanam:\n")
print(coef(mod_tren_linier))

# Visualisasi Kurva Tren Respons Linier vs Kuadratik terhadap Titik Rata-rata
rataan_jarak <- aggregate(Hasil ~ Jarak_Inci, data = df_kedelai, mean)
jarak_halus  <- seq(18, 42, length.out = 100)

plot(rataan_jarak$Jarak_Inci, rataan_jarak$Hasil,
     pch = 19, col = "#2c3e50", cex = 1.5, ylim = c(27, 36.5),
     xlab = "Jarak Tanam Kedelai (Inci)",
     ylab = "Rata-rata Hasil Kedelai (bushels/acre)",
     main = "Kurva Tren Polinomial Hasil Kedelai Ottawa Mandarin vs Jarak Tanam")

# Garis tren Linier (nyata) dan kurva Kuadratik (pembanding)
lines(jarak_halus,
      predict(lm(Hasil ~ Jarak_Inci, data = df_kedelai),
              newdata = data.frame(Jarak_Inci = jarak_halus)),
      col = "#2980b9", lwd = 2.5)
lines(jarak_halus,
      predict(lm(Hasil ~ poly(Jarak_Inci, 2), data = df_kedelai),
              newdata = data.frame(Jarak_Inci = jarak_halus)),
      col = "#e74c3c", lwd = 2, lty = 2)
legend("topright",
       legend = c("Rata-rata Aktual Perlakuan", "Tren Linier (p < 0.001 ***)", "Tren Kuadratik (p = 0.0757 ns)"),
       pch = c(19, NA, NA), lty = c(NA, 1, 2), lwd = c(NA, 2.5, 2),
       col = c("#2c3e50", "#2980b9", "#e74c3c"), bty = "n")
