# ==============================================================================
# MODUL 06: ANALISIS REGRESI, DIAGNOSTIK ASUMSI, & SELEKSI PEUBAH
# Deskripsi   : 1. Regresi Linier Sederhana (OLS, ANOVA, Uji-t 2 & 1 Arah, R2, Root MSE)
#               2. Regresi Linier Berganda & Aljabar Matriks (X'X)^(-1) X'Y
#               3. Regresi Logistik Biner (glm binomial logit & Odds Ratio)
#               4. Diagnostik Visual, Uji Normalitas (Lilliefors & Khi-Kuadrat),
#                  serta Uji Kehomogenan Ragam (Bartlett & F-Max Hartley)
#               5. Deteksi Multikolinieritas (VIF), Seleksi Stepwise, serta
#                  Ridge Regression (lm.ridge & glmnet), PCR, dan PLS
# ==============================================================================

if (!dir.exists("data") && dir.exists("../data")) setwd("..")

library(nortest)
library(car)
library(MASS)
library(glmnet)
library(pls)

# ==============================================================================
# ANALISIS 6.1: REGRESI LINIER SEDERHANA (OLS)
# Studi Kasus: Konsentrasi Atrazine (ppm) vs Relative Peak Area
# ==============================================================================

cat("=== 6.1. REGRESI LINIER SEDERHANA ATRAZINE VS PEAK AREA ===\n")
df_atrazine <- read.csv("data/12_atrazine_regresi_sederhana.csv")

# 1. Estimasi Model OLS dengan lm() & Tabel ANOVA Regresi
mod_slr <- lm(Peak_Area_Y ~ Konsentrasi_X, data = df_atrazine)
print(summary(mod_slr))
print(anova(mod_slr))

# 2. Uji-t Satu Arah Kanan untuk H0: beta1 <= 0 vs H1: beta1 > 0
t_hitung    <- coef(summary(mod_slr))["Konsentrasi_X", "t value"]
db_galat    <- df.residual(mod_slr)
p_satu_arah <- pt(t_hitung, df = db_galat, lower.tail = FALSE)
cat(sprintf("Uji-t 1 Arah (H1: beta1 > 0): t_hit = %.4f | db = %d | p-value = %s\n",
            t_hitung, db_galat, format.pval(p_satu_arah)))

# 3. Selang Kepercayaan 95% bagi beta0 dan beta1
cat("\nSelang Kepercayaan 95% Parameter Regresi:\n")
print(confint(mod_slr, level = 0.95))

# 4. Plot Garis Regresi beserta Pita Kepercayaan (Confidence Band) 95%
x_baru <- data.frame(Konsentrasi_X = seq(0, 21, length.out = 100))
pita_ci <- predict(mod_slr, newdata = x_baru, interval = "confidence", level = 0.95)

plot(df_atrazine$Konsentrasi_X, df_atrazine$Peak_Area_Y,
     pch = 19, col = "#2c3e50", cex = 1.4,
     xlab = "Konsentrasi Atrazine (ppm)", ylab = "Relative Peak Area",
     main = "Garis Regresi Linier Sederhana: Y_hat = 0.7857 + 1.3635 X")
lines(x_baru$Konsentrasi_X, pita_ci[, "fit"], col = "#e74c3c", lwd = 2.2)
lines(x_baru$Konsentrasi_X, pita_ci[, "lwr"], col = "#2980b9", lty = 2)
lines(x_baru$Konsentrasi_X, pita_ci[, "upr"], col = "#2980b9", lty = 2)

# ==============================================================================
# ANALISIS 6.2: REGRESI LINIER BERGANDA & PENDUGAAN MATRIKS (X'X)^(-1) X'Y
# Studi Kasus: 10 Individu Pengamatan X1 dan X2 terhadap Respon Y
# ==============================================================================

cat("\n=== 6.2. REGRESI LINIER BERGANDA & ALJABAR MATRIKS OLS ===\n")
df_mlr <- read.csv("data/13_individu_regresi_berganda.csv")

# 1. Regresi Linier Berganda menggunakan lm()
mod_mlr <- lm(Y ~ X1 + X2, data = df_mlr)
print(summary(mod_mlr))
print(anova(mod_mlr))

# 2. Pembuktian Aljabar Matriks b = (X'X)^(-1) X'Y dan Galat Baku = sqrt(diag(KTG * (X'X)^(-1)))
X_mat   <- as.matrix(cbind(Intersep = 1, df_mlr[, c("X1", "X2")]))
Y_vec   <- as.matrix(df_mlr$Y)
XtX     <- t(X_mat) %*% X_mat
XtX_inv <- solve(XtX)
XtY     <- t(X_mat) %*% Y_vec
b_mat   <- XtX_inv %*% XtY

ktg_mlr <- sum((Y_vec - X_mat %*% b_mat)^2) / (nrow(X_mat) - ncol(X_mat))
se_mat  <- sqrt(diag(ktg_mlr * XtX_inv))

cat("\nPerbandingan Penduga Matriks Manual vs Fungsi lm():\n")
print(round(cbind(b_Matriks = as.vector(b_mat), b_lm = coef(mod_mlr),
                  SE_Matriks = se_mat, SE_lm = coef(summary(mod_mlr))[, "Std. Error"]), 5))

# ==============================================================================
# ANALISIS 6.3: REGRESI LOGISTIK BINER (RESPON Y KATEGORIK 0/1)
# Kasus: Bioassay Dosis Pestisida Nabati vs Kematian Serangga Hama (0=Hidup, 1=Mati)
# ==============================================================================

cat("\n=== 6.3. REGRESI LOGISTIK BINER (BIOASSAY MORTALITAS HAMA) ===\n")
df_log <- data.frame(
  Dosis_Pestisida = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 3, 5, 9, 13, 17),
  Mati_Biner      = c(0, 0, 0, 0,  1,  0,  1,  1,  1,  1, 0, 0, 1,  1,  1)
)

mod_logistik <- glm(Mati_Biner ~ Dosis_Pestisida, data = df_log,
                    family = binomial(link = "logit"))
print(summary(mod_logistik))

cat("\nNilai Rasio Kecenderungan (Odds Ratio = exp(b)):\n")
print(round(exp(coef(mod_logistik)), 4))

# Estimasi LD50 (Dosis yang mematikan 50% populasi, yaitu saat logit = 0 -> X = -b0 / b1)
ld50 <- -coef(mod_logistik)[1] / coef(mod_logistik)[2]
cat(sprintf("Dugaan Dosis Letal Tengah (LD50) = %.2f ml/L\n", ld50))

# Visualisasi Kurva Sigmoid Peluang Kematian Regresi Logistik
dosis_grid <- seq(0, 22, length.out = 200)
peluang_duga <- predict(mod_logistik, newdata = data.frame(Dosis_Pestisida = dosis_grid),
                        type = "response")
plot(df_log$Dosis_Pestisida, df_log$Mati_Biner,
     pch = 19, col = "#2c3e50", cex = 1.3,
     xlab = "Dosis Pestisida Nabati (ml/L)", ylab = "Peluang Kematian P(Y = 1)",
     main = "Kurva Sigmoid Regresi Logistik Biner & Titik LD50")
lines(dosis_grid, peluang_duga, col = "#e74c3c", lwd = 2.5)
abline(h = 0.5, v = ld50, col = "#2980b9", lty = 2)

# ==============================================================================
# ANALISIS 6.4: DIAGNOSTIK VISUAL, UJI NORMALITAS SISAAN, & KEHOMOGENAN RAGAM
# ==============================================================================

cat("\n=== 6.4. DIAGNOSTIK SISAAN, UJI NORMALITAS, & UJI KEHOMOGENAN RAGAM ===\n")
sisaan <- residuals(mod_mlr)
y_duga <- fitted(mod_mlr)

# 1. Diagnostik Visual: Boxplot Sisaan & Plot Sisaan vs Nilai Dugaan
par(mfrow = c(1, 2))
boxplot(sisaan, col = "#aed6f1", border = "#1b4f72",
        main = "Boxplot Sisaan Regresi", ylab = "Sisaan (e_i)")
abline(h = 0, col = "#e74c3c", lty = 2)

plot(y_duga, sisaan, pch = 19, col = "#2c3e50", cex = 1.3,
     xlab = "Nilai Dugaan (Y_hat)", ylab = "Sisaan (e_i)",
     main = "Plot Sisaan vs Nilai Dugaan")
abline(h = 0, col = "#e74c3c", lty = 2, lwd = 2)
par(mfrow = c(1, 1))

# 2. Uji Normalitas Sisaan: Uji Lilliefors & Uji Khi-Kuadrat Goodness-of-Fit
cat("\n--- Uji Normalitas Lilliefors (Kolmogorov-Smirnov Terkoreksi) ---\n")
print(lillie.test(sisaan))

cat("\n--- Uji Normalitas Khi-Kuadrat Pearson ---\n")
print(pearson.test(sisaan))

# 3. Uji Kehomogenan Ragam: Uji Bartlett & Uji F-Max Hartley
df_clover <- read.csv("data/02_red_clover_ral.csv")
df_clover$Perlakuan <- factor(df_clover$Perlakuan)

cat("\n--- Uji Kehomogenan Ragam Bartlett ---\n")
print(bartlett.test(Kadar_N ~ Perlakuan, data = df_clover))

ragam_per_grup <- tapply(df_clover$Kadar_N, df_clover$Perlakuan, var)
F_max_hartley  <- max(ragam_per_grup) / min(ragam_per_grup)
cat("Ragam per Perlakuan:\n")
print(round(ragam_per_grup, 4))
cat(sprintf("Statistik Uji F-Max Hartley = s2_max / s2_min = %.4f\n", F_max_hartley))

# ==============================================================================
# ANALISIS 6.5: DETEKSI & PENANGANAN MULTIKOLINIERITAS + SELEKSI PEUBAH
#               (VIF, Stepwise, Ridge Regression, PCR, dan PLS)
# ==============================================================================

cat("\n=== 6.5. DETEKSI & PENANGANAN MULTIKOLINIERITAS + SELEKSI PEUBAH ===\n")

# Model penuh yang mengandung peubah kolinier (X3_Kolinier) dan peubah derau (X4_Noise)
mod_penuh <- lm(Y ~ X1 + X2 + X3_Kolinier + X4_Noise, data = df_mlr)
print(summary(mod_penuh))

# 1. Deteksi Multikolinieritas: Matriks Korelasi Antar-X & Nilai VIF
cat("\n--- Matriks Korelasi Antar-Peubah Bebas ---\n")
print(round(cor(df_mlr[, c("X1", "X2", "X3_Kolinier", "X4_Noise")]), 4))

cat("\n--- Variance Inflation Factor (VIF > 10 menandakan multikolinieritas serius) ---\n")
print(round(vif(mod_penuh), 2))

# 2. Seleksi Peubah Bebas dengan Regresi Stepwise Berbasis AIC
cat("\n--- Seleksi Peubah Stepwise (Both Directions) ---\n")
mod_stepwise <- step(mod_penuh, direction = "both", trace = 1)
print(summary(mod_stepwise))

# 3A. Penanganan Multikolinieritas: Ridge Regression via MASS::lm.ridge
cat("\n--- Ridge Regression menggunakan MASS::lm.ridge ---\n")
mod_ridge <- lm.ridge(Y ~ X1 + X2 + X3_Kolinier, data = df_mlr, lambda = seq(0, 1, 0.1))
select(mod_ridge)
print(round(coef(mod_ridge), 4))

# 3B. Penanganan Multikolinieritas: Ridge Regression via glmnet (alpha = 0)
cat("\n--- Ridge Regression menggunakan glmnet (alpha = 0) ---\n")
set.seed(512)
X_ridge <- as.matrix(df_mlr[, c("X1", "X2", "X3_Kolinier")])
Y_ridge <- df_mlr$Y
cv_ridge <- cv.glmnet(X_ridge, Y_ridge, alpha = 0, nfolds = 5, grouped = FALSE)
cat(sprintf("Lambda Optimum CV glmnet = %.4f\n", cv_ridge$lambda.min))
print(round(as.matrix(coef(cv_ridge, s = "lambda.min")), 4))

# 4. Principal Component Regression / PCR
cat("\n--- Principal Component Regression (PCR - pls::pcr) ---\n")
mod_pcr <- pcr(Y ~ X1 + X2 + X3_Kolinier, data = df_mlr, scale = TRUE, validation = "LOO")
summary(mod_pcr)
cat("\nKoefisien Regresi PCR (pada 2 Komponen Utama):\n")
print(round(coef(mod_pcr, ncomp = 2, intercept = TRUE), 4))

# 5. Partial Least Squares / PLS
cat("\n--- Partial Least Squares Regression (PLS - pls::plsr) ---\n")
mod_pls <- plsr(Y ~ X1 + X2 + X3_Kolinier, data = df_mlr, scale = TRUE, validation = "LOO")
summary(mod_pls)
cat("\nKoefisien Regresi PLS (pada 2 Komponen Laten):\n")
print(round(coef(mod_pls, ncomp = 2, intercept = TRUE), 4))
