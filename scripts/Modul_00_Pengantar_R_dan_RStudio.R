# ==============================================================================
# MODUL 00: PENGANTAR R DAN RSTUDIO UNTUK PEMULA
# Deskripsi   : Panduan dasar pemrograman R dari nol: fungsi, paket, direktori
#               kerja, operator dasar, tipe data, struktur data, dan impor data.
# ==============================================================================

# Mengatur lokasi folder data secara otomatis (baik dijalankan dari root proyek maupun folder scripts/)
if (!dir.exists("data") && dir.exists("../data")) setwd("..")

# ==============================================================================
# BAGIAN 1: PENGENALAN KOMENTAR, FUNGSI DASAR, DAN BANTUAN (HELP)
# ==============================================================================

# Teks setelah tanda pagar (#) adalah komentar dan tidak dieksekusi oleh R.
# Tekan Ctrl + Enter (Windows/Linux) atau Cmd + Return (macOS) untuk menjalankan baris kode.

cat("Selamat datang di Panduan Analisis Statistik & Komputasi R!\n")

# Contoh penggunaan fungsi matematika & statistika bawaan R
angka_contoh <- c(10, 15, 20, 25, 30)
mean(angka_contoh)   # Menghitung nilai rata-rata (mean)
sd(angka_contoh)     # Menghitung simpangan baku (standard deviation)
sum(angka_contoh)    # Menghitung jumlah total
sqrt(144)            # Menghitung akar kuadrat

# Membuka dokumentasi bantuan fungsi di Panel Help (hilangkan tanda # saat di RStudio)
# ?aov
# ?lm
# help(read.csv)

# ==============================================================================
# BAGIAN 2: PAKET (PACKAGES) DAN DIREKTORI KERJA (WORKING DIRECTORY)
# ==============================================================================

# 1. Instalasi paket: CUKUP DIJALANKAN 1 KALI saat paket belum terpasang di komputer
# install.packages("agricolae")
# install.packages("readxl")

# 2. Memuat paket: DIJALANKAN SETIAP KALI membuka sesi RStudio baru
library(agricolae)

# 3. Mengecek lokasi direktori kerja aktif saat ini
getwd()

# Contoh mengubah direktori kerja (gunakan garis miring '/', BUKAN backslash '\')
# setwd("D:/Analisis_Statistik_R")

# ==============================================================================
# BAGIAN 3: OPERATOR DASAR PADA R
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Operator Penugasan (Assignment)
# ------------------------------------------------------------------------------
hasil_panen <- 125.5   # Standar penugasan yang paling direkomendasikan di R
jumlah_petak = 24      # Alternatif penugasan dengan tanda sama dengan
hasil_panen
jumlah_petak

# ------------------------------------------------------------------------------
# 3.2 Operator Aritmatika
# ------------------------------------------------------------------------------
6 + 4     # Penjumlahan (Addition) -> 10
6 - 4     # Pengurangan (Subtraction) -> 2
6 * 4     # Perkalian (Multiplication) -> 24
6 / 4     # Pembagian (Division) -> 1.5
6 ^ 4     # Pemangkatan (Exponentiation) -> 1296
6 %% 4    # Modulo / sisa pembagian -> 2
6 %/% 4   # Pembagian bilangan bulat (Integer division) -> 1

# Penggunaan tanda kurung untuk mengatur urutan prioritas perhitungan
(6 + 4) * 2
6 + 4 * 2

# ------------------------------------------------------------------------------
# 3.3 Operator Perbandingan (Relasional)
# ------------------------------------------------------------------------------
a <- 25
b <- 27

a == b    # Sama dengan (FALSE)
a != b    # Tidak sama dengan (TRUE)
a > b     # Lebih besar dari (FALSE)
a < b     # Lebih kecil dari (TRUE)
a >= b    # Lebih besar atau sama dengan (FALSE)
a <= b    # Lebih kecil atau sama dengan (TRUE)

# ------------------------------------------------------------------------------
# 3.4 Operator Logika
# ------------------------------------------------------------------------------
v <- c(TRUE, TRUE, FALSE)
t_log <- c(FALSE, TRUE, FALSE)

# Operator logika per elemen (element-wise: memeriksa seluruh pasangan elemen vektor)
print(v & t_log)       # DAN (AND) per elemen
print(v | t_log)       # ATAU (OR) per elemen
print(!v)              # TIDAK / Negasi (NOT)

# Operator logika tunggal (memeriksa satu nilai skalar / elemen pertama)
print(v[1] && t_log[1])
print(v[1] || t_log[1])

# ==============================================================================
# BAGIAN 4: TIPE DATA DASAR DI R
# ==============================================================================

# 1. Numeric (Angka desimal / bilangan bulat)
bobot_gabah <- 5.82
class(bobot_gabah)

# 2. Character (Teks / string di dalam tanda kutip)
nama_varietas <- "Padi S-969"
class(nama_varietas)

# 3. Logical (Nilai kebenaran TRUE atau FALSE)
lolos_uji_benih <- TRUE
class(lolos_uji_benih)

# 4. Factor (Peubah kategorik / taraf perlakuan / kelompok pada ANOVA)
dosis <- c("50gr", "75gr", "100gr", "50gr", "75gr", "100gr", "50gr", "75gr", "100gr", "50gr")

# Mengatur urutan level faktor secara eksplisit
faktor_dosis <- factor(dosis, levels = c("50gr", "75gr", "100gr"))
faktor_dosis
class(faktor_dosis)

# Faktor terurut (ordered factor) untuk skala bertingkat/ordinal
faktor_dosis_urut <- factor(dosis, levels = c("50gr", "75gr", "100gr"), ordered = TRUE)
faktor_dosis_urut

# ==============================================================================
# BAGIAN 5: LIMA STRUKTUR DATA UTAMA DI R
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.1 Vector (Vektor 1 Dimensi)
# ------------------------------------------------------------------------------
kebun  <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
divisi <- c(rep("div 1", 3), rep("div 2", 3), rep("div 3", 4))
panen  <- c(100, 120, 110, 105, 135, 125, 130, 115, 140, 145)

# Membuat deret angka dan pengulangan dengan cepat
urut_1_10 <- 1:10
deret_loncat <- seq(from = 0, to = 64, by = 16)
ulang_angka  <- rep(1:4, each = 3)

urut_1_10
deret_loncat
ulang_angka

# Mengakses elemen vektor dengan indeks kurung siku [ ]
panen[1]          # Elemen pertama
panen[c(1, 2, 5)] # Elemen ke-1, ke-2, dan ke-5
length(panen)     # Banyaknya elemen vektor
class(panen)      # Tipe data vektor

# ------------------------------------------------------------------------------
# 5.2 Matrix (Matriks 2 Dimensi: Baris x Kolom dengan tipe data seragam)
# ------------------------------------------------------------------------------
# Pengisian default per kolom (byrow = FALSE)
mat_kolom <- matrix(1:9, nrow = 3, ncol = 3)
mat_kolom

# Pengisian per baris dari kiri ke kanan (byrow = TRUE)
mat_baris <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
mat_baris

# Membentuk matriks dengan dim(), cbind() (gabung kolom), dan rbind() (gabung baris)
x <- 1:4
y <- 10:13
m <- 1:10
dim(m) <- c(2, 5)
m

gabung_kolom <- cbind(x, y)
gabung_kolom

gabung_baris <- rbind(x, y)
gabung_baris

# ------------------------------------------------------------------------------
# 5.3 Array (Struktur Multidimensi > 2 Dimensi)
# ------------------------------------------------------------------------------
v1  <- c(5, 10, 15, 20)
v2  <- c(25, 30, 35, 40, 45, 50, 55, 60)
arr <- array(c(v1, v2), dim = c(4, 3, 2))
arr
dim(arr)

# ------------------------------------------------------------------------------
# 5.4 Data Frame (Tabel Data Pengamatan: Standar Utama ANOVA & Regresi)
# ------------------------------------------------------------------------------
datapanen <- data.frame(
  kebun  = kebun,
  divisi = factor(divisi),
  panen  = panen
)
datapanen

# Mengakses kolom pada data frame dengan tanda '$'
datapanen$panen
mean(datapanen$panen)

# Menyaring baris dengan kondisi tertentu (misal panen > 120)
subset(datapanen, panen > 120)

# ------------------------------------------------------------------------------
# 5.5 List (Kumpulan Objek dengan Berbagai Tipe & Struktur Berbeda)
# ------------------------------------------------------------------------------
objek_teks   <- "Analisis Percobaan Pertanian"
objek_vektor <- 1:10
objek_mat    <- matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE)

contoh_list <- list(
  Judul   = objek_teks,
  Urutan  = objek_vektor,
  Matriks = objek_mat,
  Tabel   = datapanen
)
contoh_list
contoh_list$Tabel

# ==============================================================================
# BAGIAN 6: MENGIMPOR DATA KE R & PEMERIKSAAN STRUKTUR DATA PERCOBAAN
# ==============================================================================

# ------------------------------------------------------------------------------
# 6.1 Membaca File CSV (Format Standar Dataset Penelitian)
# ------------------------------------------------------------------------------
df_padi <- read.csv("data/01_padi_rak_s969.csv", sep = ",")

# Memeriksa 6 baris pertama dan 6 baris terakhir
head(df_padi)
tail(df_padi)

# Memeriksa dimensi (jumlah baris & kolom) dan struktur tipe data awal
dim(df_padi)
str(df_padi)

# PENTING UNTUK ANOVA: Mengubah kolom Perlakuan & Kelompok menjadi Factor!
df_padi$Perlakuan <- factor(df_padi$Perlakuan,
                            levels = c("Kontrol", "PK", "N", "NP", "NK", "NPK"))
df_padi$Kelompok  <- factor(df_padi$Kelompok,
                            levels = c("I", "II", "III", "IV"))

# Verifikasi kembali struktur data dan ringkasan statistik 5 serangkai
str(df_padi)
summary(df_padi)

# ------------------------------------------------------------------------------
# 6.2 Template Membaca File Excel (.xls / .xlsx) dan Teks (.txt)
# ------------------------------------------------------------------------------
# Pastikan paket readxl sudah terinstal untuk membaca file Excel:
# library(readxl)
# data_excel <- readxl::read_xlsx("data/NamaFileData.xlsx", sheet = 1)

# Membaca file teks berpemisah tab (.txt):
# data_txt <- read.delim("data/NamaFileData.txt", sep = "\t")

# ------------------------------------------------------------------------------
# 6.3 Visualisasi Eksplorasi Pertama dari Dataset yang Diimpor
# ------------------------------------------------------------------------------
boxplot(Hasil_Gabah ~ Perlakuan, data = df_padi,
        col  = c("#d5dbdb", "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1"),
        main = "Sebaran Hasil Gabah Padi S-969 per Perlakuan Pupuk NPK",
        xlab = "Perlakuan Kombinasi Pupuk",
        ylab = "Hasil Gabah (ton/ha)")
