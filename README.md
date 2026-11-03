<div align="center">

# Panduan Komprehensif Analisis Statistik & Komputasi R
### *Interactive Statistical Analysis & Design of Experiments in R*

[![R Version](https://img.shields.io/badge/R-%3E%3D%204.0.0-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://cran.r-project.org/)
[![R Markdown](https://img.shields.io/badge/R%20Markdown-Rmd%20%26%20Bookdown-75AADB?style=for-the-badge&logo=rstudio&logoColor=white)](https://rmarkdown.rstudio.com/)
[![HTML Reports](https://img.shields.io/badge/HTML%20Reports-Robobook%20%2F%20Gitbook-2980B9?style=for-the-badge&logo=html5&logoColor=white)](HTML/index.html)
[![Status](https://img.shields.io/badge/Status-Complete%20%2F%20Active-27AE60?style=for-the-badge)](#daftar-lengkap-bagian--cakupan-analisis)
[![License: MIT](https://img.shields.io/badge/License-MIT-F39C12?style=for-the-badge)](#lisensi)

</div>

---

Repositori ini merupakan dokumentasi interaktif dan portofolio komputasi statistik terapan menggunakan bahasa pemrograman **R** yang disusun secara sistematis dari tingkat dasar hingga pemodelan lanjut. Cakupan pembahasannya meliputi fondasi pemrograman R & RStudio, perancangan percobaan faktor tunggal (RAL & RAK), uji lanjut perbandingan berganda (*post-hoc* dan kontras/polinomial terencana), percobaan faktorial multifaktor, rancangan bertingkat (*split-plot* & *strip-plot*), analisis asosiasi & korelasi, hingga analisis regresi, uji diagnostik asumsi klasik, serta penanganan multikolinieritas. Seluruh materi dirancang sepenuhnya *reproducible* melalui perpaduan skrip R murni (`.R`), dokumen sumber *R Markdown* (`.Rmd`), 13 dataset studi kasus (`.csv`), serta laporan web interaktif mandiri (`.html`) bergaya *Gitbook/Robobook*.

---

## Sorotan Utama Proyek

- **Alur Metodologis Utuh di Setiap Analisis**: Setiap topik menyajikan penjelasan konsep & kriteria pemilihan metode, formulasi model linier aditif beserta pasangan hipotesis formal, implementasi kode R langkah demi langkah, visualisasi data, hingga panduan interpretasi keluaran dan perumusan kesimpulan teknis.
- **Tiga Format Dokumen Terintegrasi**: Tersedia dalam bentuk skrip eksekusi langsung di folder [`scripts/`](scripts/), naskah sumber terstruktur di folder [`Rmd/`](Rmd/), serta dokumen web mandiri siap baca di folder [`HTML/`](HTML/).
- **100% *Reproducible Pipeline***: Seluruh 13 dataset studi kasus dan 8 berkas keluaran HTML (termasuk portal utama [`HTML/index.html`](HTML/index.html)) dapat dibangun ulang dari awal hanya dengan satu perintah eksekusi.
- **Antarmuka Bacaan Modern (*Robobook / Gitbook*)**: Laporan HTML dilengkapi navigasi daftar isi di sisi kiri, tipografi bersih (*Roboto Slab*, *Roboto*, & *Fira Code*), fitur lipat kode (*code folding*), serta tampilan pembesar grafik (*lightbox*).

---

## Struktur Direktori Proyek

```text
.
├── data/                  # 13 dataset studi kasus (.csv)
├── assets/                # File styling kustom CSS (style_robobook.css)
├── scripts/               # Script R murni (.R) siap jalan per bagian
├── Rmd/                   # Dokumen R Markdown (.Rmd) sumber laporan
├── HTML/                  # Laporan web interaktif mandiri (.html)
├── 00_generate_datasets.R # Generator otomatis 13 dataset studi kasus
├── 99_render_all_html.R   # Script otomasi kompilasi Rmd ke HTML
└── README.md              # Dokumentasi utama proyek
```

<details>
<summary><strong>Lihat rincian lengkap seluruh berkas di dalam repositori</strong></summary>

```text
.
├── data/
│   ├── 01_padi_rak_s969.csv
│   ├── 02_red_clover_ral.csv
│   ├── 03_ayam_kontras_ral.csv
│   ├── 04_kedelai_polinomial_rak.csv
│   ├── 05_tomat_faktorial_ral.csv
│   ├── 06_sapi_faktorial_rak.csv
│   ├── 07_debu_splitplot_ral.csv
│   ├── 08_gandum_splitplot_rak.csv
│   ├── 09_padi_stripplot_rak.csv
│   ├── 10_emisi_hc_korelasi.csv
│   ├── 11_jangkrik_suhu.csv
│   ├── 12_atrazine_regresi_sederhana.csv
│   └── 13_individu_regresi_berganda.csv
├── assets/
│   └── style_robobook.css
├── scripts/
│   ├── Modul_00_Pengantar_R_dan_RStudio.R
│   ├── Modul_01_Rancangan_Satu_Faktor_RAL_dan_RAK.R
│   ├── Modul_02_Uji_Lanjut_Perbandingan_Berganda.R
│   ├── Modul_03_Percobaan_Faktorial_Multifaktor.R
│   ├── Modul_04_Rancangan_Split_Plot_dan_Strip_Plot.R
│   ├── Modul_05_Eksplorasi_dan_Analisis_Korelasi.R
│   └── Modul_06_Analisis_Regresi_Diagnostik_dan_Seleksi_Peubah.R
├── Rmd/
│   ├── Modul_00_Pengantar_R_dan_RStudio.Rmd
│   ├── Modul_01_Rancangan_Satu_Faktor_RAL_dan_RAK.Rmd
│   ├── Modul_02_Uji_Lanjut_Perbandingan_Berganda.Rmd
│   ├── Modul_03_Percobaan_Faktorial_Multifaktor.Rmd
│   ├── Modul_04_Rancangan_Split_Plot_dan_Strip_Plot.Rmd
│   ├── Modul_05_Eksplorasi_dan_Analisis_Korelasi.Rmd
│   └── Modul_06_Analisis_Regresi_Diagnostik_dan_Seleksi_Peubah.Rmd
├── HTML/
│   ├── index.html
│   ├── Modul_00_Pengantar_R_dan_RStudio.html
│   ├── Modul_01_Rancangan_Satu_Faktor_RAL_dan_RAK.html
│   ├── Modul_02_Uji_Lanjut_Perbandingan_Berganda.html
│   ├── Modul_03_Percobaan_Faktorial_Multifaktor.html
│   ├── Modul_04_Rancangan_Split_Plot_dan_Strip_Plot.html
│   ├── Modul_05_Eksplorasi_dan_Analisis_Korelasi.html
│   └── Modul_06_Analisis_Regresi_Diagnostik_dan_Seleksi_Peubah.html
├── 00_generate_datasets.R
├── 99_render_all_html.R
└── README.md
```

</details>

---

## Daftar Lengkap Bagian & Cakupan Analisis

Tabel berikut merangkum ketujuh bagian panduan beserta metode statistik yang dibahas, fungsi R utama, serta tautan langsung menuju berkas skrip `.R`, sumber `.Rmd`, dan laporan `.html`:

| Bagian | Topik Utama | Cakupan Metode & Analisis Statistik | Fungsi & Paket R Utama | Berkas Sumber & Laporan |
| :---: | :--- | :--- | :--- | :---: |
| **Bagian 00** | **Pengantar R & RStudio untuk Pemula** | Pengenalan antarmuka 4 panel RStudio, manajemen direktori kerja & pustaka, operator aritmetika/relasional/logika, tipe data dasar, struktur objek R (`vector`, `matrix`, `data.frame`, `factor`, `list`), serta impor berkas `.csv` dan Excel (`.xlsx`). | `c()`, `matrix()`, `data.frame()`, `factor()`, `list()`, `read.csv()`, `readxl::read_excel()` | [`.R`](scripts/Modul_00_Pengantar_R_dan_RStudio.R) · [`.Rmd`](Rmd/Modul_00_Pengantar_R_dan_RStudio.Rmd) · [`.html`](HTML/Modul_00_Pengantar_R_dan_RStudio.html) |
| **Bagian 01** | **Rancangan Satu Faktor (RAL & RAK)** | Pengacakan denah percobaan *Completely Randomized Design* (CRD/RAL) & *Randomized Complete Block Design* (RCBD/RAK), ANOVA Satu Arah (RAL), ANOVA Dua Arah Tanpa Interaksi (RAK), Koefisien Keragaman (KK/CV), serta Efisiensi Relatif pemblokan. | `agricolae::design.crd()`, `agricolae::design.rcbd()`, `aov()`, `summary()`, `agricolae::cv.model()` | [`.R`](scripts/Modul_01_Rancangan_Satu_Faktor_RAL_dan_RAK.R) · [`.Rmd`](Rmd/Modul_01_Rancangan_Satu_Faktor_RAL_dan_RAK.Rmd) · [`.html`](HTML/Modul_01_Rancangan_Satu_Faktor_RAL_dan_RAK.html) |
| **Bagian 02** | **Perbandingan Berganda / Uji Lanjut** | Uji lanjut *post-hoc*: BNT (*Fisher's LSD*) ulangan sama & tidak sama, BNJ (*Tukey's HSD*), Duncan (DMRT), *Student-Newman-Keuls* (SNK), Dunnett terhadap Kontrol, dan Scheffé; Uji terencana (*a priori*): Kontras Ortogonal & Polinomial Ortogonal (Linier, Kuadratik, Kubik, Kuartik). | `agricolae::LSD.test()`, `TukeyHSD()`, `HSD.test()`, `duncan.test()`, `SNK.test()`, `scheffe.test()`, `multcomp::glht()`, `contrasts()`, `contr.poly()` | [`.R`](scripts/Modul_02_Uji_Lanjut_Perbandingan_Berganda.R) · [`.Rmd`](Rmd/Modul_02_Uji_Lanjut_Perbandingan_Berganda.Rmd) · [`.html`](HTML/Modul_02_Uji_Lanjut_Perbandingan_Berganda.html) |
| **Bagian 03** | **Percobaan Faktorial (Multifaktor)** | Pengacakan kombinasi perlakuan faktorial dalam RAL, RAK, & RBSL (*Latin Square*), ANOVA Faktorial 2 Faktor dalam RAL, RAK, & RBSL, diagnostik visual Plot Interaksi, serta Uji Pengaruh Sederhana (*Simple Effects*) saat interaksi nyata. | `agricolae::design.ab()`, `aov(Y ~ A * B)`, `aov(Y ~ Blok + A * B)`, `interaction.plot()`, `emmeans::emmeans()`, `pairs()`, `multcomp::cld()` | [`.R`](scripts/Modul_03_Percobaan_Faktorial_Multifaktor.R) · [`.Rmd`](Rmd/Modul_03_Percobaan_Faktorial_Multifaktor.Rmd) · [`.html`](HTML/Modul_03_Percobaan_Faktorial_Multifaktor.html) |
| **Bagian 04** | **Rancangan Split-Plot & Strip-Plot** | Pengacakan dua tahap & pemodelan *Split-Plot* dalam RAL dan RAK dengan pemisahan strata galat (`Error()`), *Strip-Plot* (*Split-Block*) dalam RAK dengan 3 lapis galat, perhitungan Koefisien Keragaman bertingkat (`KKa`, `KKb`, `KKc`), serta uji lanjut *LSMeans*. | `agricolae::design.split()`, `design.strip()`, `aov(... + Error(...))`, `agricolae::sp.plot()`, `strip.plot()`, `emmeans::emmeans()` | [`.R`](scripts/Modul_04_Rancangan_Split_Plot_dan_Strip_Plot.R) · [`.Rmd`](Rmd/Modul_04_Rancangan_Split_Plot_dan_Strip_Plot.Rmd) · [`.html`](HTML/Modul_04_Rancangan_Split_Plot_dan_Strip_Plot.html) |
| **Bagian 05** | **Analisis Asosiasi & Korelasi** | Eksplorasi diagram pencar (*scatter plot*), Korelasi Linier Pearson + uji-*t*, Transformasi *Z* Fisher untuk hipotesis nilai korelasi tertentu (`H0: rho = rho_0`) & selang kepercayaan, Korelasi Peringkat Spearman yang kekar (*robust*) terhadap pencilan (*outlier*), serta Tabel Kontingensi & Uji Chi-Square. | `plot()`, `ggplot2::ggplot()`, `cor()`, `cor.test()`, `atanh()`, `tanh()`, `chisq.test()` | [`.R`](scripts/Modul_05_Eksplorasi_dan_Analisis_Korelasi.R) · [`.Rmd`](Rmd/Modul_05_Eksplorasi_dan_Analisis_Korelasi.Rmd) · [`.html`](HTML/Modul_05_Eksplorasi_dan_Analisis_Korelasi.html) |
| **Bagian 06** | **Analisis Regresi & Diagnostik Asumsi** | Regresi Linier Sederhana (OLS & uji-*t* 1 arah), Regresi Linier Berganda via `lm()` & aljabar matriks `(X'X)^-1 X'Y`, Regresi Logistik Biner & pendugaan `LD50`, Diagnostik Visual sisaan, Uji Normalitas Lilliefors & Pearson Chi-Square, Uji Homogenitas Bartlett & *F*-Max Hartley, Deteksi Multikolinieritas (VIF), Seleksi *Stepwise*, *Ridge Regression*, PCR, dan PLS. | `lm()`, `solve()`, `glm(..., family = binomial)`, `nortest::lillie.test()`, `bartlett.test()`, `car::vif()`, `step()`, `MASS::lm.ridge()`, `glmnet::cv.glmnet()`, `pls::pcr()`, `pls::plsr()` | [`.R`](scripts/Modul_06_Analisis_Regresi_Diagnostik_dan_Seleksi_Peubah.R) · [`.Rmd`](Rmd/Modul_06_Analisis_Regresi_Diagnostik_dan_Seleksi_Peubah.Rmd) · [`.html`](HTML/Modul_06_Analisis_Regresi_Diagnostik_dan_Seleksi_Peubah.html) |

### Ringkasan 13 Dataset Studi Kasus (`data/`)

| No | Berkas Dataset | Struktur Data & Skenario Analisis | Digunakan Pada |
| :---: | :--- | :--- | :---: |
| 01 | [`01_padi_rak_s969.csv`](data/01_padi_rak_s969.csv) | 6 perlakuan pupuk NPK × 4 kelompok hasil gabah padi genotipe S-969 (RAK) | Bagian 00, 01 |
| 02 | [`02_red_clover_ral.csv`](data/02_red_clover_ral.csv) | 6 strain bakteri *Rhizobium* × 5 ulangan kadar nitrogen tanaman *Red Clover* (RAL) | Bagian 01, 02, 06 |
| 03 | [`03_ayam_kontras_ral.csv`](data/03_ayam_kontras_ral.csv) | 6 sumber pakan suplemen × 4 ulangan bobot akhir ayam pedaging (5 Kontras Ortogonal) | Bagian 02 |
| 04 | [`04_kedelai_polinomial_rak.csv`](data/04_kedelai_polinomial_rak.csv) | 5 taraf jarak tanam berjarak sama × 6 kelompok hasil kedelai (Polinomial Ortogonal) | Bagian 02 |
| 05 | [`05_tomat_faktorial_ral.csv`](data/05_tomat_faktorial_ral.csv) | Faktorial 5 dosis fumigan × 2 durasi fumigasi × 3 ulangan daya kecambah benih tomat (RAL) | Bagian 03 |
| 06 | [`06_sapi_faktorial_rak.csv`](data/06_sapi_faktorial_rak.csv) | Faktorial 4 taraf suplemen seng × 3 taraf minyak ikan × 3 blok pertambahan bobot sapi (RAK) | Bagian 03 |
| 07 | [`07_debu_splitplot_ral.csv`](data/07_debu_splitplot_ral.csv) | *Split-Plot* RAL: 2 lokasi pengamatan (petak utama) × 10 jenis tanaman (anak petak) × 3 ulangan | Bagian 04 |
| 08 | [`08_gandum_splitplot_rak.csv`](data/08_gandum_splitplot_rak.csv) | *Split-Plot* RAK: 4 varietas gandum (petak utama) × 4 perlakuan benih (anak petak) × 4 kelompok | Bagian 04 |
| 09 | [`09_padi_stripplot_rak.csv`](data/09_padi_stripplot_rak.csv) | *Strip-Plot* RAK: 6 varietas padi (jalur mendatar) × 3 dosis nitrogen (jalur tegak) × 3 kelompok | Bagian 04 |
| 10 | [`10_emisi_hc_korelasi.csv`](data/10_emisi_hc_korelasi.csv) | Jarak tempuh kendaraan terhadap emisi hidrokarbon (HC) kondisi normal vs berpencilan (*outlier*) | Bagian 05 |
| 11 | [`11_jangkrik_suhu.csv`](data/11_jangkrik_suhu.csv) | Frekuensi derik jangkrik per detik terhadap suhu udara (Korelasi Pearson & Transformasi *Z* Fisher) | Bagian 05 |
| 12 | [`12_atrazine_regresi_sederhana.csv`](data/12_atrazine_regresi_sederhana.csv) | Kurva kalibrasi konsentrasi herbisida Atrazine terhadap *Relative Peak Area* (Regresi Sederhana) | Bagian 06 |
| 13 | [`13_individu_regresi_berganda.csv`](data/13_individu_regresi_berganda.csv) | Data multivariat prediktor normal, kolinier, dan *noise* (OLS Matriks, VIF, *Stepwise*, *Ridge*, PCR, PLS) | Bagian 06 |

---

## Panduan Instalasi & *Reproducibility* (*How to Run*)

### 1. Prasyarat Sistem
Pastikan perangkat Anda telah terpasang:
- **R** versi **`>= 4.0.0`** — unduh melalui [CRAN](https://cran.r-project.org/).
- **RStudio Desktop** — direkomendasikan karena sudah menyertakan **Pandoc** untuk kompilasi dokumen R Markdown ke HTML ([Unduh RStudio](https://posit.co/download/rstudio-desktop/)).

### 2. Kloning Repositori
```bash
git clone https://github.com/<username>/<repository-name>.git
cd <repository-name>
```

### 3. Instalasi Paket Dependensi R
Jalankan perintah berikut pada konsol R atau RStudio untuk memasang seluruh pustaka yang diperlukan:

```r
install.packages(c(
  "rmarkdown", "knitr", "rmdformats", "bookdown",
  "agricolae", "emmeans", "multcomp", "car",
  "nortest", "MASS", "glmnet", "pls",
  "ggplot2", "readxl"
), repos = "https://cloud.r-project.org")
```

### 4. Menjalankan *Pipeline* Otomasi Lengkap
Untuk menghasilkan ulang ke-13 berkas dataset `.csv` ke dalam folder `data/`, mengompilasi ketujuh dokumen `.Rmd` menjadi laporan `.html` mandiri, serta memperbarui portal [`HTML/index.html`](HTML/index.html) secara otomatis, cukup jalankan satu perintah berikut dari terminal pada direktori utama proyek:

```bash
Rscript 99_render_all_html.R
```

Atau langsung dari dalam konsol **RStudio**:

```r
source("99_render_all_html.R")
```

> **Catatan Penggunaan Modular:**
> - Apabila Anda hanya ingin membuat ulang berkas dataset di folder `data/` tanpa mengompilasi HTML, jalankan `Rscript 00_generate_datasets.R`.
> - Setiap skrip analisis murni di dalam folder [`scripts/`](scripts/) dapat dibuka dan dieksekusi langsung baris demi baris di RStudio untuk eksplorasi interaktif.

---

## Cara Membuka & Membaca Laporan HTML

Seluruh dokumen laporan di dalam folder [`HTML/`](HTML/) telah dikompilasi menggunakan konfigurasi **`self_contained: true`**. Artinya, seluruh lembar gaya CSS, pustaka interaktif, tabel ringkasan, hingga grafik beresolusi tinggi telah tertanam utuh di dalam masing-masing berkas `.html`.

1. **Tanpa *Web Server* Lokal**: Anda **tidak** memerlukan *web server* lokal maupun instalasi R untuk membaca hasil laporan—cukup klik ganda (*double-click*) berkas `.html` menggunakan peramban web modern apa pun (*Google Chrome*, *Mozilla Firefox*, *Microsoft Edge*, atau *Safari*).
2. **Portal Navigasi Utama**: Buka berkas **[`HTML/index.html`](HTML/index.html)** terlebih dahulu sebagai gerbang utama untuk menelusuri ringkasan topik dan berpindah antarbagian analisis (`Bagian 00` hingga `Bagian 06`) dengan mudah.
3. **Fitur Bacaan Interaktif**: Di dalam setiap dokumen laporan, gunakan panel navigasi di sisi kiri untuk melompat ke subanalisis tertentu, tombol **`Code`** (*Show/Hide*) untuk melipat atau menampilkan blok sintaks R, serta klik pada grafik untuk memperbesar tampilan (*lightbox*).

---

## Kontribusi

Kontribusi untuk memperkaya studi kasus, menyempurnakan visualisasi, maupun meningkatkan efisiensi kode sangat terbuka:

1. Lakukan **Fork** pada repositori ini.
2. Buat *branch* fitur atau perbaikan baru (`git checkout -b fitur/penambahan-analisis`).
3. Pastikan seluruh skrip berjalan bersih dan *pipeline* `Rscript 99_render_all_html.R` selesai tanpa galat (*error*).
4. Kirimkan **Pull Request** dengan deskripsi perubahan yang jelas, atau buka diskusi terlebih dahulu melalui menu **Issues** apabila Anda menemukan kendala teknis.

---

## Lisensi

Proyek ini didistribusikan di bawah lisensi **MIT License**. Anda bebas menggunakan, memodifikasi, dan menyebarluaskan kode maupun dokumentasi di dalam repositori ini untuk keperluan riset, pengembangan, maupun pembelajaran mandiri dengan tetap mencantumkan atribusi hak cipta asli.
