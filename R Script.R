library(httr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
#GETTING DATA
Jakarta <- GET("https://data.covid19.go.id/public/api/prov_detail_DKI_JAKARTA.json")
Banten <- GET("https://data.covid19.go.id/public/api/prov_detail_BANTEN.json")
JABAR <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
JATENG <- GET ("https://data.covid19.go.id/public/api/prov_detail_JAWA_TENGAH.json")
JOGJA <- GET("https://data.covid19.go.id/public/api/prov_detail_DAERAH_ISTIMEWA_YOGYAKARTA.json")
JATIM <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TIMUR.json")
JAKARTA_RAW <- content(Jakarta, as = "parsed", simplifyVector = TRUE)
BANTEN_RAW <- content(Banten, as = "parsed", simplifyVector = TRUE)
JABAR_RAW <- content(JABAR, as = "parsed", simplifyVector = TRUE)
JATENG_RAW <- content(JATENG, as = "parsed", simplifyVector = TRUE)
JOGJA_RAW <- content(JOGJA, as = "parsed", simplifyVector = TRUE)
JATIM_RAW <- content(JATIM, as = "parsed", simplifyVector = TRUE)
JATIM_COV <- JATIM_RAW$list_perkembangan
JAKARTA_COV <- JAKARTA_RAW$list_perkembangan
BANTEN_COV <- BANTEN_RAW$list_perkembangan
JABAR_COV <- JABAR_RAW$list_perkembangan
JATENG_COV <- JATENG_RAW$list_perkembangan
JOGJA_COV<- JOGJA_RAW$list_perkembangan
#CLEANING DATA
JABAR_COV_NEW<-
  JABAR_COV %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
BANTEN_COV_NEW<-
  BANTEN_COV %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
JAKARTA_COV_NEW<-
  JAKARTA_COV %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
JATENG_COV_NEW<-
  JATENG_COV %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
JOGJA_COV_NEW<-
  JOGJA_COV %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
JATIM_COV_NEW<-
  JATIM_COV %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )


JABAR_COV_NEW <- JABAR_COV_NEW[-1:-16,]
BANTEN_COV_NEW <- BANTEN_COV_NEW[-1:-12,]
JAKARTA_COV_NEW <- JAKARTA_COV_NEW[-1:-17,]
JOGJA_COV_NEW <- JOGJA_COV_NEW[-1:-2,]
print(JATIM_COV_NEW)
#PERHITUNGAN AKUMULASI
BANTEN_COV_AKUMULASI <- 
  BANTEN_COV_NEW %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )
JAKARTA_COV_AKUMULASI <- 
  JAKARTA_COV_NEW %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )
JABAR_COV_AKUMULASI <- 
  JABAR_COV_NEW %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )
JATENG_COV_AKUMULASI <- 
  JATENG_COV_NEW %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )
JOGJA_COV_AKUMULASI <- 
  JOGJA_COV_NEW %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )
JATIM_COV_AKUMULASI <- 
  JATIM_COV_NEW %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )
#Transformasi Data
BANTEN_COV_AKUMULASI_PIVOT <- 
  BANTEN_COV_AKUMULASI %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )
JABAR_COV_AKUMULASI_PIVOT <- 
  JABAR_COV_AKUMULASI %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )
JAKARTA_COV_AKUMULASI_PIVOT <- 
  JAKARTA_COV_AKUMULASI %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )
JATENG_COV_AKUMULASI_PIVOT <- 
  JATENG_COV_AKUMULASI %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )
JOGJA_COV_AKUMULASI_PIVOT <- 
  JOGJA_COV_AKUMULASI %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )
JATIM_COV_AKUMULASI_PIVOT <- 
  JATIM_COV_AKUMULASI %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )

#PLOT DINAMIKA PERKEMBANGAN COVID DI PROVINSI DI PULAU JAWA
ggplot(BANTEN_COV_AKUMULASI_PIVOT, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Banten",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 15,
    plot_title_size = 19,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
ggplot(JAKARTA_COV_AKUMULASI_PIVOT, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Jakarta",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 15,
    plot_title_size = 19,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
ggplot(JABAR_COV_AKUMULASI_PIVOT, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Jawa Barat",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 15,
    plot_title_size = 19,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
ggplot(JATENG_COV_AKUMULASI_PIVOT, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Jawa Tengah",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 15,
    plot_title_size = 19,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
ggplot(JOGJA_COV_AKUMULASI_PIVOT, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Yogyakarta",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 15,
    plot_title_size = 19,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
ggplot(JATIM_COV_AKUMULASI_PIVOT, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Jawa Timur",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 15,
    plot_title_size = 19,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )

#RUBAH NAMA KOLOM
JAKARTA_COV_NEW
colnames(BANTEN_COV_NEW) <- c("tanggal","Banten.kasus_baru","Banten.meninggal","Banten.sembuh")
colnames(JAKARTA_COV_NEW) <- c("tanggal","Jakarta.kasus_baru","Jakarta.meninggal","Jakarta.sembuh")
colnames(JABAR_COV_NEW) <- c("tanggal","Jabar.kasus_baru","Jabar.meninggal","Jabar.sembuh")
colnames(JATENG_COV_NEW) <- c("tanggal","Jateng.kasus_baru","Jateng.meninggal","Jateng.sembuh")
colnames(JOGJA_COV_NEW) <- c("tanggal","Jogja.kasus_baru","Jogja.meninggal","Jogja.sembuh")
colnames(JATIM_COV_NEW) <- c("tanggal","Jatim.kasus_baru","Jatim.meninggal","Jatim.sembuh")

#JOIN DATA

JABAR_COV_NEW$tanggal <- NULL
JAKARTA_COV_NEW$tanggal <- NULL
JATENG_COV_NEW$tanggal <- NULL
JOGJA_COV_NEW$tanggal <- NULL
JATIM_COV_NEW$tanggal <- NULL
COV_GABUNGAN <- cbind.data.frame(BANTEN_COV_NEW,JAKARTA_COV_NEW,JABAR_COV_NEW,JATENG_COV_NEW,JOGJA_COV_NEW,JATIM_COV_NEW)
head(COV_GABUNGAN)

#GRAFIK PERKEMBANGAN COVID DI PULAU JAWA BERDASARKAN PROVINSI
grafik.gabungan.sembuh <- ggplot(COV_GABUNGAN,aes(tanggal)) + geom_line(aes(y = Banten.sembuh, color = "Banten"),size = 1.0) + geom_line(aes(y = Jakarta.sembuh, color = "Jakarta"),size = 1.0) + geom_line(aes(y = Jabar.sembuh, color = "Jawa Barat") ,size = 1.0) + geom_line(aes(y = Jogja.sembuh, color = "Yogyakarta"),size = 1.0) +  geom_line(aes(y = Jateng.sembuh, color = "Jawa Tengah"),size = 1.0) +  geom_line(aes(y =Jatim.sembuh, color = "Jawa Timur"),size = 1.0) + theme_minimal() + labs(y="kasus baru",title = "Perkembangan Pasien Sembuh dari COVID-19 di Pulau Jawa Berdasarkan Provinsi", caption = "Sumber data: covid.19.go.id") +  scale_color_manual(name = "Provinsi", values = c("Jawa Timur" = "yellow", "Jawa Tengah" = "green", "Jawa Barat" = "red", "Jakarta" = "black", "Banten"="blue","Yogyakarta"="orange")) +  scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m") +  theme(legend.position = "top")
grafik.gabungan.kasus_baru <- ggplot(COV_GABUNGAN,aes(tanggal)) + geom_line(aes(y = Banten.kasus_baru, color = "Banten"),size = 1.0) + geom_line(aes(y = Jakarta.kasus_baru, color = "Jakarta"),size = 1.0) + geom_line(aes(y = Jabar.kasus_baru, color = "Jawa Barat") ,size = 1.0) + geom_line(aes(y = Jogja.kasus_baru, color = "Yogyakarta"),size = 1.0) +  geom_line(aes(y = Jateng.kasus_baru, color = "Jawa Tengah"),size = 1.0) +  geom_line(aes(y =Jatim.kasus_baru, color = "Jawa Timur"),size = 1.0) + theme_minimal() + labs(y="kasus baru",title = "Perkembangan Pasien Sembuh dari COVID-19 di Pulau Jawa Berdasarkan Provinsi", caption = "Sumber data: covid.19.go.id") +  scale_color_manual(name = "Provinsi", values = c("Jawa Timur" = "yellow", "Jawa Tengah" = "green", "Jawa Barat" = "red", "Jakarta" = "black", "Banten"="blue","Yogyakarta"="orange")) +  scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m") +  theme(legend.position = "top")
grafik.gabungan.meninggal <- ggplot(COV_GABUNGAN,aes(tanggal)) + geom_line(aes(y = Banten.meninggal, color = "Banten"),size = 1.0) + geom_line(aes(y = Jakarta.meninggal, color = "Jakarta"),size = 1.0) + geom_line(aes(y = Jabar.meninggal, color = "Jawa Barat") ,size = 1.0) + geom_line(aes(y = Jogja.meninggal, color = "Yogyakarta"),size = 1.0) +  geom_line(aes(y = Jateng.meninggal, color = "Jawa Tengah"),size = 1.0) +  geom_line(aes(y =Jatim.meninggal, color = "Jawa Timur"),size = 1.0) + theme_minimal() + labs(y="kasus baru",title = "Perkembangan Pasien Sembuh dari COVID-19 di Pulau Jawa Berdasarkan Provinsi", caption = "Sumber data: covid.19.go.id") +  scale_color_manual(name = "Provinsi", values = c("Jawa Timur" = "yellow", "Jawa Tengah" = "green", "Jawa Barat" = "red", "Jakarta" = "black", "Banten"="blue","Yogyakarta"="orange")) +  scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m") +  theme(legend.position = "top")
