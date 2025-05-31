ungrouped_data = c(48, 52, 56, 61, 58, 55, 62, 62, 55, 53, 49, 56, 59, 62, 54)
mean_ungrouped = mean(ungrouped_data)
mean_ungrouped
median_ungrouped
modus_ungrouped <- as.numeric(names(sort(table(ungrouped_data), decreasing = TRUE)[1]))
modus_ungrouped

data = sort(ungrouped_data)
n = length(ungrouped_data)
K = 1 + (3.322 * log10(n))
K
k = round(K)
k
nmax = max(ungrouped_data)
nmax
nmin = min(ungrouped_data)
nmin
r = nmax - nmin
r
I = r/k
I
i = ceiling(I)
i
frekuensi = function(x,y,z){
	a = 0
	for(i in 1:n){
		if(x[i]>=y && x[i]<=z){
			a = a + 1
		}
	}
	print(a)
}
frekuensi(ungrouped_data, 48, 50)
frekuensi(ungrouped_data, 51, 53)
frekuensi(ungrouped_data, 54, 56)
frekuensi(ungrouped_data, 57, 59)
frekuensi(ungrouped_data, 60, 62)


# Mendeklarasikan data tunggal
ungrouped_data = c(48, 52, 56, 61, 58, 55, 62, 62, 55, 53, 49, 56, 59, 62, 54)
ungrouped_data = c(48, 49, 52, 53, 54, 55, 55, *56*, 56, 58, 59, 61, 62, 62, 62)
ungrouped_data


# Mendeklarasikan data kelompok
Kelas = c("48-50", "51-53", "54-56", "57-59", "60-62")
Frekuensi = c(2, 2, 5, 2, 4)
Titik_tengah = c(49, 52, 55, 58, 61)
grouped_data <- data.frame(Kelas, Frekuensi, Titik_tengah)
print(grouped_data)


# Menghitung banyak data tunggal
n_ungrouped = length(ungrouped_data)
n_ungrouped


# Menghitung banyak data kelompok
n_grouped = sum(grouped_data$Frekuensi)
n_grouped


# Mean data tunggal
mean_ungrouped = mean(ungrouped_data)
mean_ungrouped


# Mean data kelompok
mean_grouped = sum(grouped_data$Frekuensi * grouped_data$Titik_tengah) / n_grouped
mean_grouped


# Median data tunggal
median_ungrouped = median(ungrouped_data)
median_ungrouped


# Median data kelompok
n_grouped
cumulative_frequency = cumsum(grouped_data$Frekuensi)
kelas_median <- which(cumulative_frequency >= n_grouped / 2)[1]
TB_median <- as.numeric(sub("-.*", "", grouped_data$Kelas[kelas_median])) - 0.5 # TB = Batas bawah - 0.5
fkum <- if (kelas_median > 1) cumulative_frequency[kelas_median - 1] else 0 # Jika kelas_median 1, maka fkum 0
f_median <- grouped_data$Frekuensi[kelas_median]
i <- (as.numeric(sub(".*-", "", grouped_data$Kelas[kelas_median])) + 0.5) - TB_median # i = TA - TB

median_grouped <- TB_median + (((n_grouped / 2) - fkum) / f_median) * i
median_grouped
TB_median
i
fkum
f_median


# Modus data tunggal
modus_ungrouped <- as.numeric(names(sort(table(ungrouped_data), decreasing = TRUE)[1]))
modus_ungrouped

# Modus data kelompok
n_grouped
kelas_modus <- which.max(grouped_data$Frekuensi)
TB_modus <- as.numeric(sub("-.*", "", grouped_data$Kelas[kelas_modus])) - 0.5 # TB = Batas bawah - 0.5
f_modus <- grouped_data$Frekuensi[kelas_modus]
d1 <- if (kelas_modus > 1) f_modus - grouped_data$Frekuensi[kelas_modus - 1] else f_modus
d2 <- if (kelas_modus < n_grouped) f_modus - grouped_data$Frekuensi[kelas_modus + 1] else f_modus
i <- (as.numeric(sub(".*-", "", grouped_data$Kelas[kelas_modus])) + 0.5) - TB_modus # i = TA - TB

modus_grouped <- TB_modus + (d1 / (d1 + d2)) * i
modus_grouped


# Kuartil data tunggal
quartiles_ungrouped <- quantile(ungrouped_data, probs = c(0.25, 0.5, 0.75), type = 6)
quartiles_ungrouped


# Kuartil data kelompok
quartiles_grouped <- numeric(3) # Placeholder untuk Q1, Q2, Q3
for (k in 1:3) {
  posisi_kuartil <- k / 4 * n_grouped
  kelas_kuartil <- which(cumsum(grouped_data$Frekuensi) >= posisi_kuartil)[1]
  TB_kuartil <- as.numeric(sub("-.*", "", grouped_data$Kelas[kelas_kuartil])) - 0.5 # TB = Batas bawah - 0.5
  fkum_kuartil <- if (kelas_kuartil > 1) cumsum(grouped_data$Frekuensi)[kelas_kuartil - 1] else 0
  f_kuartil <- grouped_data$Frekuensi[kelas_kuartil]
  i <- (as.numeric(sub(".*-", "", grouped_data$Kelas[kelas_kuartil])) + 0.5) - TB_kuartil # i = TA - TB
  
  quartiles_grouped[k] <- TB_kuartil + ((posisi_kuartil - fkum_kuartil) / f_kuartil) * i
}
quartiles_grouped


# Desil data tunggal
deciles_ungrouped <- quantile(ungrouped_data, probs = seq(0.1, 0.9, 0.1), type = 6)
deciles_ungrouped


# Desil data kelompok
deciles_grouped <- numeric(9) # Placeholder untuk D1 hingga D9
for (k in 1:9) {
  posisi_desil <- k / 10 * n_grouped
  kelas_desil <- which(cumsum(grouped_data$Frekuensi) >= posisi_desil)[1]
  TB_desil <- as.numeric(sub("-.*", "", grouped_data$Kelas[kelas_desil])) - 0.5 # TB = Batas bawah - 0.5
  fkum_desil <- if (kelas_desil > 1) cumsum(grouped_data$Frekuensi)[kelas_desil - 1] else 0
  f_desil <- grouped_data$Frekuensi[kelas_desil]
  i <- (as.numeric(sub(".*-", "", grouped_data$Kelas[kelas_desil])) + 0.5) - TB_desil # i = TA - TB
  
  deciles_grouped[k] <- TB_desil + ((posisi_desil - fkum_desil) / f_desil) * i
}
deciles_grouped


# Persentil data tunggal
percentiles_ungrouped <- quantile(ungrouped_data, probs = seq(0.01, 0.99, 0.01), type = 6)
percentiles_ungrouped


# Persentil data kelompok
percentiles_grouped <- numeric(99) # Placeholder untuk P1 hingga P99
for (k in 1:99) {
  posisi_persentil <- k / 100 * n_grouped
  kelas_persentil <- which(cumsum(grouped_data$Frekuensi) >= posisi_persentil)[1]
  TB_persentil <- as.numeric(sub("-.*", "", grouped_data$Kelas[kelas_persentil])) - 0.5 # TB = Batas bawah - 0.5
  fkum_persentil <- if (kelas_persentil > 1) cumsum(grouped_data$Frekuensi)[kelas_persentil - 1] else 0
  f_persentil <- grouped_data$Frekuensi[kelas_persentil]
  i <- (as.numeric(sub(".*-", "", grouped_data$Kelas[kelas_persentil])) + 0.5) - TB_persentil # i = TA - TB
  
  percentiles_grouped[k] <- TB_persentil + ((posisi_persentil - fkum_persentil) / f_persentil) * i
}
print(percentiles_grouped)

