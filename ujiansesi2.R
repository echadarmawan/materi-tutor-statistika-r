# Jawaban nomor 1
nomor1 = c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 150, 130, 160, 120, 140, 110, 180, 170, 150)
# mengurutkan data
sort1 = sort(nomor1)
sort1
# jumlah data
n1 = length(sort1)
n1
# menghitung jumlah kelas dengan aturan Sturges
K = round(1 + 3.322 * log10(n1))
K
# menghitung range
r = max(sort1) - min(sort1)
r
# menghitung panjang kelas interval
I = ceiling(r/K)
I
# menentukan kelas
Kelas = character(K)
awal = min(sort1) #inisiasi nilai awal
for(k in 1:K) {
	akhir = awal + (I-1)
	Kelas[k] = paste(awal, "-", akhir)
	awal = akhir + 1
}
Kelas
# menentukan frekuensi tiap kelas
Frekuensi = numeric(K)
awal = min(sort1) #inisiasi nilai awal
for(k in 1:K) {
	a = 0
	akhir = awal + (I-1)
	for(i in 1:n1) {
		if(sort1[i] >= awal && sort1[i] <= akhir) {
			a = a + 1
		}
	}
	awal = akhir + 1
	Frekuensi[k] = a
}
Frekuensi
# Membuat TDF (Kelas dan Frekuensi)
TDF = data.frame(Kelas, Frekuensi)
TDF
# TDF = data.frame(
# 	Kelas = c("150-157", "158-165", "166-173", "174-181", "182-189"),
# 	Frekuensi = c(5, 3, 4, 5, 3)
# )

# Jawaban nomor 2
nomor2 = c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 150, 130, 160, 120, 140, 110, 180, 170, 150)
sort2 = sort(nomor2)
sort2
# Mean data tunggal
mean(sort2)
# Median data tunggal
median(sort2)
# Modus data tunggal
as.numeric(names(which.max(table(sort2))))
# Kuartil data tunggal
quantile(sort2, probs = c(0.25, 0.5 , 0.75))

# Jawaban nomor 3
nomor3 = data.frame(
	Kelas = c("2-6", "7-11", "12-16", "17-21", "22-26", "27-31"),
	Frekuensi = c(2, 3, 4, 5, 6, 10),
	Titik_tengah = c(4, 9, 14, 19, 24, 29)
)
nomor3
mean3 = sum(nomor3$Frekuensi * nomor3$Titik_tengah) / sum(nomor3$Frekuensi)
mean3
var3 = sum(nomor3$Frekuensi * (nomor3$Titik_tengah - mean3)^2) / sum(nomor3$Frekuensi)
var3
sd3 = sqrt(var3)
sd3
