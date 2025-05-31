data = c(85, 90, 78, 93, 88, 74, 80, 85, 91, 87, 89, 84, 76, 98, 82, 77, 90, 85, 96, 81)
dataSort = sort(data)
dataSort
n = length(dataSort)
n

K = 1 + (3.322 * log10(n))
K
k = round(K)
k

nmax = max(dataSort)
nmax
nmin = min(dataSort)
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
frekuensi(dataSort, 74, 78)
frekuensi(dataSort, 79, 83)
frekuensi(dataSort, 84, 88)
frekuensi(dataSort, 89, 93)
frekuensi(dataSort, 94, 98)
f = c(4, 3, 6, 5, 2)
f

TDF = edit(data.frame())
TDF
TDF$Frekuensi = f
TDF

# Membuat Titik Tengah pada TDF
# Rumus TTK = (TB + TA) / 2
(74 - 78) / 2
(79 - 84) / 2
(84 - 88) / 2
(89 - 94) / 2
(94 - 98) / 2
TTK = c(76, 81, 86, 91, 96)
TDF$Titik_tengah = TTK
TDF

# Jika sudah membuat TDF, langsung saja buat histogramnya
# Visualisasi TDF
Tabel_TDF <- data.frame(
	Kelas = c("74-78", "79-83", "84-88", "89-93", "94-98"),
	Frekuensi = c(4, 3, 6, 5, 2),
	Titik_tengah = c(76, 81, 86, 91, 96)
)

# Buat histogram dan garis poligon menggunakan library ggplot2
library(ggplot2)
# Buat plot dengan ggplot2
ggplot(Tabel_TDF, aes(x = Kelas, y = Frekuensi)) +
geom_bar(stat = "identity", fill = "skyblue", color = "black") +
geom_point(aes(x = as.numeric(as.factor(Kelas)), y = Frekuensi), color = "black", size = 3) +
geom_line(aes(x = as.numeric(as.factor(Kelas)), y = Frekuensi), color = "red", linetype = "solid", size = 1) +
labs(title = "Histogram dan Poligon TDF", x = "Kelas", y = "Frekuensi") +
theme_minimal()



# Membuat Titik Tengah pada TDF
# Langsung saja membuat TA dan TB
# Rumus TTK = (TB + TA) / 2
(73.5 + 78.5) / 2
(78.5 + 83.5) / 2
(83.5 + 88.5) / 2
(88.5 + 93.5) / 2
(93.5 + 98.5) / 2
TTK = c(76, 81, 86, 91, 96)
TDF$Titik_tengah = TTK
TDF

TA_TB = c("73.5-78.5", "78.5-83.5", "83.5-88.5", "88.5-93.5", "93.5-98.5")
TDF$Tepi = TA_TB
TDF

# Buat histogram dan garis poligon menggunakan library ggplot2
library(ggplot2)
# Buat plot dengan ggplot2
ggplot(TDF, aes(x = Tepi, y = Frekuensi)) +
geom_bar(stat = "identity", fill = "skyblue", color = "black") +
geom_point(aes(x = as.numeric(as.factor(Tepi)), y = Frekuensi), color = "black", size = 3) +
geom_line(aes(x = as.numeric(as.factor(Tepi)), y = Frekuensi), color = "red", linetype = "solid", size = 1) +
labs(title = "Histogram dan Poligon TDF", x = "Kelas", y = "Frekuensi") +
theme_minimal()
