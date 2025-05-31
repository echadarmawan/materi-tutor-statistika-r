# Mendeklarasikan data tunggal dan menghitung banyak data
ungrouped_data = c(74, 76, 77, 78, 80, 81, 82, 84, 85, 85, 85, 87, 88, 89, 90, 90, 91, 93, 96, 98)
ungrouped_data
n_ungrouped = length(ungrouped_data)
n_ungrouped

# Mendeklarasikan data kelompok dan menghitung banyak data
Kelas = c("74-78", "79-83", "84-88", "89-93", "94-98")
Frekuensi = c(4, 3, 6, 5, 2)
Titik_tengah = c(76, 81, 86, 91, 96)
grouped_data <- data.frame(Kelas, Frekuensi, Titik_tengah)
grouped_data
n_grouped = sum(grouped_data$Frekuensi)
n_grouped

# Varians data tunggal
mean_ungrouped <- mean(ungrouped_data) # Mean data tunggal
var_ungrouped <- sum((ungrouped_data - mean_ungrouped)^2) / n_ungrouped # Varians data tunggal
cat("Varians Data Tunggal:", var_ungrouped, "\n")

# Varians data kelompok
mean_grouped <- sum(grouped_data$Titik_tengah * grouped_data$Frekuensi) / n_grouped # Mean data kelompok
var_grouped <- sum(grouped_data$Frekuensi * (grouped_data$Titik_tengah - mean_grouped)^2) / n_grouped # Varians data kelompok
cat("Varians Data Kelompok:", var_grouped, "\n")

# Standar deviasi data tunggal
sd_ungrouped <- sqrt(var_ungrouped)
cat("Standar Deviasi Data Tunggal:", sd_ungrouped, "\n")

# Standar deviasi data kelompok
sd_grouped <- sqrt(var_grouped)
cat("Standar Deviasi Data Kelompok:", sd_grouped, "\n")