# ============================================================
# Example 2.2 - Moving Averages (Hareketli Ortalamalar)
# RStudio'da calistirmak icin: sag ustteki "Source" butonuna bas
# Ek paket gerekmez, sadece temel R kullanir.
# ============================================================

# Onceki denemelerden kalan nesneleri temizle
rm(list = ls())

# ---- 1. Veri -------------------------------------------------
# Ucak motoru ariza sayilari (ceyreklik).
# Baska veri icin sadece bu satiri degistir.
D <- c(170,240,183,219,222,302,188,278)
n <- length(D)

# ---- 2. MA(N) tahmin fonksiyonu ------------------------------
# Formul: F_t = (1/N) * (D_{t-1} + D_{t-2} + ... + D_{t-N})
# Ilk N donem icin tahmin yapilamaz, NA kalir.
moving_average <- function(D, N) {
  n <- length(D)
  if (N >= n) {
    stop("N, veri sayisindan kucuk olmali.")
  }
  
  Ft <- rep(NA, n)
  for (t in (N + 1):n) {
    Ft[t] <- (1 / N) * sum(D[(t - N):(t - 1)])
  }
  return(Ft)
}

# ---- 3. Tahmin hatasi fonksiyonu -----------------------------
# Kitaptaki tanim: e_t = F_t - D_t  (tahmin - gerceklesen)
forecast_error <- function(Ft, D) {
  return(Ft - D)
}

# ---- 4. Hata olcutleri fonksiyonu ----------------------------
# MAD  = ortalama mutlak hata
# MSE  = ortalama kare hata
# MAPE = ortalama mutlak yuzde hata
error_measures <- function(e, D) {
  gecerli <- !is.na(e)
  e <- e[gecerli]
  D <- D[gecerli]
  
  MAD  <- mean(abs(e))
  MSE  <- mean(e^2)
  MAPE <- mean(abs(e / D)) * 100
  
  return(c(MAD = MAD, MSE = MSE, MAPE = MAPE))
}

# ---- 5. Tahminler ve hatalar ---------------------------------
ma3 <- moving_average(D, 3)
ma6 <- moving_average(D, 6)

error_ma3 <- forecast_error(ma3, D)
error_ma6 <- forecast_error(ma6, D)

# ---- 6. Sonuc tablosu ----------------------------------------
result <- data.frame(
  Quarter         = 1:n,
  Engine_Failures = D,
  MA_3            = round(ma3, 2),
  Error_MA3       = round(error_ma3, 2),
  MA_6            = round(ma6, 2),
  Error_MA6       = round(error_ma6, 2)
)

cat("\n--- Tahmin tablosu ---\n")
print(result, row.names = FALSE)

# ---- 7. Yontemlerin karsilastirilmasi ------------------------
measures <- rbind(
  "MA(3)" = error_measures(error_ma3, D),
  "MA(6)" = error_measures(error_ma6, D)
)

cat("\n--- Hata olcutleri ---\n")
print(round(measures, 2))

# ---- 8. Bir sonraki donem (t = n + 1) tahmini ----------------
# Son N gozlemin ortalamasi
next_ma3 <- (1 / 3) * sum(D[(n - 2):n])
next_ma6 <- (1 / 6) * sum(D[(n - 5):n])

cat("\n--- Donem", n + 1, "tahmini ---\n")
cat("MA(3):", round(next_ma3, 2), "\n")
cat("MA(6):", round(next_ma6, 2), "\n")

# ---- 9. Guncelleme formulu ile kontrol -----------------------
# F_{t+1} = F_t + (1/N) * (D_t - D_{t-N})
# Ortalamayi bastan hesaplamadan ayni sonucu vermeli.
check_ma3 <- ma3[n] + (1 / 3) * (D[n] - D[n - 3])
check_ma6 <- ma6[n] + (1 / 6) * (D[n] - D[n - 6])

cat("\n--- Guncelleme formulu ile kontrol ---\n")
cat("MA(3):", round(check_ma3, 2), "\n")
cat("MA(6):", round(check_ma6, 2), "\n")

# ---- 10. Grafik ----------------------------------------------
plot(1:n, D, type = "b", pch = 19, lwd = 2,
     xlab = "Quarter", ylab = "Engine Failures",
     main = "Gerceklesen ve MA tahminleri",
     ylim = range(c(D, ma3, ma6), na.rm = TRUE))
lines(1:n, ma3, type = "b", pch = 17, lty = 2, col = "blue")
lines(1:n, ma6, type = "b", pch = 15, lty = 3, col = "red")
legend("topleft",
       legend = c("Gerceklesen", "MA(3)", "MA(6)"),
       col = c("black", "blue", "red"),
       pch = c(19, 17, 15), lty = c(1, 2, 3), bty = "n")