# Import Library ---------------------------------------------------------------
packages <- c("rugarch","fGarch","FinTS","forecast","readr", "lubridate","dplyr","xts","readxl", "BETS", "forecast", "TSA", "tseries", "ggplot2", 
              "ggfortify", "TSstudio", "zoo", "lmtest", "fpp2", "knitr", 
              "reshape2", "tidyr", "tidyverse")

# Loop melalui setiap library
for (package in packages) {
  # Periksa apakah library sudah terinstal
  if (!requireNamespace(package, quietly = TRUE)) {
    # Jika belum terinstal, instal library
    install.packages(package)
  }
  # Impor library
  library(package, character.only = TRUE)
}

# Import Data ------------------------------------------------
tlkm <- read_csv("C:/Users/Jovana/Downloads/close_price_data(2).csv")
tlkm <- as.data.frame(tlkm)
str(tlkm)
summary(tlkm)

# Missing Value
sapply(tlkm, function(x) sum(is.na(x)))

tlkm$Date <- as.Date(tlkm$Date)
ts.tlkm <- ts(tlkm)
str(tlkm)

# Boxplot
y <- tlkm$Date
x <- tlkm$Close
boxplot(x, xlab="Harga Tutupan Saham TLKM", col="lightblue")

# plot harian
ggplot(tlkm, aes(x=Date, y=Close)) +
  geom_line() +
  labs(title = "Plot Data Harian Harga Tutupan Saham TLKM",
       subtitle = "4 Desember 2014 - 13 Desember 2024") +
  theme(plot.title =  element_text(face = "bold", hjust=.5),
        plot.subtitle = element_text(hjust=.5),
        legend.position = "bottom",
        panel.background = element_rect(fill=NA))


p=(2474/2481)
f=as.integer(p*2481)
jumlah.train <- as.integer(p*nrow(tlkm))
ggplot(tlkm, aes(x=Date, y=Close)) +
  geom_line() + 
  labs(title = "Plot Data Harian Harga Tutupan Saham TLKM",
       subtitle = "4 Desember 2014 - 13 Desember 2024") +
  geom_vline(aes(xintercept = Date[jumlah.train], 
                 col="Pemisah_data_train"), lty=2, lwd=.7) +
  theme(plot.title =  element_text(face = "bold", hjust=.5),
        plot.subtitle = element_text(hjust=.5),
        legend.position = "bottom",
        panel.background = element_rect(fill=NA)) +
  scale_color_manual(name = "", values = c(Pemisah_data_train="blue"))

tlkm[jumlah.train,1]

## Splitting Data Training dan Data Testing -------------
train=1:f
training.ts <- ts(tlkm$Close[train])
testing.ts <- ts(tlkm$Close[-train], start = f+1)

## Data testing --------
print(tlkm[2475:2481,])
ggplot(tlkm[2475:2481,], aes(x=Date, y=Close)) +
  geom_line() + 
  labs(title = "Plot Time Series Data Testing Harga Tutupan Saham TLKM",
       subtitle = "(4 Desember 2014 - 13 Desember 2024)") +
  theme(plot.title =  element_text(face = "bold", hjust=.5),
        plot.subtitle = element_text(hjust=.5),
        legend.position = "bottom",
        panel.background = element_rect(fill=NA))

# Stationarity Test ------------------------------------------------------------
## Visualisasi: Time Plot ----
ggplot(tlkm[1:2474,], aes(x = Date, y = Close)) +
  geom_line() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Menambahkan garis tren
  labs(title = "Plot Time Series Data Training Harga Tutupan Saham TLKM",
       subtitle = "(4 Desember 2014 - 3 Desember 2024)",
       x = "Waktu",
       y = "Harga Tutupan") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        panel.background = element_rect(fill = NA)) +
  scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 2000)) # Skala y setiap 2000

## Visualisasi: ACF dan PACF ----
par(mfrow=c(1,2))
acf(training.ts, main = "ACF Plot for MA(q)")
pacf(training.ts, main = "PACF Plot for AR(p)")

## Uji Augmented Dickey–Fuller ----
#H0: Data tidak stasioner
#H1: Data stasioner
#alpha = 0.05
adf.test(training.ts)
#Karena p-value > alpha (yakni H0 GAGAL ditolak) maka data BELUM stasioner

# Differencing ----
train.diff<-diff(training.ts, differences = 1) 

## Visualisasi: ACF dan PACF ----
par(mfrow=c(1,2))
acf(train.diff, main = "ACF Plot for MA(q)")
pacf(train.diff, main = "PACF Plot for AR(p)")

## Uji Augmented Dickey–Fuller ----
#H0: Data tidak stasioner
#H1: Data stasioner
#alpha = 0.05
adf.test(train.diff)
#Karena p-value < alpha (yakni H0 ditolak) maka data SUDAH stasioner

# Model Identification ---------------------------------------------------------
#Identifikasi Model ARIMA dengan ACF, PACF, dan EACF
acf(train.diff, main="ACF data harga tutupan saham")
pacf(train.diff, main="PACF data harga tutupan saham")
eacf(train.diff)

#liat model (3,1,4) ; (0,1,2) ; (1,1,3) ; (2,1,3)

# Parameter Estimation ARIMA Model ---------------------------------------------
# Install dan load package yang dibutuhkan
if (!require("forecast")) install.packages("forecast", dependencies = TRUE)
library(forecast)

# Estimasi ARIMA model berdasarkan data train.diff
# Pastikan train.diff adalah data time series yang sudah di-differencing
# Initialize lists to store AIC, BIC, and parameter estimates
aic_values <- list()
bic_values <- list()
parameter_estimates <- list()

# Iterate over combinations of p and q
for (p in 0:4) {
  for (q in 0:4) {
    # Try fitting the ARIMA model
    tryCatch({
      model <- Arima(train.diff, order = c(p, 0, q), include.mean = TRUE)
      cat("ARIMA(", p, ",0,", q, ") Model Summary:\n", sep = "")
      
      # Print model summary
      print(summary(model))
      
      # Print AIC and BIC
      cat("AIC: ", model$aic, "\n")
      cat("BIC: ", model$bic, "\n")
      
      # Extract parameter estimates and significance
      coefs <- model$coef
      se <- sqrt(diag(model$var.coef))
      z_values <- coefs / se
      p_values <- 2 * (1 - pnorm(abs(z_values)))  # Two-tailed test
      
      # Print parameter estimates and p-values
      cat("\nParameter Estimates and Significance:\n")
      for (i in seq_along(coefs)) {
        cat(names(coefs)[i], ": Estimate = ", coefs[i], 
            ", Std. Error = ", se[i], 
            ", z-value = ", z_values[i], 
            ", p-value = ", p_values[i], "\n")
      }
      cat("\n")
      
      # Store AIC, BIC, and parameter estimates
      aic_values[[paste0("ARIMA(", p, ",1,", q, ")")]] <- model$aic
      bic_values[[paste0("ARIMA(", p, ",1,", q, ")")]] <- model$bic
      parameter_estimates[[paste0("ARIMA(", p, ",1,", q, ")")]] <- list(coefs = coefs, p_values = p_values)
    }, error = function(e) {
      cat("ARIMA(", p, ",1,", q, ") failed to converge.\n", sep = "")
    })
  }
}

# Find the best model based on AIC and BIC
best_aic_model <- names(aic_values)[which.min(unlist(aic_values))]
best_bic_model <- names(bic_values)[which.min(unlist(bic_values))]

cat("\nBest ARIMA model based on AIC: ", best_aic_model, "\n")
cat("Best ARIMA model based on BIC: ", best_bic_model, "\n")

### 
### Pilih dari aic aja jd pake yg  (0,1,2)


## Overfitting Model --------------------------------------
# Menguji overfitt model dengan membandingkan dengan ARIMA(1,1,2) dan ARIMA(0,1,3)    
fit <- Arima(training.ts, order = c(0,1,2))
summary(fit)
overfit1 <- Arima(training.ts, order = c(1,1,2))
overfit2 <- Arima(training.ts, order = c(0,1,3))
cbind(overfit1,fit,overfit2)

t_test(fit)
t_test(overfit1)
t_test(overfit2)

# Model terbaik adalah ARIMA(0,1,2)

## Analisis Residual  --------------------------------------
### Eksplorasi ---------------------------------------------
sisaan <- fit$residuals
tsdiag(fit)

par(mfrow = c(2,2))

qqnorm(sisaan)
qqline(sisaan, col = "blue", lwd = 2)

plot(sisaan, type="o", 
     ylab = "Sisaan", xlab = "Order", main = "Sisaan ARIMA(3,1,4) vs Order")
abline(h = 0, col='red')

acf(sisaan)
pacf(sisaan)

hist(sisaan, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue", 
     border = "black", 
     breaks = 20) # Adjust breaks for better granularity


### Uji Formal ----------------------------------------------
#### Uji Jarque Bera (Uji Normalitas) ----
jarque.bera.test(sisaan)

#### Uji Ljung-Box (Uji Independensi) ----
Box.test(sisaan, type = "Ljung") 

#### Nilai tengah sisaan sama dengan nol ----
t.test(sisaan, mu = 0, conf.level = 0.95)


# Identifikasi Efek ARCH -----------------------------------------------------
# Uji Lagrange Multiplier (LM)
for (i in 1:15) {
  ArchTest <- ArchTest(fit$residuals, lags=i, demean=TRUE)
  cat("P Value LM Test lag ke", i,"adalah" , ArchTest$p.value, "\n") }
# Model cocok menggunakan GARCH karena p-value<0.05 hingga lag 15


# Pemodelan GARCH  -----------------------------------------------------------
# Akan dicoba untuk setiap kombinasi GARCH(m,s) dari 1 hingga 4

# Loop through all combinations of m and s
for (m in 1:4) {
  for (s in 1:4) {
    cat("\n## ARIMA(0,1,2)-GARCH(", m, ",", s, ") -------------------------\n", sep = "")
    garchSpec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(m, s)),
      mean.model = list(armaOrder = c(0, 2))
    )
    
    garchFit <- tryCatch(
      ugarchfit(spec = garchSpec, data = train.diff),
      error = function(e) NULL
    )
    
    if (!is.null(garchFit)) {
      #print(garchFit)
      
      # Signifikansi
      print(round(garchFit@fit$matcoef, 6))
      
      # Goodness of fit for the mean prediction
      e <- residuals(garchFit)
      cat("Mean Squared Error:", mean(e^2), "\n")
      
      # Results information criteria
      print(infocriteria(garchFit))
      
      # Diagnosing absolute standardized returns
      stdret <- residuals(garchFit, standardize = TRUE)
      acf(abs(stdret), 22)
      
      # Uji Jarque Bera (Uji Normalitas)
      print(jarque.bera.test(abs(stdret)))
      
      # Uji Ljung-Box (Uji Independensi)
      print(Box.test(abs(stdret), type = "Ljung"))
      
      # Uji ARCH Effect (LM Test)
      for (i in 1:15) {
        ArchTest <- ArchTest(abs(stdret), lags=i, demean=TRUE)
        cat("P Value LM Test lag ke", i,"adalah" , ArchTest$p.value, "\n") }
      
    } else {
      cat("Model failed to converge for GARCH(", m, ",", s, ").\n", sep = "")
    }
  }
}

# Model Terbaik (mempertimbangkan signifikansi parameter dan prinsip parsimoni) ARIMA(0,1,2)-GARCH(1,3)

# Forecasting ------------------------------------------------------------------
str(training.ts)
str(testing.ts)

# Mengkonstruksi model ARIMA dari data training
garchSpec <- ugarchspec(
  variance.model=list(
    model="sGARCH",garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(0,2)))
garchFitt <- ugarchfit(spec=garchSpec, data=train.diff)


forc<- ugarchforecast(fitORspec = garchFitt, 
                      data = tlkm$Close, n.ahead = 7, n.roll = 0)
pt_1 <- tlkm$Close[2474] #nilai akhir data latih
hasil.forc.Diff <- forc@forecast$seriesFor[,1]
hasil <- diffinv(hasil.forc.Diff, differences = 1) + pt_1
perbandingan <- data.frame("Aktual"= testing.ts,
                           "Ramalan" = hasil[-1])
head(perbandingan,10)

#Menghitung nilai MAE, RMSE, dan MAPE
aktual <- perbandingan$Aktual
prediksi <- perbandingan$Ramalan
mean(abs(aktual - prediksi)) #MAE
sqrt(mean((aktual - prediksi)^2)) #RMSE
mean(abs((aktual - prediksi)/aktual)) #MAPE

# Data untuk Plot
perbandingan1 <- data.frame(
  Tanggal = as.Date(c("2024-12-03", "2024-12-04", "2024-12-05", "2024-12-08", "2024-12-09", "2024-12-10", "2024-12-11")),
  Aktual = perbandingan$Aktual,
  Ramalan = perbandingan$Ramalan
)
merged_data <- merge(tlkm, perbandingan1, by.x = "Date", by.y = "Tanggal", all.x = TRUE)

# Plot Full Data
plot <- ggplot() +
  # Plot the full historical data
  geom_line(data = tlkm, aes(x = Date, y = Close, color = "Historical"), size = 1) +
  geom_line(data = merged_data, aes(x = Date, y = Aktual, color = "Aktual"), size = 1) +
  geom_line(data = merged_data, aes(x = Date, y = Ramalan, color = "Ramalan"), size = 1) +
  labs(
    title = "Perbandingan Harga Historis, Aktual, dan Ramalan",
    x = "Tanggal",
    y = "Harga",
    color = "Legenda"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Historical" = "black", "Aktual" = "blue", "Ramalan" = "red"))
print(plot)

# Plot Diperbesar
# Filter data dari bulan Juni
tlkm1 <- tlkm[tlkm$Date >= as.Date("2024-06-01"), ]
merged_data1 <- merge(tlkm1, perbandingan1, by.x = "Date", by.y = "Tanggal", all.x = TRUE)
tail(merged_data1)
plot2 <- ggplot() +
  geom_line(data = tlkm1, aes(x = Date, y = Close, color = "Historical"), size = 1) +
  geom_line(data = merged_data1, aes(x = Date, y = Aktual, color = "Aktual"), size = 1, na.rm = TRUE) +
  geom_line(data = merged_data1, aes(x = Date, y = Ramalan, color = "Ramalan"), size = 1, na.rm = TRUE) +
  labs(
    title = "Perbandingan Harga Historis, Aktual, dan Ramalan",
    x = "Tanggal",
    y = "Harga",
    color = "Legenda"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Historical" = "black", "Aktual" = "blue", "Ramalan" = "red"))
print(plot2)

