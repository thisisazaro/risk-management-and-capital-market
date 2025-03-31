

# Установка нужных пакетов
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")


# Подключение библиотек
library(quantmod)
library(dplyr)
library(ggplot2)

### Шаг 2: Выбор тикеров и загрузка цен ----
# Список тикеров из разных секторов (можно будет скорректировать под задание)
tickers <- c("INTC", "AMD", "TXN", "AVGO", "MRVL", 
             "RMBS", "QCOM", "LSCC", "ADI", "MCHP", "SOXX")  # Пример: полупроводники
# Лучше разнообразить сектора: можно добавить акции из финансов, потребительских товаров, здравоохранения и т.д.

# Дата начала (5 лет назад)
start_date <- as.Date("2019-03-31")
end_date <- Sys.Date()


# Функция для загрузки Adjusted Close
get_monthly_prices <- function(ticker) {
        tryCatch({
                data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
                adj <- Ad(data)
                monthly <- to.monthly(adj, indexAt = "lastof", OHLC = FALSE)
                colnames(monthly) <- ticker
                return(monthly)
        }, error = function(e) {
                message(paste("Error:", ticker))
                return(NULL)
        })
}

# Загрузка всех акций
price_list <- lapply(tickers, get_monthly_prices)
price_xts <- do.call(merge, price_list)


### Шаг 3: Загрузка данных о 3-месячных T-Bills ----
# Загрузка T-Bills с FRED
getSymbols("TB3MS", src = "FRED")
tbill_data <- TB3MS
# Преобразование в месячные простые доходности
tbill_monthly <- tbill_data["2019/"]
tbill_monthly <- na.approx(tbill_monthly) / 100 / 12  # Преобразование в доли и деление на 12
colnames(tbill_monthly) <- "TBill_Return"

### Шаг 4: Расчет месячных доходностей ----
# Используем формулу (1a) из задания: доходность по Adjusted Close
# Используем lag из xts/zoo
monthly_returns <- diff(price_xts) / lag.xts(price_xts, k = 1)
monthly_returns <- na.omit(monthly_returns)


# Совмещаем с доходностями T-Bills по дате
# Приведение индекса T-Bills к последнему дню месяца
index(tbill_monthly) <- as.Date(as.yearmon(index(tbill_monthly)), frac = 1)

# Повторная подгонка по дате
tbill_trimmed <- tbill_monthly[index(monthly_returns)]

# Объединяем снова
data_combined <- merge(monthly_returns, tbill_trimmed)

# В дата-фрейм
returns_df <- fortify.zoo(data_combined)

# Проверим
head(returns_df, 10)


# Визуализация: сравнение доходностей одной акции и T-Bills
library(ggplot2)
ggplot(returns_df, aes(x = Index)) +
        geom_line(aes(y = INTC, color = "INTC")) +
        geom_line(aes(y = TBill_Return, color = "T-Bill")) +
        labs(title = "INTC vs T-Bill Monthly Returns",
             x = "Date", y = "Return") +
        scale_color_manual(values = c("INTC" = "blue", "T-Bill" = "red"))


