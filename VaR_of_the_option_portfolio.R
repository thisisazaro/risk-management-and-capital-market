

library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)

# NVIDIA (NVDA)
# Один из мировых лидеров по производству GPU. Акции на историческом максимуме 
# из-за бума искусственного интеллекта. Отличный кандидат для call опциона (на рост).

# Taiwan Semiconductor Manufacturing Company (TSM)
# Крупнейший контрактный производитель микросхем. Стратегически важная компания, 
# поставляет чипы для Apple, AMD, и даже NVIDIA. Можно взять put опцион, 
# если предполагается краткосрочная волатильность из-за геополитических рисков.

symbols <- c("NVDA", "TSM")
getSymbols(symbols, from = Sys.Date() - 750, auto.assign = TRUE)

# Получаем ежедневные лог-доходности
returns_NVDA <- dailyReturn(Ad(NVDA), type = "log")
returns_TSM <- dailyReturn(Ad(TSM), type = "log")

plot(returns_NVDA)
plot(returns_TSM)

# Сохраняем последние 500 значений
returns_df <- merge(returns_NVDA, returns_TSM)
returns_df <- tail(returns_df, 500)
colnames(returns_df) <- c("NVDA", "TSM")

returns_df

# Комбинация
# NVDA (Long Call) — ставка на рост из-за продолжающегося ИИ-бума.
# TSM (Short Put) — зарабатываем, если акции не упадут сильно, но готовы выкупить по страйку.

# 2. Волатильности и корреляция

# Стандартные отклонения дневных доходностей
vol_NVDA <- sd(returns_df$NVDA)
vol_TSM <- sd(returns_df$TSM)

# Годовая волатильность (252 торговых дня)
vol_NVDA_annual <- vol_NVDA * sqrt(252)
vol_TSM_annual <- vol_TSM * sqrt(252)

# Вывод
cat("Annualized Volatility - NVDA:", round(vol_NVDA_annual, 4), "\n")
cat("Annualized Volatility - TSM :", round(vol_TSM_annual, 4), "\n")

# 3. Корреляция между акциями
correlation <- cor(returns_df$NVDA, returns_df$TSM)
cat("Correlation between NVDA and TSM:", round(correlation, 4), "\n")

# 3. Добавим безрисковую ставку
# будем использовать данные по 3-месячным казначейским векселям (3-Month T-Bill) 
# от источника FRED (Federal Reserve Economic Data). 
# Серия векселей "DTB3" — 3-Month Treasury Bill: Secondary Market Rate

# Загружаем безрисковую ставку по тикеру "DTB3"
getSymbols("DTB3", src = "FRED")

# Приводим к нужному диапазону и берем последние 500 значений (как для акций)
risk_free_raw <- na.omit(DTB3)
risk_free_data <- tail(risk_free_raw, 500)

# Средняя ставка за период
avg_rf <- mean(risk_free_data) / 100  # делим на 100 для перевода в доли

# Вывод
cat("Average annualized risk-free rate (3M T-Bill):", round(avg_rf, 4), "\n")


## СОХРАНЯЕМ В EXCEL
library(writexl)

# 1. Summary
summary_df <- data.frame(
        Metric = c("Annualized Volatility NVDA", 
                   "Annualized Volatility TSM", 
                   "Correlation NVDA-TSM", 
                   "Risk-free Rate"),
        Value = c(round(vol_NVDA_annual, 4), 
                  round(vol_TSM_annual, 4), 
                  round(correlation, 4), 
                  round(avg_rf, 4))
)

# 2. returns_export (с датой как колонкой)
returns_export <- data.frame(
        Date = index(returns_df),
        coredata(returns_df)
)

# 3. returns_df_as_table — тоже с датой как колонкой (тот же результат)
returns_df_as_table <- data.frame(
        Date = index(returns_df),
        coredata(returns_df)
)

# 4. Save to Excel
write_xlsx(
        list(
                "Summary"         = summary_df,
                "Returns_xts"     = returns_df_as_table,
                "Returns_table"   = returns_export
        ),
        path = "NVDA_TSM_analysis.xlsx"
)


# 5. Задаём параметры опционов

# Предположим, NVDA — европейский Call, позиция Long, т.к. ставка на рост GPU/ИИ-сектора.
# TSM — европейский Put, позиция Short, т.к. ставка на фундаментальную устойчивость при волатильности.
# Срок до экспирации: 30 календарных дней.
# Страйки: Call NVDA: 5% выше текущей цены. Put TSM: 5% ниже текущей цены.

# Срок опциона
T_days <- 30
T <- T_days / 252  # в годах

# Последние цены акций
S_NVDA <- as.numeric(last(Cl(NVDA)))
S_TSM  <- as.numeric(last(Cl(TSM)))

# Страйки (±5%)
K_NVDA <- S_NVDA * 1.05  # для call
K_TSM  <- S_TSM * 0.95   # для put

# Типы и позиции
type_NVDA <- "call"
type_TSM  <- "put"
position_NVDA <- 1    # Long call
position_TSM  <- -1   # Short put

# Расчет премий опционов (Black-Scholes)
# Black-Scholes функция
bs_price <- function(S, K, r, sigma, T, type) {
        d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
        d2 <- d1 - sigma * sqrt(T)
        if (type == "call") {
                price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
        } else if (type == "put") {
                price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
        } else {
                stop("error")
        }
        return(price)
}


# Расчёт цен опционов
price_NVDA <- bs_price(S_NVDA, K_NVDA, avg_rf, vol_NVDA_annual, T, type_NVDA)
price_TSM  <- bs_price(S_TSM,  K_TSM,  avg_rf, vol_TSM_annual,  T, type_TSM)

cat("NVDA: Long Call | S =", round(S_NVDA, 2), "| K =", round(K_NVDA, 2), "| T =", T_days, "days | Price =", round(price_NVDA, 2), "\n")
cat("TSM : Short Put | S =", round(S_TSM, 2),  "| K =", round(K_TSM, 2),  "| T =", T_days, "days | Price =", round(price_TSM, 2), "\n")

# NVDA — длинная позиция по опциону Call
# Цель: Получение прибыли за счёт ожидаемого роста акций.
# Обоснование: Компания NVIDIA занимает ключевое положение в индустрии искусственного интеллекта 
# и производства графических процессоров. Спрос на её продукцию стремительно растёт, 
# особенно в сфере машинного обучения и дата-центров.
# Почему именно этот опцион: Покупка опциона Call позволяет получить прибыль при росте акций, 
# при этом ограничивая потенциальные убытки размером уплаченной премии.
# Ожидания: Если акции NVDA продолжат расти, стоимость опциона существенно увеличится 
# за счёт высокой дельты и чувствительности к волатильности (вега).

# TSM — короткая позиция по опциону Put
# Цель: Получение дохода с ограниченным риском.
# Обоснование: Тайваньская компания TSMC — крупнейший в мире производитель микросхем, 
# обладающий стабильной репутацией и сильными фундаментальными показателями. 
# Несмотря на геополитические риски, значительного падения акций в ближайшее время не ожидается.
# Почему именно этот опцион: Продажа опциона Put позволяет заработать на временном 
# распаде стоимости опциона. Даже если цена немного снизится, вы готовы купить акции 
# по цене страйка — с 5% дисконтом.
# Ожидания: Если акции останутся выше страйка, вы сохраняете всю полученную премию. 
# Даже в случае исполнения опциона, покупка TSM по более низкой цене выглядит разумным вложением.

# Комбинация длинного call-опциона на растущую технологичную компанию (NVDA) и 
# короткого put-опциона на стабильного производственника (TSM) позволяет реализовать 
# гибридную стратегию, сочетающую спекулятивный потенциал и стабильный премиальный доход. 
# Стратегия сбалансирована: одна позиция выигрывает от роста, вторая — от стабильности.

library(ggplot2)
library(scales)
library(lubridate)


## Динамика лог-доходностей NVDA и TSM

returns_long <- data.frame(
        Date = index(returns_df),
        NVDA = coredata(returns_df$NVDA),
        TSM  = coredata(returns_df$TSM)
) %>%
        pivot_longer(cols = c("NVDA", "TSM"), names_to = "Ticker", values_to = "Return")

ggplot(returns_long, aes(x = Date, y = Return, color = Ticker)) +
        geom_line(alpha = 0.8, linewidth = 0.6) +
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        scale_color_manual(values = c("NVDA" = "#00FF00", "TSM" = "#FFA500")) +
        labs(title = "Daily Log Returns: NVDA vs TSM",
             subtitle = "500 most recent trading days",
             x = "Date", y = "Log Return",
             caption = "Source: Bloomberg ") +
        theme_dark() +
        theme(
                plot.title = element_text(face = "bold", size = 16, color = "darkblue"),
                plot.subtitle = element_text(color = "gray10"),
                plot.caption = element_text(color = "gray60"),
                axis.text = element_text(color = "gray10"),
                axis.title = element_text(color = "gray10"),
                legend.title = element_blank(),
                legend.text = element_text(color = "gray20"),
                panel.grid.major = element_line(color = "gray30")
        )


## Сравнение волатильностей
vol_data <- data.frame(
        Ticker = c("NVDA", "TSM"),
        Volatility = c(vol_NVDA_annual, vol_TSM_annual)
)

ggplot(vol_data, aes(x = Ticker, y = Volatility, fill = Ticker)) +
        geom_col(width = 0.5) +
        scale_fill_manual(values = c("NVDA" = "#00FF00", "TSM" = "#FFA500")) +
        scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
        labs(title = "Annualized Volatility",
             y = "Volatility", x = "",
             caption = "500-day rolling volatility") +
        theme_dark() +
        theme(
                plot.title = element_text(face = "bold", size = 16, color = "darkblue"),
                plot.caption = element_text(color = "gray60"),
                axis.text = element_text(color = "gray10"),
                axis.title = element_text(color = "gray10"),
                legend.position = "none",
                panel.grid.major = element_line(color = "gray30")
        )



