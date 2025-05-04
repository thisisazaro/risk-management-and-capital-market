

library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(kableExtra)
library(ggplot2)
library(scales)
library(lubridate)

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
cat("Annualized Volatility - NVDA:", round(vol_NVDA_annual, 4), "\n")
cat("Annualized Volatility - TSM :", round(vol_TSM_annual, 4), "\n")

# Корреляция между акциями
correlation <- cor(returns_df$NVDA, returns_df$TSM)
cat("Correlation between NVDA and TSM:", round(correlation, 4), "\n")

# Добавим безрисковую ставку
# будем использовать данные по 3-месячным казначейским векселям (3-Month T-Bill) 
# от источника FRED (Federal Reserve Economic Data). 
# Серия векселей "DTB3" — 3-Month Treasury Bill: Secondary Market Rate

# Загружаем безрисковую ставку по тикеру "DTB3"
getSymbols("DTB3", src = "FRED")

# Приводим к нужному диапазону и берем последние 500 значений (как для акций)
risk_free_raw <- na.omit(DTB3)
risk_free_data <- tail(risk_free_raw, 500)
avg_rf <- mean(risk_free_data) / 100  # делим на 100 для перевода в доли
cat("Average annualized risk-free rate (3M T-Bill):", round(avg_rf, 4), "\n")

## Save to Excel
library(writexl)
summary_df <- data.frame(
        Metric = c("Annualized Volatility NVDA", "Annualized Volatility TSM", 
                   "Correlation NVDA-TSM", "Risk-free Rate"),
        Value = c(round(vol_NVDA_annual, 4), round(vol_TSM_annual, 4), 
                  round(correlation, 4), round(avg_rf, 4))
)

returns_export <- data.frame(
        Date = index(returns_df),
        coredata(returns_df)
)

returns_df_as_table <- data.frame(
        Date = index(returns_df),
        coredata(returns_df)
)

write_xlsx(
        list("Summary" = summary_df, 
             "Returns_xts" = returns_df_as_table,
             "Returns_table" = returns_export),
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
library(patchwork)

# Подготовка таблицы параметров
option_df <- data.frame(
        Stock = c("NVDA", "TSM"),
        Option_Type = c("Call", "Put"),
        Position = c("Long", "Short"),
        Strike = c(K_NVDA, K_TSM),
        Spot = c(S_NVDA, S_TSM),
        Option_Price = c(price_NVDA, price_TSM)
)

# График 1: Премии опционов
chart1 <- ggplot(option_df, aes(x = Stock, y = Option_Price, fill = Option_Type)) +
        geom_col(width = 0.5, alpha = 0.85) +
        scale_fill_manual(values = c("Call" = "#00FF00", "Put" = "#FFA500")) +
        labs(title = "Option Premiums", subtitle = "NVDA (Call) & TSM (Put)",
             y = "Premium (USD)", x = NULL, fill = "Option Type") +
        theme_dark() +
        theme(plot.title = element_text(face = "bold", color = "darkblue", size = 16),
              plot.subtitle = element_text(color = "gray40"),
              axis.text = element_text(color = "gray10"),
              axis.title = element_text(color = "gray10"),
              legend.text = element_text(color = "gray10"),
              legend.title = element_text(color = "gray10"),
              panel.grid.major = element_line(color = "gray30")
        )

# График 2: Сравнение Spot и Strike
chart2 <- ggplot(option_df, aes(x = Stock)) +
        geom_point(aes(y = Spot), color = "cyan", size = 4) +
        geom_point(aes(y = Strike), color = "red", shape = 4, size = 4) +
        geom_segment(aes(x = Stock, xend = Stock, y = Spot, yend = Strike),
                     color = "gray70", linetype = "dashed") +
        labs(title = "Spot vs Strike Price",
             subtitle = "Comparing market and strike levels",
             y = "Price (USD)", x = NULL) +
        theme_dark() +
        theme(plot.title = element_text(face = "bold", color = "darkblue", size = 16),
              plot.subtitle = element_text(color = "gray40"),
              axis.text = element_text(color = "gray10"),
              axis.title = element_text(color = "gray10"),
              panel.grid.major = element_line(color = "gray30")
        )

# Объединение графиков
combined_plot <- chart1 + chart2
combined_plot

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

## III.	Deliverables

### 3.1. Calculate 99% 10-day VaR of the option part of your portfolio 
# using delta-normal (linear) model:
# (i)	Find deltas of both options 
# (ii)	Write the delta-linear model for ∆P
# (iii)	Calculate the variance of the portfolio 
# (iv)	Find VaR


# 1. Delta-Normal (Линейная модель)
# Шаг 1: Расчет дельт
bs_delta <- function(S, K, r, sigma, T_annual, type) {
        d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T_annual) / (sigma * sqrt(T_annual))
        if (type == "call") return(pnorm(d1))
        if (type == "put")  return(pnorm(d1) - 1)
        stop("Unknown option type")
}

delta_NVDA <- bs_delta(S_NVDA, K_NVDA, avg_rf, vol_NVDA_annual, T, "call")
delta_NVDA
delta_TSM  <- bs_delta(S_TSM,  K_TSM,  avg_rf, vol_TSM_annual,  T, "put")
delta_TSM

# Шаг 2: Линейная модель ∆P и расчет дисперсии портфеля
# Веса портфеля (позиции в одном контракте каждого опциона)
w_NVDA <- 1
w_TSM  <- -1

# 10-дневная ковариационная матрица
sigma_matrix <- matrix(c(
        vol_NVDA_annual^2, correlation * vol_NVDA_annual * vol_TSM_annual,
        correlation * vol_NVDA_annual * vol_TSM_annual, vol_TSM_annual^2
), nrow = 2) * (10 / 252)
sigma_matrix

# Вектор дельт и позиций
delta_vec <- c(delta_NVDA * S_NVDA, delta_TSM * S_TSM)
position_vec <- c(w_NVDA, w_TSM)

# Вар = Z * sqrt(w'Σw)
portfolio_var <- t(delta_vec * position_vec) %*% sigma_matrix %*% (delta_vec * position_vec)
VaR_delta_normal <- qnorm(0.99) * sqrt(portfolio_var)
VaR_delta_normal

# VaR (Delta-Normal, 99%, 10 days): ~ $21.65
# С 99% уверенностью, потери по опционному портфелю NVDA (Long Call) и TSM (Short Put) 
# не превысят $21.65 в течение 10 торговых дней, при условии, что изменения цен акций 
# подчиняются нормальному распределению и используются только дельты.

library(ggplot2)

# Генерируем нормальное распределение убытков
x_vals <- seq(-3, 3, length.out = 1000)
mean_p <- 0
sd_p <- sqrt(portfolio_var)

# Значения плотности
density_vals <- dnorm(x_vals, mean = mean_p, sd = sd_p)
df_density <- data.frame(
        Return = x_vals * as.numeric(sd_p),
        Density = density_vals
)

# Граница VaR
VaR_val <- as.numeric(-VaR_delta_normal)

# График
ggplot(df_density, aes(x = Return, y = Density)) +
        geom_line(color = "cyan", linewidth = 1) +
        geom_area(data = subset(df_density, Return <= VaR_val), 
                  aes(x = Return, y = Density), fill = "red", alpha = 0.6) +
        geom_vline(xintercept = VaR_val, linetype = "dashed", color = "red", linewidth = 1) +
        annotate("text", x = VaR_val + 1, y = max(density_vals) * 0.95,
                 label = paste0("VaR = $", round(-VaR_val, 2)), color = "red", hjust = 0, size = 5) +
        labs(title = "Delta-Normal VaR (99%, 10 Days)",
             subtitle = "Simulated Loss Distribution under Normality",
             x = "Change in Portfolio Value (USD)",
             y = "Density") +
        theme_dark() +
        theme(
                plot.background = element_rect(fill = "#1e1e1e"),
                panel.background = element_rect(fill = "#1e1e1e"),
                plot.title = element_text(face = "bold", size = 16, color = "white"),
                plot.subtitle = element_text(size = 12, color = "gray70"),
                axis.title = element_text(size = 13, color = "gray90"),
                axis.text = element_text(color = "gray80"),
                panel.grid.major = element_line(color = "gray30", linewidth = 0.2),
                panel.grid.minor = element_blank()
        )


# 2. модель Delta-Gamma VaR (Квадратичная модель)

# Шаг 1: Расчет гамм
bs_gamma <- function(S, K, r, sigma, T_annual) {
        d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
        return(dnorm(d1) / (S * sigma * sqrt(T)))
}

gamma_NVDA <- bs_gamma(S_NVDA, K_NVDA, avg_rf, vol_NVDA_annual, T)
gamma_TSM  <- bs_gamma(S_TSM,  K_TSM,  avg_rf, vol_TSM_annual,  T)

# Шаг 2: Первый и второй момент ∆P и VaR
        
# 10-дневный хи-сигма
sigmas_10 <- c(vol_NVDA_annual, vol_TSM_annual) * sqrt(10 / 252)

# mu = deltaᵀ * sigma * ε = 0 (в ожидании)
# sigma² = delta'Σdelta + 0.5 * trace(gamma²Σ²)
variance_delta <- t(delta_vec * position_vec) %*% sigma_matrix %*% (delta_vec * position_vec)

gamma_vec <- c(gamma_NVDA * S_NVDA^2, gamma_TSM * S_TSM^2) * position_vec
gamma_var <- 0.5 * sum((gamma_vec^2) * (sigmas_10^4))

var_total <- variance_delta + gamma_var
VaR_delta_gamma <- qnorm(0.99) * sqrt(var_total)
VaR_delta_gamma

# VaR (Delta-Gamma, 99%, 10 дней): ~ $22.66
# Учитывая как чувствительность к небольшим изменениям цены (дельта), так и изменение 
# чувствительности при более крупных колебаниях (гамма), 99% VaR по твоему портфелю 
# за 10 дней составляет $22.66.

# По сравнению с линейной моделью (Delta-Normal) в $21.65, здесь риск чуть выше 
# из-за учёта нелинейных потерь, особенно актуальных для опционов, находящихся вблизи страйка.

library(ggplot2)

set.seed(123)
N <- 10000  # число симуляций

# Генерация нормальных случайных величин
Z1 <- rnorm(N)
rho <- as.numeric(correlation)  # явное приведение
Z2 <- rho * Z1 + sqrt(1 - rho^2) * rnorm(N)

# 10-дневные лог-доходности
dx1 <- sigmas_10[1] * Z1
dx2 <- sigmas_10[2] * Z2

# Расчёт изменений стоимости портфеля с гаммой
dP_dg <- (delta_vec[1] * dx1 + delta_vec[2] * dx2) +
        0.5 * (gamma_vec[1] * dx1^2 + gamma_vec[2] * dx2^2)

# Эмпирическое значение VaR
VaR_emp_dg <- -quantile(dP_dg, 0.01)

# Построение графика
df_dg <- data.frame(Change = dP_dg)

ggplot(df_dg, aes(x = Change)) +
        geom_density(fill = "#00bfc4", alpha = 0.5) +
        geom_vline(xintercept = -VaR_emp_dg, color = "red", linetype = "dashed", linewidth = 1) +
        annotate("text", x = -VaR_emp_dg + 2, y = max(density(df_dg$Change)$y) * -0.07,
                 label = paste0("99% VaR = $", round(VaR_emp_dg, 2)),
                 color = "red", hjust = 0, size = 5) +
        labs(
                title = "Delta-Gamma VaR (99%, 10 Days)",
                subtitle = "Simulated Distribution with Nonlinearity (Gamma)",
                x = "Change in Portfolio Value (USD)",
                y = "Density"
        ) +
        theme_dark() +
        theme(
                plot.background = element_rect(fill = "#1e1e1e"),
                panel.background = element_rect(fill = "#1e1e1e"),
                plot.title = element_text(face = "bold", size = 16, color = "white"),
                plot.subtitle = element_text(size = 12, color = "gray70"),
                axis.title = element_text(size = 13, color = "gray90"),
                axis.text = element_text(color = "gray80"),
                panel.grid.major = element_line(color = "gray30", linewidth = 0.2),
                panel.grid.minor = element_blank()
        )


# 3. Monte Carlo с дельтой и гаммой (partial simulation)

set.seed(42)
N <- 5000

# Генерация MVN
Z1 <- rnorm(N)
rho <- as.numeric(correlation)  # явное приведение
Z2 <- rho * Z1 + sqrt(1 - rho^2) * rnorm(N)

# Симулированные ∆x (log-returns)
dx_NVDA <- sigmas_10[1] * Z1
dx_TSM  <- sigmas_10[2] * Z2

# Расчет ∆P по дельта-гамма модели
dP <- (delta_vec[1] * dx_NVDA + delta_vec[2] * dx_TSM) +
        0.5 * (gamma_vec[1] * dx_NVDA^2 + gamma_vec[2] * dx_TSM^2)

VaR_mc_partial <- -quantile(dP, 0.01)
VaR_mc_partial

# VaR (Delta-Gamma Monte Carlo, 99%, 10 дней): ~ $10.57
# Используя симуляции на основе многомерного нормального распределения с учётом 
# дельты и гаммы (но без пересчёта цены через Black-Scholes), мы получили, что с 
# вероятностью 99% потери не превысят $10.57 за 10 дней.


# 4. Full Monte Carlo (Black-Scholes revaluation)

T1 <- (T_days - 1) / 252  # время до экспирации после 1 дня

# Функция переоценки опциона
reprice_option <- function(S_new, S0, K, r, sigma, T, T_new, type) {
        old_price <- bs_price(S0, K, r, sigma, T, type)
        new_price <- bs_price(S_new, K, r, sigma, T_new, type)
        return(new_price - old_price)
}

# Симулируем цены (логнорм. модель без дрейфа)
S_NVDA_sim <- S_NVDA * exp(dx_NVDA)
S_TSM_sim  <- S_TSM * exp(dx_TSM)

# Пересчет цены
delta_NVDA_MC <- mapply(reprice_option, S_NVDA_sim, MoreArgs = list(
        S0 = S_NVDA, K = K_NVDA, r = avg_rf, sigma = vol_NVDA_annual, 
        T = T, T_new = T1, type = "call"
))
delta_TSM_MC <- mapply(reprice_option, S_TSM_sim, MoreArgs = list(
        S0 = S_TSM, K = K_TSM, r = avg_rf, sigma = vol_TSM_annual, 
        T = T, T_new = T1, type = "put"
))

# Суммарное изменение портфеля
dP_full <- position_vec[1] * delta_NVDA_MC + position_vec[2] * delta_TSM_MC
VaR_full_mc <- -quantile(dP_full, 0.01)
VaR_full_mc

# Full Monte Carlo Simulation с переоценкой опционов по формуле Блэка-Шоулза после 1 дня.
# VaR (Full Monte Carlo, 99%, 10 дней): ~ $22.05

# Финальная таблица результатов
full_table_results <- data.frame(
        Method = c("Delta-Normal", "Delta-Gamma", "Monte Carlo (partial)", "Monte Carlo (full BS)"),
        VaR_99_10day = round(c(VaR_delta_normal, VaR_delta_gamma, VaR_mc_partial, VaR_full_mc), 2)
)

kable(full_table_results)

# Выводы:
# Delta-Gamma analytic и Full MC дают самые реалистичные оценки — около $22.
# Partial MC (с линейной симуляцией) занижает риск, т.к. не переоценивает опционы.
# Delta-Normal чуть занижает VaR, не учитывая кривизну (гамму).

# Сравнение VaR разных моделей (bar chart)

library(ggplot2)

# График сравнения VaR
ggplot(full_table_results, aes(x = Method, y = VaR_99_10day, fill = Method)) +
        geom_col(width = 0.6) +
        geom_text(aes(label = paste0("$", VaR_99_10day)), vjust = -0.5, color = "white", size = 5) +
        labs(title = "Comparison of 99% 10-day VaR by Method",
             subtitle = "Delta vs Gamma vs Monte Carlo Approaches",
             x = NULL, y = "VaR (USD)") +
        scale_fill_manual(values = c("#00bfc4", "#f8766d", "#7cae00", "#c77cff")) +
        theme_dark() +
        theme(
                plot.background = element_rect(fill = "#1e1e1e"),
                panel.background = element_rect(fill = "#1e1e1e"),
                plot.title = element_text(face = "bold", size = 16, color = "white"),
                plot.subtitle = element_text(size = 12, color = "gray70"),
                axis.title = element_text(size = 13, color = "gray90"),
                axis.text = element_text(color = "gray80"),
                panel.grid.major = element_line(color = "gray30", linewidth = 0.2),
                legend.position = "none"
        )


# Histogram of Portfolio PnL (Full Monte Carlo)

ggplot(data.frame(Change = dP_full), aes(x = Change)) +
        geom_histogram(fill = "#00bfc4", bins = 60, color = "black") +
        geom_vline(xintercept = -VaR_full_mc, color = "red", linetype = "dashed", linewidth = 1) +
        annotate("text", x = -VaR_full_mc + 2, y = max(table(cut(dP_full, 60))) * 0.75,
                 label = paste0("99% VaR = $", round(VaR_full_mc, 2)),
                 color = "red", hjust = 0, size = 5) +
        labs(title = "Full Monte Carlo VaR: Distribution of Option Portfolio PnL",
             subtitle = "Histogram of Simulated Changes (10-day horizon)",
             x = "Change in Portfolio Value (USD)",
             y = "Frequency") +
        theme_dark() +
        theme(
                plot.background = element_rect(fill = "#1e1e1e"),
                panel.background = element_rect(fill = "#1e1e1e"),
                plot.title = element_text(face = "bold", size = 16, color = "white"),
                plot.subtitle = element_text(size = 12, color = "gray70"),
                axis.title = element_text(size = 13, color = "gray90"),
                axis.text = element_text(color = "gray80"),
                panel.grid.major = element_line(color = "gray30", linewidth = 0.2)
        )


# Sensitivity Chart: Delta & Gamma of Options

S_range <- seq(0.8 * S_NVDA, 1.2 * S_NVDA, length.out = 100)

# Дельта и гамма для Call на NVDA
delta_vals <- sapply(S_range, bs_delta, K = K_NVDA, r = avg_rf, sigma = vol_NVDA_annual, T_annual = T, type = "call")
gamma_vals <- sapply(S_range, bs_gamma, K = K_NVDA, r = avg_rf, sigma = vol_NVDA_annual, T_annual = T)

df_sensitivity <- data.frame(Price = S_range, Delta = delta_vals, Gamma = gamma_vals)

# График
library(tidyr)
df_long <- pivot_longer(df_sensitivity, cols = c("Delta", "Gamma"), names_to = "Metric", values_to = "Value")

ggplot(df_long, aes(x = Price, y = Value, color = Metric)) +
        geom_line(size = 1) +
        labs(title = "Sensitivity of NVDA Call Option",
             subtitle = "Delta and Gamma vs Underlying Price",
             x = "Underlying Price (NVDA)", y = "Sensitivity Value") +
        scale_color_manual(values = c("Delta" = "#00bfc4", "Gamma" = "#f8766d")) +
        geom_vline(xintercept = S_NVDA, linetype = "dotted", color = "gray70") +
        annotate("text", x = S_NVDA + 1, y = 0.85, label = "Current Price", color = "gray70", hjust = 0) +
        theme_dark() +
        theme(
                plot.background = element_rect(fill = "#1e1e1e"),
                panel.background = element_rect(fill = "#1e1e1e"),
                plot.title = element_text(face = "bold", size = 16, color = "white"),
                plot.subtitle = element_text(size = 12, color = "gray70"),
                axis.title = element_text(size = 13, color = "gray90"),
                axis.text = element_text(color = "gray80"),
                legend.title = element_blank(),
                panel.grid.major = element_line(color = "gray30", linewidth = 0.2)
        )



# Sensitivity of TSM Put Option (Delta & Gamma)

# Диапазон цен TSM (±20%)
S_range_tsm <- seq(0.8 * S_TSM, 1.2 * S_TSM, length.out = 100)

# Расчёт дельты и гаммы для Put
delta_vals_tsm <- sapply(S_range_tsm, bs_delta, 
                         K = K_TSM, r = avg_rf, sigma = vol_TSM_annual, T_annual = T, type = "put")
gamma_vals_tsm <- sapply(S_range_tsm, bs_gamma, 
                         K = K_TSM, r = avg_rf, sigma = vol_TSM_annual, T_annual = T)

# Таблица
df_sensitivity_tsm <- data.frame(
        Price = S_range_tsm,
        Delta = delta_vals_tsm,
        Gamma = gamma_vals_tsm
)

# Перевод в long-формат
library(tidyr)
df_long_tsm <- pivot_longer(df_sensitivity_tsm, cols = c("Delta", "Gamma"), names_to = "Metric", values_to = "Value")

# Построение графика
library(ggplot2)
ggplot(df_long_tsm, aes(x = Price, y = Value, color = Metric)) +
        geom_line(size = 1) +
        labs(
                title = "Sensitivity of TSM Put Option",
                subtitle = "Delta and Gamma vs Underlying Price",
                x = "Underlying Price (TSM)", y = "Sensitivity Value"
        ) +
        scale_color_manual(values = c("Delta" = "#00bfc4", "Gamma" = "#f8766d")) +
        theme_dark() +
        theme(
                plot.background = element_rect(fill = "#1e1e1e"),
                panel.background = element_rect(fill = "#1e1e1e"),
                plot.title = element_text(face = "bold", size = 16, color = "white"),
                plot.subtitle = element_text(size = 12, color = "gray70"),
                axis.title = element_text(size = 13, color = "gray90"),
                axis.text = element_text(color = "gray80"),
                legend.title = element_blank(),
                panel.grid.major = element_line(color = "gray30", linewidth = 0.2)
        )


# Delta ожидаемо идёт от -1 к 0, отражая поведение Put-опциона: при низкой цене 
# базового актива (deep ITM) — дельта ≈ -1, при высокой цене (deep OTM) — дельта ≈ 0
# Gamma положительная и образует колокол вблизи страйка, что также типично для 
# опционов, чувствительных к цене возле точки «на деньгах».



# таблицу всех основных Greeks (Delta, Gamma, Vega, Theta) для NVDA Call и TSM Put 

# Общая функция греков
bs_greeks <- function(S, K, r, sigma, T, type) {
        d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
        d2 <- d1 - sigma * sqrt(T)
        delta <- if (type == "call") pnorm(d1) else pnorm(d1) - 1
        gamma <- dnorm(d1) / (S * sigma * sqrt(T))
        vega  <- S * dnorm(d1) * sqrt(T) / 100  # на 1% изменения волатильности
        theta <- if (type == "call") {
                (-S * dnorm(d1) * sigma / (2 * sqrt(T)) - r * K * exp(-r * T) * pnorm(d2)) / 252
        } else {
                (-S * dnorm(d1) * sigma / (2 * sqrt(T)) + r * K * exp(-r * T) * pnorm(-d2)) / 252
        }
        return(c(Delta = delta, Gamma = gamma, Vega = vega, Theta = theta))
}

# NVDA Call
greeks_nvda <- bs_greeks(S_NVDA, K_NVDA, avg_rf, vol_NVDA_annual, T, "call")

# TSM Put
greeks_tsm <- bs_greeks(S_TSM, K_TSM, avg_rf, vol_TSM_annual, T, "put")
# Создаем таблицу
greeks_table <- data.frame(
        Option = c("NVDA Call", "TSM Put"),
        Delta = c(greeks_nvda["Delta"], greeks_tsm["Delta"]),
        Gamma = c(greeks_nvda["Gamma"], greeks_tsm["Gamma"]),
        Vega  = c(greeks_nvda["Vega"], greeks_tsm["Vega"]),
        Theta = c(greeks_nvda["Theta"], greeks_tsm["Theta"])
)

# Отображаем красиво
library(knitr)
kable(greeks_table, digits = 4, caption = "Greeks Summary for Option Portfolio")

# Delta: чувствительность к изменению цены базового актива
# Gamma: чувствительность дельты
# Vega: чувствительность к изменению волатильности (на 1%)
# Theta: потери стоимости опциона в день (в долларах), т.е. временной распад

# Greek	Значение

# Delta	Направленный риск: если акция ↑ на $1 — насколько изменится цена опциона. 
# NVDA Call реагирует на рост, TSM Put — на падение.

# Gamma	Кривизна: насколько изменится дельта, если акция изменится на $1. Важно 
# при высокой волатильности.

# Vega	Чувствительность к изменению волатильности. Оба опциона реагируют сильно: 
# особенно важно во время отчетов компаний.

# Theta	Временной распад: сколько теряет опцион в день (в долларах). 
# Отрицательный — потеря стоимости с течением времени.

