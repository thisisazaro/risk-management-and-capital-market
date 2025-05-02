
# Установим необходимые пакеты
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(quadprog)

# Твои тикеры
tickers <- c("NVDA", "AMZN", "GOOGL", "CRM", "MCO",
             "PDBC", "FTGC", "COMB", "BND", "BSV", "GOVT", "BBRE")


#| Тикер | Название                                                             | Категория             | Что внутри                                                              |
#|--------|---------------------------------------------------------------------|------------------------|-------------------------------------------------------------------------|
#| PDBC   | Invesco Optimum Yield Diversified Commodity Strategy No K-1 ETF     | Коммодити ETF         | Фьючерсы на нефть, золото, кукурузу и др. (сырьевые товары)           |
#| FTGC   | First Trust Global Tactical Commodity Strategy Fund                 | Коммодити ETF         | Глобальные сырьевые активы + активное управление                       |
#| COMB   | GraniteShares Bloomberg Commodity Broad Strategy No K-1 ETF         | Коммодити ETF         | Широкий индекс сырья Bloomberg (агро, металлы, энергия)                |
#| BND    | Vanguard Total Bond Market ETF                                      | Облигации (всё)       | Государственные и корпоративные облигации США                          |
#| BSV    | Vanguard Short-Term Bond ETF                                        | Краткосрочные облиги  | Облигации со сроком до 3 лет                                           |
#| GOVT   | iShares US Treasury Bond ETF                                        | Госдолг США           | Только U.S. Treasuries (казначейские бумаги)                           |
#| BBRE   | JPMorgan BetaBuilders MSCI U.S. REIT ETF                            | Недвижимость (REITs)  | Инвестиции в американскую коммерческую недвижимость через REIT         |
        



# Загрузим данные за 2 года
getSymbols(tickers, from = Sys.Date() - 365*2, src = "yahoo")

# Доходности (log-returns)
prices <- map(tickers, ~ Ad(get(.x))) %>%
        reduce(merge) %>%
        na.omit()
prices
returns <- na.omit(Return.calculate(prices, method = "log"))
returns

# Средняя доходность и ковариация
mean_ret <- colMeans(returns)
mean_ret
cov_mat <- cov(returns)
cov_mat
# Безрисковая ставка (4% годовых)
rf <- 0.04 / 252

# Единичный вектор
ones <- rep(1, length(tickers))

# ---------------------------
# 🔹 1. Minimum Variance Portfolio (no short)
# ---------------------------

n_assets <- length(tickers)
Dmat <- 2 * cov_mat
dvec <- rep(0, n_assets)

# Ограничение: суммы = 1 и веса >= 0
Amat <- cbind(rep(1, n_assets), diag(n_assets))
bvec <- c(1, rep(0, n_assets))
meq <- 1

mve_constrained <- solve.QP(Dmat, dvec, Amat, bvec, meq)
w_mve_constrained <- round(mve_constrained$solution / sum(mve_constrained$solution), 4)

# Печатаем результат
w_mve_constrained



# ---------------------------
# 🔸 Tangency Portfolio (long-only, max Sharpe)
# ---------------------------

# 💡 Вектор относительных избыточных доходностей
excess_ret <- mean_ret - rf

# 💡 Считаем начальные веса до нормализации
raw_weights <- solve(cov_mat, excess_ret)  # = Σ⁻¹ * (μ − rf)
w_tan <- raw_weights / sum(raw_weights)    # нормализуем

# 💡 Обнулим отрицательные веса, снова нормализуем (long-only)
w_tan_longonly <- pmax(w_tan, 0)
w_tan_longonly <- w_tan_longonly / sum(w_tan_longonly)

# Выводим финальные веса
w_tan_longonly <- round(w_tan_longonly, 4)
names(w_tan_longonly) <- colnames(returns)
w_tan_longonly

# ---------------------------
# 💡 Вывод: веса
# ---------------------------

w_df <- tibble(
        Ticker = tickers,
        MeanReturn = round(mean_ret * 252, 3),  # годовая доходность
        MVE_Weight = round(w_mve_constrained, 4),
        Tangency_Weight = round(w_tan_longonly, 4)
)

print(w_df)




# 🔍 Ожидаемая доходность, риск и Sharpe для произвольного портфеля
analyze_portfolio <- function(w, mean_ret, cov_mat, rf, label = "Portfolio") {
        mu <- sum(w * mean_ret)
        sigma <- sqrt(t(w) %*% cov_mat %*% w)
        sharpe <- (mu - rf) / sigma
        tibble(
                Name = label,
                ExpectedReturn = round(mu * 252, 4),    # годовая
                StdDev = round(sigma * sqrt(252), 4),   # годовая
                Sharpe = round(sharpe * sqrt(252), 4)   # годовой SR
        )
}

# Анализ MVE и Tangency
mve_stats <- analyze_portfolio(w_mve_constrained, mean_ret, cov_mat, rf, "MVE (min variance)")
tan_stats <- analyze_portfolio(w_tan_longonly, mean_ret, cov_mat, rf, "Tangency (max Sharpe)")

bind_rows(mve_stats, tan_stats)





allocation_mve <- round(w_mve_constrained * 10000, 2)
allocation_tan <- round(w_tan_longonly * 10000, 2)

allocation_df <- tibble(
        Ticker = names(w_tan_longonly),
        MVE_Allocation = allocation_mve,
        Tangency_Allocation = allocation_tan
)

allocation_df



# Уровень доверия
alpha <- 0.05

# Возьмём доходности твоих активов
# returns — уже готовые лог-доходности (n × 12)
# w_tan_longonly — веса Tangency (вектор длины 12)

# Расчёт доходности портфеля
portf_returns_tan <- as.numeric(returns %*% w_tan_longonly)

# Historical VaR
VaR_hist <- quantile(portf_returns_tan, probs = alpha)

# Historical ES (среднее условное падение ниже VaR)
ES_hist <- mean(portf_returns_tan[portf_returns_tan <= VaR_hist])

# Преобразуем в годовые (если хочешь)
VaR_annual <- VaR_hist * sqrt(252)
ES_annual <- ES_hist * sqrt(252)

cat("📉 Value at Risk (5%):", round(VaR_hist, 4), " (daily), ", round(VaR_annual, 4), " (annual)\n")
cat("📉 Expected Shortfall (5%):", round(ES_hist, 4), " (daily), ", round(ES_annual, 4), " (annual)\n")

# Если ты инвестируешь $1000 в Tangency-портфель: 
# VaR (годовой) ≈ $53.20
# ES (годовой) ≈ $75.00
# То есть: С вероятностью 95% ты не потеряешь больше $53 за год. 
# Если же случится "плохой год", средняя потеря составит около $75.

# Историческая годовая доходность
# ⚙️ Возьмем последние 252 дня (1 торговый год)
returns_last_year <- tail(returns, 252)

# 📈 Доходности портфелей
portf_ret_mve <- as.numeric(returns_last_year %*% w_mve_constrained)
portf_ret_tan <- as.numeric(returns_last_year %*% w_tan_longonly)

# 🔢 Кумулятивная лог-доходность → экспонента = финальный рост
cum_mve <- exp(sum(portf_ret_mve)) - 1
cum_tan <- exp(sum(portf_ret_tan)) - 1

# 📊 Вывод
tibble(
        Portfolio = c("MVE", "Tangency"),
        CumulativeReturn = round(c(cum_mve, cum_tan), 4),
        CumulativePercent = round(c(cum_mve, cum_tan) * 100, 2)
)

# CumulativeReturn = суммарный рост портфеля за последний год
# Например: 0.0645 → 6.45% доходности за год

