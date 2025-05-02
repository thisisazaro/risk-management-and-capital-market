

if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("quadprog", quietly = TRUE)) install.packages("quadprog")
library(quantmod)
library(dplyr)
library(ggplot2)
library(corrplot)
library(quadprog)
library(tibble)
library(ggplot2)
library(kableExtra)
library(tibble)
library(tidyr)
library(rugarch)
library(PerformanceAnalytics)

## II. Data 

### Выбор тикеров и загрузка цен ----
tickers <- c("INTC",  # intel (tech, large-cap)
             "AMD",   # advanced micro devices (tech, large-cap/growth)
             "TXN",   # texas instruments (tech, value)
             "AVGO",  # broadcom (tech, dividends)
             "MRVL",  # marvell (mid-cap, growth)
             "RMBS",  # rambus (small-cap, growth)
             "QCOM",  # qualcomm (tech, dividends)
             "LSCC",  # lattice semiconductor (small-cap)
             "ADI",   # analog devices (dividends)
             "MCHP",  # microchip technology
             "SOXX")   # ETF  

start_date <- as.Date("2019-03-31")
end_date <- Sys.Date()


# Adjusted Close
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

price_list <- lapply(tickers, get_monthly_prices)
price_xts <- do.call(merge, price_list)

### T-Bills from FRED
getSymbols("TB3MS", src = "FRED")
tbill_data <- TB3MS
tbill_monthly <- tbill_data["2019/"]
tbill_monthly <- na.approx(tbill_monthly) / 100 / 12 
colnames(tbill_monthly) <- "TBill_Return"

monthly_returns <- diff(price_xts) / lag.xts(price_xts, k = 1)
monthly_returns <- na.omit(monthly_returns)
index(tbill_monthly) <- as.Date(as.yearmon(index(tbill_monthly)), frac = 1)
tbill_trimmed <- tbill_monthly[index(monthly_returns)]
data_combined <- merge(monthly_returns, tbill_trimmed)
returns_df <- fortify.zoo(data_combined)
head(returns_df, 10)


plot_data <- returns_df %>% filter(!is.na(INTC), !is.na(TBill_Return))
ggplot(plot_data, aes(x = Index)) +
        geom_line(aes(y = INTC, color = "INTC")) +
        geom_line(aes(y = TBill_Return, color = "T-Bill")) +
        labs(title = "INTC vs T-Bill Monthly Returns",
             x = "Date", y = "Return") +
        scale_color_manual(values = c("INTC" = "blue", "T-Bill" = "red"))


# III. Deliverables 

stock_names <- tickers
mean_monthly_returns <- colMeans(returns_df[stock_names], na.rm = TRUE)
annualized_returns <- round(mean_monthly_returns * 12, 4)
print(annualized_returns)

# T-Bills
mean_tbill <- mean(returns_df$TBill_Return, na.rm = TRUE)
annualized_tbill <- mean_tbill * 12

return_table <- data.frame(
        Ticker = c(stock_names, "TBill"),
        Annualized_Return = c(round(annualized_returns, 4), round(annualized_tbill, 4))
)
print(return_table)

return_matrix <- as.matrix(returns_df[stock_names])
cov_matrix <- cov(return_matrix, use = "pairwise.complete.obs")
#cor_matrix <- cor(return_matrix, use = "pairwise.complete.obs")
cor_matrix_spearman <- cor(return_matrix, method = "spearman", use = "pairwise.complete.obs")
print("Covariance Matrix:")
print(round(cov_matrix, 5))
print("Correlation Matrix:")
print(round(cor_matrix_spearman, 3))

corrplot(cor_matrix_spearman,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.8,
         addCoef.col = "black", 
         number.cex = 0.7,
         col = colorRampPalette(c("red", "white", "blue"))(200),
         title = "Correlation Matrix of Stock Returns",
         mar = c(0,0,2,0))

kable(cor_matrix_spearman)

# Deliverable 2: Find the weights of the minimum variance efficient (MVE) portfolio.

# Шаг 1: Ковариационная матрица и количество активов
cov_monthly <- cov(return_matrix, use = "pairwise.complete.obs")
cov_annual <- cov_monthly * 12  # Переводим в годовую
n_assets <- ncol(cov_annual)

# Шаг 2: Построим параметры для solve
Dmat <- cov_annual
dvec <- rep(0, n_assets) 
Amat <- matrix(1, nrow = n_assets, ncol = 1)  # Сумма весов = 1
bvec <- 1
meq <- 1  # 1 равенство

# Шаг 3: Решаем задачу
mve_solution <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)

# Шаг 4: Результаты
mve_weights <- mve_solution$solution
names(mve_weights) <- stock_names
print(round(mve_weights, 4))
cat("Сумма весов:", sum(mve_weights), "\n")


assets <- c("INTC", "AMD", "TXN", "AVGO", "MRVL", "RMBS", "QCOM", "LSCC", "ADI", "MCHP", "SOXX")
mve_weights <- c(0.0926, 0.0036, 1.0172, 0.2363, -0.1802, 0.1706, 0.1266, 0.0077, 0.1794, -0.5311, -0.1226)
total_investment <- 5000000
positions <- mve_weights * total_investment
mve_table <- data.frame(
        Asset = assets,
        MVE_weights = round(mve_weights, 4),
        Positions = round(positions, 2)
)
print(mve_table)


# ДОПОЛНИТЕЛЬНО: Доходность и риск портфеля

# Годовая ожидаемая доходность
mean_annual_returns <- colMeans(return_matrix, na.rm = TRUE) * 12

# Ожидаемая доходность MVE портфеля
mve_return <- sum(mve_weights * mean_annual_returns)

# Дисперсия и стандартное отклонение
mve_variance <- t(mve_weights) %*% cov_annual %*% mve_weights
mve_sd <- sqrt(mve_variance)

cat("\n Minimum Variance Portfolio:\n")
cat("Expected Return: ", round(mve_return, 4), "\n")
cat("Standard Deviation: ", round(mve_sd, 4), "\n")
cat("Variance: ", round(mve_variance, 6), "\n")


# Deliverable 3: Find the weights of the tangency portfolio.

mean_annual_returns <- colMeans(return_matrix, na.rm = TRUE) * 12
rf <- mean(returns_df$TBill_Return, na.rm = TRUE) * 12  
excess_returns <- mean_annual_returns - rf  # E[R] - rf
cov_annual <- cov(return_matrix, use = "pairwise.complete.obs") * 12
n_assets <- ncol(cov_annual)

# Оптимизация: max (w' * excess_returns) при w' Σ w = 1
Dmat <- 2 * cov_annual  # коэффициент перед квадратичным членом (2Σ)
dvec <- rep(0, n_assets)

# Ограничение: дисперсия = 1
Amat <- matrix(excess_returns, ncol = 1)
bvec <- 1
meq <- 1

# Решаем
tangency_solution <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)

# 3. Получаем ненормализованные веса
w_unnormalized <- tangency_solution$solution
# Нормализуем веса так, чтобы сумма = 1
w_tangency <- w_unnormalized / sum(w_unnormalized)
names(w_tangency) <- stock_names
print(round(w_tangency, 4))


assets <- c("INTC", "AMD", "TXN", "AVGO", "MRVL", "RMBS", "QCOM", "LSCC", "ADI", "MCHP", "SOXX")
tangency_weights <- c(-0.5982, 0.1702, 1.0264, 1.0024, -0.7554, 0.7513, 0.0464, 0.3473, 0.2611, -1.1612, -0.0903)
total_investment <- 5000000
positions <- tangency_weights * total_investment
tangency_table <- data.frame(
        Asset = assets,
        Tangency_weights = round(tangency_weights, 4),
        Positions = round(positions, 2)
)
print(tangency_table)



# Доходность и риск портфеля
expected_return_tangency <- sum(w_tangency * mean_annual_returns)
variance_tangency <- t(w_tangency) %*% cov_annual %*% w_tangency
sd_tangency <- sqrt(variance_tangency)
sharpe_tangency <- (expected_return_tangency - rf) / sd_tangency

cat("\n Tangency Portfolio (Max Sharpe):\n")
cat("Expected Return: ", round(expected_return_tangency, 4), "\n")
cat("Standard Deviation: ", round(sd_tangency, 4), "\n")
cat("Sharpe Ratio: ", round(sharpe_tangency, 4), "\n")






# efficient frontier (дополним Tangency)

# число портфелей, которые мы хотим сгенерировать
n_portfolios <- 100
set.seed(123)

results <- data.frame(Risk = numeric(n_portfolios),
                      Return = numeric(n_portfolios))

for (i in 1:n_portfolios) {
        w <- runif(n_assets)
        w <- w / sum(w)  
        
        port_return <- sum(w * mean_annual_returns)
        port_variance <- t(w) %*% cov_annual %*% w
        port_sd <- sqrt(port_variance)
        
        results[i, ] <- c(Risk = port_sd, Return = port_return)
}

results <- results[order(results$Risk), ]

# точка Tangency
tangency_point <- data.frame(Risk = as.numeric(sd_tangency), Return = as.numeric(expected_return_tangency))
mve_point <- data.frame(Risk = as.numeric(mve_sd), Return = as.numeric(mve_return))

# линия CML
cml_df <- data.frame(
        Risk = c(0, tangency_point$Risk),
        Return = c(rf, tangency_point$Return)
)

# Expected Return и Volatility
individual_points <- data.frame(
        Ticker = names(mean_annual_returns),
        Risk = apply(return_matrix, 2, sd) * sqrt(12),
        Return = mean_annual_returns
)


ggplot(results, aes(x = Risk, y = Return)) +
        geom_line(color = "steelblue", linewidth = 1.2) +  
        geom_point(data = mve_point, aes(x = Risk, y = Return), color = "gold", size = 4) +
        geom_point(data = tangency_point, aes(x = Risk, y = Return), color = "limegreen", size = 4) +
        geom_line(data = cml_df, aes(x = Risk, y = Return), color = "limegreen", linetype = "dashed", linewidth = 1) +
        geom_point(data = individual_points, aes(x = Risk, y = Return), shape = 21, fill = "white", color = "white", size = 3) +
        geom_text(data = individual_points, aes(x = Risk, y = Return, label = Ticker), 
                  hjust = -0.15, vjust = 0.3, color = "white", size = 3.2, fontface = "bold") +
        geom_text(data = mve_point, aes(x = Risk, y = Return, label = "MVE"), 
                  vjust = -1, color = "gold", size = 4, fontface = "bold") +
        geom_text(data = tangency_point, aes(x = Risk, y = Return, label = "Tangency"), 
                  vjust = -1, color = "limegreen", size = 4, fontface = "bold") +
        labs(title = "Efficient Frontier with MVE, Tangency Portfolio and CML",
             x = "Risk (σ, Annualized Std Dev)", y = "Expected Return (Annualized)") +
        annotate("text",
                 x = 0.275,
                 y = 0.27, 
                 label = "Efficient Frontier",
                 color = "steelblue",
                 size = 4,
                 fontface = "bold",
                 angle = 0) +
        theme_minimal(base_family = "Helvetica") +
        theme(
                plot.background = element_rect(fill = "black", color = NA),
                panel.background = element_rect(fill = "black", color = NA),
                panel.grid.major = element_line(color = "gray30"),
                panel.grid.minor = element_blank(),
                axis.text = element_text(color = "white", size = 11),
                axis.title = element_text(color = "white", size = 13),
                plot.title = element_text(face = "bold", size = 16, color = "white", hjust = 0.5),
                legend.position = "none"
        )




weights_df <- tibble(
        Ticker = stock_names,
        MVE = mve_weights,
        Tangency = w_tangency
) %>%
        pivot_longer(cols = -Ticker, names_to = "Portfolio", values_to = "Weight")

ggplot(weights_df, aes(x = Ticker, y = Weight, fill = Portfolio)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("MVE" = "red", "Tangency" = "darkgreen")) +
        labs(title = "Portfolio Weights: MVE vs Tangency", y = "Weight", x = "Stock") +
        theme_minimal()


risk_return_df <- tibble(
        Ticker = stock_names,
        Return = mean_annual_returns,
        Risk = apply(return_matrix, 2, sd, na.rm = TRUE) * sqrt(12)
)

ggplot(risk_return_df, aes(x = Risk, y = Return, label = Ticker)) +
        geom_point(color = "darkblue", size = 3) +
        geom_text(vjust = -1, hjust = 0.5) +
        labs(title = "Risk vs Return for Individual Stocks",
             x = "Standard Deviation (Annualized)", y = "Expected Return (Annualized)") +
        theme_minimal()


str(returns_df)
## Garch (1,1)
library(rugarch)

# Загрузка дневных цен и расчет лог-доходностей
log_returns_list <- list()

for (ticker in tickers) {
        tryCatch({
                getSymbols(ticker, from = start_date, to = end_date, src = "yahoo", auto.assign = FALSE) %>%
                        Ad() %>%
                        na.omit() %>%
                        {diff(log(.))} %>%
                        na.omit() -> log_ret
                log_returns_list[[ticker]] <- log_ret
        }, error = function(e) {
                message(paste("Error", ticker, ":", e$message))
        })
}


# Спецификация GARCH(1,1) с t-распределением
spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
        mean.model     = list(armaOrder = c(0,0), include.mean = TRUE),
        distribution.model = "std"
)

# Результаты
garch_results <- data.frame()

for (ticker in names(log_returns_list)) {
        data <- na.omit(log_returns_list[[ticker]])
        fit <- tryCatch(
                ugarchfit(spec, data = data, solver = "solnp"),
                error = function(e) NULL
        )
        if (!is.null(fit)) {
                params <- coef(fit)
                sigma  <- sigma(fit)
                shape  <- params["shape"]
                # VaR и ES на уровне 1% (99% доверие)
                q_t <- qt(0.01, df = shape)
                d_t <- dt(q_t, df = shape)
                VaR_1pct <- quantile(sigma * q_t, probs = 1)
                ES_1pct  <- mean(sigma * d_t / 0.01)
                garch_results <- rbind(garch_results, data.frame(
                        Ticker = ticker,
                        Omega  = round(params["omega"], 6),
                        Alpha  = round(params["alpha1"], 6),
                        Beta   = round(params["beta1"], 6),
                        Shape  = round(shape, 2),
                        VaR_1pct = round(VaR_1pct, 4),
                        ES_1pct  = round(ES_1pct, 4)
                ))
        } else {
                message(paste("GARCH fit failed for", ticker))
        }
}

print(garch_results)

# Стоимость портфеля (в долларах)
portfolio_value <- 5000000  # $5,000,000
weights <- rep(1 / length(tickers), length(tickers))
names(weights) <- tickers
# Добавим расчет денежного VaR и ES
garch_results$VaR_USD <- round(garch_results$VaR_1pct * weights[garch_results$Ticker] * portfolio_value, 2)
garch_results$ES_USD  <- round(garch_results$ES_1pct  * weights[garch_results$Ticker] * portfolio_value, 2)
# Результат
print(garch_results[, c("Ticker", "VaR_1pct", "VaR_USD", "ES_1pct", "ES_USD")])


