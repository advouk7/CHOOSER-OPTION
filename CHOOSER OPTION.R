# Function to calculate chooser option price using Black-Scholes model
chooser_option_black_scholes <- function(S, K, r, delta, sigma, T) {
  d1 <- (log(S / K) + (r - delta + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  # Calculate call and put prices using Black-Scholes formula
  call_price <- S * exp(-delta * T) * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  put_price <- K * exp(-r * T) * pnorm(-d2) - S * exp(-delta * T) * pnorm(-d1)
  
  # Chooser option price is the maximum of call and put prices
  chooser_option_price <- max(call_price, put_price)
  return(chooser_option_price)
}

# Parameters (adjusted to match the manual calculation)
S <- 1000  # Spot price
K <- 1025  # Strike price
r <- 0.04  # Risk-free interest rate
delta <- 0.03  # Dividend yield
sigma <- 0.35  # Volatility
T <- 5  # Time to maturity

# Calculate chooser option price using Black-Scholes model
black_scholes_price <- chooser_option_black_scholes(S, K, r, delta, sigma, T)
print(paste("Chooser Option Price (Black-Scholes):", black_scholes_price))
