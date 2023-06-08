ggplot(data = mean_adr) +
  geom_point(aes(x=lead_time_case, y=mean), size = 4, color = "#0066cc") +
  geom_errorbar(aes(x=lead_time_case, y=mean, ymin = a, ymax = b),
                width = 0.4, color = "#0066cc", size = 1) +
  labs(x = "lead_time_case", title = "Adr ~ lead_time_case (mean)") +
  theme(text = element_text(size = 15))

ggplot(data = median_adr) +
  geom_point(aes(x=lead_time_case, y=median), size = 4, color = "#0066cc") +
  geom_errorbar(aes(x=lead_time_case, y=median, ymin = a, ymax = b),
                width = 0.4, color = "#0066cc", size = 1) +
  labs(x = "lead_time_case", title = "Adr ~ lead_time_case (mediana)") +
  theme(text = element_text(size = 15))


mean_hat_s <- df %>% filter(lead_time_case == 'Short') %>% pull(mean_hat)
mean_hat_ns <- df %>% filter(lead_time_case == 'Long') %>% pull(mean_hat)
var_hat_s <- df %>% filter(lead_time_case == 'Short') %>% pull(var_hat)
var_hat_ns <- df %>% filter(lead_time_case == 'Long') %>% pull(var_hat)
se <- sqrt(var_hat_s + var_hat_ns)
T <- (mean_hat_s - mean_hat_ns) / se
p_value <- pnorm(T, lower.tail = FALSE)
conf.int <- c(mean_hat_s - mean_hat_ns - qnorm(0.95)*se, Inf)

mean_hat_s
mean_hat_ns

var_hat_s
var_hat_ns

p_value

conf.int


mean_hat_s <- df %>% filter(lead_time_case == 'Middle') %>% pull(mean_hat)
mean_hat_ns <- df %>% filter(lead_time_case == 'Long') %>% pull(mean_hat)
var_hat_s <- df %>% filter(lead_time_case == 'Middle') %>% pull(var_hat)
var_hat_ns <- df %>% filter(lead_time_case == 'Long') %>% pull(var_hat)
se <- sqrt(var_hat_s + var_hat_ns)
T <- (mean_hat_s - mean_hat_ns) / se
p_value <- pnorm(T, lower.tail = FALSE)
conf.int <- c(mean_hat_s - mean_hat_ns - qnorm(0.95)*se, Inf)

mean_hat_s
mean_hat_ns

var_hat_s
var_hat_ns

p_value

conf.int

