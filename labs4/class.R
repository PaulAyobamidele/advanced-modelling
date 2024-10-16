library(tidyverse)
library(broom)

icon <- read_csv("perry_winter_2017_iconicity.csv")
icon %>% print(n = 4, width = Inf)




icon <- mutate(icon, Log10Freq = log10(Freq))
View(icon)



icon_mdl <- lm(Iconicity ~ SER + CorteseImag + Syst + Log10Freq, data = icon)
summary(icon_mdl)
glance(icon_mdl)$r.squared


tidy(icon_mdl) %>%
    select(term, estimate) %>%
    mutate(estimate = round(estimate, 1))

range(icon$Syst, na.rm = TRUE)

icon <- mutate(icon,
    SER_z = scale(SER),
    CorteseImag_z = scale(CorteseImag),
    Syst_z = scale(Syst),
    Freq_z = scale(Log10Freq)
)


icon_mdl_z <- lm(Iconicity ~ SER_z + CorteseImag_z + Syst_z + Freq_z, data = icon)
glance(icon_mdl_z)


tidy(icon_mdl_z) %>%
    select(term, estimate) %>%
    mutate(estimate = round(estimate, 1))


#  (Intercept)        1.3
# 2 SER_z              0.5 biggest effect we consider the magnitude
# 3 CorteseImag_z     -0.4 runner up, we only consider the magnitude
# 4 Syst_z             0
# 5 Freq_z            -0.3


res <- residuals(icon_mdl_z)
plot(res)
par(mfrow = c(1, 3))
hist(res)
qqnorm(res)
qqline(res)
plot(fitted(icon_mdl_z), res)
