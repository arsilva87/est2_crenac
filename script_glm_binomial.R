
n <- 20
dead <- c(0, 1, 5, 17, 19, 19)
alive <- n - dead
dose <- c(0, 0.25, 0.5, 1, 1.5, 2)
plot(dead/n ~ dose)

# GLM binomial, exemplo 1
modelo_bin = glm( cbind(dead, alive) ~ dose, family = binomial )
summary(modelo_bin)

coef(modelo_bin)
curve( exp(-3.42 + 4.46*x)/(1 +  exp(-3.42 + 4.46*x)), add=T)

# DL50
abline(h = 0.5, v = 0.78)

# GLM normal
modelo_normal = glm(dead/20 ~ dose)
coef(modelo_normal)
curve(0.0238 + 0.5536*x, add = T, col = 'red')

dead/n - fitted(modelo_normal)
dead/n - fitted(modelo_bin)


# GLM binomial, exemplo 2
dados = read.csv('http://arsilva.weebly.com/uploads/2/1/0/0/21008856/auxina.csv')
dados

# H0: as médias de proporção de regenerados das três auxinas são iguais

modelo_bin = glm( cbind(regen, nregen) ~ Auxina, data = dados, family = binomial)
summary(modelo_bin)

# Intervalos de confiança
library(emmeans)
med = emmeans(modelo_bin, specs = 'Auxina', type = 'response')

# Intervalos de confiança
plot(med)

# analise de resíduos
res = residuals(modelo_bin, type = 'pearson')
plot(res)

LS = qt(1 - 0.025/18, df = modelo_bin$df.residual)
LI = qt(0.025/18, df = modelo_bin$df.residual)
abline(h = c(LS, LI))

# teste t-Student para comparação de médias
pairs(med, adjust = 'none')


# Exercício
