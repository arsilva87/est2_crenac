# Script 2 - GLM

# Dados de abundância de communidades de plantas em
# duas ilhas do rio Vistula

library(rgdal)
norte <- readOGR('data_vistula/abundance_n.shp')
sul <- readOGR('data_vistula/abundance_s.shp')
vistula <- readOGR('data_vistula/vistula_islands.shp')
plot(vistula)
plot(sul, add = TRUE)

grupo = rep(c('N', 'S'), times = c(nrow(norte), nrow(sul)))
dados = data.frame(grupo, 
                   resp = c(norte$abundance, sul$abundance))
str(dados)
boxplot(resp ~ grupo, dados)

# Em média, as ilhas diferem quanto à abundância?
# médias
aggregate(resp ~ grupo, data = dados, mean)

# GLM para variável discreta (Poisson ou Bin. Negativo)
# y = f(x) + erro

# Modelo Poisson
# y = exp(x) + erro
modelo_p = glm(resp ~ grupo, data = dados, family = poisson)
summary(modelo_p)

# média grupoN = intercepto = 0.766
exp(0.766)  # média do grupoN

# média grupoS = intercepto + grupoS = 0.766 - 0.0253
exp(0.766 + (-0.0253))

# grupoL = intercepto + grupoL

# Comparação de médias: teste Tukey
library(emmeans)
m = emmeans(modelo_p, specs = 'grupo', type = 'response')
pairs(m)
# p-valor > 0.05, médias iguais


# Modelo Binomial Negativo
# y = exp(x) + erro
library(MASS)
modelo_bn = glm.nb(resp ~ grupo, data = dados)
summary(modelo_bn)

# Comparação de médias: teste Tukey
m2 = emmeans(modelo_bn, specs = 'grupo', type = 'response')
m2
pairs(m2)
# p-valor > 0.05, médias iguais

# comparação de modelos
AIC(modelo_p, modelo_bn)



# Modelo Gamma
# y = 1/x + erro

modelo_g = glm(resp ~ grupo, data = dados, family = Gamma)
summary(modelo_g)

# Média do grupo N
1/0.464730
1/(0.4673 + 0.0119)
  
# Comparação de médias: teste Tukey
m3 = emmeans(modelo_g, specs = 'grupo', type = 'response')
m3
pairs(m3)
plot(m3)
# p-valor > 0.05, médias iguais

# comparação de modelos
AIC(modelo_p, modelo_bn, modelo_g)

# check de resíduos
res = residuals(modelo_g, type = 'pearson')
plot(res, ylim = c(-5, 5))

n = nrow(dados)
LS = qt(1 - 0.025/n, df = modelo_g$df.residual)
LI = qt(0.025/n, df = modelo_g$df.residual)
abline(h = c(LS, LI), col = 'red')

# Exercício, dados genótipos de pimenta
pimenta = read.csv('https://raw.githubusercontent.com/arsilva87/statsbook/main/datasets/pimenta.csv')
pimenta

