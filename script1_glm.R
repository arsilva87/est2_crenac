
# Script 1 - ANOVA e GLM

dados = read.csv('http://arsilva.weebly.com/uploads/2/1/0/0/21008856/pulgao.csv')
dados

dados$pulgoes

# boxplot
boxplot(pulgoes ~ trat, dados)

# teste de normalidade
shapiro.test( dados$pulgoes )

#teste de homogeneidade de variâncias
bartlett.test( pulgoes ~ trat, dados )


# Ajuste de GLM
modelo_normal = glm(pulgoes ~ trat, data = dados)
modelo_poisson = glm(pulgoes ~ trat, data = dados, family = poisson)
modelo_gamma = glm(pulgoes ~ trat, data = dados, family = Gamma)
modelo_ni = glm(pulgoes ~ trat, data = dados, family = inverse.gaussian)

AIC(modelo_normal, modelo_poisson, modelo_gamma, modelo_ni)


# checagem de resíduos
res = residuals(modelo_gamma, type = 'pearson')
res
plot(res)

# intervalo aceitável para resíduos
LS = qt(1 - 0.025/30, df = modelo_gamma$df.residual)
LS
LI = qt(0.025/30, df = modelo_gamma$df.residual)
LI
abline(h = c(LI, LS))

# Comparações de médias de trat
aggregate(pulgoes ~ trat, data = dados, mean)

# pacote emmeans
# install.packages('emmeans')
library(emmeans)
med = emmeans(modelo_gamma, specs = 'trat')

# Teste Tukey
pairs(med)

# Teste t
pairs(med, adjust = 'none')

# Exercício
especies


  