###################################################################
# Minijornada ILB                                                 #
# Avalição de Políticas Públicas por meio do Controle Sintético   #
# Bruno Gasparotto Ponne - set. 2022                              #
###################################################################

# Bibliotecas necessárias:

library(Synth)
library(dplyr)

load("DATA_COMPLETE.RData")

# Dados do Ensino Fundamental, anos iniciais, referentes a Português:

ai_port <- filter(DATA_COMPLETE, grade == "P", subject == "port")

# Atenção: é preciso garantir que os dados sejam um data frame para 
# que sejam aceitos pela função dataprep:

ai_port <- as.data.frame(ai_port)

preditores <- c("homicides", "TWh", "ln_pop", "unemployment", "edu_invest_pc")

dados_prep <- dataprep(foo = ai_port,
                    predictors = preditores,
                    dependent     = "score",
                    unit.variable = "code_state",
                    time.variable = "year",
                    unit.names.variable = "abbr_state",
                    treatment.identifier  = 23,
                    controls.identifier   = c(11:17, 21:22, 24:29, 31:33, 35, 41:43, 50:53),
                    time.predictors.prior = seq(1995, 2007, 2),
                    time.optimize.ssr     = seq(1995, 2007, 2),
                    time.plot             = seq(1995, 2019, 2))

# Verificando os vetores X0 e X1:

dados_prep$X0
dados_prep$X1

# Estimando o controle sintético:
cs <- synth(dados_prep)

# Verificando a solução encontrada:

cs$solution.v
round(cs$solution.w, 2)

# Gráficos

path.plot(synth.res    = cs,
          dataprep.res = dados_prep,
          Ylab         = c("Pontuação"),
          Xlab         = c("Ano"),
          Legend       = c("Ceará","Ceará Sintético"),
          Legend.position = c("bottomleft"),
          Main = "Ceará vs Ceará Sintético")

gaps.plot(synth.res    = cs,
          dataprep.res = dados_prep,
          Ylab         = c("Efeito"),
          Xlab         = c("Ano"),
          Main = "Efeito")

### Calculando o efeito médio no período de 2008 até 2019

tabela_resultados <- synth.tab(synth.res = cs,
                               dataprep.res = dados_prep)

tabela_w <- data.frame(tabela_resultados$tab.w)

ai_port_w <- left_join(ai_port, tabela_w, by = c("code_state" = "unit.numbers"))
ai_port_w$w.weights[is.na(ai_port_w$w.weights)] <- 0

ceara <- ai_port %>% 
  filter(code_state == 23)

controle <- ai_port_w %>%
  group_by(year) %>% 
  summarise(cs = weighted.mean(score, w.weights))

efeitos <- left_join(controle, select(ceara, year, score), by = "year") %>% 
  mutate(efeito = score-cs)

mean(efeitos$efeito[8:13])

