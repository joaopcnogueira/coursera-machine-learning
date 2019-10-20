# Distribuições de Probabilidade ----

# Uma distribuição de probabilidade descreve como os valores de uma variável aleatória são distribuídos. 
# Por exemplo, a coleção de todos os resultados possíveis de uma sequência de lançamento de moeda é conhecido por 
# seguir a distribuição binomial. 

# Médias de amostras suficientemente grandes de uma população de dados são conhecidos por se 
# assemelhar a distribuição normal. 

# Como as características dessas distribuições teóricas são bem compreendidas, elas podem ser usadas para 
# fazer inferências estatísticas em toda a população de dados como um todo. Os algoritmos de Machine Learning
# são baseados em distribuições de probabilidade.

# Abaixo você encontra as principais distribuições de probabilidade


# Distribuição Binomial ----
# A distribuição binomial é uma distribuição de probabilidade discreta. 
# Ela descreve o resultado de n experimentos independentes em um experimento. 
# Cada ensaio é suposto ter apenas dois resultados, seja sucesso ou fracasso.

# Suponha que haja 12 perguntas de múltipla escolha em um exame. 
# Cada pergunta tem 5 respostas possíveis, e apenas 1 delas está correta. 
# Encontre a probabilidade de ter quatro ou menos respostas corretas se um aluno tentar responder a todas 
# as perguntas aleatoriamente.
?dbinom

# Uma vez que apenas uma entre cinco respostas possíveis está correta, a probabilidade de responder a 
# uma pergunta corretamente por aleatoriedade é 1/5 = 0,2. Podemos encontrar a probabilidade de ter 
# exatamente 4 respostas corretas por tentativas aleatórias como segue.
dbinom(4, size = 12, prob = 0.2) 

# A probabilidade de quatro ou menos perguntas respondidas corretamente por sorteio em um questionário 
# de múltipla escolha é de 92,7%.
pbinom(4, size = 12, prob = 0.2) 



# Distribuição Poisson ----
# A distribuição de Poisson é a distribuição de probabilidade de ocorrências de eventos independentes em um 
# intervalo.

# Se há doze carros cruzando uma ponte por minuto em média, encontre a probabilidade de ter 
# dezessete ou mais carros cruzando a ponte em um minuto específico.
?ppois

# A probabilidade de ter 17 ou mais carros cruzando a ponte em um minuto está na parte superior da cauda 
# da função de densidade de probabilidade. Se há doze carros cruzando uma ponte por minuto em média, 
# a probabilidade de ter dezessete ou mais carros que cruzam a ponte em um minuto particular é 10.1%.
ppois(16, lambda = 12, lower = FALSE)

# A probabilidade de ter dezesseis ou menos carros cruzando a ponte em um minuto específico é dada pela 
# função ppois.
ppois(16, lambda = 12)   



# Distribuição Uniforme Contínua ----
# A distribuição uniforme contínua é a distribuição de probabilidade de seleção de números aleatórios a partir 
# do intervalo contínuo entre a e b. 
runif(10, min = 1, max = 3) 
hist(runif(10, min = 1, max = 3))



# Distribuição Exponencial ----
# A distribuição exponencial descreve o tempo de chegada de uma seqüência de eventos independentes, 
# aleatoriamente recorrentes.

# Suponha que o tempo médio de checkout de um caixa de supermercado seja de 3 minutos. 
# Encontre a probabilidade de uma verificação de cliente ser concluída pelo caixa em menos de 2 minutos.

# A taxa de processamento de saída é igual a 1 dividido pelo tempo médio de conclusão do checkout. 
# Daí a taxa de processamento é 1/3 checkouts por minuto. 
# Aplicamos então a função pexp da distribuição exponencial com taxa = 1/3.

# A probabilidade de terminar um checkout em menos de dois minutos pelo caixa é de 48,7%
pexp(2, rate = 1/3) 



# Distribuição Normal ----
# A distribuição normal é uma das mais importantes distribuições da estatística, conhecida também 
# como Distribuição de Gauss ou Gaussiana.

# Além de descrever uma série de fenômenos físicos e financeiros, possui grande uso na estatística inferencial. 
# É inteiramente descrita por seus parâmetros de média e desvio padrão, ou seja, conhecendo-se estes valores 
# consegue-se determinar qualquer probabilidade em uma distribuição Normal.

# Um interessante uso da Distribuição Normal é que ela serve de aproximação para o cálculo de outras 
# distribuições quando o número de observações for muito grande. 
# Essa importante propriedade provém do Teorema do Limite Central que diz que 
# "toda soma de variáveis aleatórias independentes de média finita e variância limitada é aproximadamente Normal, 
# desde que o número de termos da soma seja suficientemente grande"

# Suponha que as pontuações dos exames de vestibular se enquadrem numa distribuição normal. 
# Além disso, o escore médio do teste é 72 e o desvio padrão é 15,2. Qual é a percentagem de alunos com 
# mais de 84 pontos no exame?

# Aplicamos a função pnorm da distribuição normal com média 72 e desvio padrão 15,2. Uma vez que 
# estamos procurando o percentual de alunos com pontuação superior a 84, estamos interessados na cauda 
# superior da distribuição normal.

# A percentagem de alunos com pontuação de 84 ou mais no vestibular é de 21,5%.
pnorm(84, mean = 72, sd = 15.2, lower.tail = FALSE) 
rnorm(84, mean = 72, sd = 15.2)
hist(rnorm(84, mean = 72, sd = 15.2))



# Outras Distribuições ----

# Distribuição do Qui-quadrado
qchisq(.95, df = 7) 

# Distribuição T de Student
qt(c(.025, .975), df = 5)

# Distribuição F
qf(.95, df1=5, df2 = 2) 

