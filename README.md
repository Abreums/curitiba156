Curitiba\_156
================
Marcos Abreu
7/14/2020

## Análise inicial

Para as primeiras avaliações podemos obter a quantidade de registros no
tempo. A seguir estão os dados sumarizados por dia, semana e mês.

Os dados de *maio de 2019* não foram encontrados nos arquivos
disponibilizados.

``` r
cwb_by_day <- cwb %>% select(datetime) %>% 
  group_by(day = date(datetime)) %>% 
  summarise(total = n()) %>% 
  mutate(week = as.Date(cut(day, "week")), # by default weeks start on monday
         month = as.Date(cut(day, "month")))

ggplot(cwb_by_day, aes(x = day, y = total)) + 
  geom_line() +
  labs(title = "Total de solicitações por dia",
       subtitle = "solicitações no 156 da prefeitura de Curitiba",
       x = "Número de registros",
       y = "Dia") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d %b %Y") 
```

<img src="README_figs/README-by_day-1.png" width="672" />

``` r

ggplot(cwb_by_day, aes(x = week, y = total)) + 
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Total de solicitações por semana",
       subtitle = "solicitações no 156 da prefeitura de Curitiba",
       y = "Número de registros",
       x = "Semana") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d %b %Y") 
```

<img src="README_figs/README-by_day-2.png" width="672" />

``` r

ggplot(cwb_by_day, aes(x = month, y = total)) + 
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Total de solicitações por mês",
       subtitle = "solicitações no 156 da prefeitura de Curitiba",
       y = "Número de registros",
       x = "Mês") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d %b %Y") 
```

<img src="README_figs/README-by_day-3.png" width="672" />

Uma análise dos dados mostra que os atributos mais significativos
aparentam ser:  
\- Unidade Regional, - Órgão Responsável e - Assunto.

Sendo *assunto* uma propriedade com forte correlação com *Órgão
Responsável*, apresentando em alguns casos como uma subdivisão
exclusiva.  
A seguir as informações históricas destes dados.

### Unidade Regional

``` r
cwb_by_ur <- cwb %>% 
  select(datetime, regional_ass) %>% 
  group_by(day = date(datetime), ur = regional_ass) %>% 
  summarise(total = n()) %>% 
  mutate(week = as.Date(cut(day, "week")),
         month = as.Date(cut(day, "month"))) %>% 
  na.omit(ur)

ggplot(cwb_by_ur,aes(x = day, y = total, colour = ur)) + 
  geom_line() +
  labs(title = "Total de solicitações por regional",
       subtitle = "solicitações no 156 da prefeitura de Curitiba",
       x = "Dia",
       y = "Número de solicitações") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), 
        axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d %b %Y") 
```

<img src="README_figs/README-by_ur-1.png" width="672" />

``` r

ggplot(cwb_by_ur,aes(x = week, y = total, colour = ur)) + 
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Total de solicitações por regional",
       subtitle = "solicitações no 156 da prefeitura de Curitiba",
       x = "Semana",
       y = "Número de solicitações") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), 
        axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d %b %Y") 
```

<img src="README_figs/README-by_ur-2.png" width="672" />

``` r

ggplot(cwb_by_ur,aes(x = month, y = total, colour = ur)) + 
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Total de solicitações por regional por mês",
       subtitle = "solicitações no 156 da prefeitura de Curitiba",
       x = "Mês",
       y = "Número de solicitações") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), 
        axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d %b %Y") 
```

<img src="README_figs/README-by_ur-3.png" width="672" />

``` r

# por facet:
ggplot(cwb_by_ur,aes(x = day, y = total, colour = ur)) + 
  geom_line() +
  labs(title = "Total de solicitações por regional",
       subtitle = "solicitações no 156 da prefeitura de Curitiba",
       x = "Mês",
       y = "Registros") +
  theme_fivethirtyeight() +
  theme(axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank()) +
  facet_grid(~ur)
```

<img src="README_figs/README-by_ur-4.png" width="672" />

### Número de solicitações por regional proporcional a 1.000 habitantes

Como as regionais possuem populações diferentes, vamos reavaliar
considerando a relação de número de atendimentos dividido por 1.000
habitantes:

``` r
cwb_by_ur_by_pop <- cwb %>% select(ur = regional_ass) %>% 
  group_by(ur) %>% 
  summarise(total = n()) %>% 
  inner_join(y = regional, by = c("ur" = "nome")) %>% 
  mutate(relativo = as.integer(format(round((total/populacao)*1000, digits = 0), nsmall = 0)))

cwb_by_ur_by_pop <- cwb_by_ur_by_pop %>% mutate(ur = fct_reorder(ur, relativo))

ggplot(cwb_by_ur_by_pop, aes(x=relativo, y=ur))+ 
  geom_bar(stat="identity") +
  labs(title = "Curitiba 156",
      subtitle = "Número de solicitações por 1.000 habitantes") +
  ylab(label = "Regional") +
  xlab(label = "Solicitações por 1000 habitantes") +
  geom_text(aes(label = relativo), hjust = -0.2)
```

<img src="README_figs/README-by_ur_by_pop-1.png" width="672" />

### Órgão Responsável

Vamos filtrar apenas os órgãos responsáveis que receberam mais que 0.5%
dos solicitações registrados.

``` r
# vamos obter a lista dos órgãos que respondem por mais de 90%
# dos solicitações:
principais <- cwb %>% select(orgao_resp) %>% 
  group_by(orgao_resp) %>% 
  summarise(total = n(),perc_total = format(round((total / nrow(.))*100, digits = 2), nsmall = 2)) %>% 
  filter(perc_total > 0.5)

cwb_orgao_resp <- cwb %>% select(data, orgao_resp) %>% 
  group_by(data, orgao_resp) %>% 
  summarise(total = n()) %>% 
  filter(orgao_resp %in% principais$orgao_resp)

ggplot(cwb_orgao_resp,aes(x = data, y = total, colour = orgao_resp)) + 
  geom_line() +
  labs(title = "Total de solicitações por regional",
       subtitle = "solicitações no 156 da prefeitura de Curitiba",
       x = "Mês",
       y = "Registros") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), 
        axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(),
        legend.direction = "horizontal") +
  guides(colour = guide_legend(nrow = 5)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d %b %Y")
```

<img src="README_figs/README-by_or-1.png" width="672" />

``` r
  
# por facet:
ggplot(cwb_orgao_resp,aes(x = data, y = total, colour = orgao_resp)) + 
  geom_line() +
  labs(title = "Total de solicitações por regional",
       subtitle = "solicitações no 156 da prefeitura de Curitiba",
       x = "",
       y = "") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), 
        axis.text.x = element_blank(),
        legend.title = element_blank()) +
  guides(colour = guide_legend(nrow = 4)) +
  facet_grid(~ orgao_resp) 
```

<img src="README_figs/README-by_or-2.png" width="672" />

Há uma quantidade muito grande de assuntos cadastrada no sistema. Vamos
filtrar apenas os assuntos cujas chamadas ultrapassam 1% do total:

``` r
cwb_assunto <- cwb %>% select(assunto) %>% 
  group_by(assunto) %>% 
  summarise(total = n()) %>% 
  mutate(relativo = round(total/sum(total)*100,digits = 0)) %>% 
  mutate(assunto = fct_reorder(assunto, total)) %>% 
  filter(relativo > 0)

ggplot(cwb_assunto, aes(x = total, y = assunto)) +
  geom_bar(stat = "identity") +
  labs(title = "Solicitações por Assunto",
       subtitle = "Apenas assuntos com mais de 1% do total de solicitações") +
  ylab(label = "Assunto") +
  xlab(label = "Solicitações") +
  theme(axis.title = element_text(), 
        axis.text.x = element_blank(),
        legend.title = element_blank()) +
  geom_text(aes(label = total), hjust = -0.2)
```

<img src="README_figs/README-principais_assuntos-1.png" width="672" />

``` r
### Série histórica dos assuntos acima de x% dos solicitações:
cwb_assunto_2 <- cwb_assunto %>% filter(relativo > 2)

cwb_assunto_serie <- cwb %>% select(datetime, assunto) %>% 
  inner_join(cwb_assunto_2) %>% 
  group_by(day = date(datetime), assunto) %>% 
  summarise(total = n()) %>% 
  mutate(week = as.Date(cut(day, "week")), 
         month = as.Date(cut(day, "month")))
  
ggplot(cwb_assunto_serie, aes(x = day, y = total, colour = assunto)) +
  geom_line() +
  labs(title = "Curitiba 156",
       subtitle = "solicitações pelos principais assuntos - série diária",
       x = "Mês",
       y = "Registros") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), 
        axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank()) +
  guides(colour = guide_legend(nrow = 3)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d %b %Y")
```

<img src="README_figs/README-by_assunto-1.png" width="672" />

``` r

ggplot(cwb_assunto_serie, aes(x = week, y = total, colour = assunto)) +
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Curitiba 156",
       subtitle = "solicitações pelos principais assuntos - série semanal",
       x = "Mês",
       y = "Registros") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), 
        axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank()) +
  guides(colour = guide_legend(nrow = 3)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d %b %Y")
```

<img src="README_figs/README-by_assunto-2.png" width="672" />

``` r

ggplot(cwb_assunto_serie, aes(x = month, y = total, colour = assunto)) +
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Curitiba 156",
       subtitle = "solicitações pelos principais assuntos - série mensal",
       x = "Mês",
       y = "Registros") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), 
        axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank()) +
  guides(colour = guide_legend(nrow = 3)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d %b %Y")
```

<img src="README_figs/README-by_assunto-3.png" width="672" />

Podemos verificar se existe uma correção entre assunto e órgão
responsável, sendo em vários casos o assunto exclusivo do órgão
responsável.

# Ações realizadas de saneamento dos dados

As seguintes ações de saneamento foram conduzidas antes da importação,
todas elas através da ferramenta *Libre Office*:

1.  Conversão de padrão Western-Europe(Windows1251/Latin1) para UTF-8.  
2.  Modificação do header para minúsculas.  
3.  Exclusão da linha 2 que continha hífens (p.ex.: “——–”).  
4.  Atribuição do nome “texto\_resposta\_final” para a coluna 21, que se
    encontrava sem nome.  
5.  Quando o texto da coluna 21 possuía “line-feeds”, o mesmo gerava uma
    nova linha de registro errada apenas com informações parciais do
    texto da coluna 21 da linha anterior. Estas linhas foram excluídas.
    Como não estaremos analisando o conteúdo dos textos das respostas,
    consideramos o esforço de preservar os mesmos desnecessário para a
    análise proposta aqui. Este problema poderia ter sido evitado com a
    inclusão de aspas envolvendo os campos textos durante a extração dos
    dados (ver ações de saneamento números 6 e 7).  
    Ao invés deste procedimento (5) optou-se por excluir os registros
    com número de solicitação inválido após a carga dos dados,
    otimizando o processo.
6.  Troca de qualquer caracter aspas duplas (") dos campos textos para
    não interferir no passo 7.  
7.  Salvar o arquivo envolvendo os campos texto com aspas para melhor
    controle da importação (permite, por exemplo que campos textos
    tenham o caracter “;”, utilizado para separação dos campos).  
8.  Exportação do arquivo utilizando formato UTF-8, separador “;” e
    campos textos envolvidos por aspas-duplas (").
