
# All csv files from data folder will be imported
files <- list.files(path = "data", pattern = "*.csv")
files <- files %>% paste0("./data/", .)
tbl <- files %>% map_df(~ read_csv2(., col_types = cols(.default = "c")))

# Some broken lines generate fake columns (should have sanitized better), 
# ... lets filter only expected columns:
tbl <- tbl %>% select("solicitacao", 
                      "tipo",
                      "orgao", 
                      "data", 
                      "horario", 
                      "assunto",
                      "subdivisao", 
                      "descricao", 
                      "logradouro_ass",
                      "bairro_ass",
                      "regional_ass", 
                      "meio_resposta",
                      "observacao",
                      "sexo",
                      "bairro_cidadao",
                      "regional_cidadao", 
                      "data_nasc", 
                      "tipo_cidadao", 
                      "orgao_resp", 
                      "resposta_final",
                      "texto_resposta_final")

# Converting solicitacao to numeric generate NA values for invalid rows
tbl$solicitacao <- as.numeric(tbl$solicitacao)

# data.table has better performance than data.frame:
cwb <- as.data.table(tbl)

# Let`s count how good our data is:`
total_records.ori <- dim(cwb)[1]

# solicitacao with NA means bad rows still came from sanitization
cwb <- na.omit(cwb, cols = "solicitacao")
total_records.invalid <- (total_records.ori - dim(cwb)[1])
# drop out all duplicated solicitaco numbers
setkey(cwb,solicitacao)
cwb <-  unique(cwb)
total_records.duplicated = total_records.ori - total_records.invalid - dim(cwb)[1]

# Replace empty string by NA
cwb$regional_ass <- str_replace(cwb$regional_ass, pattern = "NULL", replacement = "")
cwb[cwb == ""] <- NA

# Make char columns as factors
cwb$tipo <- as.factor(cwb$tipo)
cwb$orgao <- as.factor(cwb$orgao)
cwb$assunto <- as.factor(cwb$assunto)
cwb$subdivisao <- as.factor(cwb$subdivisao)
cwb$bairro_ass <- as.factor(cwb$bairro_ass)
cwb$regional_ass <- str_replace(cwb$regional_ass, pattern = "Unidade Regional", replacement = "UR")
cwb$regional_ass <- as.factor(cwb$regional_ass)
cwb$bairro_cidadao <- as.factor(cwb$bairro_cidadao)
cwb$sexo <- as.factor(cwb$sexo)
cwb$tipo_cidadao <- as.factor(cwb$tipo_cidadao)
cwb$orgao_resp <- str_replace(cwb$orgao_resp, pattern = "Secretaria|SECRETARIA", replacement = "S.")
cwb$orgao_resp <- str_replace(cwb$orgao_resp, pattern = "Municipal|MUNICIPAL", replacement = "M.")
cwb$orgao_resp <- as.factor(cwb$orgao_resp)

cwb <- cwb %>% mutate(datetime = dmy_hms(paste(cwb$data, cwb$horario, sep = " ")))
cwb$data <- as.Date(cwb$date, "%d/%m/%Y")
cwb$data_nasc <- dmy(cwb$data_nasc)

# Extra columns for year, month, day organization
cwb <- cwb %>% mutate(ano = year(datetime)) %>% 
  mutate(mes = month(datetime)) %>% 
  mutate(dia = day(datetime)) %>% 
  mutate(periodo = ifelse(am(datetime) == TRUE, "manhã", "tarde")) %>% 
  mutate(dia_semana = wday(datetime))

# Population by UR from wikipedia
regional <- data.table(nome = c("UR Matriz", 
                                "UR Boa Vista", 
                                "UR Cajuru", 
                                "UR Portao", 
                                "UR Boqueirao", 
                                "UR Pinheirinho", 
                                "UR Santa Felicidade", 
                                "UR CIC", 
                                "UR Bairro Novo", 
                                "UR Tatuquara"), 
                       populacao = c(208674, 
                                     268556, 
                                     232563, 
                                     184437, 
                                     205248, 
                                     151202, 
                                     166525, 
                                     200271, 
                                     163651, 
                                     112873))
regional$nome <- as.factor(regional$nome)
regional$populacao <- as.integer(regional$populacao)

# Just a sample ...
amostra <- cwb %>% slice_sample(n = 8) %>% 
  select(solicitacao, 
         tipo,
         orgao_resp,
         data,
         assunto,
         regional_ass)
