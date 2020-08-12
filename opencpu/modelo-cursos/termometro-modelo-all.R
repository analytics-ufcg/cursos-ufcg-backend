library(dplyr)

# historico <- read.csv("~/Documents/ad/termometro/historico.csv", sep="")

## Função que recebe os dados de histórico referentes a um curso e retorna
## um dataset esparso Para este curso. Este dataset será utilizado para o
## treinamento de um modelo.
geraDfEsparso <- function(historico) {

  #filtrando reprovações por falta e trancamentos.
  historico <- historico %>% filter(!(situacao %in% c("Reprovado por Falta", "Trancado")))

  disciplinas <- unique(historico$codigo_disciplina)
  disciplinas <- as.factor(disciplinas)

  #Feature CRA para cada matrícula.
  matricula_cra <- historico %>% group_by(matricula) %>% summarise(cra=mean(na.omit(media)))


  ##Gerando features referentes a um set de disciplinas.
  ##Set de disciplinas (set_disc) se refere ao grupo de disciplinas associadas a uma matrícula em um dado perído.
  ##Ex.: matrícula: 123123
  ##     perído: 2012.1
  ##     set_disc: eda, leda, prob, linear, GI.
  ##     media_set: 6.8
  ##     n_disc: 5
  ##     n_reprov: 1
  matricula_em_periodo <- historico %>% group_by(matricula, periodo) %>%
    summarise(set_disc = paste(sort(codigo_disciplina), collapse=" "),#set de disciplinas
              n_disc=length(codigo_disciplina), #número de disciplinas
              n_reprov = length(media[media < 5 || is.na(media)]), #número de reprovações
              media_set=mean(na.omit(media))) #média das médias do set de disciplinas.


  ##Adicionando CRA de uma matrícula ao conjunto de variáveis.
  matricula_em_periodo <- merge(matricula_em_periodo, matricula_cra, by="matricula")

  #Calcula período de ingresso
  periodo_ingresso <- historico %>% group_by(matricula) %>% summarise(ingresso = min(as.numeric(periodo)))

  ##Adicionando período de ingresso de uma matrícula ao conjunto de variáveis.
  matricula_em_periodo <- merge(matricula_em_periodo, periodo_ingresso, by="matricula")

  ##Adicionando feature de TOTAL de períodos cursados
  matricula_em_periodo <- matricula_em_periodo %>% group_by(matricula) %>% mutate(total_periodos_cursados= n())

  ##Adicionando feature de número de períodos cursados.
  matricula_em_periodo <- matricula_em_periodo %>% group_by(matricula, periodo) %>%
    mutate(num_periodos_cursados=floor((as.numeric(periodo) - as.numeric(ingresso))*2 + 1))

  matricula_em_periodo$num_periodos_cursados <- ifelse(matricula_em_periodo$num_periodos_cursados >
                                                         matricula_em_periodo$total_periodos_cursados,
                                                       matricula_em_periodo$total_periodos_cursados,
                                                       matricula_em_periodo$num_periodos_cursados)


  ##Criando esqueleto do dafaframe de saída (esparso).
  df = data.frame(matrix(NA, ncol = length(disciplinas), nrow = nrow(matricula_em_periodo)))
  colnames(df) <- disciplinas
  df <- cbind(df,matricula_em_periodo)


  ## Gerando colunas do dataframe esparso.
  ## As disciplinas do curso serão representadas por variáveis categóricas possuindo
  ## um valor booleando, indicando se o aluno se matriculou naquela disciplina ou não.
  ## Obs.: A observação ainda é referente ao set de disciplinas.
  time <- proc.time()
  for(i in 1:nrow(df)) {
    row <- df[i,]
    disc <- strsplit(row$set_disc, " ")
    for (d in disc){
      row[d] <- TRUE
      # print(row[d])
    }
    df[i,] <- row
  }
  time <- proc.time() - time

  df[is.na(df)] <- FALSE
  ## Removendo as colunas de matrícula e set_disc. set_disc não se faz mais necessária
  ## pois a a forma como esta variável é representada foi substituída pelas múltiplas
  ## colunas das disciplinas do curso.
  df <- df %>% select(-matricula, -set_disc)
  IDs <- data.frame(id =1:nrow(df))
  df <- cbind(id = IDs$id, df)


  ##
  for (i in 2:(length(df)-8)){
    df[,i] <- as.factor(df[,i])
  }

  ###Transformando variáveis para factors.
  df$ingresso <- as.factor(df$ingresso)
  df$periodo <- as.factor(df$periodo)
  df$n_reprov <- as.factor(df$n_reprov)

  ## Gerando valores para variável resposta.
  ## A variável de interesse (target) é construída a partir de um mapeamento
  ## da variável n_reprov (número de reprovações). Sendo assim, o modelo prevê
  ## classes que fazem referência ao número de reprovações de uma matrícula em
  ## um dado período.
  df <- df %>% mutate(target = ifelse(n_reprov == 0, "VERDE",
                                      ifelse(n_reprov == 1, "AMARELO","VERMELHO")) )

  df$target <- as.factor(df$target)

  ##Feature POS_REUNI.
  df$pos_reuni <- as.factor(ifelse( as.numeric(as.character(df$ingresso)) >= 2009.1 , TRUE, FALSE))


  return (df);
}

##Aplica uma técnica de balanceamento chamada SMOTE.
##Retorna um conjunto de dados com MAIS observações e mais balanceado.
geraDfBalanceado <- function(df) {

  library(unbalanced)

  df$balance <- factor(ifelse(df$target == "VERDE", 0, 1))
  n<-ncol(df)
  output<-df$balance
  input<-df[,-n] #todas as colunas, exceto a balance.

  data<-ubBalance(X= input, Y=as.factor(output), type="ubSMOTE",positive=1, percOver=300, percUnder=150, verbose=TRUE)

  detach(package:unbalanced)

  df_resp <- data$X

  return (df_resp);
}

##Função que recebe o dataframe no formato esparso e retorna um modelo.
##Esta função deve ser utilizada para cada dataset esparso de cada curso.
geraModelo <- function(df){

  library(caret)

  fitControl <- trainControl(## 10-fold CV
    method = "cv",
    number = 10)

  time <- proc.time()
#   treeFit <- train(target ~ ., data = select(df,-id,-n_reprov),
#                    method = "C5.0",
#                    trControl = fitControl,
#                    tuneGrid=expand.grid(model = "tree", winnow = FALSE, trials = c(1:10)))
  treeFit <- train(target ~ ., data = select(df,-id,-n_reprov, -periodo, -ingresso, -total_periodos_cursados, -num_periodos_cursados, -pos_reuni),
                   method = "C5.0",
                   trControl = fitControl,
                   tuneGrid=expand.grid(model = "tree", winnow = FALSE, trials = c(1:10)))

  time <- proc.time() - time
  detach(package:caret)
  detach(package:plyr)
  return (treeFit);
}

##Salva um modelo com um dado nome.
salvaModelo <- function(modelo,nome_curso){
  save(modelo,file = paste0(nome_curso, ".rda"))
}

##Cria um modelo para um curso.
criaModelo <- function(historico_curso, nome_curso){
  df_esparso <- geraDfEsparso(historico_curso)
  df_balanceado <- geraDfBalanceado(df_esparso)
  modelo <- geraModelo(df_balanceado)
  # predict(modelo
  # salvaModelo(modelo, nome_curso)
}

# criaModelo(historico, "computacao")

cursos <- c(
  # ok                "administracao_d_cg"
  # ok                ,"administracao_n_cg"
  # failed               ,"administracao_n_ss"
  # ok               "agronomia_d_pl"
  "arquitetura_e_urbanismo_d_cg"
  ,"arte_e_midia_d_cg"
  ,"ciencia_da_computacao_d_cg"
  ,"ciencias_biologicas_lic_d_ct"
  ,"ciencias_biologicas_lic_d_pt"
  ,"ciencias_biologicas_lic_m_cz"
  ,"ciencias_biologicas_lic_n_ct"
  ,"ciencias_biologicas_lic_n_pt"
  ,"ciencias_contabeis_n_ss"
  ,"ciencias_economicas_m_cg"
  ,"ciencias_economicas_n_cg"
  ,"ciencias_sociais_lic_d_cg"
  ,"ciencias_sociais_lic_n_sm"
  ,"comunicacao_social_d_cg"
  ,"comunicacao_social_n_cg"
  ,"curso_sup_de_tecn_em_agroecologia_d_sm"
  ,"curso_sup_de_tecn_em_gestao_publica_n_sm"
  ,"design_d_cg"
  ,"educacao_do_campo_lic_d_sm"
  ,"enfermagem_d_cg"
  ,"enfermagem_d_ct"
  ,"enfermagem_d_cz"
  ,"eng_de_biotecnologia_e_bioprocessos_d_sm"
  ,"engenharia_agricola_d_cg"
  ,"engenharia_ambiental_d_pl"
  ,"engenharia_civil_d_cg"
  ,"engenharia_de_alimentos_d_pl"
  ,"engenharia_de_biossistemas_d_sm"
  ,"engenharia_de_materiais_d_cg"
  ,"engenharia_de_minas_d_cg"
  ,"engenharia_de_petroleo_d_cg"
  ,"engenharia_de_producao_d_cg"
  ,"engenharia_de_producao_d_sm"
  ,"engenharia_eletrica_cg"
  ,"engenharia_florestal_d_pt"
  ,"engenharia_mecanica_d_cg"
  ,"engenharia_quimica_d_cg"
  ,"estatistica_d_cg"
  ,"farmacia_d_ct"
  ,"filosofia_bac_n_cg"
  ,"filosofia_lic_n_cg"
  ,"fisica_lic_d_cg"
  ,"fisica_lic_d_ct"
  ,"fisica_lic_n_ct"
  ,"fisica_lic_n_cz"
  ,"geografia_lic_d_cg"
  ,"geografia_lic_n_cg"
  ,"geografia_licenciatura_m_cz"
  ,"geografia_licenciatura_n_cz"
  ,"historia_lic_d_cg"
  ,"historia_lic_d_parfor_cg"
  ,"historia_lic_m_cz"
  ,"historia_lic_n_cg"
  ,"historia_lic_n_cz"
  ,"letras_espanhol_licenciatura_n_cg"
  ,"letras_ling_port_ling_franc_lic_d_cg"
  ,"letras_lingua_inglesa_lic_d_cg"
  ,"letras_lingua_inglesa_lic_d_cz"
  ,"letras_lingua_portuguesa_lic_d_cg"
  ,"letras_lingua_portuguesa_lic_m_cz"
  ,"letras_lingua_portuguesa_lic_n_cg"
  ,"letras_lingua_portuguesa_lic_n_cz"
  ,"matematica_bac_d_cg"
  ,"matematica_lic_d_cg"
  ,"matematica_lic_d_ct"
  ,"matematica_lic_d_cz"
  ,"matematica_lic_d_parfor_cg"
  ,"matematica_lic_n_cg"
  ,"matematica_lic_n_ct"
  ,"medicina_d_cg"
  ,"medicina_d_cz"
  ,"medicina_veterinaria_d_pt"
  ,"meteorologia_d_cg"
  ,"musica_bac_d_cg"
  ,"musica_lic_d_cg"
  ,"nutricao_d_ct"
  ,"nutricao_n_ct"
  ,"odontologia_d_pt"
  ,"pedagogia_lic_m_cg"
  ,"pedagogia_lic_n_cg"
  ,"pedagogia_licenciatura_m_cz"
  ,"pedagogia_licenciatura_n_cz"
  ,"psicologia_n_cg"
  ,"quimica_lic_d_ct"
  ,"quimica_lic_n_ct"
  ,"quimica_lic_n_cz"
  ,"quimica_lic_n_parfor_cg"
  ,"servico_social_m_ss")

getHistoricoCurso <- function(nome_curso){
  library(RODBC)
  myconn <- odbcConnect("mypre")
  historico <- sqlQuery(myconn,paste0("SELECT * FROM ",nome_curso, ".historico"))
  detach(package:RODBC)
  return(historico);
}


##Cria os modelos para todos os nomes de cursos contidos no vetor.
criaModeloForAll <- function(cursos){

  for (curso in cursos){
    historico <- getHistoricoCurso(curso)
    x <- tryCatch(criaModelo(historico, curso), error = function(e) e)
    if(inherits(x, "error")) next
    # criaModelo(historico, curso)
  }
  return ("ok");
}


criaModeloForAll(cursos)
