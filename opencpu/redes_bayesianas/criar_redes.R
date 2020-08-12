#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if(length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      'Script usado para criar as redes do curso da ufcg. Caso o argumento --cursos não seja
      fornecido, então o script irá criar a rede para todos os cursos.'
 
      Argumentos:
      --cursos      -character, Nomes dos cursos como se encontra no banco e separados por virgula (Veja exemplo abaixo)
      --path        -character, Diretório onde o arquivo contendo as redes será salvo
      --help        -Imprime esse texto
 
      Exemplo:
      Rscript criar_redes.R --cursos='engenharia_eletrica_cg,ciencia_da_computacao_d_cg' --path='/home/user/' 
  \n\n")
  q(save="no")
}

parseArgs <- function(x) strsplit(sub("^--", "", x), "=")
argsDF <- as.data.frame(do.call("rbind", parseArgs(args)))
argsL <- as.list(as.character(argsDF$V2))

names(argsL) <- argsDF$V1

if(is.null(argsL$path)) {
  cat("Erro:\n\tPath não fornecido\n")
  q(save="no")
}

vector <- list() #Vetor que armazenará todas as redes. Ao fim da execução, ele será salvo em um arquivo.
names <- list()

i <- 1

# O método abaixo salva as redes no vetor global declarado logo acim
salvaModelo <- function(matricula_cc,curso,p){
  matricula_cc_temp <- matricula_cc
  
  matricula_cc_temp <- filter(matricula_cc_temp, periodos_cursados == p ) # Filtrando pelo periodo correto
  
  matricula_cc_temp <- select(matricula_cc_temp,matricula,codigo_disciplina,situacao)
  input_data = cast(matricula_cc_temp,matricula~codigo_disciplina, value = "situacao", fun.aggregate = "length")
  input_data=input_data[,-1]
  input_data[input_data!='0' & input_data!='1'] = 1
  
  col_names = colnames(input_data) 
  
  ########################################################################  

  # Quando filtramos pelo período, provavelmente alguma(s) disciplina(s)
  # vão sair do dataframe de treino, de modo que precisamos garantir que essas
  # disciplinas se encontrem nele com valor '0', pois significa que ningúem daquele
  #período coloca tais disiciplinas."
  for (d in unique(matricula_cc$codigo_disciplina)){
    if (!(d %in% col_names)){
      temp <- data.frame(matrix(0, ncol = 1, nrow = nrow(input_data)))
      colnames(temp) <- c(d)
      input_data <- cbind(input_data,temp)
    }
  }
    
  ########################################################################
  ########################################################################
  # Esse bloco de código garante que todas as disciplinas tenham pelo menos uma
  #observação me que a mesma não se encontre, ou seja, com valor '0' "
  
  temp <- input_data [1,]
  for (z in 1:ncol(temp)){
    temp[1,z] <- 0
  }
  input_data <- rbind(input_data, temp)
  ########################################################################
  
  input_data[,col_names] = lapply(input_data[,col_names],factor) #transformando as variáveis em fatores
  bnet_grafo = hc(input_data)
  
  tpc4 = bn.fit(bnet_grafo,data=input_data)
  
  names[[i]] <<- paste0(curso, p)
  vector[[i]] <<- tpc4
  
  i <<- i + 1
}

# A funçao abaixo acessa o banco a fim de recuperar o histórico dos alunos do curso passado como parâmetro

criaRede<- function(curso){
  library(RODBC, warn.conflicts = F, quietly = T)
  library(dplyr, warn.conflicts = F, quietly = T)
  library(bnlearn, warn.conflicts = F, quietly = T)
  library(reshape, warn.conflicts = F, quietly = T)
  
  myconn <- odbcConnect("mypre")
  historico2 <- sqlQuery(myconn,paste0("SELECT * FROM ", curso, ".historico"))
  
  historico2 <- historico2 %>% filter(!(situacao %in% c("Reprovado por Falta", "Trancado","Reprovado")))
  
  matricula_em_periodo <- historico2 %>% group_by(matricula, periodo) %>%
    summarise(set_disc = paste(sort(codigo_disciplina), collapse=" "),#set de disciplinas
              n_disc=length(codigo_disciplina), #número de disciplinas
              media_set=mean(na.omit(media))) #média das médias do set de disciplinas.
  
  matricula_em_periodo <- matricula_em_periodo %>% group_by(matricula) %>% mutate(total_periodos_cursados= n())
  
  matricula_em_periodo <- select(matricula_em_periodo , matricula, total_periodos_cursados)%>% 
    group_by(matricula) %>% 
    summarise(periodos_cursados = first(total_periodos_cursados))
  
  matricula_cc <- (merge( historico2,matricula_em_periodo, by="matricula"))
  
  periodos <- unique(matricula_cc$periodos_cursados)
  
  for (p in periodos){
    if (p != 1){
      x <- tryCatch(salvaModelo(matricula_cc,curso,p), error = function(e) e)
      if(inherits(x, "error")){
        cat("\nErro encontrado ao salvar o rede: ")
        cat(x)
        next
      } 
    }
  }
  
  cat(paste0("Redes do Curso: ",curso, " criada para cada um dos seguintes períodos:\n"))
  cat(sort((unique(matricula_cc$periodos_cursados))),"\n")
}

# A função abaixo itera sobre os cursos e cria as redes para cada período de cada curso
criarRedes <- function(lista_cursos){
  cursos <- NULL
  if (is.null(lista_cursos)){
    cursos <- c("administracao_d_cg"
                ,"administracao_n_cg"
                ,"administracao_n_ss",
                "agronomia_d_pl",
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
  }
  else{
    cursos <- lista_cursos
  }
  
  for (curso in cursos){
    cat("-------------------------------------------------------------------------\n")
    cat("Iniciando criação das redes para o curso de", curso,"\n")
    x <- tryCatch(criaRede(curso), error = function(e) e)
    if(inherits(x, "error")) next
  }
  cat("Execução terminada...\n")
}

if (is.null(argsL$cursos)){ # Se não foi especificado nenhum curso, então o script cria a rede para todos os cursos
    criarRedes(NULL)
} else{ # Casos contrário, cria apenas para os cursos passados como parâmetro
    lista_cursos <- as.character(unlist(strsplit(argsL$cursos,",")))
    criarRedes(lista_cursos)
}

names(vector) <- names
save(vector, file = paste0(argsL$path,"redes.rda")) # Salva as redes em um único arquivo no diretório passado na linha de comando
