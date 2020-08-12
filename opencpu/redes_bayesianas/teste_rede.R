#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if(length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      'Script para testar a recomendação da rede: Antes de rodar esse script, tenha certeza que o arquivo que
      contém as redes bayesianas dos cursos foi criado usando o script 'criar_redes.R', pois este script só 
      rodará corretamente com a execução do mesmo.'
 
      Argumentos:
      --curso       -character, Nome do curso como se encontra no banco
      --historico   -character, Códigos das disciplinas do histórico separados por virgulas (Veja exemplo abaixo)
      --disciplinas -character, Códigos das disciplinas da matricula separadas por virgulas (Veja exemplo abaixo)
      --rede        -character, Caminho para onde se encontra salva as redes
      --periodo     -numeric, Período que se refere a consulta 
      --help        -Imprime esse texto
 
      Exemplo:
      Rscript teste_rede.R --curso='engenharia_eletrica_cg' 
                    --historico='1109103,1109035,1113019,1108030'
                    --disciplinas='1108081,1107212,1108023' 
                    --rede='~/eletrica_rede/eletrica_computacao.rda' 
                    --periodo='2' \n\n")
  
  q(save="no")
}

parseArgs <- function(x) strsplit(sub("^--", "", x), "=")
argsDF <- as.data.frame(do.call("rbind", parseArgs(args)))
argsL <- as.list(as.character(argsDF$V2))

names(argsL) <- argsDF$V1

if(is.null(argsL$curso)) {
  print("Curso não encontrado")
  q(save="no")
}

if(is.null(argsL$historico)) {
  print("Historico não encontrado")
  q(save="no")
}

if(is.null(argsL$disciplinas)) {
  print("Disciplinas não encontradas")
  q(save="no")
}

if(is.null(argsL$rede)) {
  print("Rede não encontrada")
  q(save="no")
}

if(is.null(argsL$periodo)) {
  print("Faltando argumetPeriodo não encontrado")
  q(save="no")
}

###################################### Recuperando os dados do curso no banco baseado no periodo fornecido ##########################################
eletrica_input <- function(curso,p){                                                                                                               
  require(RODBC)                                                                                                                                                       
  require(dplyr)                                                                                                                                    
  require(bnlearn)                                                                                                                                  
  require(reshape)                                                                                                                                  
                                                                                                                                                    
  myconn <- odbcConnect("mypre")                                                                                                                    
  historico2 <- sqlQuery(myconn,paste0("SELECT * FROM ", curso, ".historico"))                                                                      
  
  historico2 <- historico2 %>% filter(!(situacao %in% c("Reprovado por Falta", "Trancado")))                                                        
  
  matricula_em_periodo <- historico2 %>% group_by(matricula, periodo) %>%                                                                           
    summarise(set_disc = paste(sort(codigo_disciplina), collapse=" "),#set de disciplinas                           
              n_disc=length(codigo_disciplina), #número de disciplinas                                                                              
              media_set=mean(na.omit(media))) #média das médias do set de disciplinas.                                                              
  
  matricula_em_periodo <- matricula_em_periodo %>% group_by(matricula) %>% mutate(total_periodos_cursados= n())                                     
                                                                                                                                                    
  matricula_em_periodo <- select(matricula_em_periodo , matricula, total_periodos_cursados)%>%                                                      
    group_by(matricula) %>%                                                                                                                         
    summarise(periodos_cursados = first(total_periodos_cursados))
  
  matricula_eletrica <- (merge( historico2,matricula_em_periodo, by="matricula"))                                                                   
  
  matricula_cc_temp <- matricula_eletrica
  
  matricula_cc_temp <- filter(matricula_cc_temp, periodos_cursados == p )
  
  matricula_cc_temp <- select(matricula_cc_temp,matricula,codigo_disciplina,situacao)
  
  input_data = cast(matricula_cc_temp,matricula~codigo_disciplina)
  input_data=input_data[,-1]
  input_data[input_data!='0' & input_data!='1'] = 1
  
  col_names = colnames(input_data) 
                                                                                                                                                    
  input_data[,col_names] = lapply(input_data[,col_names],factor) #transformando as variáveis em fatores                                             
  return(input_data)                                                                                                                                
}                                                                                                                                                   

########################## Retorna a probabilidade de se matricular naquele conjunto de disciplinas #############################  
get_prob<-  function(course_name,p,historico_,matricula_atual_){  							                                                
  require(bnlearn, warn.conflicts = F)														          

  if (length(historico_) == 0 & length(matricula_atual_) == 0){									
    return(0)															
  }																
  else if (all(length(matricula_atual_)==length(historico_)) && all(matricula_atual_==historico_)){				
    return(1)															
  }																
  else{																
    if (p < 2){															
      p <- 2															
    }																
    if (p > 9){															
      p <- 9															
    }																
    model_local_ <- paste(course_name,p, sep = "")										
    str_ <<- NULL														
    str2_ <<- paste("(`", matricula_atual_[-(length(matricula_atual_) + 1)], "`==", + 1, ")",  sep = "", collapse = " & ")	
    #
    if (length(historico_) != 0){												
      str_ <<- paste("(`", historico_[-(length(historico_) + 1)], "`==", + 1, ")",  sep = "", collapse = " & ")			
    }																#
    #
    if (is.null(str_) == FALSE){												
      r <-  cpquery(vector[[model_local_]], eval(parse(text = str2_)), eval(parse(text = str_)))				
      return(r)															
    }																
    else{															
      r <-  cpquery(vector[[model_local_]], eval(parse(text = str2_)), evidence=TRUE)						
      return(r)															
    }    															
  }																
}                   														                                                                                

########################## Retorna a probabilidade/recomendação de cada disciplina fornecida ####################################
recomenda <- function(course_name,p,historico_,disciplinas){
  
  require(bnlearn, warn.conflicts = F)
  
  if (length(historico_) == 0 & length(disciplinas) == 0){
    return(0)
  }
  else if (all(length(disciplinas)==length(historico_)) && all(disciplinas==historico_)){
    return(1)
  }
  else{
    if (p < 2){
      p <- 2
    }
    if (p > 9){
      p <- 9
    }
    model_local_ <- paste(course_name,p, sep = "")
    str_ <<- NULL
    ans <- c()
    
    for (d in disciplinas){
      str2_ <<- paste("(`", d, "`==", +1, ")",  sep = "")
      
      if (length(historico_) != 0){
        str_ <<- paste("(`", historico_[-(length(historico_) + 1)], "`==", + 1, ")",  sep = "", collapse = " & ")
      }
      
      if (is.null(str_) == FALSE){
        r <-  cpquery(vector[[model_local_]], eval(parse(text = str2_)), eval(parse(text = str_)))
        ans <- c(ans, r)
      }
      else{
        r <-  cpquery(vector[[model_local_]], eval(parse(text = str2_)), evidence=TRUE)
        ans <- c(ans, r)
      }
    }
    names(ans) <- disciplinas
    ans <- sort(ans, decreasing = T)
    return(ans)
  }
}

#################### Função principal ######################
main <- function(){
  
  historico <- as.numeric(unlist(strsplit(argsL$historico,",")))
  disciplinas <- as.numeric(unlist(strsplit(argsL$disciplinas,",")))
  periodo <- as.numeric(argsL$periodo)
  prob<- get_prob(argsL$curso, periodo, historico, disciplinas)
  cat(paste0("\nProbabilidade de se matricular nessas disciplinas, dado seu histórico: ",round(prob,3)*100,"%\n\n"))
  cat("Recomendação de disciplinas:\n")
  rec <- recomenda(argsL$curso, periodo, historico, disciplinas)
  print(rec)
  cat("\n")
}

carregaRede <- tryCatch({
              load(argsL$rede) #Carrega a rede referente aquele curso e periodo
            }, 
            error = function(e) {
              print("Não foi possível carregar a rede. Verifique o caminho do arquivo.")
              q(save = "no")
          })

main() #Roda o script
