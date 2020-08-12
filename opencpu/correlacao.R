cor.pvalue <- function(X,method, use) {
  dfr = nrow(X) - 2
  R <- cor(X,method=method, use= use)
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R
}
countParesAlunos <- function(x,stringsAsFactors=FALSE,...){
  matriz_pares <- crossprod(!is.na(x))
  id <- lower.tri(matriz_pares)
  df_pares <- matriz_pares[id]
  disciplinaA <- colnames(matriz_pares)[col(matriz_pares)[id]]
  disciplinaB <- rownames(matriz_pares)[row(matriz_pares)[id]]
  count <- df_pares
  data.frame(disciplinaA,disciplinaB,count)
}
calcula_correlacao <- function(schema, periodo_inicial, periodo_final){
  library('RODBC')
  library(reshape)
  channel <- odbcConnect("MyPRE",DBMSencoding="latin1");
  query_desempenho <- c("select matricula, codigo_disciplina, zscore from ", schema, ".historico where situacao = 'Aprovado' and periodo >= ", periodo_inicial, " and periodo <= ", periodo_final, ";")
  query_nomes_disciplinas <- c("select codigo_disciplina, disciplina, semestre from ", schema, ".disciplinas;")
  zscores_curso_escolhido <- sqlQuery(channel, paste(query_desempenho,collapse=""), stringsAsFactor = FALSE)
  nomes_disciplinas <- sqlQuery(channel, paste(query_nomes_disciplinas,collapse=""), stringsAsFactor = FALSE)
  cursos <- sqlQuery(channel, 'select `schema` from preanalytics2015.cursos',  stringsAsFactor = FALSE)
  odbcClose(channel)
  zscores_curso_escolhido <- merge(zscores_curso_escolhido,nomes_disciplinas,by.x=('codigo_disciplina'), by.y= ('codigo_disciplina'))
  correlacao <- cast(zscores_curso_escolhido, matricula ~ disciplina, value = 'zscore', fun.aggregate = mean)
  ncorrelacoes <- countParesAlunos(correlacao[,-1])
  correlacao <- cor.pvalue(correlacao,use = "pairwise.complete.obs",method = 'spearman')
  correlacao[lower.tri(correlacao) & t(correlacao) > 0.05] <- NA
  correlacao[upper.tri(correlacao,diag=T)] <- NA
  correlacao <- melt(correlacao)
  correlacao <- na.omit(correlacao)
  correlacao <- merge(correlacao,ncorrelacoes,by.x = c('X1','X2'), by.y = c('disciplinaB','disciplinaA'))
  correlacao <- correlacao[correlacao$count > 20,]
  correlacao <- correlacao[,-4]
  colnames(correlacao) <- c('disciplina_A', 'disciplina_B', 'correlacao')
  correlacao <- correlacao[order(correlacao$correlacao),]
  correlacao_filtrada <- tail(correlacao, 20)
  nomes_disciplinas$codigo_disciplina <- NULL
  correlacao_filtrada <- merge(correlacao_filtrada,nomes_disciplinas,by.x = ('disciplina_A'), by.y = ('disciplina'))
  names(correlacao_filtrada)[4] <- 'semestre_A'
  correlacao_filtrada <- merge(correlacao_filtrada,nomes_disciplinas,by.x = ('disciplina_B'), by.y = ('disciplina'))
  names(correlacao_filtrada)[5] <- 'semestre_B'
  correlacao_filtrada <- na.omit(correlacao_filtrada)
  correlacao_filtrada$correlacao <- round(correlacao_filtrada$correlaca,digits = 2 )
  correlacao_filtrada
}
