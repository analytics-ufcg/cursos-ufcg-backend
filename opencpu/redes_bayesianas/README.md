#Esse diretório contém os scripts relacionados a criação e teste das redes bayesianas 

##criar_redes.R

'Script usado para criar as redes do curso da ufcg. Caso o argumento --cursos não seja
      fornecido, então o script irá criar a rede para todos os cursos.'
 
      Argumentos:
      --cursos      -character, Nomes dos cursos como se encontra no banco e separados por virgula (Veja exemplo abaixo)
      --path        -character, Diretório onde o arquivo contendo as redes será salvo
      --help        -Imprime esse texto
 
      Exemplo:
      Rscript criar_redes.R --cursos='engenharia_eletrica_cg,ciencia_da_computacao_d_cg' --path='/home/user/'

##teste_rede.R 
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
                    --periodo='2'
