#
# Parâmetros:
#   - diretório de saída (deve existir)
#   - URL da instância do SonarQube (sem o http)
#   - datahora inicial (formato Java: yyyy-MM-dd'T'HH:mm:ss)
#   - datahora final (formato Java: yyyy-MM-dd'T'HH:mm:ss)
#   - sequência de uma ou mais chaves de métricas do SonarQube
#   (ver http://docs.sonarqube.org/display/SONAR/Metric+Definitions)
# 
# Script que extrai, dentro de um período, os valores da(s) métrica(s)
# de todos os projetos registrados numa instância do SonarQube, e os
# salva em arquivo(s) CSV, para análise posterior.
#
# O(s) arquivo(s) são salvo(s) no diretório de saída, recebem
# o mesmo nome da métrica passada, e possuem a extensão TXT. Nos exemplos
# mostrados mais abaixo, apenas um arquivo é gerado no primeiro e no
# segundo casos, enquanto no terceiro são três os arquivos resultantes.
#
# As métricas podem ser passadas individualmente, separadas por
# espaço, ou combinadas, separadas por vírgula. Quando combinadas,
# o valor resultado é a soma delas. No segundo exemplo mostrado
# mais abaixo, a quantidade de violações com severidade blocker e
# critical são somadas para produzir o valor resultado.
#
# Os valores das métricas são obtidos de todas as análises realizadas,
# dentro do período informado, mas são agrupadas por mês. O valor que
# vale, como resultado de cada mês, é o produzido pela última análise.
#
# As informações são obtidas da API Json do SonarQube, por isso o
# R script requer a biblioteca jsonlite, responsável por transformar
# o texto Json em objetos R. Isto é feito pela função fromJSON.
#
# São feitas várias chamadas à API REST do SonarQube. A primeira
# obtem todos os projetos registrados (resources) e as subsequentes
# obtem os valores de cada métrica solicitada de cada projeto registrado
# (timemachine). Dependendo da quantidade de projetos e das métricas
# passadas por parâmetro, a execução do script pode demorar mais, ou menos.
#
# Exemplos de execução do script:
# Rscript metrics.R C:\\sonarqube\\data sonarqube.com 2016-01-01T00:00:00 2016-06-30T23:59:59 ncloc
# Rscript metrics.R C:\\sonarqube\\data sonarqube.com 2016-01-01T00:00:00 2016-06-30T23:59:59 blocker_violations,critical_violations
# Rscript metrics.R C:\\sonarqube\\data sonarqube.com 2016-01-01T00:00:00 2016-06-30T23:59:59 ncloc blocker_violations critical_violations
#

# functions

getResources <- function(sonarURL) {
  temp <- tempfile()
  url <- paste("http://", sonarURL, "/api/resources?format=json", sep="")
  download.file(url, temp, quiet=T)
  resources <- fromJSON(temp)
  rm(temp)
  return(resources$key)
}

getMetricValues <- function(sonarURL, resource, metrics, fromDateTime, toDateTime) {
  temp <- tempfile()
  url <- paste("http://", sonarURL, "/api/timemachine/index?resource=", resource, "&metrics=", metrics, "&fromDateTime=", fromDateTime, "&toDateTime=", toDateTime, sep="")
  download.file(url, temp, quiet=T)
  timemachine <- fromJSON(temp)
  rm(temp)
  
  cells <- timemachine$cells[[1]]
  if (length(cells) == 0) {
    return(data.frame(m=character(), v=integer()))
  } else {
    cells$v <- unlist(lapply(cells$v, function(x) sum(x)))
    maxs <- aggregate(cells["d"], list(m=substr(cells$d, 1, 7)), max)
    return(merge(maxs, cells)[c("m", "v")])
  }
}

getResult <- function(sonarURL, resources, metrics, fromDateTime, toDateTime) {
  for (resource in resources) {
    values <- getMetricValues(sonarURL, resource, metrics, fromDateTime, toDateTime)
    if (nrow(values) > 0) {
      colnames(values) <- c("mes", resource)
      if (exists("result"))
        result <- merge(values, result, by="mes", all=T, sort=T)
      else
        result <- values
    }
  }
  return(result)
}

saveResults <- function(output, sonarURL, resources, metrics, fromDateTime, toDateTime) {
  setwd(output)
  for (metric in metrics) {
    result <- getResult(sonarURL, resources, metric, fromDateTime, toDateTime)
    write.csv2(result, paste(metric, ".txt", sep=""), row.names=F)
  }
}

# execution

require(jsonlite)
args <- commandArgs(TRUE)
if (length(args) < 5) {
  stop("Parametros obrigatorios:\n  - diretorio de saida (deve existir)\n  - URL da instancia do SonarQube (sem o http)\n  - datahora inicial (formato: yyyy-MM-dd'T'HH:mm:ss)\n  - datahora final (formato: yyyy-MM-dd'T'HH:mm:ss)\n  - sequencia de uma ou mais chaves de metricas do SonarQube\n  (ver http://docs.sonarqube.org/display/SONAR/Metric+Definitions)")
}

output <- args[1]
sonarURL <- args[2]
resources <- getResources(sonarURL)
metrics <- args[5:length(args)]
fromDateTime <- args[3]
toDateTime <- args[4]

saveResults(output, sonarURL, resources, metrics, fromDateTime, toDateTime)
