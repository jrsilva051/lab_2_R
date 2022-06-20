##### Dashboard Analítico ####
setwd("C:/Users/junior.silva/OneDrive - Ibratan Ilimitada/Documentos/pessoal/DataScience/CursoMachineLearningR/Lab_2_BigData")
getwd()

library(dplyr)
library(tidyr)
library(readxl)
library(readr)

# Load Datasets
dados_netflix <- read.csv("datasets_originais/dados_netflix_dec_2021.csv")
#View(dataset1)

dados_pib <- read.csv("datasets_originais/dados_world_bank.csv", header=FALSE)
#View(dados_pib)

dados_salario <- read.csv("datasets_originais/dados_desigualdade_salarial_harvard.csv")
#View(dados_salario)

dados_IMDB <- read_tsv("datasets_originais/dados_imdb.tsv")
#View(dados_IMDB)

dados_top10 <- read_excel("datasets_originais/top_10_shows_netflix.xlsx")
#View(dados_top10)

dados_sub <- read.csv("datasets_originais/assinantes_netflix_jul_2021.csv")
#View(dados_sub)

countrycode <- read.csv("datasets_originais/wikipedia-iso-country-codes.csv")
#View(countrycode)


# ETL
#dataset1
dados_netflix$basic_standard_diff = (dados_netflix$Cost.Per.Month...Premium.... - dados_netflix$Cost.Per.Month...Basic....)
dados_netflix$standard_premium_diff = (dados_netflix$Cost.Per.Month...Premium.... - dados_netflix$Cost.Per.Month...Standard....)

names(dados_pib)[names(dados_pib) == 'V1'] <- 'Country'
dados_netflix_pib <- merge(dados_netflix, dados_pib, by="Country")


dados_netflix_pib2020 <- dados_netflix_pib[-c(11:72, 74, 75)]
names(dados_netflix_pib2020)[names(dados_netflix_pib2020) == 'V64'] <- "2020 GDP (World Bank)"

dados_salario <- dados_salario[,c(1:3)]
dados_salarios_ano <- dados_salario %>% group_by(country) %>% summarise(max=max(year, na.rm=TRUE))

dados_salario <- merge(dados_salario, dados_salarios_ano, by.x=c("country", "year"), by.y=c("country", "max"))
dados_netflix_pib2020 <- merge(dados_netflix_pib2020, dados_salario, by.x = c("Country"), by.y=c("country"))

dados_sub <- dados_sub[,c(1,23,24)]
complete <- merge(dados_netflix_pib2020, dados_sub, by=c("Country"))

countrycode <- countrycode[,c(1,3)]
complete <- merge(complete, countrycode, by.x=c("Country"), by.y=c("English.short.name.lower.case"))

write.csv(complete, "datasets_limpos/dataset1.csv", row.names=FALSE)

#dataset2
genero <- dados_IMDB[,-c(1,4:8)]
names(genero)[names(genero) == 'primaryTitle'] <- 'show_title'

topgenero <- merge(dados_top10, genero, by="show_title")
topgenero <- topgenero[(topgenero$category == "Films" & topgenero$titleType == "movie") | (topgenero$category == "TV" & topgenero$titleType == "tvSeries"),]
topgenero <- distinct(topgenero, show_title, week, country_name, category, titleType, cumulative_weeks_in_top_10, .keep_all=TRUE)

topgeneropaises <- topgenero[,-c(1,3:9)]
topgeneropaises <- separate(topgeneropaises, c("genres"), c("genero1", "genero2", "genero3"), sep=",")
topgeneropaises <- pivot_longer(topgeneropaises, c("genero1", "genero2", "genero3"), names_to = "genero123", values_to = "genres")
View(topgeneropaises)

generocount <- count(topgeneropaises, country_name, genres)
generocount <- na.omit(generocount)
generocount <- subset(generocount, genres!="\\N")
generocount$n <- as.numeric(generocount$n)

write.csv(complete, "datasets_limpos/dataset2.csv", row.names=FALSE)


#dataset3
sunburst <- rename(generocount, label=country_name)
sunburst$genres = sub("-", "", sunburst$genres)
sunburst$parent = c("total - ")
sunburst$parent <- paste(sunburst$parent, sunburst$genres)
sunburst$id = c(" - ")
sunburst$id <- paste(sunburst$parent, sunburst$id)
sunburst$id <- paste(sunburst$id, sunburst$label)
sunburst$n <- as.numeric(sunburst$n)

added <- aggregate(sunburst$n, list(sunburst$genres), FUN=sum)
added <- rename(added, label=Group.1)
added <- rename(added, n=x)
added$n <- as.numeric(added$n)
added$genres <- c(NA)
added$parent <- c("total")
added$id <- c(" - ")

added$id <- paste(added$parent, added$id)
added$id <- paste(added$id, added$label)

total = sum(added$n)
total

sunburst <- rbind(added, sunburst)
sunburst <- rbind(c("total",total,NA,NA,"total"), sunburst)
sunburst <- sunburst[,-c(3)]
sunburst$n <- as.numeric(sunburst$n)
View(sunburst)

















