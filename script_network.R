
 #script para exploração dos dados de contratos do estado
 #criado em 05-maio-2025, 10:12h

 #.libPaths('D:\\Dimitri\\Docs 17-out-15\\R\\win-library\\3.1') #caminho dos pacotes

 library('xlsx')         #para ler/criar arquivos xlsx
 library('ggplot2')      #networkrficos (mais usado)
 library('reshape2')     #para remodular data.frames
 library('dplyr')        #manipulao de dados - tydiverse
 library('data.table')   #para funo fread()
 library('stringr')      #funes de string  - t ydiverse
 library('magrittr')     #para mudar nome de colunas

 #pacotes para grafos 
 library('igraph')
 library('ggnetwork')
 library('intergraph')

 #lendo os dados de contratros
 #TODO ler direto do portal de dados abertos 
 #https://dados.sc.gov.br/dataset/93dab950-e805-4388-8418-cfb3b73f1623/resource/8bb98383-7043-4d2f-ae32-9377656e71ee/download/contratos.csv

 dado <- read.csv('contratos.csv', sep = ';')
 names(dado) <- tolower(names(dado))

 #arrumando as variáveis
 dado <- within(dado,{
        dtinicio   <- substr(dtinicio,1,10) %>% as.Date(., format = '%Y-%m-%d')
        dtfimatual <- substr(dtfimatual,1,10) %>% as.Date(., format = '%Y-%m-%d')
        vlatual    <- as.numeric(vlatual)
        ones       <- 1
 })

 #criando redes
 dadoi <- subset(dado, dtfimatual >= as.Date('2025-01-01'))
 rede <- aggregate(ones ~ cdunidadegestora + contratado + idcontratadomascarado, data = dadoi, FUN = sum)
 rede <- rede[order(rede$ones, decreasing = T),]

 redei <- aggregate(vlatual ~ cdunidadegestora + contratado + idcontratadomascarado, data = dadoi, FUN = sum, na.rm = T)
 redei <- redei[order(redei$vlatual, decreasing = T),]

 rede <- dplyr::full_join(rede, redei, by = c('cdunidadegestora','contratado', 'idcontratadomascarado'))

 links=data.frame(
    source=rede[,1],
    target=rede[,2]
    )

 #retirado de https://bookdown.org/jdholster1/idsr/network-analysis.html

# Turn it into igraph object
network <- graph_from_data_frame(d=links, directed=F) 
 
# Count the number of degree for each node:
deg <- degree(network, mode="all")
 
# Plot
#plot(network)

#usando ggplot
ggplot(network, aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges() +
  geom_nodes()


  # Compute the degree centrality for our graph network. 
degr_cent <- centr_degree(network, mode = 'all')
degr_cent <- degr_cent$res

# Compute the eigenvector centrality of our network
eign_cent <- eigen_centrality(network)
eign_cent <- eign_cent$vector

# Compute the closeness centraility
clos_cent <- igraph::closeness(network)

# Compute betweeness centrality
betw_cent <- igraph::betweenness(network)

# Compute the degree centrality for our graph network. 
degr_cent <- centr_degree(network, mode = 'all')
degr_cent <- degr_cent$res

# Compute the eigenvector centrality of our network
eign_cent <- eigen_centrality(network)
eign_cent <- eign_cent$vector

# Compute the closeness centraility
clos_cent <- igraph::closeness(network)

# Compute betweeness centrality
betw_cent <- igraph::betweenness(network)

nodes <- c(unique(rede[,2], rede[,1]))

# Create data frame storing all of the measures of centrality
datai <- data.frame(#vertex = nodes,
                   #label = people,
                   degree = degr_cent, 
                   eigen = eign_cent, 
                   closeness = clos_cent, 
                   betweeness = betw_cent)

# Order the data by degree centrality
datai <- datai %>% dplyr::arrange(desc(degree))
