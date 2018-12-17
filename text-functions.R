library(readr)
library(tidyr)
library(tibble)
library(dplyr)
library(tm)
library(tidytext)
library(textstem)
library(topicmodels)
library(LDAvis)
library(splitstackshape)
library(ggplot2)
library(ggwordcloud)
library(htmlwidgets)
library(igraph)
library(ggraph)
library(ggplot2)
library(visNetwork)



# Create and clean text ---------------------------------------------------


clean_corpus <- function(corpus){
  
  # Clean a tm corpus
  
  stopwds <- stopwords('en')
  all_stop <- c("concern", "concerned", "concerns", "may", "also",
                "will", "see", "around","yet","take","though",'work', "hmrc", stopwds)
  corpus %>%
    tm_map(function(x) iconv(enc2native(x$content), sub = "byte")) %>%
    tm_map(PlainTextDocument) %>%
    tm_map(content_transformer(tolower)) %>%    
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    # tm_map(stemDocument) %>%
    tm_map(lemmatize_strings) %>%
    tm_map(removeWords, all_stop) %>%
    tm_map(stripWhitespace, lazy = TRUE) %>%
    tm_map(PlainTextDocument) -> clean_corpus
  return(clean_corpus)
}


generate_clean_dtm <- function(text_file, column){
  
  # Clean the text, create a document-term-matrix and remove blank elements
  #
  # Inputs:
  #   text_file: path to text file
  #   column: name of column of interest
  #
  # Outputs:
  #   list containing text and dtm
  
  df <- read_csv(text_file)[, column] %>% na.omit() 
  tm_corpus <- VCorpus(VectorSource(df[[column]])) %>% clean_corpus()
  
  dtm <- DocumentTermMatrix(tm_corpus)

  row_total <- as.matrix(dtm) %>% rowSums()
  dtm <- dtm[row_total>0,]
  df <- df[row_total>0,]
  
  return(list("text" = df, "dtm" = dtm))
}


generate_dtm_from_df <- function(df, column){
  
  # Clean the text, create a document-term-matrix and remove blank elements
  #
  # Inputs:
  #   df: dataframe containing text
  #   column: name of column of interest
  #
  # Outputs:
  #   list containing text and dtm
  
  df <- df[, column] %>% na.omit()
  tm_corpus <- VCorpus(VectorSource(df[[column]])) %>% clean_corpus()
  
  dtm <- DocumentTermMatrix(tm_corpus)
  
  row_total <- as.matrix(dtm) %>% rowSums()
  dtm <- dtm[row_total>0,]
  df <- df[row_total>0,]
  
  return(list("text" = df, "dtm" = dtm))
}

# Create wordclouds -------------------------------------------------------


display_wordcloud <- function(dtm, min_occur){
  
  # Display a wordcloud
  #
  # Inputs:
  #   dtm: document-term-matrix
  #   min_occur: only include words which atleast this number of times
  
  word_counts <- 
    data.frame(freq = dtm %>% as.matrix() %>% colSums()) %>% 
    rownames_to_column("word")
  
  words_filtered <- word_counts %>% 
    filter(freq > min_occur) %>% 
    arrange(desc(freq))
  
  return(ggwordcloud2(words_filtered, shuffle = F, size = 1.4, ellipticity = 0.9))
}



save_wordcloud <- function(dtm, min_occur, output_file){
  
  # Save a wordcloud object from ggwordcloud or wordcloud
  #
  # Inputs:
  #   dtm: document-term-matrix
  #   min_occur: only include words which atleast this number of times
  #   output_file: path to where the wordcloud should be saved
  

  wc <- display_wordcloud(dtm, min_occur)
  ggsave(filename = output_file, plot = wc)
}


#save_wordcloud2 <- function(dtm, min_occur, output_file){
  
  # Save a wordcloud object from wordcloud2
  #
  # Inputs:
  #   dtm: document-term-matrix
  #   min_occur: only include words which atleast this number of times
  #   output_file: path to where the wordcloud should be saved
  
  # From https://github.com/Lchiffon/wordcloud2/issues/20
#  simpleFix = function(inputFile, outputFile){
#    a = readLines(inputFile)
#    output = paste(a, collapse = "\n")
#    output = gsub(">\n\n</div>","></div>",output)
#    writeLines(output, outputFile)
#    invisible(NULL)
#  }
  
  
#  display_wordcloud(dtm, min_occur) %>% saveWidget(output_file, selfcontained = T)
#  simpleFix(output_file, output_file)
#}



# Word frequencies --------------------------------------------------------


count_words <- function(dtm){
  
  # Calculate word frequencies from document term matrix
  
  word_counts <- data.frame(freq = dtm %>% as.matrix() %>% colSums()) %>% 
    rownames_to_column("word")
  
  return(word_counts)
}


count_docs <- function(dtm){
  
  # Calculate the number of documents a word occurs in
  
  dtm_mat <- as.matrix(dtm)
  
  doc_counts <- data.frame(freq = ifelse(dtm_mat > 0, 1, 0) %>% colSums()) %>% 
    rownames_to_column("word")
  
  return(doc_counts)
}


plot_word_freq <- function(dtm, top_n){
  
  # Display a word frequency chart
  #
  # Inputs:
  #   dtm: document-term-matrix
  #   top_n: only include the top_n most frequently occuring words
  
  
  wf <- dtm %>% count_words %>% top_n(top_n, freq) 
  
  chart <- ggplot(wf, aes(x = reorder(word, freq), y = freq, fill="")) +
            geom_bar(stat = "identity", colour="black") + 
            scale_fill_manual(values=c("#3399FF")) +
            coord_flip() +
            ylab("frequency") +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_text(size = 16),
                  legend.position = "none",
                  aspect.ratio = 1.66,
                  plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
  return(chart)
  
}


save_word_freq <- function(dtm, min_occur, output_file){
  
  # Save a word frequency chart
  #
  # Inputs:
  #   dtm: document-term-matrix
  #   min_occur: only include words which atleast this number of times
  #   output_file: path to where the wordcloud should be saved
  
  
  chart <- plot_word_freq(dtm, min_occur)
  ggsave(output_file, plot = chart)
  
}


# Topic models ------------------------------------------------------------


# From https://github.com/trinker/topicmodels_learning/blob/master/functions/topicmodels2LDAvis.R
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}


display_ldavis <- function(lda_model){
  
  # Display LDA model using LDAvis
  
  serVis(topicmodels2LDAvis(lda_model), open.browser = TRUE)
}


save_ldavis <- function(lda_model, output_folder){
  
  # Save LDA model using to output_folder
  
  serVis(topicmodels2LDAvis(lda_model), out.dir = output_folder, open.browser = FALSE)
}


get_top_comments <- function(lda_model, topic, df, n){
  
  # Return the text from n documents that are most strongly
  # associated with that topic
  
  df$gamma <- lda_model@gamma[, topic]
  text <- df %>% 
            top_n(10, gamma) %>% 
            arrange(desc(gamma)) %>% 
           select(-gamma)
  
  return(text)
}


get_top_topic <- function(lda_model){
  
  # Get top topic from model
  
  return(apply(lda_model@gamma, 1, which.max))
  
}


get_random_comments <- function(lda_model, df, n){
  
  # Get n random comments from each topic
  
  set.seed(1006)
  
  # Assign the most strong topic
  df$topic <- get_top_topic(lda_model)
  random_sample_topics <- stratified(df, "topic", n)
  
  return(random_sample_topics)
  
}



# Networks ----------------------------------------------------------------


calc_bigrams <- function(text){
  
  # Generate bigrams and count their occurances
  #
  # Input:
  #   text: vector of text
  #
  # Output:
  #   bigrams: dataframe of bigrams
  
  bigrams <- VCorpus(VectorSource(text)) %>% 
                clean_corpus() %>% 
                tidy() %>%
                unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
                count(bigram, sort = TRUE) %>%
                separate(bigram, c("word1", "word2"), sep = " ") %>%
                select(word1, word2, n) %>%
                filter(word1 != word2)
  
  return(bigrams)
}


calc_assocs <- function(dtm){
  
  # Calculate word associations from document term matrix
  
  edges <- as.matrix(dtm) %>% cor() %>% as.table() %>% as.data.frame(stringsAsFactors = FALSE)
  colnames(edges) <- c("word1", "word2", "n")
  
  edges <- edges %>%
            filter(n > 0.2) %>%
            filter(word1 > word2) %>%
            filter(word1 != word2) %>%
            distinct()
  
  return(edges)
  
}


remove_infreq_words <- function(nodes, edges, min_occur, dtm){
  
  # Remove words appearing less than min_occur from node and edge list
  
  words_retain <- count_docs(dtm) %>% filter(freq >= min_occur)
  
  nodes <- nodes %>% filter(word %in% words_retain$word)
  edges <- edges %>%
    filter(word1 %in% words_retain$word & word2 %in% words_retain$word)
  
  return(list(edges = edges, nodes = nodes))
}


generate_static_graph <- function(nodes, edges){

  # Generate static graph using igraph and ggraph
  
  static_graph <- graph_from_data_frame(edges, vertices = nodes)
  
  arr <- grid::arrow(type = "closed", length = unit(.1, "inches"))
  network <- ggraph(static_graph, layout = "fr") +
              geom_edge_arc(
              aes(width = n), 
              curvature = 0.1,
              colour = "lightblue",
              show.legend = FALSE,
              arrow=arr) +
            geom_node_point(
              aes(size = freq), 
              fill = "cornflowerblue", 
              color = "black", 
              stroke = 1, 
              shape = 21, 
              show.legend = FALSE) +
            scale_size(range = c(2, 10)) +
            geom_node_text(aes(label = name), vjust = 1.2, hjust = 1.2) +
            scale_edge_width(range = c(1, 2.5)) + 
            theme_void()

  # Add more horizontal space for text
  x_max = max(ggplot_build(network)$data[[3]]$x) + 0.2
  x_min = min(ggplot_build(network)$data[[3]]$x) - 1.1
  network <- network + xlim(x_min, x_max)
  
  return(network)
}


generate_interactive_graph <- function(nodes, edges){
  
  # Generate interactive graph using visNetwork
  
  colnames(nodes) <- c("id", "value")
  colnames(edges) <- c("from", "to", "value")
  
  network <-
    visNetwork(nodes, edges) %>%
    visOptions(highlightNearest = T) %>%
    visNodes(scaling = list(min=5, max=30), font = '26px arial #343434') %>%
    visEdges(scaling = list(min=2, max=10)) %>%
    visPhysics(
      solver = 'forceAtlas2Based',
      forceAtlas2Based = (
        list(
          gravitationalConstant = -50,
          centralGravity = 0.01,
          springLength  = 100,
          springConstant = 0.08,
          damping = 0.4,
          avoidOverlap = 0.2
        )
      )
    )
  
  
  return(network)
}


generate_graph <- function(text, dtm, top_n, static=FALSE, bigrams=FALSE, filter_nodes=FALSE){
  
  # Generate different types of network graph
  #
  # Inputs:
  #   text: vector of text
  #   dtm: document term matrix
  #   top_n: only include top_n entries (either by node word counts or edge strength)
  #   static: if TRUE produce static graph, if FALSE produce interactive graph
  #   bigrams: if TRUE use bigrams for edges, if FALSE use word associations
  #   filter_nodes: if TRUE only include top_n nodes, if FALSE only include top_n edges
  #
  # Outputs:
  #   network: network object

  
  nodes <- count_words(dtm)
  
  if (bigrams){
    edges <- calc_bigrams(text)
  } else {
    edges <- calc_assocs(dtm)
  }
  
  if (filter_nodes){
    nodes <- nodes %>% top_n(top_n, freq)
    edges <- edges %>%
              filter(word1 %in% nodes$word & word2 %in% nodes$word)
    
  } else {
    
    # Words appearing in only one document can show perfect
    # correlation so remove these first
    nodes_edges <- remove_infreq_words(nodes, edges, 3, dtm)
    nodes <- nodes_edges$nodes
    edges <- nodes_edges$edges
    
    edges <- edges %>% top_n(top_n, wt = n)
    nodes <- nodes %>% filter(word %in% edges$word1 | word %in% edges$word2)
  }
  
  if (static){
    network <- generate_static_graph(nodes, edges)
  } else {
    network <- generate_interactive_graph(nodes, edges)
  }
  
  return(network)
}


save_network <- function(text, dtm, top_n, output_file, 
                         static=FALSE, bigrams=FALSE, filter_nodes=FALSE){
    
  # Generate and save different types of network graph
  #
  # Inputs:
  #   text: vector of text
  #   dtm: document term matrix
  #   top_n: only include top_n entries (either by node word counts or edge strength)
  #   output_file: path to where the network should be saved
  #   static: if TRUE produce static graph, if FALSE produce interactive graph
  #   bigrams: if TRUE use bigrams for edges, if FALSE use word associations
  #   filter_nodes: if TRUE only include top_n nodes, if FALSE only include top_n edges
  
  (network <- generate_graph(text, dtm, top_n, static, bigrams, filter_nodes))
  
  if( is(network, "visNetwork") ){
    visSave(network, output_file)
  }
  else {
    ggsave(output_file, plot = network, dpi = 500)
  }
  
}

