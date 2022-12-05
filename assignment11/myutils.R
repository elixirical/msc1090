check.args <- function(x) {
  if (!length(x) %in% c(1,2)) {
    cat("Invalid: please supply a valid filepath to count words from and an ")
    cat("optional file of words to exclude.\n")
    cat("    Example: Rscript count_words.R file.txt words.txt\n")
    quit()
  }
  # trycatch to check files since file.exists doesnt work for URLs
  tryCatch(
    expr = {
      read_file(x[1])
      read_file(x[2])
      return(length(args))
    },
    error = function(e) {
      cat("Error loading one of the files; are you sure it exists?\n")
      print(e)
      quit()
    },
    warning = function(w) {
      cat("Possible error reading one of the files! Exiting...\n")
      print(w)
      quit()
    }
  )
}

word_count <- function(input, stopwords) {
  split.string <- unlist(strsplit(input, split = "[^a-zA-Z']+"))
  if (!missing(stopwords)) {
    split.string <- split.string[!split.string %in% stopwords]
  }
  return(as.data.frame(table(split.string)))

  #split.string <- unlist(strsplit(tolower(input), split = "[^a-zA-Z']+"))
  #if (!missing(stopwords)) {
  #  split.string <- split.string[!split.string %in% tolower(stopwords)]
  #}
  #return(as.data.frame(table(split.string)))
}

read_file <- function(input, lower = FALSE) {
  my.file <- file(input, "r")
  file.content <- readLines(my.file, warn = FALSE)
  close(my.file)
  # the line below is necessary because realLines trims trailing whitespace
  # which would break reading the stop words
  file.content <- paste(file.content, " ")
  one.string <- paste(file.content, collapse = "")
  if (!lower) {
    one.string <- tolower(one.string)
  }
  return(one.string)
}

plotTopWords <- function(wordcount, n = 10, filename = "word_count.pdf") {
  library(ggplot2)

  wordcount_sorted <- wordcount[order(-wordcount$count),]

  topN <- wordcount_sorted[1:n,]

  ggplot(topN, aes(x = count, xend = 0, y = word, yend = word, colour = word)) +
    geom_segment() +
    geom_point() +
    scale_y_discrete(limits = rev(topN$word)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Number of occurrences",
         y = "Words") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(), legend.position = "off")

  ggsave(filename, height = n/4)
}

# since readfile returns a string, this fn formats a stopwords read to a vector
format.stop.words <- function(readfile.output) {
  return(unlist(strsplit(readfile.output, split = "[^a-zA-Z']+")))
}

test <- "https://pages.scinet.utoronto.ca/~afedosee/shakespeare.sonnets.txt"
#stops <- "https://pages.scinet.utoronto.ca/~afedosee/stopwords_en.txt"
#test.read <- read_file(test)
#stop.read <- read_file(stops)
#print(stop.read)
#print(strsplit(stop.read, split = "[^a-zA-Z']+"))
#print(word_count(test.read, format.stop.words(stop.read)))
#print(word_count(test.read))

#print(file.exists(test))
#print(file.access(test))
