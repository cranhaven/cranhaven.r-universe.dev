
## setting up data source

#' Clean JSON text from Wikipedia
#' @param x `character` JSON text
#' @returns `character` JSON text
wiki_clean <- function(x){
  x = gsub("''[^']+?''",'', x)
}

WIKI_JSON_PATH=c('query','export','*')

download_gutenberg_list <- function(
  output_directory='data/gutenberg'
){
  dir.create(output_directory, recursive=T)
  url_list = utils::read.table('data/url_list.txt')[,1,T]
  for (url in url_list){
    fn = gsub(
      '/',
      '_',
      gsub('.*files/','', url)
    )
    utils::download.file(url, file.path(output_directory, fn))
  }
}

make_wiki_urls <- function(terms){
  sprintf('https://en.wikipedia.org/w/api.php?action=query&format=json&titles=%s&prop=extracts&exintro&explaintext&exlimit=max&export', terms)
}

download_science_wiki_list <- function(
  output_directory='data/science_wiki'
){
  dir.create(output_directory, recursive=T)
  url_list = utils::read.table('data/science_url_list.txt')[,1,T]
  for (url in url_list){
    fn = gsub(
      '/',
      '_',
      gsub('.*titles=(.+?)&.*','\\1', url)
    )
    utils::download.file(url, file.path(output_directory, fn))
  }
}

download_wikis <- function(
  topics,
  output_directory,
  delay=0.01,
  create_output_directory=TRUE,
  overwrite=FALSE
){
  if (create_output_directory)
    dir.create(output_directory, showWarnings=FALSE, recursive=TRUE)
  urls = make_wiki_urls(topics)
  for (url in urls){
    fn = gsub(
      '/',
      '_',
      gsub('.*titles=(.+?)&.*','\\1', url)
    )
    if (file.exists(file.path(output_directory, fn)) & !overwrite){
      warning(sprintf('Skipping %s since it already exists', fn))
      next
    }
    utils::download.file(url, file.path(output_directory, fn))
  }
}

generate_wiki_word_list <- function(
  wiki_directory,
  output_file,
  ...
){
  word_list = corpora_to_word_list(
    file.path(
      wiki_directory,
      dir(wiki_directory)
    ),
    output_file=output_file,
    stopword_fn=c(DEFAULT_STOPWORDS, WIKI_STOPWORDS),
    json_path=WIKI_JSON_PATH,
    ...
  )
  word_list
}

make_animal_wiki_list <- function(...){
  topics = c('Cat','Dog','Fish','Moose','Bee','Bird','Falcon','Robin','Cougar','Lion','Wolf','Squirrel','Rabbit','Pigeon','Crow','Raven',
             'Catfish','Snake','Salamander','Python','Dinosaur','Butterfly','Moth','Ant','Zebra','Horse','Dragonfly','Monkey','Gorilla',
             'Lemur','Raccoon','Bear','Whale','Dolphin','Otter','Frog','Toucan','Elephant','Rhinocerous','Kangaroo','Pig','Tiger','Ox',
             'Cow','Golden_Retriever','Maine_Coon','Owl','Jellyfish','Octopus','Squid','Cricket_(insect)','Hyena','Komodo_dragon',
             'Turtle','Crocodile','Beaver','Chipmunk','Woodpecker','Eagle','Sparrow','Seagull','Beetle','Praying_mantis','Chicken','Turkey_(bird)')
  data_directory='data/animal_wiki'
  output_file='data/animal_wiki_word_list.txt'

  download_wikis(topics, data_directory)
  generate_wiki_word_list(data_directory, output_file,...)
}


generate_default_word_list <- function(){
  word_list = corpora_to_word_list(
    file.path(
      'data/gutenberg',
      dir('data/gutenberg')
    ),
    output_file='data/default_word_list.csv',
    stopword_fn=DEFAULT_STOPWORDS
  )

  word_list
}

generate_science_wiki_word_list <- function(){
  word_list = corpora_to_word_list(
    file.path(
      'data/science_wiki',
      dir('data/science_wiki')
    ),
    output_file='data/science_word_list.csv',
    stopword_fn=c(DEFAULT_STOPWORDS, WIKI_STOPWORDS),
    json_path=WIKI_JSON_PATH
  )

  word_list
}
