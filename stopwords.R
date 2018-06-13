read_in_stopwords <- function() {
  library(here)
  here::here() %>%
    setwd()
  file_stop <- 'stopwords.txt'
  # read in stopwords
  stop_words1 <- readChar(file_stop, file.info(file_stop)$size) 
  stop_words1 <- unlist(str_split(stop_words1, ',')) # split into vector
  stop_words1 <- str_trim(stop_words1, side = 'both') # strip whitespace
  extra_stopwords <- c(letters,'now', 'going', 'will', 'just', 'right', 'say', 
                     'back', 'today', 'get', 'see', 'think', 'well', 'want', 
                     'got', 'go', 'tonight', 'just', 'know','said', 're', 
                     'look', 'can', 'saying', 'also', 'way', 'last', 'new', 
                     'time', 'really', 'lot', 'one','morning','good', 'bad',
                     'don','actually','old', 'yes', 'no', 'maybe', 've','make',
                     'year', 'years', 'ray', 'al', 've', 'talk', 'take', 'two',
                     'wolf', 'like', 'dislike', 'people', 'ready', 'let',
                     'many', 'pat', 'thank', 'tell', 'mean', 'folks', 'kind', 
                     'understand', 'believe', 'keep', 'sort', 'things', 'may',
                     'something', 'show', 'ph', 'come', 'says', 'able', 'fact',
                     'much', 'every', 'coming', 'talking', 'never', 'around',
                     'latest', 'breaking', 'night', 'day', 'seen', 'trying', 
                     'week', 'thing', 'true', 'false', 'away', 'still', 'issue',
                     'little', 'took', 'best', 'thanks', 'came', 'pretty',
                     'important', 'always', 'great', 'next', 'sure', 'days',
                     'nights', 'saw', 'later', 'voice-over', 'put', 'part',
                     'even', 'another', 'area', 'com', 'exactly', 'case', 
                     'girl', 'com', 'news', 'john', 'anderson', 'twitter',  
                     'didn', 'casey', 'anthony','hurricane', 'winds', 'tornado', 
                     'stars', 'star', 'kid', 'kids', 'hollywood', 'jackson', 
                     'wife', 'husband', 'attorney', 'child', 'baby', 'lawyer',
                     'dr', 'love', 'mitt', 'oscar', 'kyra','ll', 'o.j','oj', 
                     "doesn't", 'north', 'south', 'east','west','sex', 'kim', 
                     'mrs', 'mrs.', 'oprah','plane', 'miners','earthquake', 
                     'deal', 'god', 'penn', 'tornadoes','looking', 'video', 
                     'tiger', 'hour','hear','tj','t.j','course', 'middle', 
                     'mother', 'joran', 'county', 'megyn', 'scene', 'different', 
                     'aj', 'won', 'lost', 'lose', 'music', 'guy', 'guys', 
                     'mom', 'music', 'win', 'tape', 'children', 'fire', 'home',
                     'storm', 'book', 'phone', 'city', 'snow', 'hair', 'find',
                     'picture', 'league', 'players', 'snow', 'weather', 
                     'missing', 'brown', 'father', 'family', 'life', 'grand',
                     'st', 'absolutely', 'wind', 'weather', 'live', 'ground',
                     'doctor', 'rape', 'water', 'bit', 'give', 'call', 'movie',
                     'big', 'small', 'medium', 'hi', 'boy', "aruba", "mine",    
                     "ocean", "airport", "house", "stand", "jurors", "natalee",
                     "done", "united", "cabin", "super", "ad", "plan", "flag",  
                     "testimony", "orleans", "evidence", "pilot", "feet", 
                     "streets", "inaudible", "bus", "air", "plot", 'abc', 'nbc',
                     'cnn', 'fox', 'msnbc', 'news', 'network', 'iii', 'show',
                     'copyright', 'videotape', 'highlight', 'length', 'section',
                     'type', 'guests', 'guest', 'announcer', 'voiceover', 'ms',
                     'clip', 'transcript', 'transcripts', 'hard', 'host',
                     'heard', 'start', 'top', 'behar', 'real', 'est', 'load',
                     'kotb', 'lauer', "o'reilly", "oreilly", 'cooper', "vieira",
                     'morales', 'anchors', 'correspondent', 'anchor', 
                     'situation', 'room', 'voice', 'curry', "o'brien", 'obrien',
                     'costello', 'couric', 'gibson', 'roberts', 'am', 'pm',
                     'hemmer', 'clayson', 'chen', 'hannity', 'update', 'updates',
                     'updated', 'copy', 'july', 'cut', 'vo', 'gubel', 'rush',
                     'broadcasting', 'form', 'gumbel', 'photo', 'footage', 
                     'murder', 'murphy', 'abrams', 'velshi', 'graphic', 
                     'hm', 'roker', 'nguyen', 'robach', 'gupta', 'chetry', 
                     'velez', 'pinsky', 'bolling', 'gutfeld', 'perino',
                     'beckel', 'tantaros', 'cuomo', 'sambolin', 'sanchez',
                     'hansen', 'banfield', 'hammer', 'olbermann', 'bourdain',
                     'dobbs', 'baier', 'russert', 'beckel', 'whitfield',
                     'barnes', 'dolan', 'schultz', 'hansen', 'casarez',
                     'condit', 'baldwin', "scarborough", 'hughley', 'griffin', 
                     "hayes", "stahl", "carlson", "geragos", "camerota",
                     "sheindlin", "holt", "lemon", "berman", "holmes", "sawyer",
                     'hm', 'hmm', "weir", "phillips", "flay", 'hoda', 'kotb', 
                     "bauer", "seacrest", 'huh', "walters", "osteen",
                     "assuras", 'actor', "dancing", "deen", 'rock', 'cho',
                     'malveaux', "crowley", 'kurtz', "bartiromo", "kornacki",
                     "sharpton", "guilfoyle", "beck", 'burnett', "borger",
                     'fineman', 'hume', 'keilar', 'wolfe', 'snyderman',
                     "haines", "canning", "norville", 'uh', 'text',
                     'tv', "reserved", 'cool', "announcement", "announcements", 
                     "harlow", "coulter", "novak", "schacher", "applause", 
                     'term', 'martinez', "tuchman", "hayes", "maddow", 'cosby',
                     "wouldn", "wouldn't", "wouldnt", 'lagattuta', 'joan',
                     'former', 'facebook', 'did', 'o.j.', "doesn", 'mr','mr.',
                     'miss', 'region', 'site', "maher", "hardball", 'woman',
                     'a.j', 'person', 'homes', 'work', 'sanjay', 'square',
                     'car', 'caylee', 'debris', 'holy', "media", "flight",
                     "hey", "victim", "peterson", "damage", "red", "street",
                     'cbs', 'byline', 'ago', 'crosstalk', 'document', 'gifford',
                     'blitzer', 'correspondents', 'matthews', 'broadcast',
                     'morrison', 'graphics', 'mm', 'mankiewicz', 'guilfoyle',
                     'pereira', 'roker', 'debbye', "banfield", 'watters',
                     "spitzer", "koppel", "larson", "sanchez", "muir", "robach",
                     "blackwell", "osbourne", "bianna", "begala", "cavuto",
                     'ingraham', 'colmes', "novak", "camerota", 'geragos',
                     "gosh", 'wear', "goldberg", "excerpt", "footage",
                     'food', 'fun', 'wow', "carville", 'support')
  stop_words2 <- c(extra_stopwords, stop_words1)
  return(stop_words2)
}













