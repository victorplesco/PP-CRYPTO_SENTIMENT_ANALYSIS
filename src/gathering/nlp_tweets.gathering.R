#################################################################################################################################################################################################################################################################################
## Setup Environment ############################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

require(devtools); load_all("~/CRYPTOSENT/src/packages/racademic/"); 
token <- v2_create_token(token = "INSERT_TOKEN");

#################################################################################################################################################################################################################################################################################
## Data #########################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

portfolio <- data.frame(symbol = c("TRX", "XMR", "EOS", "CAKE", "DASH", 
                                   "BTC", "ETH", "XRP", "ADA", "DOT",
                                   "LTC", "UNI", "XLM", "NEO", "MIOTA"));

#################################################################################################################################################################################################################################################################################
## Tweets by CashTags - 2 Weeks #################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

for(i in 1:nrow(portfolio)) {
  
  path = paste0("~/NLP/Project/data/", nlp_portfolio$symbol[i], "/"); dir.create(path);

  tweets <- v2_search_fullarchive(token = token, next_token = NULL, safe.dir = path, 
                                  query = paste0("$", nlp_portfolio$symbol[i], " -is:retweet -is:reply -is:quote lang:en"), max_results = 500, 
                                  start_time = "2021-04-06T12:00:00Z", end_time = "2021-04-20T12:00:00Z", since_id = NULL, until_id = NULL,
                                  tweet.fields = "all", user.fields = "all", media.fields = "all", place.fields = "all", poll.fields = "all", expansions = "all"); 
}; rm(path, i);