

scrapeWord :: T.Text -> IO Href 
scrapeWord word = do
  let googleUrl = "https://www.google.com/search?q=" <> t <> "&sxsrf=AOaemvJc28k4ka_rY6sAp-TapWE8ldF4iA:1638157797795&source=lnms&tbm=isch&sa=X&ved=2ahUKEwj24_2s1bz0AhVJnGoFHX--AuwQ_AUoAXoECAEQAw&biw=1920&bih=979&dpr=1" 

  runScraperOnUrl googleUrl $ do
    (_, attributes) <- parseOpeningTagF "class" (=="yWs4tf")
    case Map.lookup "src" attributes of
      Just href -> pure href
      Nothing -> parserZero -- not a match, continue scraping

  -- | runScraperOnUrl runs the given do block here (of type :: ScraperT a) on the supplied `googleUrl`
  -- | 1) Perform GET request -> Get html
  -- | 2) Scrape/Parse HTML by sequentially combing through it and trying to build the pattern described by the supplied (ScraperT a)
