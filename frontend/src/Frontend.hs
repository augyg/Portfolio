{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}


module Frontend where

import GHC.Generics (Generic)
import qualified Text.Email.Validate as EmailValidate
import Data.Aeson
-- import FrontendTwo
import Scrape (runScraperOnHtml)
import Requests (getHtml')
import Elem.ElemHeadParse (hrefParser)

import Data.Maybe (fromMaybe, fromJust, isJust)
import Control.Monad.IO.Class
import qualified Data.Map as Map 

import Control.Monad.Fix
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM, MonadJSM)
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Route.Frontend


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("src" =: "https://cdn.tailwindcss.com") $ blank
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://fonts.googleapis.com/css?family=Tangerine") $ blank
  , _frontend_body = subRoute_ $ \case
      -- FrontendRoute_Main -> prerender_ blank $ do
      --   (newSearchTerm, buttonClick) <- el "div" $ do 
      --     newSearchTerm <- el "div" $ fmap value (inputElement def)
      --     buttonClick <- button "scrape away"
      --     return (newSearchTerm, buttonClick)
      --   response <- performRequestAsync $ buildReq (tag (current newSearchTerm) buttonClick)
      --   let 
      --     dadBod = fmap ((fromMaybe "not sure what happened") . _xhrResponse_responseText) response 
      --   primeDadBod <- holdDyn startingImage dadBod
      --   -- dynText primeDadBod
      --   el "div" $ elDynAttr "img" (fmap funcShui primeDadBod) $ blank
      
      --   return ()

      FrontendRoute_Main -> homepage
      ScrappyDemo -> prerender_ blank $ do
        (newSearchTerm, buttonClick) <- el "div" $ do 
          newSearchTerm <- el "div" $ fmap value (inputElement def)
          buttonClick <- elStyle "span" "color:#6495ED" $ button "scrape away"
          return (newSearchTerm, buttonClick)
        response <- performRequestAsync $ buildReq (tag (current newSearchTerm) buttonClick)
        let 
          dadBod = fmap ((fromMaybe "not sure what happened") . _xhrResponse_responseText) response 
        primeDadBod <- holdDyn startingImage dadBod
        -- dynText primeDadBod
        el "div" $ elDynAttr "img" (fmap funcShui primeDadBod) $ blank
        elClass "div" "text-white" $ text "How it's done"
        elAttr "img" ("src" =: $(static "images/scrappy-explanation.png")) $ blank
        return ()
      
      -- Research -> text " research "
      --   -- should also include my general concept of probabibility
      --   -- this will probably be an explanatory page

      
      -- AI -> return () --ai
      -- AceBeta -> let args = [] in projectPage args -- :: [(Picture, Text)] )
      -- Scrappy -> projectPage []
      -- AceAlpha -> projectPage []
      -- AceMainSite -> projectPage []
      -- ProductFinder -> projectPage []
      -- SalesTool -> projectPage []
      -- AmazonAutomation -> projectPage []
      -- Pathways -> projectPage []
  }


projectPage :: ( DomBuilder t m
               , SetRoute t (R FrontendRoute) m
               , RouteToUrl (R FrontendRoute) m
               , Prerender t m 
               ) => [String] -> m ()
projectPage x = do
  -- # place element, get 
  return ()

{-
use tailwind
-- collapsible list when click on the Title of the section

-} 


aboutMeText :: T.Text
aboutMeText = "The only accurate way to describe me is \"Polymath\" ... but what is that?"
              <> " A polymath is an individual whose knowledge spans a significant number of subjects"
              <> " and is known to draw from a number of different domains in order to solve specific problems."
              <> " This is vital to my approach and skillset when applied to a given problem." 

whyDoICareText :: T.Text
whyDoICareText = "It is a deeply held belief of mine that efficient action"
                 <> " comes from a deep understanding of the problem."
                 <> " Regardless of what the problem is, the safest assumption we can make is that physics and humans are variables in the equation."
                 <> " Humans are not simple, to make any further assumptions would leak error into judgment. But if humans are constantly apart of the"
                 <> " equation, then neuroscience can help us to simplify a large array of problems. Whether the domain is visual design, functional design,"
                 <> " machine learning or backend development, we are choosing between many solutions which range from accurate identification and effective"
                 <> " handling of the case, to something which completely missed the mark, potentially thousands of dollars later."
                 <> " so neuroscience not only can help to understand user psychology, but provide inspiration about how to paint a solution to hard to"
                 <> " define problems"
                 
researchText :: T.Text
researchText = "For the last five years I have been working towards the creation of a mathematically defined model which represents the functional"
               <> " architecture of the brain. There has been plenty of development in neuroscience focused on particular areas of the brain and these"
               <> " efforts have largely worked in parallel to each other. Moving towards a functional architecture of the brain means incorporating"
               <> " findings about each of these different areas in connection with what is generally understood about the learning, development and"
               <> " macro-evolution of biological organisms. A valid model would align very closely with findings and beliefs of researchers, account"
               <> " for the development of individual differences in the location of brain areas, which form relative to an organism's field of existence."
               <> " By virtue of this objective, I have become obsessed with the basal ganglia since it's important role in learning gives meaning to"
               <> " a wide range of previously unexplainable observations about the brain." 

aiSynopsis :: T.Text
aiSynopsis = "My current adventure in AI is an implementation of my ideas for more accurate AI. While I have experimented and read into many models (SOM, LR, KNN"
             <> ", RNN, CNN) for a past"
             <> " project (see Pathways, bottom right), I have never been able to find any justification that such models actually work like the brain."
             <> " It is not absolutely necessary for a model to behave like the brain in simpler cases, but the further it is from the brain, the further"
             <> " such a design will be from a general AI, or more importantly, able to generalize properly to the full range of relevant inputs."


aiSynopsis2 :: T.Text
aiSynopsis2 = "With this project, my approach worked methodically forward from asking how do we communicate? It's not just words of course, we communicate through"
              <> " a wide range of stimuli, which can be physically classified as visual stimuli (body language) and auditory stimuli. It is via these"
              <> " bases that we communicate more abstract ideas; for example, a startup founder would want to communicate/illustrate their concept"
              <> " via the sound waves put out, the deeper meaning behind putting said words together into a visual for the audience, and then inspire"
              <> " excitement both through how these words are said (simple example, not being monotone) as well as demonstrating their excitement by"
              <> " appropriate, matching body language."

aiSynopsis3 :: T.Text
aiSynopsis3 = "The implementation breaks down into 3 core areas: content and logical/comparative-speech analysis, musical/tonal analysis, and body language analysis."
              <> " We perform the content component by comparing what the user said against a summarization in the form of a Rose Tree, which contains months of"
              <> " scraping, thanks to my other project Scrappy (see mid-left) which is the most powerful scraping library that exists today."
              <> " The musical analysis has been a collaborative project where I've been working with the direction and advice of Bradley Bennett (University of"
              <> " Waterloo computational physics) and Kyle Jensen (SET '19 and guitarist of Mortrem, a progressive metal band). It is implemented using Haskell"
              <> " arrows and incorporates fourier analysis to make waveform summarization a little easier. The last body language component actually does"
              <> " use commonly known tools and was done in python. The reasoning here was that the models will be good enough until the resources to"
              <> " create a more deterministic model. This time around was implemented using OpenCV, numpy, pandas, and keras to detect specific selected features"
              <> " about what the movement of certain head and body parts are in order to assess particular emotions. The assessment is based on established"
              <> " research on speech communication." 

nervosSynopsis :: T.Text
nervosSynopsis = "Nervos Force Bridge was a project paid for by the Nervos Blockchain to allow for cross-chain transactions between itself to and from Cardano"
                 <> " and Ethereum. In order to implement a system like this, Force Bridge required the implementation of Layer 2 coins on each chain as well"
                 <> " as a system to agree on the value of these Layer 2 coins, in order to recover the Layer 1 currency."
                 <> " while this is seemingly simple, there are a plethora of security concerns to handle. For example, just before the release of this project"
                 <> " the Axie Infinity blockchain was hacked for $620 million, which had a very similar scheme (Collector-Validator model) whereby 5 of 9 validators"
                 <> " had become compromised. We responded to this by switching to a more trustless model which disallows the collector from asking for validation"
                 <> " before the collector itself has determined it's validity, but the collector still cannot process the transaction and must ask the validators"
                 <> " to sign the multi-sig transaction."
                 <> " This was an amazing opportunity for my security knowledge to expand." 

nervosSynopsis2 :: T.Text
nervosSynopsis2 = "It is also worth mentioning that the project started out in Docker, TypeScript, and React.js but when the firm that I was working on behalf of"
                 <> " stepped in, we were able to demonstrate that all of the problems with scaling which we were already seeing, could be easily solved by using"
                 <> " Haskell and Nix instead." 

pillEl :: DomBuilder t m => T.Text -> m ()
pillEl t = elClass "span" "rounded-full bg-white text-sky-600 p-3 mx-2" $ text t 

obeliskSynopsis :: T.Text
obeliskSynopsis = "Obelisk is a full stack web framework built in Haskell and Nix which allows for rapid web development and deployment"
                  <> " When I started working on the project, Obelisk had become broken on MacOS and faced a major bug with deployment."
                  <> " Through my approach I was able to comb through a massive amount of incorrect diagnoses about what could be the problem"
                  <> " and found justification for the fix, which changed the direction of the team I was consulting for to implement this fix."
                  <> " After that I began on the deployment bug. The buggy functionality is a unique feature of Obelisk, which manages and syncs"
                  <> " server configs with the version of the server. However due to certain security considerations these are decoupled processes"
                  <> " and so the two did not reliably update with each other. I was able to find a simple solution which tricked Nix into restarting the"
                  <> " server via nginx whenever the configs or server needed to be updated."

smithySynopsis :: T.Text
smithySynopsis = "Smithy is a fast CI and binary distribution tool which leverages Nix and Haskell to build on the Nix toolset to provide elegant"
                 <> " error reporting on builds, a current downfall of the nix toolset. My role was to come in and help re-architect a bug and security holed"
                 <> " filled first implementation. This was in time for a launch in October 2022 at NixCon."
                 

scrappySynopsis :: T.Text
scrappySynopsis = "Scrappy is a brain child of mine after I realized how vital webscraping is to AI and data science. I had done a number of previous projects"
                  <> " where web-scraping was the only solution to achieve ideal results but the tools to do so were far from ideal. Selenium was often the best"
                  <> " option for it's ability to bypass a number of bot detection traps however there is a considerable amount of overhead seeing as an action"
                  <> " must travel from the originating language to the Webdriver Java API which controls a web browser. Scrappy has the full feature set of"
                  <> " a browser minus visual display and plus many features for avoiding bot detection and concurrent task switching among multiple sites for"
                  <> " advanced cases (such as content analysis, see top left corner) in order to not overload one server." 

aceBetaSynopsis :: T.Text
aceBetaSynopsis = "The primary goal of the Ace Beta was to provide a representative user experience with the least amount of engineering investment possible, but"
                  <> " also something that could be worked on later, to convert to something V1 ready. This was truly a great way to learn prioritization of"
                  <> " objectives as I was able to satisfy the goal in less than 2 weeks. The implementation features a very elegant visual design of the generic"
                  <> " user experience as users go through particular"
                  <> " exercises and navigate to the mock interview suite. The recording suite uses the WebRTC API implemented via reflex-dom and is deployed to AWS." 

aceAlphaSynopsis :: T.Text
aceAlphaSynopsis = "The primary goal of the Ace Alpha was to provide the direct solution to users in as little development time as possible and in a way that would"
                   <> " not lead to massive server costs for Ace. For this reason I used Python-Flask full stack framework with plain ol' Vanilla JS (to enable"
                   <> " my Functional Reactive practices for the sake of obviously correct code when reviewing) that is hosted on the AWS Lambda \"serverless\""
                   <> " framework. The frontend required WebRTC in order to handle user-recorded videos." 
                   
aceMarketingSynopsis :: T.Text
aceMarketingSynopsis = "Ace required a site with quite simple functionality (only emailing capabilities for contact us) but also something visually appealing"
                       <> " that could be used for a while, untouched, and give visitors to the site a warm feeling about Ace. This was a fun project to experiment"
                       <> " with CSS animations." 

eCalmmerceSynopsis :: T.Text
eCalmmerceSynopsis = "This project actually started a way to semi-retire my parents and just evolved. What this does is scrape all relevant information on each"
                     <> " product listing you have on Amazon in order to forecast inventory depletion so that it can recommend how many of each unit you should order"
                     <> " that week. The script also integrates supplier pricing sheets in order to give an objective assessment of the most profitable set of items"
                     <> " that they should choose. This saved around 3 hours per day and ever since, they have been able to have a much more care-free life." 

geoCrmSynopsis :: T.Text
geoCrmSynopsis = "This project actually started as a quick little side project to solve my frustrations as a junior sales rep, where most of the data in the"
                 <> " CRM was useless for over 90% of entries and made it impossible to hit my targets. In response I built a scraper for myself which collected and"
                 <> " organized data from Google maps so that I could efficiently cold call and form a pitch based on a quick, high-level understanding of the company"
                 <> " that could be inferred from this valid data. My immediate coworkers found out about from conversation and so I ended up running my tool for"
                 <> " them and soon, both of the Greater Toronto Area offices were 100% using my tool. After I created a very simple MVP, I had a meeting with Xerox"
                 <> " Canada who was extremely excited by the tool and decided to acquire it. I later worked at my first true development role at a competitor of"
                 <> " Xerox and sold them the system as well. The use case is there for any company that does sales, especially when geographical proximity matters"
                 <> " and they have multiple, distinct customer profiles."

amazonProductScraperSynopsis :: T.Text
amazonProductScraperSynopsis = "With having done previous work for my parents Amazon FBA business, I became interested in the world of Amazon Automation and its"
                               <> " potential for my parents, my own Amazon business and some siblings' Amazon businesses. The goal of this project was to expand off"
                               <> " E-calmmerce by not only having analytical tools for current products but tools for expanding to new profitable products."
                               <> " Based off the return data, users of this tool can tell which suppliers are worth reaching out to for price lists (based on count)"
                               <> " as well as compare with costing information sites like Alibaba to see if there's an opportunity for profit."
                               
pathwaysSynopsis :: T.Text
pathwaysSynopsis = "Pathways was the project when I found my passion for AI, Neuroscience and Software Development. I had an unfortunate co-op entrance interview"
                   <> " where a personal tragedy unexpectedly affected my mental state and had me unable to string together coherent, impressive statements about"
                   <> " myself and my experiences. With this experience and learning of all the potential for bias in interviews, I was determined to create a"
                   <> " better way to hire. To do so, I designed a matching platform between employers and job seekers where the parties are matched based on"
                   <> " 3 standards: analytics about the job description (job breadth, job depth) and the seeker's skills (which they've proven capability in)"
                   <> " , questions a company answers on culture fit compared to a user's OCEAN personality, and lastly the company mission compared to the"
                   <> " developers passions and interests. The platform used AI for the sake of going above and beyond obvious recommendations (eg. Python Job rec"
                   <> " for Python dev) to less obvious fits, such as a functional developer of Haskell getting a recommendation for Ocaml, a similar language to"
                   <> " Haskell. During this project I lead a team of 5 developers, which forced me to learn fast as this was my first software project. I ultimately"
                   <> " discontinued Pathways due to the chicken and egg problem and much better prospective projects that were being offered for me to work on." 

elStyle :: DomBuilder t m =>
           T.Text
        -> T.Text -- change this to map
        -> m a
        -> m a 
elStyle tag styleString inner = elAttr tag ("style" =: styleString) inner

-- modalDom :: T.Text
--          -> [T.Text]
-- modalDom = do
--   elClass "div" "" $ do
--     elClass "div" "pb-8 titleText text-2xl" $ text "Communication Analytics"
--     elClass "p" "pb-4" $ text aiSynopsis
--     elClass "p" "pb-4" $ text aiSynopsis2
--     elClass "p" "pb-4" $ text aiSynopsis3
--     elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
--     elClass "div" "pb-8" $ mapM_ pillEl ["numpy", "keras", "pandas", "python", "OpenCV", "Audio Analysis", "Haskell", "Haskell Arrows", "Data mining"]
--     elClass "div" "pb-8" $ mapM_ pillEl [ "youtube-dl", "scrappy"]
--     elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
--     elClass "div" "" $ elAttr "img" ("src" =: $(static "images/visual-ai.png")) blank

homepage :: ( DomBuilder t m
            , SetRoute t (R FrontendRoute) m
            , RouteToUrl (R FrontendRoute) m
            , Prerender t m
            , MonadHold t m
            , PostBuild t m
            , MonadFix m
            ) => m ()
homepage = elClass "div" "banner" $ do
  -- TODO(Galen): Animation on Polymath:Hover of my skills as singular words
  elAttr "img" ("class" =: "bgimg" <> "src" =: "https://synergeticplaytherapy.com/wp-content/uploads/2018/11/Neuroscience-Background.jpg") $ blank
  elClass "div" "pl-1" $ elClass "div" "myClass pl-48" $ do
    text "Hello, Yo soy"
    elAttr "span" ("style" =: "color: #FC74FD;") $ text " Galen Sprout"
    text ", a passionate"
    elAttr "span" ("style" =: "color: #FC74FD;") $ text " polymath"
  ----------------------------------------------------------------------
  -- elClass "div" 
  elClass "div" "body2 px-48 pb-20" $ do 
    elClass "div" "aboutMe pr-5 pt-10" $ do
      elClass "span" "titleText text-2xl" $ text "About me"
      elClass "div" "py-5 text-white text-base" $ text aboutMeText
    elClass "span" "pt-96 mt-20 text-white text-base" $ do
      elClass "span" "pt-5" $ text "In particular, I am obsessed with:"
    ----------------------------
    elClass "div" "pt-2 pl-20 text-white text-base" $ text "Neuroscience Research" 
    elClass "div" "pt-2 pl-20 text-white text-base" $ text "AI that more accurately mimics the brains processes"
    elClass "div" "pt-2 pl-20 text-white text-base" $ text "Lambda calculus and functional programming"
    elClass "div" "pt-2 pl-20 text-white text-base" $ text "Human Language"
    elClass "div" "pt-2 pb-2 pl-20 text-white text-base" $ text "Education"
    ----------------------------
    elClass "div" "text-white text-base" $ do
      text "and the intersection of these domains."
    
    elAttr "div" ("class" =: "text-2xl pt-10 pb-5 text-white") $ do
      elAttr "div" ("style" =: "color: #FC74FD") $ text "Cool cool, so um why do I care?"
      elAttr "div" ("class" =: "grid grid-cols-2 pt-5" <> "style" =: "vertical-align:center;")  $ do 
        elAttr "img" ("src" =: "https://substackcdn.com/image/fetch/h_600,c_limit,f_auto,q_auto:good,fl_progressive:steep/https%3A%2F%2Fbucketeer-e05bbc84-baa3-437e-9518-adb32be77984.s3.amazonaws.com%2Fpublic%2Fimages%2Fb55c4931-11f0-4aa7-b7c8-ddbebfdb9619_1000x562.png" <> "style" =: "width:90%;" <> "class" =: "rounded-lg") $ blank
        elAttr "span" ("class" =: "text-base" <> "style" =: "display:flex;justify-content:center;align-content:center;flex-direction:column;") $ text whyDoICareText
  
    elAttr "div" ("class" =: "text-2xl pt-10 pb-5 text-white") $ do
      elAttr "div" ("style" =: "color: #FC74FD") $ text "Research"
      elClass "div" "pt-5 pb-5 text-white text-base" $ text researchText
      elAttr "a" ("class" =: "py-5 text-white text-base" <> "href" =: "https://docs.google.com/presentation/d/1rlThOHTDyWWFT0yebzND7nrFN47Db-tc9cPUkLU6VMM/edit#slide=id.g9c8fe24bfb_0_0")$ do
        text "you can find a research presentation of mine"
        elAttr "span" ("style" =: "color:#6495ED") $ text " here"
      elClass "div" "pt-5 pb-5 text-white text-base" $ elAttr "a" ("href" =: "https://www.instagram.com/galen.ace.research/") $ do
        elAttr "span" ("style" =: "color:#6495ED") $ text "@galen.ace.research"
        el "span" $ text " is a blog I have where my goal is to simplify concepts and ideas about the brain from the fields of psychology and neuroscience"

    elAttr "div" ("class" =: "text-2xl pt-10 pb-5 text-white") $ do
      engineering
    -- design section
    elClass "div" "pt-10" $ do
      elAttr "div" ("class" =: "text-2xl" <> "style" =: "color:#FC74FD;") $ text "Design" 
      --text "Design Section: Great design is when we aim for fit to the problem over just looks for an ugly solution"
      elClass "div" "pt-4" $ elStyle "div" "color:#FEFEFE;font-family:'Tangerine';font-size:50px" $ do
        elClass "div" "pl-10" $ text "\"Design is not just what it looks like and feels like. Design is how it works.\""
        elClass "div" "text-2xl text-center" $ text " -- Steve Jobs, co-founder of Apple, Inc."
      elClass "div" "pt-5 text-white" $ text "Kind of ironic, is cursive really all that functional? Ah oh well"
      elClass "div" "pt-5 text-white" $ do
        text "Anywho, my best mix of functional and visually appealing design has to be my timeline resume, which landed me a contract at Obsidian Systems."
        text " I say this because resumes are a horrible design. A resume is a collection of events that have happened over a professional life plus some"
        text " high-level summary or summaries. People are a trajectory of responses to a sequence of events; that is each event changes us in a unique"
        text " way that would not have been the same change for another individual's trajectory. Experience is a beautiful and deeply complex thing."
        text " For that reason, I am more likely to have a reader understand the type of hooman I will be when I present my trajectory in a well fitting fashion."

      elClass "div" "pt-4" $ do
        elAttr "img" ("src" =: $(static "images/timeline-resume-small.png")
                      <> "style" =: "display:block;margin-left:auto;margin-right:auto;width:75%"
                     ) blank

      
      elClass "div" "pt-8" $ do
        elAttr "a" ("href" =: $(static "images/timeline-resume.png")
                    <> "download" =: "Galen-Timeline-Resume.png"
                    <> "class" =: "text-white rounded-lg border-4 p-1 place-content-center"
                   ) $ do -- blank
          text "Download high-quality version"

      --elAttr "img" ("style" =: "width:10%;height:10%;transform:rotate(90deg);" <> "src" =: $(static "images/timeline-resume.png")) blank
      
      -- Need to make this expandable and collapsible      

    elClass "div" "pt-10" $ do
      elAttr "div" ("class" =: "text-2xl" <> "style" =: "color:#FC74FD;") $ text "Education"
      elClass "div" "grid grid-cols-2" $ do 
        elClass "div" "pt-5" $ do
          elAttr "a" ("href" =: "https://www.instagram.com/galen.ace.research/") $ do
            elStyle "div" "color:#6495ED" $ text "@Galen.Ace.Research" 
          elAttr "a" ("href" =: "https://www.youtube.com/channel/UCWj1YE_9RywYA92ZZH6YL6w") $ do
            elAttr "div" ("class" =: "pt-5" <> "style" =: "color:#6495ED") $ text "Simple Haskell (on the right)"
          elAttr "a" ("href" =: "https://www.linkedin.com/pulse/what-most-general-definition-probability-galen-sprout/") $ do
            elAttr "div" ("class" =: "pt-5" <> "style" =: "color:#6495ED") $ text "The Most General Definition of Probability"  
          elAttr "a" ("href" =: "https://medium.com/all-things-ace/scrappy-tutorial-135283dc2af") $ do
            elAttr "div" ("class" =: "pt-5" <> "style" =: "color:#6495ED") $ text "Scrappy Tutorial"
        elClass "div" "pt-5" $ do
        
   -- <iframe width="560" height="315" src="https://www.youtube.com/embed/uykWjDYKNtU" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

          let src = "https://www.youtube.com/embed/uykWjDYKNtU" 
      
          elAttr "iframe" ("width" =: "560" <> "height" =: "315" <> "src" =: src <> "title" =: "Youtube video player" <> "frameborder" =: "0"
                           <> "allow" =: "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                           <> "allowFullscreen" =: "true") blank

  elAttr "div" ("style" =: "background-color:black;width:100%;height:600px" <> "class" =: "pt-10 flex justify-center") $ do
    
    elClass "div" "" $ prerender (pure never) contactMeForm

      

    --- email form
             
    
  return ()

 

contactMeForm :: ( DomBuilder t m
                 , MonadJSM (Performable m)
                 , PerformEvent t m
                 , TriggerEvent t m
                 , MonadHold t m
                 , PostBuild t m 
                 ) => m (Event t ())
contactMeForm = elAttr "div" ("class" =: "p-10 border rounded-lg bg-white" <> "style" =: "border-color:#FC74FD;border-width:4px;") $ do
  let inputClass = "border rounded-md border-black p-2"
  elStyle "div" "font-size:30px;color:#FC74FD" $ text "Contact Me"
  elClass "div" "pt-3" $ text "Have a question or want to work together?"
  elClass "div" "pt-3" $ text "Your Name"
  name <- elClass "div" "" $ fmap (current . value) $ inputElement $ def & initialAttributes .~ "class" =: inputClass
  elClass "div" "pt-3" $ text "Your Email"
  email <- elClass "div" "" $ fmap (current . value) $ inputElement $ def & initialAttributes .~ "class" =: inputClass
  elClass "div" "pt-3" $ text "Message"
  message <- elStyle "div" "" $ fmap (current . value) $ textAreaElement $ def & initialAttributes .~ ("style" =: "height:100px;width:100%"
                                                                                                       <> "rows" =: "2"
                                                                                                       <> "class" =: inputClass
                                                                                                      )
             
  
  submit <- button "Submit"

  
  
  --errorMessage <- holdDyn "" $ mergeWith const [nameIsEmpty, emailInvalid, messageIsEmpty]
  
  --dynText errorMessage 
  let
    
    e = tag ((,,) <$> (("NAME:" <>) <$> name) <*> (("EMAIL:" <>) <$> email) <*> (("MESSAGE:" <>) <$> message)) submit

    formRaw = (,,) <$> name <*> email <*> message

    eithForm = ffor formRaw $ \(n, e, m) -> validateForm n e m
    (err, form) = fanEither $ tag eithForm submit
  -- Form type
  -- validate Form for the 3 checks
  -- Either Error Form 

  errorMessage <- holdDyn Nothing (Just <$> err)
  el "div" $ dynText $ (fromMaybe "") <$> errorMessage 
  
  --fanEither e 
  res <- performRequestAsync $ fmap (postJson "http://localhost:8000/email") $ gate (fmap isJust $ current errorMessage) $ form

  -- TOOO(galen): what if the internet is out? 502
  
  
  pure (() <$ res)
  -- name
  -- email
  -- message

--feedbackHelper f = dyn_ $ maybe blank (uncurry feedbackAlert) <$> f

data Form = Form T.Text T.Text T.Text deriving Generic

instance ToJSON Form

validateForm :: T.Text -> T.Text -> T.Text -> Either T.Text Form
validateForm name email msg =
  case T.null name of
    True -> Left "Name cannot be empty"
    False -> case EmailValidate.validate (T.encodeUtf8 email) of
      Left str -> Left "Invalid Email" 
      Right _ -> case T.null msg of
        True -> Left "message cannot be empty"
        False -> Right $ Form name email msg 
        
      





newtype SignupConfig t = SignupConfig
  { _signupConfig_errors :: Dynamic t (Maybe T.Text)
  }

data Signup t m = Signup
  { _signup_email :: InputEl t m
  , _signup_submit :: Event t ()
  }


type InputEl t m = InputElement EventResult (DomBuilderSpace m) t
type Template t m = (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)

signup
  :: ( Template t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , Prerender t m
     )
  => SignupConfig t
  -> m (Signup t m)
signup cfg = screenContainer $ do
  elClass "div" "p-4 mx-auto md:w-sm t_login" $ mdo
    elClass "h1" "font-karla font-bold text-h1 text-copy mt-12" $
      text "Signup"
    email <- elClass "div" "flex flex-col mt-4" $ do
      elClass "div" "font-facit font-label text-label" $ text "Email"
      inputElement $ def
        & initialAttributes .~ "type" =: "text"
    maybeDisplay errorMessage $ _signupConfig_errors cfg
    submit <- primaryButton "Confirm Email"
    elClass "div" "font-facit font-label underline text-label text-link text-center mt-4" $ blank
      --routeLink (FrontendRoute_Login :/ ()) $ text "Already have an account?"
    return $ Signup
      { _signup_email = email
      , _signup_submit = submit
      }


screenContainer :: (DomBuilder t m) => m a -> m a
screenContainer = elClass "div" "w-screen h-screen bg-background flex flex-col overflow-hidden"

primaryButton :: DomBuilder t m => T.Text -> m (Event t ())
primaryButton buttonText = do
  (e, _) <- elClass' "button" classes $ text buttonText
  pure $ domEvent Click e
  where
    classes =
      "focus:outline-none w-full p-4 mt-16 shadow-button bg-primary \
      \ font-facit font-bold text-white text-body text-center rounded \
      \ hover:bg-primary-rich active:bg-primary-desaturated \
      \ focus:ring-4 ring-primary ring-opacity-50"

-- | Render the given template only when the 'Dynamic' is 'Just'.
maybeDisplay
  :: Template t m
  => (a -> m ()) -- ^ How to render contents
  -> Dynamic t (Maybe a) -- ^ What to watch for
  -> m ()
maybeDisplay template val = dyn_ $ ffor val $ \case
  Nothing -> blank
  Just x -> template x


-- | Render an error message.
errorMessage :: Template t m => T.Text -> m ()
errorMessage t =
  divClass "font-facit text-error text-opacity-70 h-4 mt-1" $ text t


engineering :: ( DomBuilder t m
               , MonadFix m
               , MonadHold t m
               , PostBuild t m
               , SetRoute t (R FrontendRoute) m
               , RouteToUrl (R FrontendRoute) m
               , Prerender t m 
               ) => m ()
engineering = do 
  elAttr "div" ("style" =: "color: #FC74FD") $ text "Engineering"  
  elClass "div" "grid grid-cols-4 grid-rows-3 pt-5" $ do
    projectWithModal "AI" "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQQCbWxZoZI4NdOXw27x8L0_qxfx6FgO4zG-g&usqp=CAU" $ do
      elClass "div" "" $ do
        elClass "div" "pb-8 titleText text-2xl" $ text "Communication Analytics"
        elClass "p" "pb-4" $ text aiSynopsis
        elClass "p" "pb-4" $ text aiSynopsis2
        elClass "p" "pb-4" $ text aiSynopsis3
        elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
        elClass "div" "pb-8" $ mapM_ pillEl ["numpy", "keras", "pandas", "python", "OpenCV", "Audio Analysis", "Haskell", "Haskell Arrows", "Data mining"]
        elClass "div" "pb-8" $ mapM_ pillEl [ "youtube-dl", "scrappy"]
        elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
        elClass "div" "" $ elAttr "img" ("src" =: $(static "images/visual-ai.png")) blank
        -- Title
        -- Project synopsis
        -- Tech used
        -- A photo
        -- Maybe Demo 

    projectWithModal "" "https://miro.medium.com/max/1400/1*xlQkrAgszpA-Xe40V73VeA.png" $ do
      elClass "div" "" $ do
        elClass "div" "pb-8 titleText text-2xl" $ text "Nervos Force Bridge (Contributor)"
        elClass "p" "pb-4" $ text nervosSynopsis
        elClass "p" "pb-4" $ text nervosSynopsis2 
        elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
        elClass "div" "pb-8" $ mapM_ pillEl ["TypeScript", "Docker", "Nix", "React.js", "Haskell", "Obelisk (Haskell)", "Reflex-FRP (Haskell)", "PostgreSQL"]
        elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
        elClass "div" "" $ elAttr "img" ("src" =: $(static "images/force-bridge.png")) blank
            
      -- nervos
    projectWithModal "Obelisk" "https://repository-images.githubusercontent.com/134497903/52ee3600-57bd-11ea-803a-0d11e9bbc115" $ do
      elClass "div" "" $ do
        elClass "div" "pb-8 titleText text-2xl" $ text "Obelisk (Contributor)"
        elClass "p" "pb-4" $ text obeliskSynopsis 
        elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
        elClass "div" "pb-8" $ mapM_ pillEl ["Haskell", "Nix"] 
        elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
        elClass "div" "" $ elAttr "img" ("src" =: $(static "images/force-bridge.png")) blank
        -- TODO(galen): replace photo
          
    projectWithModal "Smithy" $(static "images/smithy.gif") $ do
      elClass "div" "" $ do
        elClass "div" "pb-8 titleText text-2xl" $ text "Smithy"
        elClass "p" "pb-4" $ text smithySynopsis
        elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
        elClass "div" "pb-8" $ mapM_ pillEl ["Haskell", "Nix", "PostgreSQL"] 
        elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
        elAttr "a" ("style" =: "color:#6495ED" <> "href" =: "https://smithy.build/o/osos") $ text "Check it out"
        elClass "div" "pb-3" $ elAttr "img" ("src" =: $(static "images/smithy-demo.png")) blank
        elClass "div" "" $ elAttr "img" ("src" =: $(static "images/smithy-demo2.png")) blank
            

    projectWithModal "" $(static "images/scrappy.png") $ do
      elClass "div" "" $ do
        elClass "div" "pb-8 titleText text-2xl" $ text "Scrappy"
        elClass "p" "pb-4" $ text scrappySynopsis
        elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
        elClass "div" "pb-8" $ mapM_ pillEl ["Haskell", "Nix", "NodeJS"] 
        elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
        elClass "div" "pb-2" $ routeLink (ScrappyDemo :/ ()) $ elStyle "span" "color:#6495ED" $ text "Try out the scrappy demo"
        elAttr "a" ("style" =: "color:#6495ED" <> "href" =: "https://medium.com/all-things-ace/scrappy-tutorial-135283dc2af") $ text "Read the scrappy tutorial"
 
    projectWithModal "" "https://aceinterviewprep.s3.ca-central-1.amazonaws.com/static/images/Ace-blue-logo-svg.svg" $ do
      elClass "div" "" $ do
        elClass "div" "pb-8 titleText text-2xl" $ text "Ace Beta"
        elClass "p" "pb-4" $ text aceBetaSynopsis
        elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
        elClass "div" "pb-8" $ mapM_ pillEl ["Haskell", "Nix", "PostgreSQL", "Obelisk", "Reflex(HTML,JS)", "reflex-dom", "WebRTC", "AWS EC2"] 
        elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
        elAttr "a" ("style" =: "color:#6495ED" <> "href" =: "https://aceinterviewpreparation.com/main/home/1") $ text "Try out the beta"
        elClass "div" "" $ elAttr "img" ("src" =: $(static "images/ace-beta.png")) blank

    projectWithModal "Alpha" $(static "images/star_of_ace2.png") $ do 
      elClass "div" "" $ do
        elClass "div" "pb-8 titleText text-2xl" $ text "Ace Alpha"
        elClass "p" "pb-4" $ text aceAlphaSynopsis
        elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
        elClass "div" "pb-8" $ mapM_ pillEl ["Python", "Python Flask", "Vanilla JS", "HTML", "CSS", "AWS Lambda", "API Gateway", "AWS S3"]
        elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
        elAttr "a" ("style" =: "color:#6495ED" <> "href" =: "https://5a6996u1hc.execute-api.ca-central-1.amazonaws.com/Prod/form/") $ text "Try out the Alpha"
        elClass "div" "" $ elAttr "img" ("src" =: $(static "images/ace-alpha.png")) blank

    projectWithModal "Ace Marketing Site" $(static "images/ace-marketing.png") $ do
      elClass "div" "" $ do
        elClass "div" "pb-8 titleText text-2xl" $ text "Ace Marketing Site"
        elClass "p" "pb-4" $ text aceMarketingSynopsis
        elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
        elClass "div" "pb-8" $ mapM_ pillEl ["Digital Ocean", "Vanilla JS", "CSS+Animations", "HTML"]
        elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
        elAttr "a" ("style" =: "color:#6495ED" <> "href" =: "https://aceinterviewprep.io/") $ text "Check it out"

    projectWithModal "" $(static "images/e-calmmerce.jpg") $ do
      elClass "div" "" $ do
        elClass "div" "pb-8 titleText text-2xl" $ text "Amazon FBA Order Quantity Automation"
        elClass "p" "pb-4" $ text eCalmmerceSynopsis
        elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
        elClass "div" "pb-8" $ mapM_ pillEl ["Selenium", "Python"]
        elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
        elClass "div" "pb-3" $ elAttr "a" ("style" =: "color:#6495ED"
                               <> "href" =: $(static "Daily-Inventory-Report.xlsx")
                               <> "download" =: "Daily-Inventory-Report.xlsx") $ do
          text "Download a sample"

        el "div" $ elAttr "a" ("style" =: "color:#6495ED" <> "href" =: "https://github.com/augyg/Amazon-Web-Scrape/blob/master/Amazon_PO_report_build.py") $ do
          text "Go to repository"


    projectWithModal "" $(static "images/geo-crm.jpg") $ do
      elClass "div" "" $ do
        elClass "div" "pb-8 titleText text-2xl" $ text "The Best CRM For In-Person Sales"
        elClass "p" "pb-4" $ text geoCrmSynopsis
        elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
        elClass "div" "pb-8" $ mapM_ pillEl ["Selenium", "Python"]
        elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
        elAttr "a" ("style" =: "color:#6495ED" <> "href" =: "https://www.google.com/maps/d/u/1/edit?mid=1A5lVW1l9Mt2F_O5I9_V8_KvrDRv1x_vj&ll=43.643953056447415%2C-79.37550918803711&z=14") $ do
          text "Check out the MVP"
        elClass "div" "" $ elAttr "img" ("src" =: $(static "images/geocrm.png")) blank
          
    projectWithModal "" "https://images-na.ssl-images-amazon.com/images/G/15/gc/designs/livepreview/amazon_dkblue_noto_email_v2016_ca-main._CB468775011_.png" $ do
      elClass "div" "" $ do
        elClass "div" "pb-8 titleText text-2xl" $ text "Amazon Product Scraper"
        elClass "p" "pb-4" $ text amazonProductScraperSynopsis
        elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
        elClass "div" "pb-8" $ mapM_ pillEl ["Selenium", "Python"]
        elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
        el "a" $ elAttr "a" ("style" =: "color:#6495ED" <> "href" =: $(static "AmazonProductScrape.csv") <> "download" =: "AmazonProductList.csv") $ do
          text "Download a sample"
            
    projectWithModal "" $(static "images/Pathways.png") $ do
      elClass "div" "" $ do
        elClass "div" "pb-8 titleText text-2xl" $ text "Pathways"
        elClass "p" "pb-4" $ text pathwaysSynopsis
        elClass "div" "pb-8 titleText text-2xl" $ text "Tools"
        elClass "div" "pb-8" $ mapM_ pillEl ["Express.js", "Node.js", "React", "Python", "Pandas", "Keras", "Tensorflow", "Selenium", "MySQL"]
        elClass "div" "pb-8 titleText text-2xl" $ text "Demo/Pictures"
        elAttr "img" ("src" =: $(static "images/pathways.png")) blank
          

    pure ()



projectWithModal :: ( DomBuilder t m
                    , MonadFix m
                    , MonadHold t m
                    , PostBuild t m 
                    )
                 => T.Text
                 -> T.Text
                 -> m a
                 -> m ()
projectWithModal title imgSrc modalDom = do
  click <- fmap (domEvent Click . fst) $ elAttr' "div" ("class" =: "border-2" <> "style" =: "position:relative;height:250px;width;200pxtext-align:center;")$ do
    elAttr "img" ("src" =: imgSrc <> "class" =: "bgimg" <> "style" =: "width:100%;height:100%;") blank

    elStyle "div" "z-index:1;position:absolute;left:45%;top:40%;text-align:center" $ text title
  modal click modalDom




modal :: ( DomBuilder t m
         , MonadFix m
         , MonadHold t m
         , PostBuild t m 
         ) => Event t () -> m a -> m ()
modal open modalDom = mdo  
  let styleBase = "position:fixed;z-index:2;padding-top:100px;left:0;top:0;width:100%;height:100%;overflow:auto;background-color:rgb(0,0,0);background-color:rgba(0,0,0,0.4);"
      hideModal = ("style" =: ("display:none;" <> styleBase))
      showModal = ("style" =: ("display:block;" <> styleBase))

            
  modalAttrs <- holdDyn hideModal $ mergeWith const [showModal <$ open, hideModal <$ close]
  close <- elDynAttr "div" modalAttrs $ do
    elAttr "div" ("style" =: "background-color:#00004D; ;margin:auto;padding:20px;width:80%;color:white;"
                  <> "class" =: "border-double rounded-md border-8 border-black text-base") $ do 
      e <- fmap fst $ elClass' "span" "close" $ text "x"
      modalDom
      pure $ domEvent Click e

  pure ()









type HrefURI = T.Text

funcShui :: HrefURI -> Map.Map T.Text T.Text
funcShui href = "src" =: href
                <> "width" =: "300px"
                <> "height" =: "auto"

buildReq :: Reflex t => Event t T.Text -> Event t (XhrRequest ())
buildReq = fmap (\texy -> XhrRequest "GET" (termToUrl texy) def)
  

termToUrl :: T.Text -> T.Text
termToUrl term = "/backend/" <> term

startingImage :: T.Text
startingImage = "https://aceinterviewprep.s3.ca-central-1.amazonaws.com/static/images/c88a73d8b1a44d5782dd5c2e0bfd13d0.png"

mkGoogleUrl t = "https://www.google.com/"
  -- "https://www.google.com/search?q=" <> t <> "&sxsrf=AOaemvJc28k4ka_rY6sAp-TapWE8ldF4iA:1638157797795&source=lnms&tbm=isch&sa=X&ved=2ahUKEwj24_2s1bz0AhVJnGoFHX--AuwQ_AUoAXoECAEQAw&biw=1920&bih=979&dpr=1" 













-- -- funcyAf :: (DomBuilder t m
--            -- , MonadJSM (Performable m)
--            -- , HasJSContext (Performable m)
--            -- , PerformEvent t m
--            -- , TriggerEvent t m
--            -- , MonadIO (Performable m)) => m ()
-- funcyAf :: (DomBuilder t m
--            , PostBuild t m
--            , MonadHold t m
--            , PerformEvent t m
--            , TriggerEvent t m
--            , TriggerEvent t (Client m)
--            , PerformEvent t (Client m)
--            , HasJSContext (Performable (Client m))
--            , MonadJSM (Performable (Client m))
--            , MonadIO (Client m)
--            , MonadJSM (Performable m)
--            , Prerender js t m
--            , MonadIO m
--            , HasJSContext (Performable m)) => m ()
-- -- funcyAf :: DomBuilder t m => m ()
-- funcyAf = do
--   newSearchTerm <- el "div" $ do
--     inpElem <- inputElement def
--     let
--       ourSearchTerm = _inputElement_value inpElem
--       scraper = runScraperOnHtml hrefParser
--     return $ (current ourSearchTerm)
--   buttonClick <- button "scrape away"
--   response <- getAndDecode (tag newSearchTerm buttonClick)
--   -- txt <- holdDyn "" (fmap fromJust response)
--   let
--     -- txt :: FromJSON => Event t (Maybe a)
--     -- txt = fmap fromJust $ switch (current response)
--   maybeNewVal <- holdDyn (Just "nothing" :: Maybe T.Text) response
--   dynText $ fmap (fromMaybe "Nothinggg") maybeNewVal
--   -- let
--     -- dadBod :: Event t XhrResponseBody
--     -- dadBod = fmap (fromJust . _xhrResponse_response) response
--     -- f (XhrResponseBody_Default txt) = T.unpack txt 
                                      
--   return ()
