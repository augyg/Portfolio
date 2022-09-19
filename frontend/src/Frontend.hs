{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

-- import FrontendTwo
import Scrape (runScraperOnHtml)
import Requests (getHtml')
import Elem.ElemHeadParse (hrefParser)

import Data.Maybe (fromMaybe, fromJust)
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
  , _frontend_body = subRoute_ $ \case
      FrontendRoute_Main -> prerender_ blank $ do
        (newSearchTerm, buttonClick) <- el "div" $ do 
          newSearchTerm <- el "div" $ fmap value (inputElement def)
          buttonClick <- button "scrape away"
          return (newSearchTerm, buttonClick)
        response <- performRequestAsync $ buildReq (tag (current newSearchTerm) buttonClick)
        let 
          dadBod = fmap ((fromMaybe "not sure what happened") . _xhrResponse_responseText) response 
        primeDadBod <- holdDyn startingImage dadBod
        -- dynText primeDadBod
        el "div" $ elDynAttr "img" (fmap funcShui primeDadBod) $ blank
      
        return ()

      FrontendRoute_Home -> homepage
      Research -> text " research "
        -- should also include my general concept of probabibility
        -- this will probably be an explanatory page

      AI -> return () --ai
      AceBeta -> let args = [] in projectPage args -- :: [(Picture, Text)] )
      Scrappy -> projectPage []
      AceAlpha -> projectPage []
      AceMainSite -> projectPage []
      ProductFinder -> projectPage []
      SalesTool -> projectPage []
      AmazonAutomation -> projectPage []
      Pathways -> projectPage []
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
               <> " a wide range of previously unexplainable observations about the brain" 

aiSynopsis :: T.Text
aiSynopsis = "My current adventure in AI is an implementation of my ideas for more valid AI. While I have experimented and read into many models for a past"
             <> " project (see Pathways, bottom right), I have never been able to find any justification that such models actually work like the brain."
             <> " It is not absolutely necessary for a model to behave like the brain in simpler cases, but the further it is from the brain, the further"
             <> " such a design will be from a general AI, or more importantly, able to generalize properly to the full range of inputs." 
               

elStyle :: DomBuilder t m =>
           T.Text
        -> T.Text -- change this to map
        -> m a
        -> m a 
elStyle tag styleString inner = elAttr tag ("style" =: styleString) inner

homepage :: ( DomBuilder t m
            , SetRoute t (R FrontendRoute) m
            , RouteToUrl (R FrontendRoute) m
            , Prerender t m
            , MonadHold t m
            , PostBuild t m
            , MonadFix m 
            ) => m ()
homepage = elClass "div" "banner" $ do
  elAttr "img" ("class" =: "bgimg" <> "src" =: "https://synergeticplaytherapy.com/wp-content/uploads/2018/11/Neuroscience-Background.jpg") $ blank
  elClass "div" "myClass" $ do
    text "Hello, Yo soy"
    elAttr "span" ("style" =: "color: #FC74FD;") $ text " Galen Sprout"
    text ", a passionate"
    elAttr "span" ("style" =: "color: #FC74FD;") $ text " polymath"
  elClass "div" "body2 px-40" $ do 
    elClass "div" "aboutMe pl-5 pr-5 pt-10" $ do
      elClass "span" "titleText text-2xl" $ text "About me"
      elClass "div" "pt-5 pb-5 text-white text-base" $ text aboutMeText
    elClass "span" "pl-5 pt-96 mt-20 text-white text-base" $ do
      elClass "span" "pt-5" $ text "In particular, I am obsessed with:"
    elClass "div" "pt-2 pl-20 text-white text-base" $ text "Neuroscience Research" 
    elClass "div" "pt-2 pl-20 text-white text-base" $ text "AI that more accurately mimics the brains processes"
    elClass "div" "pt-2 pl-20 text-white text-base" $ text "Lambda calculus and functional programming"
    elClass "div" "pt-2 pl-20 text-white text-base" $ text "Human Language"
    elClass "div" "pt-2 pb-2 pl-20 text-white text-base" $ text "Education"
  
    elClass "div" "pl-5 text-white text-base" $ do
      text "and the intersection of these domains."

    elAttr "div" ("class" =: "text-2xl pl-5 pt-10 pb-5 text-white") $ do
      elAttr "div" ("style" =: "color: #FC74FD") $ text "Cool cool, so um why do I care?"
      elClass "div" "grid grid-cols-2" $ do 
        elAttr "img" ("src" =: "https://substackcdn.com/image/fetch/h_600,c_limit,f_auto,q_auto:good,fl_progressive:steep/https%3A%2F%2Fbucketeer-e05bbc84-baa3-437e-9518-adb32be77984.s3.amazonaws.com%2Fpublic%2Fimages%2Fb55c4931-11f0-4aa7-b7c8-ddbebfdb9619_1000x562.png" <> "style" =: "width: 90%" <> "class" =: "pt-5 rounded-lg") $ blank
        elClass "div" "pt-24 align-middle text-base" $ text whyDoICareText
  
    elAttr "div" ("class" =: "text-2xl pl-5 pt-10 pb-5 text-white") $ do
      elAttr "div" ("style" =: "color: #FC74FD") $ text "Research"
      elClass "div" "pt-5 pb-5 text-white text-base" $ text researchText
      elAttr "a" ("class" =: "pt-5 pb-5 text-white text-base" <> "href" =: "https://docs.google.com/presentation/d/1rlThOHTDyWWFT0yebzND7nrFN47Db-tc9cPUkLU6VMM/edit#slide=id.g9c8fe24bfb_0_0")$ do
        text "you can find a research presentation of mine"
        elAttr "span" ("style" =: "color:#6495ED") $ text " here"
      elClass "div" "pt-5 pb-5 text-white text-base" $ elAttr "a" ("href" =: "https://www.instagram.com/galen.ace.research/") $ do
        elAttr "span" ("style" =: "color:#6495ED") $ text "@galen.ace.research"
        el "span" $ text " is a blog I have where my goal is to simplify concepts and ideas about the brain from the fields of psychology and neuroscience"

    elAttr "div" ("class" =: "text-2xl pl-5 pt-10 pb-5 text-white") $ do
      elAttr "div" ("style" =: "color: #FC74FD") $ text "Engineering"  
      elClass "div" "grid grid-cols-4 grid-rows-3 pt-5" $ do
        projectWithModal "AI" "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQQCbWxZoZI4NdOXw27x8L0_qxfx6FgO4zG-g&usqp=CAU" $ do
          text "hey"
        projectWithModal "" "https://miro.medium.com/max/1400/1*xlQkrAgszpA-Xe40V73VeA.png" $ do
          -- nervos
          elClass "div" "" $ do
            text "Communication Analytics"
            text aiSynopsis
            -- Title
            -- Project synopsis
            -- Tech used
            -- A photo
            -- Maybe Demo 

        projectWithModal "Obelisk" "https://repository-images.githubusercontent.com/134497903/52ee3600-57bd-11ea-803a-0d11e9bbc115" $ do
          blank
          
        projectWithModal "Smithy" $(static "images/smithy.gif") $ do
          blank

        projectWithModal "" $(static "images/scrappy.png") $ do
          blank

        projectWithModal "" "https://aceinterviewprep.s3.ca-central-1.amazonaws.com/static/images/Ace-blue-logo-svg.svg" $ do
          blank 
        projectWithModal "Alpha" $(static "images/star_of_ace2.png") $ do 
          blank

        projectWithModal "Ace Marketing Site" $(static "images/ace-marketing.png") $ do
          blank

        projectWithModal "" $(static "images/e-calmmerce.jpg") $ do
          blank 

        projectWithModal "" $(static "images/geo-crm.jpg") $ do
          blank
          
        projectWithModal "" "https://images-na.ssl-images-amazon.com/images/G/15/gc/designs/livepreview/amazon_dkblue_noto_email_v2016_ca-main._CB468775011_.png" $ do
          blank 

        projectWithModal "" $(static "images/Pathways.png") $ do
          blank
        -- let styleGen = "position:fixed;color:blue;z-index:1;" :: T.Text
        --     styleShown = "display:block;background-color:white;"
        --     styleHidden = "display:none;"

            -- attrs = ("class" =: "modal" <> "hidden" =: "true")
            -- attrs2 = ("class" =: "modal" <> "hidden" =: "false")


        pure ()
        -- let styleBase = "position:fixed;z-index:1;padding-top:100px;left:0;top:0;width:100%;height:100%;overflow:auto;background-color:rgb(0,0,0);rgba(0,0,0,0.4);"
        --     hideModal = ("style" =: ("display:none;" <> styleBase))
        --     showModal = ("style" =: ("display:block;" <> styleBase))

            
        -- modalAttrs <- holdDyn hideModal $ mergeWith const [showModal <$ click, hideModal <$ close]
        -- close <- elDynAttr "div" modalAttrs $ do
        --   elAttr "div" ("style" =: "background-color:#FEFEFE;margin:auto;padding:20px;border:1px solid #888; width:80%") $ do 
        --     e <- fmap fst $ elClass' "span" "close" $ text "x"
        --     el "p" $ text "some text in the modal"
        --     pure $ domEvent Click e

        -- pure ()
  -- el "div" $ text "What would you like to do?"
  -- el "div" $ text "Catch/Hire him" -- should open up templated email that sends to me
  -- el "div" $ do
  --   text "Query"
  --   routeLink (Research :/ ()) $ do
  --     text "Research"
      
  --   el "div" $ do
  --     text "Engineering" 
  --     routeLink (AI :/ ()) $ text "Artificial Intelligence"
  --     routeLink (Nervos :/ ()) $ text "Nervos Force Bridge (Contributor)"
  --     routeLink (Obelisk :/ ()) $ text "Obelisk (Contributor)"
  --     routeLink (Smithy :/ ()) $ text "Smithy CI (Contributor)"  
  --     routeLink (AceBeta :/ ()) $ text "Ace Beta"
  --     routeLink (Scrappy :/ ()) $ text "Scrappy"
  --     routeLink (AceAlpha :/ ()) $ text "Ace Alpha"
  --     routeLink (AceMainSite :/ ()) $ text "Ace Main Site"
  --     routeLink (ProductFinder :/ ()) $ text "Amazon Product Finder"            
  --     routeLink (SalesTool :/ ()) $ text "Xerox Sales Tool"
  --     routeLink (AmazonAutomation :/ ()) $ text "Amazon FBA Automation"      
  --     routeLink (Pathways :/ ()) $ text "Pathways"




      


      
      
      -- Need to make this expandable and collapsible
    
  el "div" $ do
      -- Need to make this expandable and collapsible      
      text "Education"
      elAttr "div" ("href" =: "galen.ace.research") $ text "@Galen.Ace.Research" 
      elAttr "div" ("href" =: "my youtube channel") $ text "Simple Haskell"
      
             
    
  return ()

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
    elAttr "div" ("style" =: "background-color:#26619C; ;margin:auto;padding:20px;width:80%;color:white;" <> "class" =: "border-double rounded-md border-8") $ do 
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
