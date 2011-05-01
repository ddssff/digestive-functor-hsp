{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Text.Digestive.HSP.Html4 where

import Control.Applicative             ((<$>))
import Control.Monad                   (mplus)
import Data.Maybe                      (fromMaybe)
import Data.Monoid                     (Monoid(mempty), mconcat)
import Data.Text                       (Text)
import qualified Data.Text             as Text
import HSP                             (XMLGenerator, XMLGenT, EmbedAsChild(..), EmbedAsAttr(..), Attr(..), genElement, genEElement, set)
import qualified HSX.XMLGenerator      as HSX
import Text.Digestive                   -- (Form, mapViews)
import Text.Digestive.Common           as Common        -- (Form, mapViews)
import Text.Digestive.Forms            as Forms hiding (inputChoice) -- (inputString, inputRead, inputBool, inputChoice)


showFormId :: FormId -> String
showFormId id' = show id'

inputString :: (Monad m, Functor m, XMLGenerator x, FormInput i f)
          => Maybe String
          -> Form m i e [XMLGenT x (HSX.XML x)] String
inputString = 
    Forms.inputString $ \id' inp ->
        [<input type="text" name=(showFormId id') id=(showFormId id') value=(fromMaybe "" inp) />]

-- FIMXE: we really need a inputText primitive on Common. or maybe inputByteString?
inputText :: (Monad m, Functor m, XMLGenerator x, FormInput i f)
          => Maybe Text
          -> Form m i e [XMLGenT x (HSX.XML x)] Text
inputText v = 
    Text.pack <$>
        ((Forms.inputString $ \id' inp ->
            [<input type="text" name=(showFormId id') id=(showFormId id') value=(fromMaybe "" inp) />]) (Text.unpack <$> v))

inputTextArea :: (Monad m, Functor m, XMLGenerator x, FormInput i f) =>
                 Maybe Int
              -> Maybe Int
              -> Maybe String
              -> Form m i e [XMLGenT x (HSX.XML x)] String
inputTextArea r c = 
    Forms.inputString $ \id' inp ->
        [<textarea name=(showFormId id') id=(showFormId id') (rows r ++ cols c)><% fromMaybe "" inp %></textarea>]
    where
      rows Nothing  = []
      rows (Just n) = [("rows" := n)]
      cols Nothing  = []
      cols (Just n) = [("cols" := n)]

inputTextRead :: (Monad m, Functor m, Show a, Read a, XMLGenerator x, FormInput i f)
              => String
              -> Maybe a
              -> Form m i String [XMLGenT x (HSX.XML x)] a
inputTextRead error' =
    flip inputRead error' $ \id' inp ->
        [<input type="text" name=(showFormId id') id=(showFormId id') value=(fromMaybe "" inp) />]

inputPassword :: (Monad m, Functor m, XMLGenerator x, FormInput i f)
              => Form m i e [XMLGenT x (HSX.XML x)] String
inputPassword =
    flip Forms.inputString Nothing $ \id' inp ->
        [<input type="password" name=(showFormId id') id=(showFormId id') value=(fromMaybe "" inp) />]

checked True = [("checked" := "checked")]
checked False = []

inputCheckBox :: (Monad m, Functor m, XMLGenerator x, FormInput i f)
              => Bool
              -> Form m i e [XMLGenT x (HSX.XML x)] Bool
inputCheckBox inp =
    flip inputBool inp $ \id' inp ->
        [<input type="checkbox" name=(showFormId id') id=(showFormId id') (checked inp) />]

inputCheckboxes :: (Monad m, Functor m, Eq a, XMLGenerator x, EmbedAsChild x c, Monoid c, FormInput i f)
           => Bool                                 -- ^ Use @<br>@ tags
           -> [a]                                  -- ^ Default option
           -> [(a, c)]                             -- ^ Choices with their names
           -> Form m i e [XMLGenT x (HSX.XML x)] [a] -- ^ Resulting form
inputCheckboxes br defs choices =
    inputChoices toView defs (map fst choices)
  where
    toView group' id' sel val =
        [ <input type="checkbox" name=(showFormId group') id=id' value=id' />
        , <label for=id'><% fromMaybe mempty $ lookup val choices %></label>
        ] ++ if br then [<br />] else []

inputRadio :: (Monad m, Functor m, Eq a, XMLGenerator x, EmbedAsChild x c, Monoid c, FormInput i f)
           => Bool                                 -- ^ Use @<br>@ tags
           -> Maybe a                              -- ^ Default option
           -> [(a, c)]                             -- ^ Choices with their names
           -> Form m i e [XMLGenT x (HSX.XML x)] (Maybe a) -- ^ Resulting form
inputRadio br def choices =
    inputChoice toView def (map fst choices)
  where
    toView group' id' sel val =
        [ <input type="radio" name=(showFormId group') id=id' value=id' />
        , <label for=id'><% fromMaybe mempty $ lookup val choices %></label>
        ] ++ if br then [<br />] else []

submit :: (Monad m, Functor m, XMLGenerator x, FormInput i f)
          => String
          -> Form m i e [XMLGenT x (HSX.XML x)] String
submit v = 
    Forms.inputString (\id' inp ->
        [<input type="submit" name=(showFormId id') id=(showFormId id') value=(fromMaybe "" inp) />]) (Just v)

label :: (Monad m, XMLGenerator x, EmbedAsChild x c, EmbedAsAttr x (Attr String String))
      => c
      -> Form m i e [XMLGenT x (HSX.XML x)] ()
label string =
    Common.label $ \id' ->
        [<label for=(showFormId id')><% string %></label>]

errorList :: (XMLGenerator x, EmbedAsChild x c, EmbedAsChild x [HSX.XML x]) => [c] -> [XMLGenT x (HSX.XML x)]
errorList [] = []
errorList children =
    [<ul>
      <% mapM (\c -> <li><% c %></li>) children %>
     </ul>
    ]

errors :: (Monad m, XMLGenerator x, EmbedAsChild x [HSX.XML x]) => 
          Form m i String [XMLGenT x (HSX.XML x)] ()
errors = Common.errors errorList

childErrors :: (Monad m, XMLGenerator x, EmbedAsChild x [HSX.XML x]) => 
               Form m i String [XMLGenT x (HSX.XML x)] ()
childErrors = Common.childErrors errorList

{-
-- Test stuff

testRadio = testForm $ inputRadio True "foo" [("foo", [<span>foo</span>])]

testForm :: (Show a) => Form IO String String [XMLGenT Identity XML] a -> IO ()
testForm form =
    do (view', result) <- runForm form (Environment $ const (return Nothing))
       case result of
         (Ok a) -> 
             do putStrLn $ "the result: " ++ show a
                mapM_ (putStrLn . renderAsHTML . runIdentity . unXMLGenT ) $ unView view' []
         (Error e) -> 
             do print e
                mapM_ (putStrLn . renderAsHTML . runIdentity . unXMLGenT ) $ unView view' e
-}

setAttrs :: (EmbedAsAttr x attr, XMLGenerator x, Monad m, Functor m) =>
            Form m i e [HSX.GenXML x] a 
         -> attr 
         -> Form m i e [HSX.GenXML x] a
setAttrs form attrs = mapView (map (`set` attrs)) form


-- functions that really belong in digestive-functors


inputChoice :: (Monad m, Functor m, FormInput i f, Monoid v, Eq a)
            => (FormId -> String -> Bool -> a -> v)  -- ^ Choice constructor
            -> Maybe a                               -- ^ Default option
            -> [a]                                   -- ^ Choices
            -> Form m i e v (Maybe a)                -- ^ Resulting form
inputChoice toView defaultInput choices = Form $ do
    inputKey <- fromMaybe "" . (getInputString =<<) <$> getFormInput
    id' <- getFormId
    formInput <- isFormInput
    let -- Find the actual input, based on the key, or use the default input
        inp = if formInput 
              then lookup inputKey $ zip (ids id') choices
              else defaultInput
        -- Apply the toView' function to all choices
        view' = mconcat $ zipWith (toView' id' inp) (ids id') choices
    return (View (const view'), Ok inp)
  where
    ids id' = map (((show id' ++ "-") ++) . show) [1 .. length choices]
    toView' id' inp key x = toView id' key (inp == Just x) x

inputChoices :: (Monad m, Functor m, FormInput i f, Monoid v, Eq a)
            => (FormId -> String -> Bool -> a -> v)  -- ^ Choice constructor
            -> [a]                                   -- ^ Default options
            -> [a]                                   -- ^ Choices
            -> Form m i e v [a]                      -- ^ Resulting form
inputChoices toView defaults choices = Form $ do
    inputKey <- fromMaybe "" . (getInputString =<<) <$> getFormInput
    id' <- getFormId
    formInput <- isFormInput
    let -- Find the actual input, based on the key, or use the default input
        inps = if formInput 
               then lookups inputKey $ zip (ids id') choices
               else defaults
        -- Apply the toView' function to all choices
        view' = mconcat $ zipWith (toView' id' inps) (ids id') choices
    return (View (const view'), Ok inps)
  where
    ids id' = map (((show id' ++ "-") ++) . show) [1 .. length choices]
    toView' id' inps key x = toView id' key (x `elem` inps) x

lookups :: (Eq a) => a -> [(a, b)] -> [b]
lookups a = map snd . filter ((== a) . fst)
