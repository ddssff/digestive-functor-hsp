{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Text.Digestive.HSP.Html4 where

import Control.Applicative             ((<$>))
import Control.Monad                   (mplus)
import Data.Maybe                      (fromMaybe, mapMaybe)
import Data.Monoid                     (Monoid(mempty), mconcat)
import Data.Text                       (Text)
import qualified Data.Text             as Text
import HSP                             (XMLGenerator, XMLGenT, EmbedAsChild(..), EmbedAsAttr(..), Attr(..), genElement, genEElement, set)
import qualified HSX.XMLGenerator      as HSX
import Text.Digestive                  (FormId, Form(..), Result(..), View(..), getFormId, getFormInput, isFormInput, mapView)
import Text.Digestive.Common           as Common
import Text.Digestive.Forms            as Forms

showFormId :: FormId -> String
showFormId id' = show id'

-- text input box that returns a 'String'
inputString :: (Monad m, Functor m, XMLGenerator x, FormInput i f)
          => Maybe String -- ^ initial value
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

selected True = [("selected" := "selected")]
selected False = []

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
        [ <input type="checkbox" name=(showFormId group') id=id' value=id' (checked sel) />
        , <label for=id'><% fromMaybe mempty $ lookup val choices %></label>
        ] ++ if br then [<br />] else []

-- | radio buttons
--
-- NOTE:
--
-- According to the spec
-- <http://www.w3.org/TR/html401/interact/forms.html#h-17.2.1> radio
-- buttons should be able to have an undefined state. But since
-- user-agents are inconsistent about handling undefined, it is
-- recommended that a default option always be provided.
inputRadio :: (Monad m, Functor m, Eq a, XMLGenerator x, EmbedAsChild x c, Monoid c, FormInput i f)
           => Bool                                 -- ^ Use @<br>@ tags
           -> a                                    -- ^ Default option
           -> [(a, c)]                             -- ^ Choices with their names
           -> Form m i e [XMLGenT x (HSX.XML x)] a -- ^ Resulting form
inputRadio br def choices =
    inputChoice toView def (map fst choices)
  where
    toView group' id' sel val =
        [ <input type="radio" name=(showFormId group') id=id' value=id' (checked sel) />
        , <label for=id'><% fromMaybe mempty $ lookup val choices %></label>
        ] ++ if br then [<br />] else []

inputSelect :: (Monad m, Functor m, Eq a, XMLGenerator x, EmbedAsChild x c, Monoid c, FormInput i f)
           => a                                    -- ^ Default option
           -> [(a, c)]                             -- ^ Choices with their names
           -> Form m i e [XMLGenT x (HSX.XML x)] a -- ^ Resulting form
inputSelect def choices = 
    Form $ do id' <- getFormId
              unForm $ mapView (\cs -> [<select name=(showFormId id') id=(showFormId id')><% cs %></select>])
                         (inputChoice toView def (map fst choices))
  where
    toView group' id' sel val =
        [ <option id=id' value=id' (selected sel)><% fromMaybe mempty $ lookup val choices %></option>]

inputMultiSelect :: (Monad m, Functor m, Eq a, XMLGenerator x, EmbedAsChild x c, Monoid c, FormInput i f)
           => [a]                                  -- ^ Default options
           -> [(a, c)]                             -- ^ Choices with their names
           -> Form m i e [XMLGenT x (HSX.XML x)] [a] -- ^ Resulting form
inputMultiSelect defs choices = 
    Form $ do id' <- getFormId
              unForm $ mapView (\cs -> [<select multiple="" name=(showFormId id') id=(showFormId id')><% cs %></select>])
                         (inputChoices toView defs (map fst choices))
  where
    toView group' id' sel val =
        [ <option id=id' value=id' (selected sel)><% fromMaybe mempty $ lookup val choices %></option>]



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

inputChoices :: (Monad m, Functor m, FormInput i f, Monoid v, Eq a)
            => (FormId -> String -> Bool -> a -> v)  -- ^ Choice constructor
            -> [a]                                   -- ^ Default options
            -> [a]                                   -- ^ Choices
            -> Form m i e v [a]                      -- ^ Resulting form
inputChoices toView defaults choices = Form $ do
    inputKeys <- maybe [] getInputStrings <$> getFormInput
    id' <- getFormId
    formInput <- isFormInput
    let -- Find the actual input, based on the key, or use the default input
        inps = if formInput 
               then mapMaybe (\inputKey -> lookup inputKey $ zip (ids id') choices) inputKeys
               else defaults
        -- Apply the toView' function to all choices
        view' = mconcat $ zipWith (toView' id' inps) (ids id') choices
    return (View (const view'), Ok inps)
  where
    ids id' = map (((show id' ++ "-") ++) . show) [1 .. length choices]
    toView' id' inps key x = toView id' key (x `elem` inps) x

lookups :: (Eq a) => a -> [(a, b)] -> [b]
lookups a = map snd . filter ((== a) . fst)
