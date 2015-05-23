--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid 
import           Hakyll
import qualified Data.Set as S
import Text.Pandoc.Options 
import qualified Text.HTML.TagSoup               as TS
import qualified Data.Map as M
import Text.Regex.TDFA ((=~~),(=~))


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/figures/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
      route idRoute
      compile copyFileCompiler


    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
{-
    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
-}

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler'
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= applyPathMangledClass

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

pandocOptions = defaultHakyllWriterOptions
--  { writerHTMLMathMethod = MathJax ""
--  }

writerOptions = defaultHakyllWriterOptions
                { writerExtensions = S.delete Ext_literate_haskell 
                  (writerExtensions defaultHakyllWriterOptions)
                }

readerOptions = defaultHakyllReaderOptions { 
                  readerExtensions = S.insert Ext_literate_haskell 
                  (readerExtensions defaultHakyllReaderOptions)
                }

pandocCompiler' = pandocCompilerWith defaultHakyllReaderOptions writerOptions

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" 
                  <> teaserField "teaser" "content"
                  <> defaultContext


applyPathMangledClass :: Item String -> Compiler (Item String)
applyPathMangledClass item = return $ fmap (withTags procImg) item
    where 
      procImg (TS.TagOpen s a) | s == "img" = TS.TagOpen s .
                                              procAttrs . M.fromList $ a
      procImg t = t
      procAttrs ma = M.toList $ maybe ma (procSrc ma) $ M.lookup "src" ma
      procSrc ma u = procMangled ma (u =~ ("__([A-Za-z0-9]+)" :: String))
      procMangled ma [[_,cls]] = M.insertWith (\a b -> a ++ " " ++ b) "class" cls ma
      procMangled ma _ = ma
