--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid 
import           Hakyll
import qualified Data.Set as S
import Text.Pandoc.Options 
import qualified Text.HTML.TagSoup               as TS
import qualified Data.Map as M
import Text.Regex.TDFA ((=~~),(=~))
import Control.Applicative
import Data.String



--------------------------------------------------------------------------------
main :: IO ()
main = map fromString . lines <$> readFile "DRAFTS" >>= \drafts -> hakyll $ do 

    let postsPattern = "posts/*" .&&. complement (fromList drafts)
    
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
            posts <- recentFirst =<< loadAll postsPattern
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
            posts <- recentFirst =<< loadAll postsPattern
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler



writerOptions = defaultHakyllWriterOptions
                { writerExtensions = S.delete Ext_literate_haskell 
                  (writerExtensions defaultHakyllWriterOptions)
                }

pandocCompiler' = pandocCompilerWith defaultHakyllReaderOptions writerOptions

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" 
                  <> boolField "isLhs" (endsWith "lhs" . toFilePath . itemIdentifier)
                  <> pathField "fpath"
                  <> teaserField "teaser" "content"
                  <> defaultContext

endsWith s = (s ==) . reverse . take (length s) . reverse 



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
