{-# LANGUAGE ScopedTypeVariables #-}
-- hw2gitit.hs
-- Creates a git repository 'wiki' containing markdown versions of all
-- the pages in haskellwiki.
-- The index of pages is cached in pages.cache.
-- Individual HTML pages and images are cached in cache/.
-- These caches should be deleted for a fresh download.

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Prelude hiding (catch)
import Control.Exception (catch)
import Network.URI
import Network.HTTP hiding (Header)
import Control.Monad
import Data.Char (isDigit)
import Data.List
import Text.HTML.TagSoup
import Data.FileStore
import Codec.Binary.UTF8.String
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import System.Environment
import System.Directory
import System.FilePath

main :: IO ()
main = do
  -- Create filestore in 'wiki' directory, unless it exists
  let fs = gitFileStore "wiki"
  exists <- doesDirectoryExist "wiki"
  unless exists $ initialize fs
  -- Fetch index of all pages from cache or from web (caching results)
  cached <- doesFileExist "pages.cache"
  pages <- if cached then
             read `fmap` readFile "pages.cache"
           else
             (nub . concat) `fmap` mapM getIndex indices
  unless cached $ writeFile "pages.cache" (show pages)
  -- Add all pages to the repository
  mapM_ (doPage fs) pages

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

indices :: [String]
indices = ["http://www.haskell.org/haskellwiki/Special:Allpages/%24",
           "http://www.haskell.org/haskellwiki/Special:Allpages/G",
           "http://www.haskell.org/haskellwiki/Special:Allpages/L",
           "http://www.haskell.org/haskellwiki/Special:Allpages/U"]

-- get list of pages listed on index URL
getIndex :: String -> IO [String]
getIndex url = do
  putStrLn $ "Fetching index of pages: " ++ url
  src <- openURL url
  let tags = parseTags src
  return $ getPageNames tags

stripPref :: String -> String -> String
stripPref pref s = maybe s id $ stripPrefix pref s

-- parse index page and return list of URLs for pages
getPageNames :: [Tag String] -> [String]
getPageNames [] = []
getPageNames (t@(TagOpen "a" _) : ts) =
  case fromAttrib "href" t of
       x | "/haskellwiki/index.php?title=" `isPrefixOf` x -> getPageNames ts
         | "/haskellwiki/" `isPrefixOf` x ->
                stripPref "/haskellwiki/" x : getPageNames ts
         | otherwise -> getPageNames ts
getPageNames (t:ts) = getPageNames ts

-- get the page from the web or cache, convert it to markdown and
-- add it to the repository.
doPage :: FileStore -> String -> IO ()
doPage fs page = do
  let page' = ulToSpace $ decodeString $ unEscapeString page
  let dropDots = filter (/='.')
  let fname = dropDots page' ++ ".page"
  -- if page already in the wiki, skip it
  catch (latest fs fname >> putStrLn ("Skipping " ++ fname)) $
       \(e :: FileStoreError) -> do
    let cachename = "cache/" ++ page' ++ ".html"
    let cachedir = takeDirectory cachename
    createDirectoryIfMissing True cachedir
    cached <- doesFileExist cachename
    src <- if cached then
              readFile cachename
            else
              openURL $ "http://www.haskell.org/haskellwiki/" ++ page
    unless cached $ writeFile cachename src

    -- convert the page
    let nonSpan (TagOpen "span" _) = False
        nonSpan (TagClose "span")  = False
        nonSpan _                  = True
    -- content marked by "start content"/"end content" comments
    let tags = handleInlineCode    -- change inline code divs to code tags
               $ removeToc         -- remove TOC
               $ filter nonSpan    -- remove span tags
               $ takeWhile (~/= TagComment " end content ")
               $ dropWhile (~/= TagComment " start content ")
               $ parseTags
               $ decodeString src  -- decode UTF-8
    let (body,foot) = break (~== TagOpen "div" [("class","printfooter")]) tags
    let categories = getCategories  -- extract categories
                   $ dropWhile (~/= TagOpen "p" [("class","catlinks")]) foot
    let html = renderTags body
    let doc' = bottomUp removeRawInlines   -- remove raw HTML
               $ bottomUp (handleHeaders . fixCodeBlocks . removeRawBlocks)
               $ readHtml defaultParserState html
    -- handle wikilinks and images
    doc'' <- bottomUpM (handleLinksImages fs) doc'
    putStrLn $ "Adding page: " ++ page'
    let md = writeMarkdown defaultWriterOptions doc''
    -- add header with categories and title
    addToWiki fs fname $
       "---\ntitle: " ++ page' ++
       (if null categories
           then ""
           else "\ncategories: " ++ intercalate "," categories) ++
       "\n...\n\n" ++ md ++ "\n"

-- remove <table id="toc"> (TOC)
removeToc :: [Tag String] -> [Tag String]
removeToc (t@(TagOpen "table" _) : ts) | fromAttrib "id" t == "toc" =
  removeToc $ dropWhile (~/= TagClose "table") ts
removeToc (t:ts) = t : removeToc ts
removeToc [] = []

-- add page to wiki
addToWiki :: Contents a => FileStore -> String -> a -> IO ()
addToWiki fs fname content = catch
  (save fs fname auth desc content) $ \(e :: FileStoreError) ->
       putStrLn ("! Could not add " ++ fname ++ ": " ++ show e)
    where auth = Author "John MacFarlane" "jgm@berkeley.edu"
          desc = "Added " ++ fname

-- extract categories from <a ... title="Category:"> tags
getCategories :: [Tag String] -> [String]
getCategories (t@(TagOpen "a" _) : xs) =
  case fromAttrib "title" t of
        x | "Category:" `isPrefixOf` x ->
             stripPref "Category:" x : getCategories xs
        _ -> getCategories xs
getCategories (x:xs) = getCategories xs
getCategories [] = []

-- Convert underline to space
ulToSpace :: String -> String
ulToSpace = map go
  where go '_' = ' '
        go c   = c

removeRawBlocks :: Block -> Block
removeRawBlocks (RawBlock _ _) = Null
removeRawBlocks (Para [LineBreak]) = Null
removeRawBlocks x = x

removeRawInlines :: Inline -> Inline
removeRawInlines (RawInline _ _) = Str ""
removeRawInlines x = x

-- Inline code is represented by an unbelievably complex nested div,
-- even in block contexts!  We just extract the code and put it in
-- a code tag with an appropriate attribute, so pandoc can handle it.
handleInlineCode :: [Tag String] -> [Tag String]
handleInlineCode (TagOpen "div" [("class","inline-code")] :
     TagOpen "div" _ : TagOpen "div" attrs : ys) =
  TagOpen "code" [("class",cls)] : TagText code : TagClose "code" :
     handleInlineCode xs
  where cls = case lookup "class" attrs of
                  Just z -> stripPref "source-" z
                  Nothing -> ""
        (codes,ws) = span isTagText ys
        code = concatMap fromTagText codes
        xs = case ws of
                  (TagClose "div" : TagClose "div" : TagClose "div" :zs) -> zs
                  _ -> ws
handleInlineCode (x:xs) = x:handleInlineCode xs
handleInlineCode [] = []

-- Handle links and images, converting URLs and fetching images when needed,
-- adding them to the repository.
handleLinksImages :: FileStore -> Inline -> IO Inline
handleLinksImages fs (Link lab (src,tit))
  | "http://www.haskell.org" `isPrefixOf` src =
      let drop_prefix = stripPref "http://www.haskell.org"
      in  handleLinksImages fs (Link lab (drop_prefix src, drop_prefix tit))
  | "/haskellwiki/Image:" `isPrefixOf` src =
      return $ Link lab ("/Image/" ++ stripPref "/haskellwiki/Image:" src,tit)
  | "/haskellwiki/" `isPrefixOf` src = do
    let suff = stripPref "/haskellwiki/" src
    if suff == ulToSpace tit then
       if stringify lab == tit then
          return $ Link lab ("","")
       else
          return $ Link lab ('/':tit,tit)
    else
       return $ Link lab ('/':suff,tit)
  | otherwise = return $ Link lab (src,tit)
handleLinksImages fs (Image alt (src,tit))
    -- math images have tex source in alt attribute
  | "/wikiupload/math" `isPrefixOf` src =
      return $ Math InlineMath $ stringify alt
  | "/wikiupload/" `isPrefixOf` src = do
      let fname = takeFileName src
      addImage fs fname src
      return $ Image alt ("/Image/" ++ fname,tit)
  | otherwise = return $ Image alt (src,tit)
handleLinksImages _ x = return x

-- get image from web or cache and add to repository
addImage :: FileStore -> String -> String -> IO ()
addImage fs fname url = do
  catch (latest fs fname >> putStrLn ("Skipping " ++ fname)) $
    \(e :: FileStoreError) -> do
       let cachename = "cache/" ++ fname
       let cachedir = takeDirectory cachename
       createDirectoryIfMissing True cachedir
       cached <- doesFileExist cachename
       raw <- if cached then
                 B.readFile cachename
              else
                 BC.pack `fmap` openURL ("http://www.haskell.org" ++ url)
       unless cached $ B.writeFile cachename raw
       putStrLn $ "Adding image: " ++ fname
       addToWiki fs fname raw

-- promote headers and remove the numbering.  on haskellwiki.org,
-- the headers start at h2, so we promote everything a level.
handleHeaders (Header lev xs) = Header (lev - 1) xs'
  where xs' = dropWhile (==Space) $ dropWhile isNum xs
        isNum (Str ys) = all (\c -> isDigit c || c == '.') ys
        isNum z = False
handleHeaders (Para (LineBreak:xs)) = Para xs
handleHeaders x = x

-- Change attribute on code blocks from source-X to X.
fixCodeBlocks (CodeBlock (id',classes,attrs) code) =
  CodeBlock (id', map (stripPref "source-") classes, attrs) code
fixCodeBlocks x = x
