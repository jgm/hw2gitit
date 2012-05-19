{-# LANGUAGE ScopedTypeVariables #-}
-- hw2gitit.hs
-- Creates a git repository 'wiki' containing markdown versions of all
-- the pages in haskellwiki.
-- Individual HTML pages and images are cached in cache/.
-- Cache should be deleted for a fresh download.

import Codec.Digest.SHA
import Data.Ord (comparing)
import Text.Printf (printf)
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
import Data.IORef
import System.IO.Unsafe

data Version = Version { vId :: Integer
                       , vUser :: String
                       , vDate :: String
                       , vDescription :: String } deriving (Show)

cache :: FilePath
cache = "cache"

wiki :: FilePath
wiki = "wiki"

-- a local list of resources that have been included,
-- to speed things up
resources :: IORef [String]
resources = unsafePerformIO $ newIORef []

main :: IO ()
main = do
  -- Create filestore in 'wiki' directory, unless it exists
  let fs = gitFileStore wiki
  exists <- doesDirectoryExist wiki
  unless exists $ initialize fs
  pages <- (nub . concat) `fmap` mapM getIndex indices
  ind <- index fs
  let pagepairs = sort
        [(fromUrl p,p) | p <- pages, (fromUrl p ++ ".page") `notElem` ind]
  -- Add all pages to the repository, except those already there
  mapM_ (doPage fs) pagepairs

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

tr :: Char -> Char -> String -> String
tr c1 c2 = map (\c -> if c == c1 then c2 else c)

openURL' :: String -> IO String
openURL' url = do
  let cachename = cache ++ "/" ++ (showBSasHex $ hash SHA256 $ BC.pack url)
  createDirectoryIfMissing True cache
  cached <- doesFileExist cachename
  src <- if cached then
            readFile cachename
          else
            openURL url
  unless cached $ writeFile cachename src
  return src

indices :: [String]
indices =  [ "http://www.haskell.org/haskellwiki/Special:Allpages/%24"
           , "http://www.haskell.org/haskellwiki/Special:Allpages/G"
           , "http://www.haskell.org/haskellwiki/Special:Allpages/L"
           , "http://www.haskell.org/haskellwiki/Special:Allpages/U"
           ]

-- get list of pages listed on index URL
getIndex :: String -> IO [String]
getIndex url = do
  putStrLn $ "Fetching index of pages: " ++ url
  src <- openURL' url
  let tags = parseTags $ decodeString src
  return $ getPageNames tags

stripPref :: String -> String -> String
stripPref pref s = maybe s id $ stripPrefix pref s

strip :: String -> String
strip = reverse . dropWhile (==' ') . reverse . dropWhile (==' ')

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

-- convert URL to page name
fromUrl :: String -> String
fromUrl = fromUrlString . decodeString . unEscapeString . takeWhile (/='?')

-- filestore can't deal with ? and * in filenames
fromUrlString :: String -> String
fromUrlString =
  unwords . words . strip . filter (\c -> c /='?' && c /='*') . ulToSpace

removeDoubleDots :: String -> String
removeDoubleDots ('.':'.':xs) = '.':removeDoubleDots xs
removeDoubleDots ['.'] = []
removeDoubleDots (x:xs) = x:removeDoubleDots xs
removeDoubleDots [] = []

toVersion :: [Tag String] -> Version
toVersion ts =
  Version{ vId = read id', vUser = auth, vDate = date, vDescription = desc }
    where id' = case rs of
                   (t:_) -> case fromAttrib "value" t of
                                  "" -> case as of
                                          ((t:_):_) -> reverse $ takeWhile isDigit
                                                       $ reverse $ fromAttrib "href" t
                                  x  -> x
                   _     -> error "toVersion, empty rs list"
          rs = case dropWhile (~/= TagOpen "input" [("type","radio")]) ts of
                    [] -> ts  -- to handle pages with just one commit
                    xs -> xs
          auth = case as of
                      (_:(_:TagText x:_):_) -> x
                      _ -> "hw2gitit"
          date = case as of
                      ((_:TagText x:_):_) -> x
                      _ -> ""
          desc = case dropWhile (~/= TagOpen "span" [("class","comment")]) ts of
                        (_:TagText x:_) -> reverse $ drop 1 $ reverse $ drop 1 x
                        _ -> ""
          as = partitions (~== TagOpen "a" []) rs

-- get the page from the web or cache, convert it to markdown and
-- add it to the repository.
doPage :: FileStore -> (String,String) -> IO ()
doPage fs (page',page) = do
  src <- openURL' $ "http://www.haskell.org/haskellwiki/index.php?title=" ++ page ++ "&limit=500&action=history"
  let tags = takeWhile (~/= TagClose "ul")
           $ dropWhile (~/= TagOpen "ul" [("id","pagehistory")])
           $ parseTags $ decodeString src
  let lis = partitions (~== TagOpen "li" []) tags
  let versions = sortBy (comparing vId) $ map toVersion lis
  -- let versions = [ Version {vId=1308, vUser="Ashley Y",vDate="23:54, 4 January 2006",vDescription = "Initial commit"}
  mapM_ (doPageVersion fs (page',page)) versions

doPageVersion :: FileStore -> (String,String) -> Version -> IO ()
doPageVersion fs (page',page) version = do
  let fname = page' ++ ".page"

  -- first, check mediawiki source to make sure it's not a redirect page
  mwsrc <- openURL' $ "http://www.haskell.org/haskellwiki/index.php?title=" ++ page ++ "&action=edit"
  let redir = case (drop 1 $ dropWhile (~/= TagOpen "textarea" [("id","wpTextbox1")])
                           $ parseTags $ decodeString mwsrc) of
                  (TagText ('#':'r':'e':'d':'i':'r':'e':'c':'t':' ':'[':'[':xs):_) ->
                         takeWhile (/=']') xs
                  (TagText ('#':'R':'E':'D':'I':'R':'E':'C':'T':' ':'[':'[':xs):_) ->
                         takeWhile (/=']') xs
                  (TagText ('#':'R':'e':'d':'i':'r':'e':'c':'t':' ':'[':'[':xs):_) ->
                         takeWhile (/=']') xs
                  _ -> ""

  src <- if null redir
            then openURL' $ "http://www.haskell.org/haskellwiki/" ++ page ++
                    if vId version > 0
                       then "?oldid=" ++ printf "%06d" (vId version)
                       else ""
            else return ""

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
  let md = if null redir
              then writeMarkdown defaultWriterOptions doc''
              else "See [" ++ fromUrlString redir ++ "]()."
  -- add header with categories
  putStrLn $ "Adding page " ++ page' ++ " r" ++ show (vId version)
  let auth = vUser version
  let descr = vDescription version ++ " (#" ++ show (vId version) ++ ", " ++
                vDate version ++ ")"
  addToWiki fs fname auth descr $
     (if null categories
         then ""
         else "---\ncategories: " ++ intercalate "," categories ++ "\n...\n\n")
     ++ md ++ "\n"

-- remove <table id="toc"> (TOC)
removeToc :: [Tag String] -> [Tag String]
removeToc (t@(TagOpen "table" _) : ts) | fromAttrib "id" t == "toc" =
  removeToc $ dropWhile (~/= TagClose "table") ts
removeToc (t:ts) = t : removeToc ts
removeToc [] = []

-- add page to wiki
addToWiki :: Contents a => FileStore -> String -> String -> String -> a -> IO ()
addToWiki fs fname auth desc content = catch
  (save fs fname (Author auth "") desc content) $ \(e :: FileStoreError) ->
       putStrLn ("! Could not add " ++ fname ++ ": " ++ show e)

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
  | "http://www.haskell.org/haskellwiki" `isPrefixOf` src ||
    "http://www.haskell.org/wikiupload" `isPrefixOf` src ||
    "http://haskell.org/haskellwiki" `isPrefixOf` src ||
    "http://haskell.org/wikiupload" `isPrefixOf` src =
      let drop_prefix = stripPref "http://www.haskell.org" . stripPref "http://haskell.org"
      in  handleLinksImages fs (Link lab (drop_prefix src, drop_prefix tit))
  | "/wikiupload/" `isPrefixOf` src = do  -- uploads like ps and pdf files
      let fname = "Upload/" ++ fromUrl (takeFileName src)
      addResource fs fname src
      return $ Link lab ('/':fname,"")
  | "/haskellwiki/Image:" `isPrefixOf` src =
      return $ Link lab ("/Image/" ++ fromUrl (stripPref "/haskellwiki/Image:" src),"")
  | "/haskellwiki/" `isPrefixOf` src = do
    let suff = fromUrl $ stripPref "/haskellwiki/" src
    if suff == fromUrlString tit then
       if stringify lab == tit then
          return $ Link lab ("","")
       else
          return $ Link lab ('/':tit,tit)
    else
       return $ Link lab ('/':suff,"")
  | otherwise = return $ Link lab (src,tit)
handleLinksImages fs (Image alt (src,tit))
  | "http://www.haskell.org/haskellwiki" `isPrefixOf` src ||
    "http://www.haskell.org/wikiupload" `isPrefixOf` src ||
    "http://haskell.org/haskellwiki" `isPrefixOf` src ||
    "http://haskell.org/wikiupload" `isPrefixOf` src =
      let drop_prefix = stripPref "http://www.haskell.org" . stripPref "http://haskell.org"
      in  handleLinksImages fs (Image alt (drop_prefix src, drop_prefix tit))
    -- math images have tex source in alt attribute
  | "/wikiupload/math" `isPrefixOf` src =
      return $ Math InlineMath $ strip $ stringify alt
  | "/wikiupload/" `isPrefixOf` src = do
      let fname = "Image/" ++ fromUrl (takeFileName src)
      addResource fs fname src
      return $ Image alt ('/':fname,"")
  | otherwise = return $ Image alt (src,tit)
handleLinksImages _ x = return x

-- get image from web or cache and add to repository
addResource :: FileStore -> String -> String -> IO ()
addResource fs fname url = do
  res <- readIORef resources
  unless (fname `elem` res) $ do
    catch (latest fs fname >> putStrLn ("Skipping " ++ fname)) $
      \(e :: FileStoreError) -> do
         raw <- BC.pack `fmap` openURL' ("http://www.haskell.org" ++ url)
         putStrLn $ "Adding resource: " ++ fname
         modifyIORef resources (fname:)
         addToWiki fs fname "hw2gitit" "Import from haskellwiki" raw

-- remove numbering from headers.
handleHeaders (Header lev xs) = Header lev xs'
  where xs' = dropWhile (==Space) $ dropWhile (== Str ".")
            $ dropWhile (==Space) $ dropWhile isNum xs
        isNum (Str ys) = all (\c -> isDigit c || c == '.') ys
        isNum z = False
handleHeaders (Para (LineBreak:xs)) = Para xs
handleHeaders x = x

-- Change attribute on code blocks from source-X to X.
fixCodeBlocks (CodeBlock (id',classes,attrs) code) =
  CodeBlock (id', map (stripPref "source-") classes, attrs) code
fixCodeBlocks x = x
