#!/home/alex/.ghcup/bin/runhaskell

import Sound.Tidal.Context
import VisCycle
import VisGradient
import VisPart
import Text.Pandoc.JSON
import qualified Data.ByteString.Lazy.Char8 as C
-- import qualified Data.ByteString.Base64 as B64
import Language.Haskell.Interpreter as Hint
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Maybe
import System.Cmd
import System.Directory (doesFileExist)
import System.IO
import Data.Digest.Pure.MD5
import Control.Concurrent (threadDelay)

libs = ["Prelude","Sound.Tidal.Context","Sound.OSC.Datum", "Data.Colour.SRGB", "Data.Colour.Names"]

imgFormat = "pdf"

putStrLnErr = hPutStrLn stderr

interp s as = do Hint.runInterpreter $ do
                   Hint.set [languageExtensions := [OverloadedStrings]]
                   Hint.setImportsQ $ (Prelude.map (\x -> (x, Nothing)) libs) ++ [("Data.Map", Nothing)]
                   pat <- Hint.interpret s (as)
                   return pat

doRender :: Block -> IO [Block]
doRender cb@(CodeBlock (id, classes, namevals) contents) =
  do case lookup "render" namevals of
       Just "gradient"    -> renderSingleGradient (id, classes, namevals) contents
       Just "part"        -> renderSinglePart (id, classes, namevals) contents
       Just "colour"      -> renderSingleColour (id, classes, namevals) contents
       Just "colourx"     -> renderSingleColourSideBySide (id, classes, namevals) contents
       Just "multicolour" -> renderMultiColour (id, classes, namevals) contents
       Just "audio"       -> renderSingleAudio (id, classes, namevals) contents
       Just "multiaudio"  -> renderMultiAudio (id, classes, namevals) contents
       _                  -> return [cb]
doRender x = return [x]

main :: IO ()
main = toJSONFilter doRender

-- renderAudio (id, classes, namevals) contents = return [CodeBlock (id, classes, namevals) contents]

renderSingleColour :: (String, [String], [(String, String)]) -> String -> IO [Block]
renderSingleColour (id, classes, namevals) contents =
  do let codeblock = CodeBlock (id, classes, []) contents
     -- img <- makeColour namevals contents
     -- return $ [codeblock, Para img]
     fn <- makeColour' namevals contents
     return $ [RawBlock (Format "tex") ("\\includegraphics[width=3em]{" ++ fn ++ "}"), codeblock]
  -- 

renderSinglePart :: (String, [String], [(String, String)]) -> String -> IO [Block]
renderSinglePart (id, classes, namevals) contents =
  do let preprocess :: String
         preprocess = fromMaybe "" $ lookup "preprocess" namevals
         prepend :: String
         prepend = fromMaybe "" $ lookup "prepend" namevals
         codeblock = CodeBlock (id, classes, []) (prepend ++ contents)
     img <- makePart namevals preprocess contents
     return $ [codeblock,Para img]

renderSingleGradient :: (String, [String], [(String, String)]) -> String -> IO [Block]
renderSingleGradient (id, classes, namevals) contents =
  do let codeblock = CodeBlock (id, classes, []) contents
     img <- makeGradient namevals contents
     return $ [codeblock,Para img]

renderSingleColourSideBySide :: (String, [String], [(String, String)]) -> String -> IO [Block]
renderSingleColourSideBySide (id, classes, namevals) contents =
  do let codeblock = CodeBlock (id, classes, []) contents
     img <- makeColour namevals contents
     return $ [Table [] [AlignLeft,AlignLeft] [0.1,0.2] [] ([[[Para img],[codeblock]]])]

renderMultiColour :: (String, [String], [(String, String)]) -> String -> IO [Block]
renderMultiColour (id, classes, namevals) contents =
  do let codelines = lines contents
         codes = map (\x -> CodeBlock ("",["haskell"],[("render","colour")]) x) codelines
         n = length codelines
         alignments = replicate n AlignDefault
         ratios = replicate (length codelines) (1/(fromIntegral n))
     imgs <- mapM (makeColour namevals) codelines
     let images = map (\x -> Para x) imgs
     return $ (codes ++ images) -- codes imgs

{-

Table [] [AlignDefault,AlignDefault] [0.4444444444444444,0.4583333333333333]
 [[Plain [Code ("",["haskell"],[("render","colour")]) "\"red blue green\""]]
 ,[Plain [Str "\"red",Space,Str "blue",Space,Str "green",Space,Str "orange",Space,Str "purple\""]]]
 [[[CodeBlock ("",["haskell"],[("render","colour")]) "\"red blue green\""]
  ,[CodeBlock ("",["haskell"],[("render","colour")]) "\"red blue green orange purple\""]]]
-}

switchSlashes '/' = '_'
switchSlashes x = x

makeGradient :: [(String,String)] -> String -> IO [Inline]
makeGradient namevals line =
  do putStrLnErr $ "[code: " ++ line ++ "]"
     let name = map switchSlashes $ show $ md5 $ C.pack line
         filename = "figures/" ++ name ++ "_gradient"
     result <- interp line (Hint.as :: Pattern ColourD)
     img <- runvis renderGradientPDF result filename namevals
     return img

makeColour :: [(String,String)] -> String -> IO [Inline]
makeColour namevals line =
  do putStrLnErr $ "[code: " ++ line ++ "]"
     let name = map switchSlashes $ show $ md5 $ C.pack line
         filename = "figures/" ++ name
     result <- interp line (Hint.as :: Pattern ColourD)
     img <- runvis renderCyclePDF result filename namevals
     return img

makeColour' :: [(String,String)] -> String -> IO String
makeColour' namevals line =
  do putStrLnErr $ "[code: " ++ line ++ "]"
     let name = map switchSlashes $ show $ md5 $ C.pack line
         filename = "figures/" ++ name
     (Right result) <- interp line (Hint.as :: Pattern ColourD)
     fn <- runvis' renderCyclePDF result filename namevals
     return $ fn

makePart :: [(String,String)] -> String -> String -> IO [Inline]
makePart namevals preprocess line =
  do putStrLnErr $ "[code: " ++ line ++ "]"
     let name = map switchSlashes $ show $ md5 $ C.pack ("part-" ++ line)
         filename = "figures/" ++ name
     result <- interp (preprocess ++ line) (Hint.as :: Pattern String)
     img <- runvis renderPartPDF result filename (("width","100%"):namevals)
     return $ img

visWith f "png" name label pat = do f name label pat
                                    system $ "inkscape " ++ name ++ ".pdf -d 300 -e " ++ name ++ ".png"
                                    return ()

visWith f _ name label pat = do f name label pat
                                return ()
  

runvis' :: (String -> String -> Pattern b -> IO ()) -> Pattern b -> [Char] -> [([Char], String)] -> IO String
runvis' f pat name namevals =
  do let w = case lookup "width" namevals of
               Nothing -> "150"
               Just str -> str
     ifMissing (name ++ "." ++ imgFormat) $ visWith f imgFormat name "" pat
     return $ name ++ "." ++ imgFormat

runvis :: Show a => (String -> String -> Pattern b -> IO ()) -> Either a (Pattern b) -> [Char] -> [([Char], String)] -> IO [Inline]
runvis _ (Left err) _ _ = return $ [Str $ show err]
runvis f (Right pat) name namevals =
  do let n = case lookup "cycles" namevals of
               Nothing -> 0
               Just str -> (read str) - 1
         w = case lookup "width" namevals of
               Nothing -> case n of
                            0 -> "150"
                            _ -> "150"
               Just str -> str
         cycles = [0 .. n]
     mapM_ (\cycle -> ifMissing (name ++ "_" ++ show cycle ++ "." ++ imgFormat) $ visWith f imgFormat (name ++ "_" ++ show cycle) (label cycle n) ((toRational cycle) `rotL` pat) 
           ) cycles 
     return $ concatMap (\cycle -> [Image ("",[],[("width", w)]) [] (name ++ "_" ++ (show cycle) ++ "." ++ imgFormat,""),Space]) cycles
       where label cycle n | n > 0 = show (cycle + 1)
                           | otherwise = ""

ifMissing :: String -> IO a -> IO (Maybe a)
ifMissing fn f = do exists <- doesFileExist fn
                    case exists of
                      False -> do putStrLnErr $ "[" ++ fn ++ " does not exist]"
                                  x <- f
                                  return (Just x)
                      True -> do putStrLnErr "[already exists]"
                                 return Nothing


renderSingleAudio :: (String, [String], [(String, String)]) -> String -> IO [Block]
renderSingleAudio (id, classes, namevals) contents =
  do let prefix :: String
         prefix = fromMaybe "" $ lookup "prefix" namevals
         codeblock = CodeBlock (id, classes, []) (prefix ++ contents)
     (filename, msg) <- makeAudio namevals contents     
     -- return $ [RawBlock (Format "html") ("<div class=\"audioexample\">"), codeblock, RawBlock (Format "html") ("<audio controls=\"1\"><source src=\"" ++ filename ++  "\" type=\"audio/mp3\" /></audio></div>")]
     return $ [RawBlock (Format "tex") ("\\marginpar[]{\\vspace{0.7em}\\href{" ++ filename ++ "}{\\includegraphics[width=1em]{playcirc.pdf}}}"),codeblock]

renderMultiAudio :: (String, [String], [(String, String)]) -> String -> IO [Block]
renderMultiAudio (id, classes, namevals) contents =
  do let codelines = lines contents
         codes = map (\x -> [CodeBlock ("",["haskell"],[("render","colour")]) x]) codelines
         n = length codelines
         alignments = replicate n AlignDefault
         ratios = replicate (length codelines) (1/(fromIntegral n))
     audios <- mapM (makeAudio namevals) codelines
     let audios' = map showAudio audios
     return $ [Table [] alignments ratios [] (codes:[audios'])]
       where showAudio (filename, msg) = [Para ([Link ("",[],[]) [Image ("",[],[("width", "80")]) [] ("playcirc." ++ imgFormat,"fig:")] (filename,"")])]

makeAudio :: [(String,String)] -> String -> IO (String, [Inline])
makeAudio namevals code =
  do let defaultName = (map switchSlashes $ show $ md5 $ C.pack code) ++ ".mp3"
         name = fromMaybe defaultName $ lookup "fn" namevals
         filename = "sounds/" ++ name
         cps = case lookup "cps" namevals of
                 Nothing -> 1
                 Just str -> (read str)
     putStrLnErr $ "rendering to " ++ filename ++ ":"
     putStrLnErr $ code
     err <- ifMissing filename $ do result <- interp code (Hint.as :: ControlPattern)
                                    runaud result filename namevals cps
     let msg = fromMaybe [] err
     return (filename, msg)


runaud :: Show a => Either a (ControlPattern) -> [Char] -> [([Char], String)] -> Double -> IO ([Inline])
runaud (Left err) _ _ _ = return [Str $ show err]
runaud (Right pat) filename namevals cpsv =
  do let seconds = 5
     tidal <- startTidal (superdirtTarget {oLatency = 0.15, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20})
     let once = streamOnce tidal
         d = streamReplace tidal 1
     once $ cps $ pure cpsv
     putStrLnErr $ "ecasound -D -t:" ++ show (seconds+2) ++ " -i jack,SuperCollider -o " ++ filename ++ " &"
     system $ "ecasound -D -t:" ++ show (seconds+2) ++ " -i jack,SuperCollider -o " ++ filename ++ " >/dev/null &"
     d pat
     threadDelay (seconds * 1000000)
     d silence
     threadDelay (2 * 1000000)
     return []

{-
import Sound.Tidal.Context
import Language.Haskell.Interpreter as Hint
import System.Exit
import System.Environment (getArgs)
import Control.Concurrent
import Text.HTML.TagSoup.Entity (lookupEntity)
import System.Posix.Resource

unescapeEntities :: String -> String
unescapeEntities [] = []
unescapeEntities ('&':xs) = 
  let (b, a) = break (== ';') xs in
  case (lookupEntity b, a) of
    (Just c, ';':as) ->  c  ++ unescapeEntities as    
    _                -> '&' : unescapeEntities xs
unescapeEntities (x:xs) = x : unescapeEntities xs

data Response = OK {parsed :: ParamPattern}
              | Error {errorMessage :: String}

seconds = 20

cpsVal = 0.75

main = do a <- getArgs
          let fn = head a
          setResourceLimit ResourceCPUTime (ResourceLimits (ResourceLimit 4) (ResourceLimit 8))
          code <- getContents
          r <- runTidal $ unescapeEntities code
          respond fn r
   where respond fn (OK p)
           = do (cpsSet, getNow) <- cpsUtils
                cpsSet cpsVal
                (d, _) <- superDirtSetters getNow
                system $ "ecasound -t:" ++ show (seconds+2) ++ " -i jack,SuperCollider -o " ++ fn ++ " &"
                d p
                threadDelay (seconds * 1000000)
                exitSuccess
         respond _ (Error s) = do putStrLn ("error: " ++ s)
                                  exitFailure
                   
libs = ["Prelude","Sound.Tidal.Context","Sound.OSC.Type","Sound.OSC.Datum"]

runTidal  :: String -> IO (Response)
runTidal code =
  do result <- do Hint.runInterpreter $ do
                  Hint.set [languageExtensions := [OverloadedStrings]]
                  --Hint.setImports libs
                  Hint.setImportsQ $ (Prelude.map (\x -> (x, Nothing)) libs) ++ [("Data.Map", Nothing)]
                  p <- Hint.interpret code (Hint.as :: ParamPattern)
                  return p
     let response = case result of
          Left err -> Error (parseError err)
          Right p -> OK p -- can happen
         parseError (UnknownError s) = "Unknown error: " ++ s
         parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
         parseError (NotAllowed s) = "NotAllowed error: " ++ s
         parseError (GhcException s) = "GHC Exception: " ++ s
         parseError _ = "Strange error"
     return response
-}
