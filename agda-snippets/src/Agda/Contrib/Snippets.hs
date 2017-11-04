{-# LANGUAGE ViewPatterns #-}
module Agda.Contrib.Snippets
    ( renderAgdaSnippets
    , CSSClass
    , CommandLineOptions (..)
    , defaultOptions
    ) where

import           Control.Monad.Except
import           Control.Monad.State (get)
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import           Network.URI
import           System.Exit (exitFailure)
import           Text.XHtml.Strict

import           Agda.Interaction.Highlighting.Precise
import qualified Agda.Interaction.Imports as Imp
import           Agda.Interaction.Options
import           Agda.Syntax.Abstract.Name (toTopLevelModuleName)
import           Agda.Syntax.Common
import           Agda.Syntax.Concrete.Name (TopLevelModuleName, moduleNameParts)
import           Agda.TypeChecking.Errors
import           Agda.TypeChecking.Monad (TCM)
import qualified Agda.TypeChecking.Monad as TCM
import           Agda.Utils.FileName
import qualified Agda.Utils.IO.UTF8 as UTF8
import           Agda.Utils.Lens


checkFile :: AbsolutePath -> TCM TopLevelModuleName
checkFile file =
    do TCM.resetState
       toTopLevelModuleName . TCM.iModuleName . fst <$> Imp.typeCheckMain file Imp.TypeCheck

getModule :: TopLevelModuleName -> TCM (HighlightingInfo, String)
getModule m =
    do Just mi <- TCM.getVisitedModule m
       Just f <- Map.lookup m . (^. TCM.stModuleToSource) <$> get
       s <- liftIO . UTF8.readTextFile . filePath $ f
       return (TCM.iHighlighting (TCM.miInterface mi), s)

pairPositions :: HighlightingInfo -> String -> [(Int, String, Aspects)]
pairPositions info contents =
    map (\cs@((mi, (pos, _)) : _) -> (pos, map (snd . snd) cs, fromMaybe mempty mi)) .
    groupBy ((==) `on` fst) .
    map (\(pos, c) -> (IMap.lookup pos infoMap, (pos, c))) .
    zip [1..] $
    contents
  where
    infoMap = toMap (decompress info)

-- TODO make these more accurate
beginCode :: String -> Bool
beginCode s = "\\begin{code}" `isInfixOf` s

endCode :: String -> Bool
endCode s = "\\end{code}" `isInfixOf` s

infixEnd :: Eq a => [a] -> [a] -> [a]
infixEnd i s = head [drop (length i) s' | s' <- tails s, i `isPrefixOf` s']

stripBegin :: (Int, String, Aspects) -> (Int, String, Aspects)
stripBegin (i, s, mi) = (i, cut (dropWhile (== ' ') (infixEnd "\\begin{code}" s)), mi)
  where cut ('\n' : s') = s'
        cut s'          = s'

groupLiterate :: [(Int, String, Aspects)]
              -> [Either String [(Int, String, Aspects)]]
groupLiterate contents =
    let (com, rest) = span (notCode beginCode) contents
    in Left ("\n\n" ++ concat [s | (_, s, _) <- com] ++ "\n\n") : go rest
  where
    go []         = []
    go (be : mis) =
        let be'@(_, s, _) = stripBegin be
            (code, rest)  = span (notCode endCode) mis
        in if "\\end{code}" `isInfixOf` s || "%" `isInfixOf` s
           then -- We simply ignore empty code blocks
                groupLiterate mis
           else Right (be' : code) :
                -- If there's nothing between \end{code} and \begin{code}, we
                -- start consuming code again.
                case rest of
                    []                                  -> error "malformed file"
                    ((_, beginCode -> True, _) : code') -> go code'
                    (_                         : com  ) -> groupLiterate com

    notCode f (_, s, _) = not (f s)

annotate :: URI -> TopLevelModuleName -> Int -> Aspects -> Html -> Html
annotate libs m pos mi = anchor ! attributes
  where
    attributes = [name (show pos)] ++
                 fromMaybe [] (definitionSite mi >>= link) ++
                 (case classes of [] -> []; cs -> [theclass (unwords cs)])

    classes = maybe [] noteClasses (note mi) ++
              otherAspectClasses (otherAspects mi) ++
              maybe [] aspectClasses (aspect mi)

    aspectClasses (Name mKind op) =
        let kindClass = maybe [] ((: []) . showKind) mKind

            showKind (Constructor Inductive)   = "InductiveConstructor"
            showKind (Constructor CoInductive) = "CoinductiveConstructor"
            showKind k                         = show k

            opClass = ["Operator" | op]
        in kindClass ++ opClass
    aspectClasses a = [show a]

    otherAspectClasses = map show

    -- Notes are not included.
    noteClasses _ = []

    link DefinitionSite {defSiteModule = m', defSitePos = pos'} =
        if m == m'
           then Just [href ("#" ++ show pos')]
           else Just [href (show (tostdliblink m') ++ "#" ++ show pos')]
    tostdliblink mn = fromMaybe nullURI (parseURIReference (intercalate "." (moduleNameParts mn ++ ["html"])))
                       `nonStrictRelativeTo`  libs

renderFragments :: URI -> String
           -> TopLevelModuleName -> [Either String [(Int, String, Aspects)]]
           -> String
renderFragments libs classpr m contents =
    concat [ case c of
                  Left s   -> s
                  Right cs ->
                      let h = pre . tag "code" . mconcat $
                              [ annotate libs m pos mi (stringToHtml s)
                              | (pos, s, mi) <- cs ]
                      in  renderHtmlFragment (h ! [theclass classpr])
           | c <- contents ]

convert :: URI -> String -> TopLevelModuleName -> TCM String
convert libs classpr m =
    do (info, contents) <- getModule m
       return . renderFragments libs classpr m . groupLiterate . pairPositions info $ contents

-- | The CSS Class to use for Agda code blocks
type CSSClass = String

-- | Render a literate Agda module's code snippets to HTML.
renderAgdaSnippets
  :: CommandLineOptions -- ^ Agda Command line options
  -> CSSClass           -- ^ CSS Class name
  -> URI                -- ^ URI where other Agda HTML listings are found (for library links etc)
  -> FilePath           -- ^ File name of literate agda file
  -> IO String          -- ^ Returns the file contents as a string, where each code block has been rendered to HTML.
renderAgdaSnippets opts classpr libs fp  =
    do afp <- absolute fp
       r <- TCM.runTCMTop $ catchError (TCM.setCommandLineOptions opts >>
                                           checkFile afp >>= convert libs classpr)
                          $ \err -> do s <- prettyError err
                                       liftIO (putStrLn s)
                                       throwError err
       case r of
           Right s -> return (dropWhile isSpace s)
           Left _  -> exitFailure

