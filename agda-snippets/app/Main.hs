{-# LANGUAGE LambdaCase, CPP #-}
module Main where

import Agda.Contrib.Snippets
import Agda.Interaction.Options
import System.Environment
import System.Exit
import System.IO
import Network.URI
import Control.Applicative
import Control.Monad.Trans.Except
note :: b -> Maybe a -> Either b a 
note b Nothing = Left b
note _ (Just a) = Right a

#if MIN_VERSION_Agda(2,4,3)
getOptions = runExceptT . parseStandardOptions
#else
getOptions = return . parseStandardOptions
#endif

main :: IO ()
main = getArgs >>= \case
   (fp:output:css:uri:agdaOpts) -> do
     opts <- getOptions agdaOpts
     case liftA2 (,) (note "Malformed URI" $ parseURIReference uri)
                     opts  of
       Right (uri', agdaOpts') -> writeFile output =<< renderAgdaSnippets agdaOpts' css uri' fp
       Left e -> hPutStrLn stderr e >> exitFailure
   _ -> do
     n <- getProgName
     putStrLn $ "Usage: " ++ n ++ " input-file.lagda output-file css-class-name /lib/uri/ [agda options]"
     exitFailure
