{-# LANGUAGE LambdaCase #-}
module Hakyll.Contrib.Agda
       ( -- * Literate Agda Compilers
         literateAgdaCompiler
       , literateAgdaCompilerWith
       , literateAgdaCompilerWithTransform
       , literateAgdaCompilerWithTransformM
         -- * Building Blocks
       , defaultFileType
       , readLiterateAgda
         -- * Command line options
       , CommandLineOptions (..)
       , defaultOptions
       ) where


import Agda.Contrib.Snippets
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Core.Identifier
import Hakyll.Web.Pandoc
import Hakyll.Web.Pandoc.FileType
import System.FilePath
import System.Directory
import Network.URI
import Control.Exception

-- | Like 'literateAgdaCompilerWith', but an arbitrary transformation of the Pandoc
--   document can be added.
literateAgdaCompilerWithTransform
  :: CommandLineOptions -- ^ Agda command line options
  -> FileType           -- ^ Format to use when reading other, non-Agda blocks.
  -> URI                -- ^ Base URI where external libraries can be found.
  -> ReaderOptions      -- ^ Pandoc reader options
  -> WriterOptions      -- ^ Pandoc writer options
  -> (Item Pandoc -> Item Pandoc) -- ^ Transformation to run
  -> Compiler (Item String)
literateAgdaCompilerWithTransform opts ft uri ro wo
  = literateAgdaCompilerWithTransformM opts ft uri ro wo . (return .)

-- | Like 'literateAgdaCompiler', but Pandoc options can be specified.
literateAgdaCompilerWith
  :: CommandLineOptions -- ^ Agda command line options
  -> FileType           -- ^ Format to use when reading other, non-Agda blocks.
  -> URI                -- ^ Base URI where external libraries can be found.
  -> ReaderOptions      -- ^ Pandoc reader options
  -> WriterOptions      -- ^ Pandoc writer options
  -> Compiler (Item String)
literateAgdaCompilerWith opts ft uri ro wo
  = literateAgdaCompilerWithTransform opts ft uri ro wo id

-- | Compile a literate Agda document with the given Agda command line options,
--   text block format type, and library uri for hyperlinks.
literateAgdaCompiler
  :: CommandLineOptions -- ^ Agda command line options
  -> FileType           -- ^ Format to use when reading other, non-Agda blocks.
  -> URI                -- ^ Base URI where external libraries can be found.
  -> Compiler (Item String)
literateAgdaCompiler opts ft uri
  = literateAgdaCompilerWith opts ft uri defaultHakyllReaderOptions defaultHakyllWriterOptions

-- | Like 'literateAgdaCompilerWithTransform', but the transformation given is monadic.
literateAgdaCompilerWithTransformM
  :: CommandLineOptions -- ^ Agda command line options
  -> FileType           -- ^ Format to use when reading other, non-Agda blocks.
  -> URI                -- ^ Base URI where external libraries can be found.
  -> ReaderOptions      -- ^ Pandoc reader options
  -> WriterOptions      -- ^ Pandoc writer options
  -> (Item Pandoc -> Compiler (Item Pandoc)) -- ^ Transformation to run
  -> Compiler (Item String)
literateAgdaCompilerWithTransformM opts ft uri ro wo transform =
           fmap (writePandocWith wo) $ getResourceBody
              >>= readLiterateAgda opts uri
              >>= defaultFileType ft (readPandocWith ro)
              >>= transform

-- | Run a function that might be part of your compiler pipeline, except that if the
--   text format type cannot be detected from the extension (e.g, if it's a @lagda@ file),
--   the specified file type will be used instead of 'Binary'.
defaultFileType :: FileType -- ^ File type to default to
                -> (Item a -> Compiler (Item b)) -- ^ Pipeline function to run
                -> Item a
                -> Compiler (Item b)
defaultFileType t act i = do
     let tau Binary = t
         tau x      = x
     x <- act (i {itemIdentifier = fromFilePath $ fn -<.> extensionFor (tau $ fileType fn) })
     return $ x {itemIdentifier = fromFilePath fn }
   where
     fn = toFilePath $ itemIdentifier i
     extensionFor = \case
           Binary              -> takeExtension fn
           Css                 -> "css"
           DocBook             -> "dbk"
           Html                -> "html"
           LaTeX               -> "tex"
           (LiterateHaskell f) -> extensionFor f ++ ".lhs"
           Markdown            -> "md"
           MediaWiki           -> "mediawiki"
           OrgMode             -> "org"
           PlainText           -> "txt"
           Rst                 -> "rst"
           Textile             -> "textile"

-- | Read a literate Agda document using the given options, producing a string with
--   literate Agda snippets replaced with HTML, but everything else untouched.
readLiterateAgda :: CommandLineOptions -- ^ Agda command line options
                 -> URI                -- ^ Base URI where external libraries can be found.
                 -> Item String
                 -> Compiler (Item String)
readLiterateAgda aopt liburi i =
    if isAgda i
    then cached cacheName $
      do fp <- getResourceFilePath
         unsafeCompiler $ bracket getCurrentDirectory setCurrentDirectory $ const $
           do abfp <- canonicalizePath fp
              setCurrentDirectory (dropFileName abfp)
              s <- renderAgdaSnippets aopt "Agda" liburi abfp
              return $ i {itemBody = s}
    else return i
  where
    cacheName = "LiterateAgda.agdaCompiler"
    isAgda = (== ".lagda") . takeExtension . toFilePath . itemIdentifier
