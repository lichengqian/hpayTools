module Util.ZipUtil where

import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict,toStrict)
import Data.Functor
import Language.Haskell.TH.Syntax
import Codec.Archive.Zip
import Data.FileEmbed

createZip :: [FilePath] -> Q Exp
createZip files = do
    mapM_ qAddDependentFile files
    (runIO $ zipFiles files) >>= bsToExp

zipFiles :: [FilePath] -> IO B.ByteString
zipFiles files = toStrict . fromArchive <$> addFilesToArchive [OptRecursive] emptyArchive files

unzipFiles :: FilePath -> B.ByteString -> IO ()
unzipFiles dest = extractFilesFromArchive [OptRecursive, OptDestination dest] . toArchive . fromStrict
