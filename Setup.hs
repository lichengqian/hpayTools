import Distribution.Simple
import qualified Data.ByteString.Lazy as B
import Data.Functor
import Codec.Archive.Zip

main = defaultMainWithHooks simpleUserHooks {
        preBuild = \a b -> zipSource >> preBuild simpleUserHooks a b
    }

zipSource = do
    putStrLn "create source.zip"
    bs <- zipFiles ["Setup.hs", "hpayTools.cabal", "src"]
    B.writeFile "dist/source.zip" bs

zipFiles :: [FilePath] -> IO B.ByteString
zipFiles files = fromArchive <$> addFilesToArchive [OptRecursive] emptyArchive files

