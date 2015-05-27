module CloudLogger.LogParser(
  pLog, withLogs, readLogs
) where

import Text.Parsec
--import Text.Parsec.String
import Control.Monad.Trans.Class (lift)
import CloudLogger.Types

type P = ParsecT String () IO

-- date parser : "2014-03-04"
pDate :: P String
pDate = do
    y <- count 4 alphaNum
    char '-'
    m <- count 2 alphaNum
    char '-'
    d <- count 2 alphaNum
    return $ y ++ "-" ++ m ++ "-" ++ d

pTime :: P String
pTime = do
    h <- count 2 alphaNum
    sep
    m <- count 2 alphaNum
    sep
    s <- count 2 alphaNum
    char ','
    ms <- count 3 alphaNum
    return $ h ++ ":" ++ m ++ ":" ++ s ++ "," ++ ms

    where
        sep = char ':'
  
pDateTime = do
    d <- pDate <?> "date"
    char ' '
    t <- pTime <?> "time"
    return $ d ++ " " ++ t
  
pLevel :: P String
pLevel = many1 upper

pID :: P String
pID = do
    many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_', '-', '.']
  
pTag = do
    char '['
    manyTill (anyChar) (char ']')
  
    -- between ( (char ']') pID
--pThread = between (char '(') (char ')') pID

pMessage :: P String
pMessage = manyTill (anyChar) (hasNext <|> eof)
  where
    hasNext = try $ do
      newline
      lookAhead pDateTime
      return ()

pLog = do
    dt <- pDateTime
    char ' '
    level <- pLevel <?> "level"
    many1 $ char ' '
    tag <- pTag <?> "tag"
    --char ' '
    --thread <- pThread
    --char ' '
    msg <- pMessage <?> "message"
    return $ LogEntry dt level tag msg

pLogs = many1 pLog  

parseLogs = runParserT pLogs ()

withLogs source input action = do
    mret <- runParserT p () source input
    case mret of
        Left err -> return $ show err
        Right _  -> return ""
    where
        p = many1 $ do
            log <- pLog
            lift $ action log
      
readLogs file w = do
    input <- readFile file
    withLogs file input w
