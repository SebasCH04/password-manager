{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Core.Storage    (saveVault, loadVault)
import Core.PinAuth    (promptPIN, verifyPIN)
import Core.Crypto     (generateSalt, deriveKey, hashPIN)
import Core.Types      (Credential(..), Vault)
import Core.Clipboard  (copyToClipboard)
import GHC.Generics    (Generic)
import Data.Serialize  (Serialize)
import qualified Data.Serialize as S
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List       (find, transpose)
import Data.Maybe      (fromMaybe)
import System.Directory (doesFileExist)
import Options.Applicative

-- | Paths
configPath, vaultPath :: FilePath
configPath = "config.dat"
vaultPath  = "vault.dat"

-- | Master configuration: salt and PIN hash
data Config = Config
  { cfgSalt :: ByteString
  , cfgHash :: ByteString
  } deriving (Show, Generic)
instance Serialize Config

-- | CLI commands
data Command
  = List
  | Add { title  :: String, username :: String, password :: String }
  | Remove { titleR :: String }
  | CopyUser { titleC :: String }
  | CopyPass { titleC :: String }
  | Edit { titleE  :: String, newUser :: Maybe String, newPass :: Maybe String }

-- | CLI parser
downloadParser :: Parser Command
downloadParser = hsubparser
  ( command "list" (info (pure List)
      (progDesc "List credentials in the vault"))
 <> command "add" (info addParser
      (progDesc "Add a new credential"))
 <> command "remove" (info removeParser
      (progDesc "Remove a credential by title"))
 <> command "copy-user" (info copyUserParser
      (progDesc "Copy username of a credential to clipboard"))
 <> command "copy-pass" (info copyPassParser
      (progDesc "Copy password of a credential to clipboard"))
 <> command "edit" (info editParser
      (progDesc "Edit an existing credential"))
  )
  where
    addParser = Add
      <$> strOption (long "title" <> metavar "TITLE" <> help "Credential title")
      <*> strOption (long "user"  <> metavar "USER"  <> help "Username")
      <*> strOption (long "pass"  <> metavar "PASSWORD" <> help "Password")
    removeParser = Remove
      <$> strOption (long "title" <> metavar "TITLE" <> help "Title to remove")
    copyUserParser = CopyUser
      <$> strOption (long "title" <> metavar "TITLE" <> help "Title to copy user from")
    copyPassParser = CopyPass
      <$> strOption (long "title" <> metavar "TITLE" <> help "Title to copy password from")
    editParser = Edit
      <$> strOption  (long "title" <> metavar "TITLE" <> help "Title of credential to edit")
      <*> optional    (strOption  (long "user"  <> metavar "USER"  <> help "New username"))
      <*> optional    (strOption  (long "pass"  <> metavar "PASSWORD" <> help "New password"))

opts :: ParserInfo Command
opts = info (downloadParser <**> helper)
  ( fullDesc
 <> header "password-manager - simple encrypted vault CLI" )

-- | Initialize or validate config, returning encryption key
ensureConfig :: IO ByteString
ensureConfig = do
  exists <- doesFileExist configPath
  if not exists
    then setupNew
    else loadExisting
  where
    setupNew = do
      putStrLn "No config found. Setting up a new PIN..."
      pin   <- promptPIN "New PIN: "
      salt  <- generateSalt
      let h   = hashPIN salt pin
      BS.writeFile configPath (S.encode (Config salt h))
      let key = deriveKey salt pin
      saveVault vaultPath key []
      putStrLn "Vault initialized empty."
      return key
    loadExisting = do
      bs <- BS.readFile configPath
      case S.decode bs of
        Left _ -> setupNew
        Right (Config salt h) -> do
          eKey <- verifyPIN salt h
          case eKey of
            Left err -> error err
            Right key -> return key

-- | Mask username: show first 2 and last 2 chars (or last char if short)
maskUser :: String -> String
maskUser s = let n = length s in
  if n <= 4 then replicate (n-1) '*' ++ [last s]
  else take 2 s ++ replicate (n-4) '*' ++ drop (n-2) s

-- | Fully hide password
encryptedPass :: String
encryptedPass = replicate 8 '*'

-- | Format table rows
formatRow :: [Int] -> [String] -> String
formatRow ws cs =
  let cells = zipWith (\w c -> c ++ replicate (w - length c) ' ') ws cs
  in "| " ++ unwords [cell ++ " |" | cell <- cells]

-- | Compute column widths from rows
computeWidths :: [[String]] -> [Int]
computeWidths rows = map (maximum . map length) (transpose rows)

main :: IO ()
main = do
  cmd <- execParser opts
  key <- ensureConfig
  eVault <- loadVault vaultPath key
  vault  <- case eVault of
    Left err -> error $ "Failed loading vault: " ++ err
    Right v  -> return v
  case cmd of
    List -> do
      let header = ["Título", "Usuario", "Contraseña"]
          rows = [ [cTitle c, maskUser (cUser c), encryptedPass] | c <- vault ]
          widths = computeWidths (header:rows)
      putStrLn $ formatRow widths header
      putStrLn $ formatRow widths (map (map (const '-')) header)
      mapM_ (putStrLn . formatRow widths) rows
    Add t u p -> do
      let cred = Credential t u (BC.pack p)
      saveVault vaultPath key (vault ++ [cred])
      putStrLn $ "Added: " ++ t
    Remove tR -> do
      let vault' = filter ((/= tR) . cTitle) vault
      saveVault vaultPath key vault'
      putStrLn $ "Removed: " ++ tR
    CopyUser tC ->
      case find ((== tC) . cTitle) vault of
        Just cred -> doCopyUser cred
        Nothing   -> putStrLn $ "Credential '" ++ tC ++ "' not found."
    CopyPass tC ->
      case find ((== tC) . cTitle) vault of
        Just cred -> doCopyPass cred
        Nothing   -> putStrLn $ "Credential '" ++ tC ++ "' not found."
    Edit tE mU mP ->
      case find ((== tE) . cTitle) vault of
        Just cred -> do
          let updated = cred { cUser = fromMaybe (cUser cred) mU
                             , cPass = maybe (cPass cred) BC.pack mP }
              vault'  = map (\c -> if cTitle c == tE then updated else c) vault
          saveVault vaultPath key vault'
          putStrLn $ "Edited: " ++ tE
        Nothing -> putStrLn $ "Credential '" ++ tE ++ "' not found."  
  where
    doCopyUser cred = do
      copyToClipboard (BC.pack $ cUser cred)
      putStrLn $ "Username for '" ++ cTitle cred ++ "' copied to clipboard."
    doCopyPass cred = do
      copyToClipboard (cPass cred)
      putStrLn $ "Password for '" ++ cTitle cred ++ "' copied to clipboard."