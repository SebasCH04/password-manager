{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Core.Storage    (saveVault, loadVault)
import Core.PinAuth    (promptPIN, verifyPIN)
import Core.Crypto     (generateSalt, deriveKey, hashPIN)
import Core.Types      (Credential(..), Vault)
import GHC.Generics    (Generic)
import Data.Serialize  (Serialize)
import qualified Data.Serialize as S
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.Directory (doesFileExist)
import Options.Applicative

-- | Rutas de los archivos de configuración y vault
configPath, vaultPath :: FilePath
configPath = "config.dat"
vaultPath  = "vault.dat"

-- | Estructura de configuración: sal y hash del PIN
data Config = Config
  { cfgSalt :: ByteString
  , cfgHash :: ByteString
  } deriving (Show, Generic)

instance Serialize Config where
  put = S.put
  get = S.get

-- | Comandos soportados por la CLI
data Command
  = List
  | Add { title :: String, username :: String, password :: String }
  | Remove { titleR :: String }

-- | Parser de comandos
downloadParser :: Parser Command
downloadParser = hsubparser
  ( command "list" (info (pure List)
      (progDesc "List credentials in the vault"))
 <> command "add" (info addParser
      (progDesc "Add a new credential"))
 <> command "remove" (info removeParser
      (progDesc "Remove a credential by title"))
  )
  where
    addParser = Add
      <$> strOption (long "title" <> metavar "TITLE" <> help "Credential title")
      <*> strOption (long "user"  <> metavar "USER"  <> help "Username")
      <*> strOption (long "pass"  <> metavar "PASSWORD" <> help "Password")
    removeParser = Remove
      <$> strOption (long "title" <> metavar "TITLE" <> help "Title to remove")

opts :: ParserInfo Command
opts = info (downloadParser <**> helper)
  ( fullDesc
 <> header "password-manager - simple encrypted vault CLI" )

-- | Inicializa config/vault o solicita PIN existente, retornando la clave
ensureConfig :: IO ByteString
ensureConfig = do
  exists <- doesFileExist configPath
  if not exists
    then do
      putStrLn "No config found. Setting up a new PIN..."
      pin  <- promptPIN "New PIN: "
      salt <- generateSalt
      let h = hashPIN salt pin
      BS.writeFile configPath (S.encode (Config salt h))
      let key = deriveKey salt pin
      saveVault vaultPath key []
      putStrLn "Initialized empty vault."
      return key
    else do
      bs <- BS.readFile configPath
      case S.decode bs of
        Left err -> error $ "Failed reading config: " ++ err
        Right (Config salt h) -> do
          eKey <- verifyPIN salt h
          case eKey of
            Left err -> error err
            Right key -> return key

main :: IO ()
main = do
  putStrLn "DEBUG: ¡He entrado a main!"
  cmd <- execParser opts
  putStrLn "DEBUG: Parser OK, comando recibido."
  key <- ensureConfig
  putStrLn "DEBUG: Configuración OK, tengo clave."
  eVault <- loadVault vaultPath key
  putStrLn "DEBUG: Vault cargado."
  vault  <- case eVault of
    Left err -> error $ "Failed loading vault: " ++ err
    Right v  -> return v
  case cmd of
    List -> mapM_ (putStrLn . cTitle) vault
    Add t u p -> do
      let cred = Credential t u (BC.pack p)
      saveVault vaultPath key (vault ++ [cred])
      putStrLn $ "Added: " ++ t
    Remove tR -> do
      let vault' = filter ((/= tR) . cTitle) vault
      saveVault vaultPath key vault'
      putStrLn $ "Removed: " ++ tR