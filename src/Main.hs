{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Options.Applicative
import System.Directory (doesFileExist)

--registro de usuario: nombre de la cuenta, salt y hash del PIN
data UserRecord = UserRecord
  { urName :: String
  , urSalt :: ByteString
  , urHash :: ByteString
  } deriving (Show, Generic)
instance Serialize UserRecord

type Config = [UserRecord]

configPath :: FilePath
configPath = "config.dat"

toVaultFile :: String -> FilePath
toVaultFile acct = "vault_" ++ acct ++ ".dat"

--comandos para CLI
data Command
  = Register { account :: String }
  | List { account :: String }
  | Add { account :: String, title :: String, username :: String, password :: String }
  | Remove { account :: String, titleR :: String }
  | CopyUser { account :: String, titleC :: String }
  | CopyPass { account :: String, titleC :: String }
  | Edit { account :: String, titleE :: String, newUser :: Maybe String, newPass :: Maybe String }

commandParser :: Parser Command
commandParser = hsubparser
  ( command "register" (info registerParser (progDesc "Registrar una nueva cuenta"))
 <> command "list" (info listParser (progDesc "Listar las credenciales de una cuenta"))
 <> command "add" (info addParser (progDesc "Agregar una nueva credencial"))
 <> command "remove" (info removeParser (progDesc "Eliminar una credencial por titulo"))
 <> command "copy-user" (info copyUserParser (progDesc "Copiar nombre de usuario al portapapeles"))
 <> command "copy-pass" (info copyPassParser (progDesc "Copiar la contraseña al portapapeles"))
 <> command "edit" (info editParser (progDesc "Editar una credencial existente"))
  )
  where
    registerParser = Register
      <$> strOption (long "account" <> metavar "ACCOUNT" <> help "Nombre de la cuenta para registrar")
    listParser = List
      <$> strOption (long "account" <> metavar "ACCOUNT" <> help "Cuenta a utilizar")
    addParser = Add
      <$> strOption (long "account" <> metavar "ACCOUNT" <> help "Cuenta a utilizar")
      <*> strOption (long "title" <> metavar "TITLE" <> help "Titulo de credential")
      <*> strOption (long "user" <> metavar "USER" <> help "Usuario de credencial")
      <*> strOption (long "pass" <> metavar "PASSWORD" <> help "Contraseña de credencial")
    removeParser = Remove
      <$> strOption (long "account" <> metavar "ACCOUNT" <> help "Cuenta a utilizar")
      <*> strOption (long "title" <> metavar "TITLE" <> help "Titulo a eliminar")
    copyUserParser = CopyUser
      <$> strOption (long "account" <> metavar "ACCOUNT" <> help "Cuenta a utilizar")
      <*> strOption (long "title" <> metavar "TITLE" <> help "Titulo a copiar usuario")
    copyPassParser = CopyPass
      <$> strOption (long "account" <> metavar "ACCOUNT" <> help "Cuenta a utilizar")
      <*> strOption (long "title" <> metavar "TITLE" <> help "Title a copiar contraseña")
    editParser = Edit
      <$> strOption (long "account" <> metavar "ACCOUNT" <> help "Cuenta a utilizar")
      <*> strOption (long "title" <> metavar "TITLE" <> help "Titulo de credencial a utilizar")
      <*> optional (strOption (long "user" <> metavar "USER" <> help "Nuevo nombre de usuario"))
      <*> optional (strOption (long "pass" <> metavar "PASSWORD" <> help "Nueva contraseña"))

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc <> header "password-manager: multi-user encrypted vault CLI" )

loadConfig :: IO Config
loadConfig = do
  exists <- doesFileExist configPath
  if not exists then return [] else do
    bs <- BS.readFile configPath
    case S.decode bs of
      Left _   -> return []
      Right cs -> return cs

saveConfig :: Config -> IO ()
saveConfig cs = BS.writeFile configPath (S.encode cs)

doRegister :: String -> IO ()
doRegister acct = do
  cfg <- loadConfig
  if any ((==acct) . urName) cfg
    then putStrLn $ "La cuenta '" ++ acct ++ "' ya existe."
    else do
      pin  <- promptPIN "Crear PIN: "
      salt <- generateSalt
      let h = hashPIN salt pin
      saveConfig (UserRecord acct salt h : cfg)
      let key = deriveKey salt pin
      saveVault (toVaultFile acct) key []
      putStrLn $ "Cuenta registrada '" ++ acct ++ "'."

ensureAuth :: String -> IO ByteString
ensureAuth acct = do
  cfg <- loadConfig
  case find ((==acct) . urName) cfg of
    Nothing -> error $ "La cuenta '" ++ acct ++ "' no fue encontrada."
    Just (UserRecord _ salt h) -> do
      eKey <- verifyPIN salt h
      case eKey of
        Left err  -> error err
        Right key -> return key

maskUser :: String -> String
maskUser s = let n = length s in if n <= 4 then replicate (n-1) '*' ++ [last s]
                                else take 2 s ++ replicate (n-4) '*' ++ drop (n-2) s
encryptedPass :: String
encryptedPass = replicate 8 '*'

enformatRow :: [Int] -> [String] -> String
enformatRow ws cs = let cells = zipWith (\w c -> c ++ replicate (w-length c) ' ') ws cs
                      in "| " ++ unwords [cell ++ " |" | cell <- cells]
encomputeWidths :: [[String]] -> [Int]
encomputeWidths rows = map (maximum . map length) (transpose rows)

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Register acct -> doRegister acct
    _ -> do
      let acct = account cmd
      key <- ensureAuth acct
      eVault <- loadVault (toVaultFile acct) key
      vault  <- case eVault of
        Left err -> error $ "Error al cargar el Vault: " ++ err
        Right v  -> return v
      case cmd of
        List{} -> do
          let header = ["Titulo","Usuario","Contraseña"]
              rows   = [[cTitle c, maskUser (cUser c), encryptedPass] | c <- vault]
              widths = encomputeWidths (header:rows)
          putStrLn $ enformatRow widths header
          putStrLn $ enformatRow widths (map (map (const '-')) header)
          mapM_ (putStrLn . enformatRow widths) rows
        Add{account,title,username,password} -> do
          let cred = Credential title username (BC.pack password)
          saveVault (toVaultFile account) key (vault ++ [cred])
          putStrLn $ "Agregado: " ++ title
        Remove{account,titleR} -> do
          let vault' = filter ((/= titleR) . cTitle) vault
          saveVault (toVaultFile account) key vault'
          putStrLn $ "Eliminado: " ++ titleR
        CopyUser{titleC} ->
          case find ((==titleC) . cTitle) vault of
            Just cred -> do
              copyToClipboard (BC.pack $ cUser cred)
              putStrLn $ "Nombre de usuario para '" ++ titleC ++ "' copiado."
            Nothing -> putStrLn $ "La credencial '" ++ titleC ++ "' no fue encontrada."
        CopyPass{titleC} ->
          case find ((==titleC) . cTitle) vault of
            Just cred -> do
              copyToClipboard (cPass cred)
              putStrLn $ "Contraseña para '" ++ titleC ++ "' copiada."
            Nothing -> putStrLn $ "La credencial '" ++ titleC ++ "' no fue encontrada."
        Edit{account,titleE,newUser,newPass} ->
          case find ((==titleE) . cTitle) vault of
            Just cred -> do
              let updated = cred { cUser = fromMaybe (cUser cred) newUser
                                 , cPass = maybe (cPass cred) BC.pack newPass }
                  vault'  = map (\c -> if cTitle c == titleE then updated else c) vault
              saveVault (toVaultFile account) key vault'
              putStrLn $ "Modificado: " ++ titleE
            Nothing -> putStrLn $ "La credencial '" ++ titleE ++ "' no fue encontrada."
        _ -> return ()