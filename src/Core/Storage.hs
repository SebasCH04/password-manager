{-# LANGUAGE OverloadedStrings #-}

module Core.Storage
  ( saveVault
  , loadVault
  ) where

import           Core.Crypto    (encryptVault, decryptVault)
import           Core.Types     (Vault)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

--guarda un vault en un archivo 
saveVault :: FilePath --ruta al archivo donde guardar
          -> ByteString --clave de cifrado (32 bytes)
          -> Vault --estructura a persistir
          -> IO ()
saveVault path key vault = do
  payload <- encryptVault key vault
  BS.writeFile path payload

--carga un vault desde un archivo
loadVault :: FilePath --ruta al archivo guardado
          -> ByteString --clave de cifrado (32 bytes)
          -> IO (Either String Vault)
loadVault path key = do
  payload <- BS.readFile path
  return $ decryptVault key payload