{-# LANGUAGE OverloadedStrings #-}

module Core.Storage
  ( saveVault
  , loadVault
  ) where

import           Core.Crypto    (encryptVault, decryptVault)
import           Core.Types     (Vault)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Guarda un vault en un archivo, usando AES-GCM-SIV
saveVault :: FilePath      -- ^ Ruta al archivo donde guardar
          -> ByteString    -- ^ Clave de cifrado (32 bytes)
          -> Vault         -- ^ Estructura a persistir
          -> IO ()
saveVault path key vault = do
  payload <- encryptVault key vault
  BS.writeFile path payload

-- | Carga un vault desde un archivo, usando AES-GCM-SIV
loadVault :: FilePath           -- ^ Ruta al archivo guardado
          -> ByteString         -- ^ Clave de cifrado (32 bytes)
          -> IO (Either String Vault)
loadVault path key = do
  payload <- BS.readFile path
  return $ decryptVault key payload