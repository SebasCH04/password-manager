{-# LANGUAGE OverloadedStrings #-}

-- | Módulo de criptografía: hashing de PIN con PBKDF2 y cifrado AES-GCM-SIV
module Core.Crypto
  ( generateSalt      -- ^ Genera sal aleatoria
  , deriveKey         -- ^ Deriva clave de cifrado del PIN
  , hashPIN           -- ^ Hash PBKDF2 del PIN
  , encryptVault      -- ^ Cifra y serializa el vault
  , decryptVault      -- ^ Descifra y deserializa el vault
  ) where

import Crypto.Cipher.AESGCMSIV
  ( generateNonce
  , nonce
  , encrypt
  , decrypt
  )
import Crypto.Cipher.AES      (AES256)
import Crypto.Cipher.Types    (cipherInit, AuthTag(..))
import Crypto.Error           (CryptoFailable(..))
import Crypto.KDF.PBKDF2      (Parameters(..), fastPBKDF2_SHA256)
import Crypto.Random          (getRandomBytes)
import Data.ByteArray         (convert)
import Data.ByteString        (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize         (encode, decode)
import Core.Types             (Vault)

-- | Longitud de la sal en bytes
saltLength :: Int
saltLength = 16

-- | Genera una sal aleatoria (16 bytes)
generateSalt :: IO ByteString
generateSalt = getRandomBytes saltLength

-- | Parámetros de PBKDF2 (100k iteraciones, salida 32 bytes)
pbkdf2Params :: Parameters
pbkdf2Params = Parameters
  { iterCounts   = 100000
  , outputLength = 32
  }

-- | Deriva clave de cifrado (32 bytes) a partir de sal y PIN
deriveKey :: ByteString  -- ^ sal
          -> ByteString  -- ^ PIN en claro
          -> ByteString  -- ^ clave derivada
deriveKey = fastPBKDF2_SHA256 pbkdf2Params

-- | Hash PBKDF2 del PIN (para comparar en autenticación)
hashPIN :: ByteString   -- ^ sal
        -> ByteString   -- ^ PIN en claro
        -> ByteString   -- ^ hash resultante
hashPIN = deriveKey

-- | Cifra y serializa el vault completo con AES-GCM-SIV
encryptVault :: ByteString    -- ^ clave de cifrado (32 bytes)
             -> Vault         -- ^ datos a cifrar
             -> IO ByteString -- ^ payload para guardar en disco
encryptVault key vault = do
  -- Inicializar el cifrador AES
  aes <- case cipherInit key of
    CryptoPassed c -> return (c :: AES256)
    CryptoFailed e -> error ("Error inicializando AES: " ++ show e)
  -- Serializar vault
  let plain = encode vault
  -- Generar nonce de 12 bytes (Nonce AES256)
  n     <- generateNonce
  -- Cifrar con AAD vacío
  let (tag, ct) = encrypt aes n BS.empty (convert plain)
  -- Payload = nonce ‖ etiqueta ‖ ciphertext
  return $ BS.concat [ convert n, convert tag, ct ]

-- | Descifra y deserializa el vault
decryptVault :: ByteString         -- ^ clave de cifrado (32 bytes)
             -> ByteString         -- ^ payload leído del disco
             -> Either String Vault-- ^ error o vault restaurado
decryptVault key payload =
  let (nBs, rest)    = BS.splitAt 12 payload
      (tagBs, ct)    = BS.splitAt 16 rest
      -- Reconstruir nonce
      n = case nonce nBs of
            CryptoPassed x -> x
            CryptoFailed e -> error ("Nonce inválido: " ++ show e)
      -- Recrear AuthTag desde bytes
      tag = AuthTag (convert tagBs)
      -- Inicializar cifrador
      aes = case cipherInit key of
              CryptoPassed c -> c :: AES256
              CryptoFailed e -> error ("Error inicializando AES: " ++ show e)
  in case decrypt aes n BS.empty ct tag of
       Nothing    -> Left "Autenticación fallida o datos corruptos"
       Just ba    -> case decode (convert ba) of
         Left err    -> Left ("Decode error: " ++ err)
         Right vault -> Right vault