{-# LANGUAGE OverloadedStrings #-}

--modulo de criptografia: hace hashing de PIN con PBKDF2 y hace cifrado AES-GCM-SIV
module Core.Crypto
  ( generateSalt --genera sal aleatoria
  , deriveKey --deriva clave de cifrado del PIN
  , hashPIN --hash PBKDF2 del PIN
  , encryptVault --cifra y serializa el vault
  , decryptVault --descifra y deserializa el vault
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

--longitud de la sal en bytes
saltLength :: Int
saltLength = 16

--genera una sal aleatoria (16 bytes)
generateSalt :: IO ByteString
generateSalt = getRandomBytes saltLength

--parametros de PBKDF2 (100k iteraciones y la salida de 32 bytes)
pbkdf2Params :: Parameters
pbkdf2Params = Parameters
  { iterCounts   = 100000
  , outputLength = 32
  }

--deriva clave de cifrado (32 bytes) a partir de sal y PIN
deriveKey :: ByteString --sal
          -> ByteString --PIN en claro
          -> ByteString --clave derivada
deriveKey = fastPBKDF2_SHA256 pbkdf2Params

--hash PBKDF2 del PIN (para comparar en autenticacion)
hashPIN :: ByteString --sal
        -> ByteString --PIN en claro
        -> ByteString --hash resultante
hashPIN = deriveKey

--cifra y serializa el vault completo con AES-GCM-SIV
encryptVault :: ByteString --clave de cifrado (32 bytes)
             -> Vault --datos a cifrar
             -> IO ByteString --payload para guardar en disco
encryptVault key vault = do
  --inicializar el cifrador AES
  aes <- case cipherInit key of
    CryptoPassed c -> return (c :: AES256)
    CryptoFailed e -> error ("Error inicializando AES: " ++ show e)
  --serializar vault
  let plain = encode vault
  --generar nonce de 12 bytes
  n     <- generateNonce
  --cifrar con AAD vacío
  let (tag, ct) = encrypt aes n BS.empty (convert plain)
  --payload = nonce|etiqueta|ciphertext
  return $ BS.concat [ convert n, convert tag, ct ]

--descifra y deserializa el vault
decryptVault :: ByteString --clave de cifrado (32 bytes)
             -> ByteString --payload leído del disco
             -> Either String Vault --error o vault restaurado
decryptVault key payload =
  let (nBs, rest)    = BS.splitAt 12 payload
      (tagBs, ct)    = BS.splitAt 16 rest
      --reconstruir nonce
      n = case nonce nBs of
            CryptoPassed x -> x
            CryptoFailed e -> error ("Nonce invalido: " ++ show e)
      --recrear AuthTag desde bytes
      tag = AuthTag (convert tagBs)
      --inicializar cifrador
      aes = case cipherInit key of
              CryptoPassed c -> c :: AES256
              CryptoFailed e -> error ("Error inicializando AES: " ++ show e)
  in case decrypt aes n BS.empty ct tag of
       Nothing    -> Left "Autenticacion fallida o datos corruptos"
       Just ba    -> case decode (convert ba) of
         Left err    -> Left ("Decode error: " ++ err)
         Right vault -> Right vault