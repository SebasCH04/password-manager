{-# LANGUAGE OverloadedStrings #-}

module Core.PinAuth
  ( promptPIN
  , verifyPIN
  , getEncryptionKey
  ) where

import           Core.Crypto               (hashPIN, deriveKey)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as B8
import           System.IO                 (hSetEcho, stdin, stdout, hFlush)

-- | Pide al usuario que ingrese un PIN ocultando la entrada
type Prompt = String

promptPIN :: Prompt    -- ^ Texto a mostrar (por ej. "Enter PIN: ")
          -> IO ByteString
promptPIN msg = do
  putStr msg
  hFlush stdout
  hSetEcho stdin False
  input <- B8.getLine
  hSetEcho stdin True
  putStrLn ""  -- nueva línea
  return input

-- | Verifica el PIN contra el hash almacenado; devuelve la clave de cifrado si es correcto
verifyPIN :: ByteString  -- ^ Sal utilizada originalmente
          -> ByteString   -- ^ Hash PBKDF2-SHA256 del PIN almacenado
          -> IO (Either String ByteString)
verifyPIN salt storedHash = do
  pin <- promptPIN "Enter PIN: "
  let computed = hashPIN salt pin
  if computed == storedHash
    then let key = deriveKey salt pin
         in return (Right key)
    else return (Left "PIN incorrecto")

-- | Deriva la clave de cifrado sin verificar el PIN directly
getEncryptionKey :: ByteString  -- ^ Sal
                 -> ByteString  -- ^ PIN en claro
                 -> ByteString
getEncryptionKey = deriveKey