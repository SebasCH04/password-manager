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

--pide al usuario que ingrese un PIN ocultando la entrada
type Prompt = String

promptPIN :: Prompt --texto a mostrar
          -> IO ByteString
promptPIN msg = do
  putStr msg
  hFlush stdout
  hSetEcho stdin False
  input <- B8.getLine
  hSetEcho stdin True
  putStrLn "" --nueva linea
  return input

--verifica el PIN contra el hash almacenado y devuelve la clave de cifrado si es correcto
verifyPIN :: ByteString --sal utilizada originalmente
          -> ByteString --hash PBKDF2-SHA256 del PIN almacenado
          -> IO (Either String ByteString)
verifyPIN salt storedHash = do
  pin <- promptPIN "Ingrese su PIN: "
  let computed = hashPIN salt pin
  if computed == storedHash
    then let key = deriveKey salt pin
         in return (Right key)
    else return (Left "PIN incorrecto")

--deriva la clave de cifrado sin verificar el PIN directly
getEncryptionKey :: ByteString --sal
                 -> ByteString --PIN en claro
                 -> ByteString
getEncryptionKey = deriveKey