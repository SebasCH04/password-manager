{-# LANGUAGE DeriveGeneric #-}

module Core.Types where

import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Serialize (Serialize)

--representa un usuario con su nombre, salt y hash del PIN
data User = User
  { uName :: String --identificador del usuario
  , uSalt :: ByteString --sal aleatorio (por ejemplo 16 bytes)
  , uHash :: ByteString --hash PBKDF2-SHA256 del PIN
  } deriving (Show, Generic)

--credencial almacenada en el vault
data Credential = Credential
  { cTitle :: String --titulo o etiqueta de la credencial
  , cUser  :: String --nombre de usuario
  , cPass  :: ByteString --contraseña en memoria (se va a cifrar al persistir)
  } deriving (Show, Generic)

--el vault es la coleccion de credenciales de un usuario
type Vault = [Credential]

--instancias de serializacion para leer/escribir usando cereal
instance Serialize User
instance Serialize Credential