{-# LANGUAGE DeriveGeneric #-}

module Core.Types where

import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Serialize (Serialize)

-- | Representa un usuario con su nombre, salt y hash del PIN
data User = User
  { uName :: String        -- ^ Identificador del usuario
  , uSalt :: ByteString    -- ^ Sal aleatorio (por ejemplo 16 bytes)
  , uHash :: ByteString    -- ^ Hash PBKDF2-SHA256 del PIN
  } deriving (Show, Generic)

-- | Credencial almacenada en el vault
data Credential = Credential
  { cTitle :: String       -- ^ Título o etiqueta de la credencial
  , cUser  :: String       -- ^ Nombre de usuario (o correo)
  , cPass  :: ByteString   -- ^ Contraseña en memoria (se cifrará al persistir)
  } deriving (Show, Generic)

-- | El vault es la colección de credenciales de un usuario
type Vault = [Credential]

-- Instancias de serialización para leer/escribir con cereal
instance Serialize User
instance Serialize Credential