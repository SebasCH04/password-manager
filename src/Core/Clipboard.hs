{-# LANGUAGE OverloadedStrings #-}

-- | Módulo para copiar texto al portapapeles, soporta macOS y Linux
module Core.Clipboard
  ( copyToClipboard  -- ^ Copia un ByteString al portapapeles
  ) where

import System.Info      (os)
import System.Process   (createProcess, proc, waitForProcess, StdStream(CreatePipe), std_in)
import System.IO        (hPutStr, hClose, hFlush)
import System.Directory (findExecutable)
import Data.ByteString  (ByteString)
import qualified Data.ByteString.Char8 as BS

-- | Envia datos a la utilidad de portapapeles
doCopy :: String -> [String] -> ByteString -> IO ()
doCopy cmd args bs = do
  let text = BS.unpack bs
  (Just inh, _, _, ph) <- createProcess (proc cmd args){ std_in = CreatePipe }
  hPutStr inh text
  hFlush inh
  hClose inh
  _ <- waitForProcess ph
  return ()

-- | Copia al portapapeles usando la utilidad disponible en el sistema
copyToClipboard :: ByteString -> IO ()
copyToClipboard bs = case os of
  "darwin" -> do
    mb <- findExecutable "pbcopy"
    case mb of
      Just _  -> doCopy "pbcopy" [] bs
      Nothing -> error "pbcopy no encontrado"
  _ -> do
    mwl <- findExecutable "wl-copy"
    case mwl of
      Just _  -> doCopy "wl-copy" [] bs
      Nothing -> do
        mx <- findExecutable "xclip"
        case mx of
          Just _  -> doCopy "xclip" ["-selection","clipboard"] bs
          Nothing -> error "No se encontró wl-copy ni xclip"