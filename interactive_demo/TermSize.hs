{-# LINE 1 "TermSize.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- Taken from the answer to - https://stackoverflow.com/questions/12806053/get-terminal-width-haskell

module TermSize (getTermSize) where

import Foreign
import Foreign.C.Error
import Foreign.C.Types




-- Trick for calculating alignment of a type, taken from
-- http://www.haskell.org/haskellwiki/FFICookBook#Working_with_structs
-- #let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- The ws_xpixel and ws_ypixel fields are unused, so I've omitted them here.
data WinSize = WinSize { wsRow, wsCol :: CUShort }

instance Storable WinSize where
  sizeOf _ = ((8))
{-# LINE 23 "TermSize.hsc" #-}
  alignment _ = (2) 
{-# LINE 24 "TermSize.hsc" #-}
  peek ptr = do
    row <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 26 "TermSize.hsc" #-}
    col <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) ptr
{-# LINE 27 "TermSize.hsc" #-}
    return $ WinSize row col
  poke ptr (WinSize row col) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr row
{-# LINE 30 "TermSize.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2)) ptr col
{-# LINE 31 "TermSize.hsc" #-}

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr WinSize -> IO CInt

-- | Return current number of (rows, columns) of the terminal.
getTermSize :: IO (Int, Int)
getTermSize = 
  with (WinSize 0 0) $ \ws -> do
    throwErrnoIfMinus1 "ioctl" $
      ioctl (1) (1074295912) ws
{-# LINE 41 "TermSize.hsc" #-}
    WinSize row col <- peek ws
    return (fromIntegral row, fromIntegral col)