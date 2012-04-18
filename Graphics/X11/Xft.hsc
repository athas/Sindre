-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.X11.Xft
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Interface to the Xft library based on the @X11-xft@ binding by
-- Clemens Fruhwirth.  This library builds upon the X11 binding to
-- Xlib ("Graphics.X11") and cannot be used with any other.  A tiny
-- part of Xrender is also exposed, as no Haskell interface exists as
-- of this writing.
--
-- The spirit of this binding is to hide away the fact that the
-- underlying implementation is accessed via the FFI, and create a
-- Haskell-like interface that does not expose many artifacts of the C
-- implementation.  To that end, the only numeric types exposed are
-- high-level (no 'CInt's), and facilities for integrating resource
-- cleanup with the Haskell garbage collector have been defined (see
-- 'XftMgr').
--
-- Another priority has been robustness.  Many naively written FFI
-- bindings to not properly check the return values of the C functions
-- they call.  In particular, null pointers are often assumed to never
-- exist, and oftentimes impossible to check by the user as the
-- underlying pointer is not visible across the module boundary.  In
-- this binding, any Xft function that can return null has been
-- translated into a Haskell function that returns a 'Maybe' value.
--
-- Two kinds of allocator functions are provided: some that use the
-- nomenclature @new@ and some that uses @open@ (for example
-- 'newColorName' versus 'openColorName').  The former require that
-- you explicitly call the corresponding deallocator ('freeColor' in
-- this case), while the latter takes an 'XftMgr' as an additional
-- argument, and automatically calls the deallocator when the value is
-- garbage-collected.  It is an error to call a deallocator on an
-- automatically managed value.
--
-----------------------------------------------------------------------------
module Graphics.X11.Xft
  ( -- * Xft management
    XftMgr (mgrDisplay, mgrScreen)
  , newXftMgr
  , freeXftMgr
  -- * Color handling
  , Color
  , pixel
  , newColorName
  , newColorValue
  , freeColor
  , openColorName
  , openColorValue
  -- * Drawable handling
  , Draw
  , display
  , colormap
  , visual
  , drawable
  , changeDraw
  , newDraw
  , newDrawBitmap
  , newDrawAlpha
  , freeDraw
  , openDraw
  , openDrawBitmap
  , openDrawAlpha
  -- * Font handling
  , Font
  , ascent
  , descent
  , height
  , maxAdvanceWidth
  , newFontName
  , newFontXlfd
  , freeFont
  , openFontName
  , openFontXlfd
  , lockFace
  , unlockFace
  -- * Drawing
  , textExtents
  , textWidth
  , textHeight
  , drawString
  , drawGlyphs
  , drawRect
  -- * XRender bits
  , RenderColor(..)
  , GlyphInfo(..)
  )
  where

import qualified Graphics.X11 as X11
import qualified Graphics.X11.Xlib.Types as X11

import Codec.Binary.UTF8.String as UTF8

import Control.Exception
import Control.Monad
import Data.IORef
import qualified Data.Map as M

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Data.Int
import Data.Word

import System.IO
import System.IO.Unsafe
import System.Mem.Weak

#include <X11/Xft/Xft.h>
#include <X11/extensions/Xrender.h>

-- | An 'IORef' containing a map from keys to finalizers.  The idea is
-- that the finalizers remove themselves from this store, and if the
-- store (or whatever owns it) is closed before the finalizers have
-- been run, they will be run immediately.  This combines the
-- ease-of-use of automatic cleanup via finalizers, with the common
-- requirement that some set of objects has been closed at a specific
-- time.
type ObjectStore k = IORef (M.Map k (IO ()))

insertObj :: Ord k => ObjectStore k -> k -> IO () -> IO ()
insertObj ref k v = atomicModifyIORef ref $ \s -> (M.insert k v s, ())

deleteObj :: Ord k => ObjectStore k -> k -> IO ()
deleteObj ref k = atomicModifyIORef ref $ \s -> (k `M.delete` s, ())

-- | A central staging point for Xft object creation.  All Xft object
-- creation functions take as argument an 'XftMgr' value that keeps
-- track of lifetime information.  You are required to manually free
-- the 'XftMgr' via 'freeXftMgr' when you are done with it.
data XftMgr = XftMgr
  { mgrDisplay :: X11.Display
  , mgrScreen  :: X11.Screen
  , xftLock    :: IO ()
  , xftUnlock  :: IO ()
  , xftObjs    :: ObjectStore IntPtr
  }

-- | Create an 'XftMgr' whose objects will be used on the given screen
-- and display.  As Xlib is not re-entrant, a synchronisation
-- mechanism must be used, so the 'XftMgr' includes actions for
-- obtaining and releasing atomic access to the display via two 'IO'
-- actions.  These will be executed before and after objects allocated
-- via the manager are released.  It is recommended to use an
-- 'Control.Concurrent.MVar' to implement a mutex for synchronising
-- the access, but if you are absolutely certain that there will not
-- be any concurrent attempts to access the display, the actions can
-- merely be @return ()@.
newXftMgr :: X11.Display -> X11.Screen
          -> IO () -- ^ Executed before deallocations.
          -> IO () -- ^ Executed after deallocations.
          -> IO XftMgr
newXftMgr dpy scr lock unlock = do
  os <- newIORef M.empty
  return XftMgr { mgrDisplay = dpy
                , mgrScreen  = scr
                , xftLock    = lock
                , xftUnlock  = unlock
                , xftObjs    = os
                }

-- | Free the manager and reclaim any objects associated with it.
-- After an 'XftMgr' has been freed, it is invalid to use any objects
-- created through it.  The lock must currently be held by the thread
-- calling 'freeXftMgr', and it will be repeatedly released and
-- reacquired throughout deallocating any remaining objects in the
-- manager.  When the command returns, the lock will once again be
-- held.
freeXftMgr :: XftMgr -> IO ()
freeXftMgr mgr = withoutLock $ finalizeObjs xftObjs
  where getobjs s = (M.empty, M.elems s)
        withoutLock = bracket_ (xftUnlock mgr) (xftLock mgr)
        finalizeObjs f = sequence_ =<< atomicModifyIORef (f mgr) getobjs

openAny :: Ord k =>
           XftMgr -> (XftMgr -> ObjectStore k)
        -> (a -> IO ()) -> k -> a -> IO a
openAny mgr field close key obj = do
  let finalizer = bracket_ (xftLock mgr) (xftUnlock mgr) $ do
        deleteObj (field mgr) key
        close obj
  obj' <- mkWeak obj (obj, finalizer) (Just finalizer)
  insertObj (field mgr) key (finalize obj')
  return obj

openAnyWith :: Ord k => XftMgr -> (XftMgr -> ObjectStore k)
            -> (c -> IO (Maybe a)) -> (a -> IO ()) -> (a -> IO k) -> c
            -> IO (Maybe a)
openAnyWith mgr field open close keyf v = do
  obj <- open v
  case obj of Nothing   -> return Nothing
              Just obj' -> do key <- keyf obj'
                              key `seq` Just `fmap` openAny mgr field close key obj'

-- | An Xft colour.
newtype Color = Color (Ptr Color)

foreign import ccall "XftColorAllocName"
  xftColorAllocName :: X11.Display -> X11.Visual -> X11.Colormap
                    -> CString -> Color -> IO (#type Bool)

foreign import ccall "XftColorAllocValue"
  xftColorAllocValue :: X11.Display -> X11.Visual -> X11.Colormap
                     -> (Ptr RenderColor) -> Color -> IO (#type Bool)

foreign import ccall "XftColorFree"
  xftColorFree :: X11.Display -> X11.Visual -> X11.Colormap -> Color -> IO ()

-- | The core X11 colour contained in an Xft colour.
pixel :: Color -> X11.Pixel
pixel (Color ptr) = fi $ unsafePerformIO $
                    peekCULong ptr #{offset XftColor, pixel}

-- | Create a new Xft colour based on a name.  The name may be either
-- a human-readable colour such as "red", "white" or "darkslategray"
-- (all core X colour names are supported) or a hexidecimal name such
-- as "#A9E2AF".  Names are not case-sensitive.  Returns 'Nothing' if
-- the given name is not recognised as a colour.
newColorName :: X11.Display -> X11.Visual -> X11.Colormap -> String
             -> IO (Maybe Color)
newColorName dpy v cm name = do
  ptr <- mallocBytes (#size XftColor)
  withCAString name $ \name' -> do
    b <- xftColorAllocName dpy v cm name' $ Color ptr
    if b /= 0 then return $ Just $ Color ptr
              else free ptr >> return Nothing

-- | As 'newColorName', but instead of a name, an XRender color value
-- is used.
newColorValue :: X11.Display -> X11.Visual -> X11.Colormap -> RenderColor
              -> IO (Maybe Color)
newColorValue dpy v cm rc = do
  ptr <- mallocBytes (#size XftColor)
  with rc $ \rc' -> do
    b <- xftColorAllocValue dpy v cm rc' $ Color ptr
    if b /= 0 then return $ Just $ Color ptr
              else free ptr >> return Nothing

-- | Free a colour that has been allocated with 'newColorName' or 'newColorValue'.
freeColor :: X11.Display -> X11.Visual -> X11.Colormap -> Color -> IO ()
freeColor dpy v cm col@(Color ptr) = do
  xftColorFree dpy v cm col
  free ptr

-- | As 'newColorName', but automatically freed through the given Xft
-- manager when no longer accessible.
openColorName :: XftMgr -> X11.Visual -> X11.Colormap -> String
              -> IO (Maybe Color)
openColorName mgr vis cm =
  openAnyWith mgr xftObjs open close (\(Color ptr) -> return $ ptrToIntPtr ptr)
  where open = newColorName (mgrDisplay mgr) vis cm
        close = freeColor (mgrDisplay mgr) vis cm

-- | As 'newColorValue', but automatically freed through the given Xft
-- manager when no longer accessible.
openColorValue :: XftMgr -> X11.Visual -> X11.Colormap -> RenderColor
               -> IO (Maybe Color)
openColorValue mgr vis cm =
  openAnyWith mgr xftObjs open close (\(Color ptr) -> return $ ptrToIntPtr ptr)
  where open = newColorValue (mgrDisplay mgr) vis cm
        close = freeColor (mgrDisplay mgr) vis cm

-- | An Xft drawable.
newtype Draw = Draw (Ptr Draw)

foreign import ccall "XftDrawCreate"
  xftDrawCreate :: X11.Display -> X11.Drawable -> X11.Visual -> X11.Colormap -> IO Draw

foreign import ccall "XftDrawCreateBitmap"
  xftDrawCreateBitmap :: X11.Display -> X11.Pixmap -> IO Draw

foreign import ccall "XftDrawCreateAlpha"
  xftDrawCreateAlpha :: X11.Display -> X11.Pixmap -> CInt -> IO Draw

foreign import ccall "XftDrawChange"
  xftDrawChange :: Draw -> X11.Drawable -> IO ()

foreign import ccall "XftDrawDisplay"
  xftDrawDisplay :: Draw -> IO X11.Display -- FIXME correct? Is X11 giving us the underlying Display?

foreign import ccall "XftDrawDrawable"
  xftDrawDrawable :: Draw -> IO X11.Drawable

foreign import ccall "XftDrawColormap"
  xftDrawColormap :: Draw -> IO X11.Colormap

foreign import ccall "XftDrawVisual"
  xftDrawVisual :: Draw -> IO X11.Visual

foreign import ccall "XftDrawDestroy"
  xftDrawDestroy :: Draw -> IO ()

-- | The display for the Xft drawable.
display :: Draw -> X11.Display
display = unsafePerformIO . xftDrawDisplay

-- | The colormap for the Xft drawable.
colormap :: Draw -> X11.Colormap
colormap = unsafePerformIO . xftDrawColormap

-- | The visual for the Xft drawable.
visual :: Draw -> X11.Visual
visual = unsafePerformIO . xftDrawVisual

-- | The X11 drawable underlying the Xft drawable.
drawable :: Draw -> X11.Drawable
drawable = unsafePerformIO . xftDrawDrawable

-- | Create a new Xft drawable on the given display, using the
-- provided 'X11.Drawable' to draw on.  Will return 'Nothing' if the
-- call to @XftDrawCreate@ fails, which it will usually only do if
-- memory cannot be allocated.  The 'Draw' has to be manually freed
-- with 'freeDraw' once you are done with it.
newDraw :: X11.Display -> X11.Drawable -> X11.Visual -> X11.Colormap
        -> IO (Maybe Draw)
newDraw dpy drw vis cm = do
  Draw ptr <- xftDrawCreate dpy drw vis cm
  if ptr == nullPtr then return Nothing
                     else return $ Just $ Draw ptr

-- | Behaves as 'newDraw', except that it uses a 'X11.Pixmap' of color
-- depth 1 instead of a 'X11.Drawable'.
newDrawBitmap :: X11.Display -> X11.Pixmap -> IO (Maybe Draw)
newDrawBitmap dpy pm = do
  Draw ptr <- xftDrawCreateBitmap dpy pm
  if ptr == nullPtr then return Nothing
                    else return $ Just $ Draw ptr

-- | Behaves as 'newDraw', except that it uses a 'X11.Pixmap' of the given depth
-- instead of a 'X11.Drawable'.
newDrawAlpha :: Integral a => X11.Display -> X11.Pixmap -> a -> IO (Maybe Draw)
newDrawAlpha dpy pm k = do
  Draw ptr <- xftDrawCreateAlpha dpy pm $ fi k
  if ptr == nullPtr then return Nothing
                    else return $ Just $ Draw ptr

-- | Free a 'Draw' created with 'newDraw'.  Do not free 'Draw's
-- created with 'openDraw', these are automatically managed.
freeDraw :: Draw -> IO ()
freeDraw = xftDrawDestroy

-- | Change the X11 drawable underlying the Xft drawable.
changeDraw :: Draw -> X11.Drawable -> IO ()
changeDraw = xftDrawChange

-- | As 'newDraw', but automatically freed when no longer used.
openDraw :: XftMgr -> X11.Drawable -> X11.Visual -> X11.Colormap
           -> IO (Maybe Draw)
openDraw mgr drw vis =
  openAnyWith mgr xftObjs open close (\(Draw ptr) -> return $ ptrToIntPtr ptr)
  where open = newDraw (mgrDisplay mgr) drw vis
        close = freeDraw

-- | As 'newDrawBitmap', but automatically freed when no longer used.
openDrawBitmap :: XftMgr -> X11.Drawable -> IO (Maybe Draw)
openDrawBitmap mgr =
  openAnyWith mgr xftObjs open close (\(Draw ptr) -> return $ ptrToIntPtr ptr)
  where open = newDrawBitmap (mgrDisplay mgr)
        close = freeDraw

-- | As 'newDrawBitmap', but automatically freed when no longer used.
openDrawAlpha :: Integral a => XftMgr -> X11.Drawable -> a -> IO (Maybe Draw)
openDrawAlpha mgr drw  =
  openAnyWith mgr xftObjs open close (\(Draw ptr) -> return $ ptrToIntPtr ptr)
  where open = newDrawAlpha (mgrDisplay mgr) drw
        close = freeDraw

-- | An Xft font.
newtype Font = Font (Ptr Font)

foreign import ccall "XftFontOpenName"
  xftFontOpenName :: X11.Display -> CInt -> CString -> IO Font

foreign import ccall "XftFontOpenXlfd"
  xftFontOpenXlfd :: X11.Display -> CInt -> CString -> IO Font

foreign import ccall "XftLockFace"
  xftLockFace :: Font -> IO ()                  -- FIXME XftLockFace returns FT_face not void

foreign import ccall "XftUnlockFace"
  xftUnlockFace :: Font -> IO ()

foreign import ccall "XftFontClose"
  xftFontClose :: X11.Display -> Font -> IO ()

-- | The ascent (vertical distance upwards from the baseline) of a
-- character in the font.
ascent :: Integral a => Font -> a
ascent (Font p) = fi $ unsafePerformIO $ peekCUShort p #{offset XftFont, ascent}

-- | The descent (vertical distance downwards from the baseline) of a
-- character in the font.
descent :: Integral a => Font -> a
descent (Font p) = fi $ unsafePerformIO $ peekCUShort p #{offset XftFont, descent}

-- | The ascent plus descent of a character in the font.
height :: Integral a => Font -> a
height (Font p) = fi $ unsafePerformIO $ peekCUShort p #{offset XftFont, height}

-- | The greatest horizontal width of a character in the font.
maxAdvanceWidth :: Integral a => Font -> a
maxAdvanceWidth (Font p) = fi $ unsafePerformIO $ peekCUShort p #{offset XftFont, height}

-- | @newFontName dpy scr s@, where @s@ is a Fontconfig pattern
-- string, finds the best match for @s@ and returns a font that can be
-- used to draw on the given screen.  This function very rarely
-- returns 'Nothing', and seems to return some default font even if
-- you feed it utter garbage (or an empty string).
newFontName :: X11.Display -> X11.Screen -> String -> IO (Maybe Font)
newFontName dpy screen fontname =
  withCAString fontname $ \fontname' -> do
    Font ptr <- xftFontOpenName dpy
                (fi (X11.screenNumberOfScreen screen)) fontname'
    if ptr == nullPtr then return Nothing else return $ Just $ Font ptr

-- | As 'newFontName', except that the name should be an X Logical
-- Font Description (the usual fourteen elements produced by
-- @xfontsel@).
newFontXlfd :: X11.Display -> X11.Screen -> String -> IO (Maybe Font)
newFontXlfd dpy screen xlfd =
  withCAString xlfd $ \xlfd' -> do
    Font ptr <- xftFontOpenXlfd dpy
                (fi (X11.screenNumberOfScreen screen)) xlfd'
    if ptr == nullPtr then return Nothing else return $ Just $ Font ptr

-- | Close the given Xft font.
freeFont :: X11.Display -> Font -> IO ()
freeFont = xftFontClose

-- | As 'newFontName', but automatically freed when no longer used.
openFontName :: XftMgr -> String -> IO (Maybe Font)
openFontName mgr =
  openAnyWith mgr xftObjs open close (\(Font ptr) -> return $ ptrToIntPtr ptr)
  where open = newFontName (mgrDisplay mgr) (mgrScreen mgr)
        close = freeFont (mgrDisplay mgr)

-- | As 'newFontXfld', but automatically freed when no longer used.
openFontXlfd :: XftMgr -> String -> IO (Maybe Font)
openFontXlfd mgr =
  openAnyWith mgr xftObjs open close (\(Font ptr) -> return $ ptrToIntPtr ptr)
  where open = newFontXlfd (mgrDisplay mgr) (mgrScreen mgr)
        close = freeFont (mgrDisplay mgr)

-- | Lock the file underlying the Xft font.  I am not certain when you
-- would need this.  The return value is supposed to be an @FT_TYPE@
-- from Freetype, but that binding has not been written yet.
lockFace :: Font -> IO ()
lockFace font = xftLockFace font >> return ()

-- | Unlock a face locked by 'lockFontFace'.
unlockFace :: Font -> IO ()
unlockFace = xftUnlockFace

foreign import ccall "XftTextExtentsUtf8"
  xftTextExtentsUtf8 :: X11.Display -> Font -> CString -> CInt -> Ptr GlyphInfo -> IO ()

-- | Note that the 'glyphWidth'/'glyphHeight' fields are the number of
-- pixels you should advance after drawing a string of this size.
textExtents :: X11.Display -> Font -> String -> IO GlyphInfo
textExtents dpy font s =
  withArrayLen (map fi (UTF8.encode s)) $ \n s' ->
    alloca $ \cglyph -> do
      xftTextExtentsUtf8 dpy font s' (fi n) cglyph
      peek cglyph

-- | Shortcut for calling 'textExtents' and picking out the
-- 'glyphWidth' field of the 'GlyphInfo'.
textWidth :: Integral a => X11.Display -> Font -> String -> IO a
textWidth dpy f = liftM (fi . glyphWidth) . textExtents dpy f

-- | Shortcut for calling 'textExtents' and picking out the
-- 'glyphHeight' field of the 'GlyphInfo'.
textHeight :: Integral a => X11.Display -> Font -> String -> IO a
textHeight dpy f = liftM (fi . glyphHeight) . textExtents dpy f

foreign import ccall "XftDrawGlyphs"
  xftDrawGlyphs :: Draw -> Color -> Font -> CInt -> CInt -> Ptr (Word32) -> CInt -> IO ()

-- | Draw a sequence of glyphs on the given drawable in the specified
-- colour and font.  Drawing begins at the baseline of the string.
drawGlyphs :: (Integral x, Integral y, Integral c) => Draw -> Color -> Font ->
              x -> y -> [c] -> IO ()
drawGlyphs drw col font x y s =
  withArrayLen (map fi s) $ \len s' ->
    xftDrawGlyphs drw col font (fi x) (fi y) s' (fi len)

foreign import ccall "XftDrawStringUtf8"
  xftDrawStringUtf8 :: Draw -> Color -> Font -> CInt -> CInt -> Ptr (Word8) -> CInt -> IO ()

-- | Draw a string on the given drawable in the specified colour and
-- font.  Drawing begins at the baseline of the string.
drawString :: (Integral x, Integral y) => Draw -> Color -> Font ->
              x -> y -> String -> IO ()
drawString drw col font x y s =
  withArrayLen (map fi (UTF8.encode s)) $ \len s' ->
    xftDrawStringUtf8 drw col font (fi x) (fi y) s' (fi len)

foreign import ccall "XftDrawRect"
  xftDrawRect :: Draw -> Color -> CInt -> CInt -> CUInt -> CUInt -> IO ()

-- | @drawRect d c x y w h@ draws a solid rectangle on @d@ with colour
-- @c@, with its upper left corner at @(x,y)@, width @w@ and height
-- @h@.
drawRect :: (Integral x, Integral y, Integral w, Integral h) =>
            Draw -> Color -> x -> y -> w -> h -> IO ()
drawRect d c x y w h = xftDrawRect d c (fi x) (fi y) (fi w) (fi h)

-- | Short-hand for 'fi'.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

peekCULong :: Ptr a -> CInt -> IO Int
peekCULong ptr off = do
  v <- peekByteOff ptr (fromIntegral off)
  return (fromIntegral (v::CULong))

peekCUShort :: Ptr a -> CInt -> IO Int
peekCUShort ptr off = do
  v <- peekByteOff ptr (fromIntegral off)
  return (fromIntegral (v::CUShort))

pokeCUShort :: Ptr a -> CInt -> Int -> IO ()
pokeCUShort ptr off v =
  pokeByteOff ptr (fromIntegral off) (fromIntegral v::CUShort)

peekCShort :: Ptr a -> CInt -> IO Int
peekCShort ptr off = do
  v <- peekByteOff ptr (fromIntegral off)
  return (fromIntegral (v::CShort))

pokeCShort :: Ptr a -> CInt -> Int -> IO ()
pokeCShort ptr off v =
  pokeByteOff ptr (fromIntegral off) (fromIntegral v::CShort)

-- | The @XRenderColor@ from the XRender library.  Note that the
-- colour channels are only interpreted as 16-bit numbers when
-- actually used.
data RenderColor = RenderColor {
    red   :: Int
  , green :: Int
  , blue  :: Int
  , alpha :: Int
}

instance Storable RenderColor where
  sizeOf _ = #{size XRenderColor}
  alignment _ = alignment (undefined::CInt)
  peek p = do
    r <- peekCUShort p #{offset XRenderColor, red}
    g <- peekCUShort p #{offset XRenderColor, green}
    b <- peekCUShort p #{offset XRenderColor, blue}
    a <- peekCUShort p #{offset XRenderColor, alpha}
    return (RenderColor r g b a)
  poke p (RenderColor r g b a) = do
    pokeCUShort p #{offset XRenderColor,red} r
    pokeCUShort p #{offset XRenderColor,green} g
    pokeCUShort p #{offset XRenderColor,blue} b
    pokeCUShort p #{offset XRenderColor,alpha} a

-- | The size of some glyph(s).  Note that there's a difference
-- between the logical size, which may include some blank pixels, and
-- the actual bitmap.
data GlyphInfo = GlyphInfo {
    glyphImageWidth  :: Int
  , glyphImageHeight :: Int
  , glyphImageX      :: Int
  , glyphImageY      :: Int
  , glyphWidth       :: Int
  , glyphHeight      :: Int
}

instance Storable GlyphInfo where
  sizeOf _ = #{size XGlyphInfo}
  alignment _ = alignment (undefined::CInt)
  peek p = do
    w  <- peekCUShort p #{offset XGlyphInfo, width}
    h <- peekCUShort p #{offset XGlyphInfo, height}
    x <- peekCShort p #{offset XGlyphInfo, x}
    y <- peekCShort p #{offset XGlyphInfo, y}
    xOff <- peekCShort p #{offset XGlyphInfo, xOff}
    yOff <- peekCShort p #{offset XGlyphInfo, yOff}
    return (GlyphInfo w h x y xOff yOff)
  poke p (GlyphInfo w h x y xOff yOff) = do
    pokeCUShort p #{offset XGlyphInfo, width} w
    pokeCUShort p #{offset XGlyphInfo, height} h
    pokeCShort p #{offset XGlyphInfo, x} x
    pokeCShort p #{offset XGlyphInfo, y} y
    pokeCShort p #{offset XGlyphInfo, xOff} xOff
    pokeCShort p #{offset XGlyphInfo, yOff} yOff
