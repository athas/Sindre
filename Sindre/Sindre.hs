{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.Sindre
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  provisional
-- Portability :  portable
--
-- General definitions for the Sindre programming language.  The
-- documentation for this module does not include a description of the
-- language semantics.
--
-----------------------------------------------------------------------------
module Sindre.Sindre ( 
  -- * Screen layout
  Rectangle(..),
  DimNeed(..),
  SpaceNeed,
  SpaceUse,
  Constraints,
  Align(..),
  -- ** Layouting functions
  constrainNeed,
  fitRect,
  splitHoriz,
  splitVert,
  rectTranspose,
  align,
  adjustRect,
  -- * Keyboard Input
  KeyModifier(..),
  Key(..),
  Chord,
  -- * Input positions
  P(..),
  at,
  SourcePos,
  nowhere,
  position,
  -- * Abstract syntax tree
  Identifier,
  Stmt(..),
  Expr(..),
  ObjectNum,
  ObjectRef,
  WidgetRef,
  -- ** Value representation
  Value(..),
  string,
  true,
  truth,
  falsity,
  -- ** Program structure
  Event(..),
  EventSource(..),
  SourcePat(..),
  Pattern(..),
  Action(..),
  Function(..),
  GUI(..),
  Program(..),
  SindreOption,
  Arguments
                     )
    where

import System.Console.GetOpt

import Control.Applicative
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T

-- | A rectangle represented as its upper-left corner, width and
-- height.  You should never create rectangles with negative
-- dimensions, and the functions in this module make no guarantee to
-- their behaviour if you do.
data Rectangle = Rectangle {
      rectX      :: Integer
    , rectY      :: Integer
    , rectWidth  :: Integer
    , rectHeight :: Integer
    } deriving (Show, Eq)

instance Monoid Rectangle where
  mempty = Rectangle 0 0 0 0
  mappend (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
    Rectangle (min x1 x2) (min y1 y2)
              (max (x1+w1) (x2+w2)) (max (y1+h1) (y2+h2))

-- | Flip the x and y coordinates and width and height of a rectangle,
-- in a sense rotating it ninety degrees.  Note that @rectTranspose
-- . rectTranspose = id@.
rectTranspose :: Rectangle -> Rectangle
rectTranspose (Rectangle x y w h) = Rectangle y x h w

zipper :: (([a], a, [a]) -> ([a], a, [a])) -> [a] -> [a]
zipper f = zipper' []
    where zipper' a (x:xs) = let (a', x', xs') = f (a, x, xs)
                             in zipper' (x':a') xs'
          zipper' a [] = reverse a

divide :: Integral a => a -> a -> [a]
divide total n = map (\i -> if i == n-1
                            then total-quant*i
                            else quant)
                 [0..n-1]
    where quant = total `div` n

-- | @splitHoriz rect dims@ splits @rect@ horizontally into a number
-- of non-overlapping equal-width rectangles stacked on top of each
-- other.  @dims@ is a list of height requirements that the function
-- will attempt to fulfill as best it is able.  The union of the list
-- of returned rectangles will always be equal to @rect@.  No
-- rectangle will ever have negative dimensions.
splitHoriz :: Rectangle -> [DimNeed] -> [Rectangle]
splitHoriz (Rectangle x1 y1 w h) parts =
    snd $ mapAccumL mkRect y1 $ map fst $
        zipper adjust $ zip (divide h nparts) parts
    where nparts = genericLength parts
          mkRect y h' = (y+h', Rectangle x1 y w h')
          frob d (v, Min mv) = let d' = min d $ max 0 $ v-mv
                               in ((v-d', Min mv), d-d')
          frob d (v, Max mv) | d > 0     = let d' = min v d
                                           in ((v-d', Max mv), d-d')
                             | otherwise = let d' = max 0 $ min (mv-v) d
                                           in ((v-d', Max mv), d-d')
          frob d (v, Unlimited) = let v' = max 0 $ v - d
                                  in ((v', Unlimited), v'-v+d)
          frob d (v, Exact ev) | v > ev = let d' = min v $ min d $ v-ev
                                          in ((v-d', Exact ev), d-d')
          frob d (v, Exact ev) | v < ev = let d' = min v $ min d $ ev-v
                                          in ((v-d', Exact ev), d-d')
          frob d (v, Exact ev) = ((v, Exact ev), d)
          nunlims = genericLength . filter ((==Unlimited) . snd)
          frobunlim ((d:ds, r)) (v, Unlimited) =
            let v' = max 0 $ v - d
            in ((ds, r-min v d), (v', Unlimited))
          frobunlim a x = (a, x)
          obtain _ [] []   = ([], [], 0)
          obtain v bef aft =
            let q = divide v (nparts-1)
                n = length bef
                (bef', x) = unzip $ zipWith frob q bef
                (aft', y) = unzip $ zipWith frob (drop n q) aft
                r = sum x+sum y
                q' = divide r $ max 1 $ nunlims $ bef'++aft'
                ((q'', r'), bef'') = mapAccumL frobunlim (q', r) bef'
                ((_, r''), aft'') = mapAccumL frobunlim (q'', r') aft'
            in  (bef'',aft'', v-r'')
          adjust (bef, (v, Min mv), aft)
            | v < mv = adjust' Min bef v mv aft
          adjust (bef, (v, Max mv), aft)
            | v > mv = adjust' Max bef v mv aft
          adjust (bef, (v, Exact ev), aft)
            | v /= ev = adjust' Exact bef v ev aft
          adjust x = x
          adjust' f bef v mv aft =
            let (bef', aft', d) = obtain (mv-v) bef aft
            in (bef', (v+d, f mv), aft')

-- | As @splitHoriz@, but splits vertically instead of horizontally,
-- so the rectangles will be next to each other.
splitVert :: Rectangle -> [DimNeed] -> [Rectangle]
splitVert r = map rectTranspose . splitHoriz (rectTranspose r)

-- | A size constraint in one dimension.
data DimNeed = Min Integer -- ^ At minimum this many pixels.
             | Max Integer -- ^ At most this many pixels.
             | Unlimited -- ^ As many or as few pixels as necessary.
             | Exact Integer -- ^ Exactly this many pixels.
         deriving (Eq, Show, Ord)

-- | Size constraints in both dimensions.
type SpaceNeed = (DimNeed, DimNeed)

-- | The amount of space actually used by a widget.
type SpaceUse = [Rectangle]

-- | Externally-imposed optional minimum and maximum values for width
-- and height.
type Constraints = ( (Maybe Integer, Maybe Integer)
                   , (Maybe Integer, Maybe Integer))

-- | @constrainNeed need constraints@ reduces the space requirement
-- given by @need@ in order to fulfill @constraints@.
constrainNeed :: SpaceNeed -> Constraints -> SpaceNeed
constrainNeed (wreq, hreq) ((minw, maxw), (minh, maxh)) =
  (f wreq minw maxw, f hreq minh maxh)
    where f x Nothing Nothing = x
          f (Max x) (Just y) _ | x > y = Min x
          f (Max _) (Just y) _ = Max y
          f _ (Just y) _ = Min y
          f _ _ (Just y) = Max y

-- | @fitRect rect need@ yields a rectangle no larger than @rect@ that
-- tries to fulfill the constraints @need@.
fitRect :: Rectangle -> SpaceNeed -> Rectangle
fitRect (Rectangle x y w h) (wn, hn) =
  Rectangle x y (fit w wn) (fit h hn)
    where fit d dn = case dn of
                      Max dn'   -> min d dn'
                      Min dn'   -> min d $ max dn' d
                      Exact ev  -> min d ev
                      Unlimited -> d

-- | Instruction on how to align a smaller interval within a larger
-- interval.
data Align = AlignNeg -- ^ Align towards negative infinity.
           | AlignPos -- ^ Align towards positive infinity.
           | AlignCenter -- ^ Align towards the center of the interval.
             deriving (Show, Eq)

-- | @align a lower x upper@, where @lower<=upper@, aligns a
-- subinterval of length @x@ in the interval @lower@ to @upper@,
-- returning the coordinate at which the aligned subinterval starts.
-- For example,
--
-- >>> align AlignCenter 2 4 10
-- 4
-- >>> align AlignNeg 2 4 10
-- 2
-- >>> align AlignPos 2 4 10
-- 6
align :: Integral a => Align -> a -> a -> a -> a
align AlignCenter minp d maxp = minp + (maxp - minp - d) `div` 2
align AlignNeg minp _ _ = minp
align AlignPos _ d maxp = maxp - d

-- | @adjustRect (walign, halign) bigrect smallrect@ returns a
-- rectangle with the same dimensions as @smallrect@ aligned within
-- @bigrect@ in both dimensions.
adjustRect :: (Align, Align) -> Rectangle -> Rectangle -> Rectangle
adjustRect (walign, halign) (Rectangle sx sy sw sh) (Rectangle _ _ w h) =
    Rectangle cx' cy' w h
    where cx' = frob walign sx w sw
          cy' = frob halign sy h sh
          frob AlignCenter c d maxv = c + (maxv - d) `div` 2
          frob AlignNeg c _ _ = c
          frob AlignPos c d maxv = c + maxv - d

-- | A keyboard modifier key.  The precise meaning (and location) of
-- these is somewhat platform-dependent.  Note that the @Shift@
-- modifier should not be passed along if the associated key is a
-- @CharKey@, as @Shift@ will already have been handled.
data KeyModifier = Control | Meta | Super | Hyper | Shift
                   deriving (Eq, Ord, Show)

-- | Either a key corresponding to a visible character, or a control
-- key not associated with any character.
data Key = CharKey Char -- ^ Unicode character associated with the key.
         | CtrlKey String -- ^ Name of the control key, using X11
                          -- key names (for example @BackSpace@ or
                          -- @Return@).
    deriving (Show, Eq, Ord)

-- | A combination of a set of modifier keys and a primary key,
-- representing a complete piece of keyboard input.
type Chord = (S.Set KeyModifier, Key)

-- | Low-level reference to an object.
type ObjectNum = Int
-- | High-level reference to an object, containing its class and name
-- (if any) as well.  For non-widgets, the object name is the same as
-- the object class.
type ObjectRef = (ObjectNum, Identifier, Maybe Identifier)
-- | High-level reference to a widget.
type WidgetRef = ObjectRef

-- | The type of names (such as variables and classes) in the syntax
-- tree.
type Identifier = String

-- | Dynamically typed run-time value in the Sindre language.
data Value = StringV T.Text
           | IntegerV Integer
           | Reference ObjectRef
           | Dict (M.Map Value Value)
             deriving (Eq, Ord)

instance Show Value where
  show (IntegerV v) = show v
  show (Reference (_,_,Just k)) = k
  show (Reference (r,c,Nothing)) = "#<" ++ c ++ " at " ++ show r ++ ">"
  show (Dict m) = "#<dictionary with "++show (M.size m)++" entries>"
  show (StringV v) = T.unpack v

-- | @string s@ returns a Sindre string.
string :: String -> Value
string = StringV . T.pack

-- | @true v@ returns 'True' if @v@ is interpreted as a true value in
-- Sindre, 'False' otherwise.
true :: Value -> Bool
true (IntegerV 0) = False
true (StringV s) = s /= T.empty
true (Dict m) = m /= M.empty
true _ = True

-- | Canonical false value, see 'true'.
truth, falsity :: Value
-- ^ Canonical true value, see 'true'.
truth = IntegerV 1
falsity = IntegerV 0

-- | A position in a source file, consisting of a file name,
-- one-indexed line number, and one-indexed column number.
type SourcePos = (String, Int, Int)

-- | A default position when no other is available.
nowhere :: SourcePos
nowhere = ("<nowhere>", 0, 0)

-- | Prettyprint a source position in a human-readable form.
--
-- >>> position ("foobar.sindre", 5, 15)
-- "foobar.sindre:5:15: "
position :: SourcePos -> String
position (file, line, col) =
  file ++ ":" ++ show line ++ ":" ++ show col ++ ": "

-- | Wrap a value with source position information.
data P a = P { sourcePos :: SourcePos, unP :: a }
    deriving (Show, Eq, Ord, Functor)

-- | @x `at` y@ gives a value containing @x@, but with the same source
-- position as @y@.
at :: a -> P b -> P a
at e1 e2 = const e1 <$> e2

-- | The syntax of Sindre statements.
data Stmt = Print [P Expr]
          | Exit (Maybe (P Expr))
          | Return (Maybe (P Expr))
          | Next
          | If (P Expr) [P Stmt] [P Stmt]
          | While (P Expr) [P Stmt]
          | For (P Expr) (P Expr) (P Expr) [P Stmt]
          | Do [P Stmt] (P Expr)
          | Break
          | Continue
          | Expr (P Expr)
          | Focus (P Expr)
            deriving (Show, Eq)

-- | The syntax of Sindre expressions.
data Expr = Literal Value
          | Var Identifier
          | FieldOf Identifier (P Expr)
          | Lookup Identifier (P Expr)
          | Not (P Expr)
          | LessThan (P Expr) (P Expr)
          | LessEql (P Expr) (P Expr)
          | Equal (P Expr) (P Expr)
          | Assign (P Expr) (P Expr)
          | PostInc (P Expr)
          | PostDec (P Expr)
          | Concat (P Expr) (P Expr)
          | Plus (P Expr) (P Expr)
          | Minus (P Expr) (P Expr)
          | Times (P Expr) (P Expr)
          | Divided (P Expr) (P Expr)
          | Modulo (P Expr) (P Expr)
          | RaisedTo (P Expr) (P Expr)
          | Funcall Identifier [P Expr]
          | Methcall (P Expr) Identifier [P Expr]
          | Cond (P Expr) (P Expr) (P Expr)
            deriving (Show, Eq, Ord)

-- | Something that happened in the world.
data Event = KeyPress Chord
           | NamedEvent { eventName   :: Identifier  -- ^ The name of the event.
                        , eventValue  :: [Value]     -- ^ The payload of the event.
                        , eventSource :: EventSource -- ^ Where it's from.
                        }
             deriving (Show)

-- | The origin of an event.  This is used when determining where to
-- handle it.
data EventSource = FieldSrc ObjectRef Identifier
                   -- ^ @FieldSrc obj f@ designates that the source of
                   -- the event is the property @f@ of @obj@
                 | ObjectSrc ObjectRef -- ^ The source is the given object.
                 | BackendSrc -- ^ The source is something within the
                              -- bowels of the active backend,
                              -- probably from the external world.
        deriving (Show)

-- | Description of sets of sources, values of this type can be used
-- to pattern-match @EventSource@s.
data SourcePat = NamedSource Identifier (Maybe Identifier)
               -- ^ For @NamedSource k fk@, the source must be the
               -- object named @k@.  If @fk@ is @Just fk'@, the source
               -- must also be the field named @fk'@.
               | GenericSource Identifier Identifier (Maybe Identifier)
                 -- ^ For @GenericSource cn k fk@, the source must be
                 -- of class @cn@.  If @fk@ is @Just fk'@, the source
                 -- must also be the field named @fk'@.  The variable
                 -- named @k@ should be bound to the actual object if
                 -- this pattern matches.
                 deriving (Eq, Ord, Show)

-- | A description of an event used to indicate how to handle
-- different events.
data Pattern = ChordPattern Chord -- ^ Match if the event is a chord.
             | OrPattern Pattern Pattern -- ^ Match if either pattern
                                         -- matches.
             | SourcedPattern { patternSource :: SourcePat
                              , patternEvent  :: Identifier
                              , patternVars   :: [Identifier]
                              }
               -- ^ @SourcedPattern src ev vars@ matches if @src@
               -- matches the event source (see 'SourcePat') an @ev@
               -- matches the event name.  @vars@ should be bound to
               -- the values in the payload of the event.
               deriving (Eq, Ord, Show)

-- | A function consists of lexically bound parameters and a body.
data Function = Function [Identifier] [P Stmt]
              deriving (Show, Eq)

-- | Reaction to an event.
data Action = StmtAction [P Stmt] -- ^ Execute these statements.
              deriving (Show)

-- | Widget arguments are key-value pairs, with a unique value for
-- each key.
type WidgetArgs = M.Map Identifier (P Expr)

-- | A Sindre GUI is a recursive tree, with each node representing a
-- single widget and consisting of the following fields.
data GUI = GUI {
      widgetName :: Maybe Identifier -- ^ Name of the widget, if any.
    , widgetClass :: P Identifier -- ^ Class of the widget.
    , widgetArgs :: WidgetArgs -- ^ The arguments passed to the widget.
    , widgetChildren :: [(Maybe (P Expr), GUI)] -- ^ Children of the widget, if any.
    } deriving (Show)

-- | A command line argument.
type SindreOption = OptDescr (Arguments -> Arguments)
-- | The arguments passed to the Sindre program from the command line.
type Arguments = M.Map String String

-- | A complete Sindre program.  Note that this is intentionally
-- defined such that some invalid programs, like those with duplicate
-- definitions can be represented - the compiler (see
-- "Sindre.Compiler") should detect and handle such errors.
data Program = Program {
      programGUI       :: (Maybe (P Expr), GUI)
    , programActions   :: [P (Pattern, Action)]
    , programGlobals   :: [P (Identifier, P Expr)]
    , programOptions   :: [P (Identifier, (SindreOption, Maybe Value))]
    , programFunctions :: [P (Identifier, Function)]
    , programBegin     :: [P Stmt] -- ^ The contents of the @BEGIN@ block.
    }
