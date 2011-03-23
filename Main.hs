-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  unportable
--
-- Sindre, a programming language for writing simple GUIs
--
-----------------------------------------------------------------------------

module Main(main)
    where

import Sindre.Compiler
import Sindre.Runtime
import Sindre.Widgets
import Sindre.Parser
import Sindre.Sindre
import Sindre.Util
import Sindre.X11

import Control.Applicative

import System.Environment
import System.Exit
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  dstr <- getEnv "DISPLAY" `catch` const (return "")
  args <- getArgs
  result <- parseSindre (args!!0) <$> readFile (args!!0)
  case result of
    Left err -> error $ show err
    Right program -> do sindreX11 program classMap dstr
                        exitSuccess
  
classMap :: ClassMap SindreX11M
classMap = M.fromList [ ("Dial", sizeable mkDial)
                      , ("Horizontally", mkHorizontally) 
                      , ("Vertically", mkVertically)
                      ]
{-
dialProgram :: Program
dialProgram = do
  Program { programGUI = gui
          , programActions = M.fromList [ {-readStdin, onChange, -}onQ, onN, onP ] }
    where readStdin = ( SourcedPattern { patternSource = NamedSource "stdin"
                                       , patternEvent  = "data" 
                                       , patternVars   = ["data"] }
                      , ExprAction $ "value" `FieldOf` Var "dial" `Assign` Var "data" )
          onChange  = ( SourcedPattern { patternSource = NamedSource "dial"
                                       , patternEvent  = "changed" 
                                       , patternVars   = ["from", "to"] }
                      , ExprAction $ Print [Var "data"] )
          onQ = ( KeyPattern (S.empty, CharacterKey "q")
                , ExprAction Quit )
          onN = ( KeyPattern (S.empty, CharacterKey "n")
                , ExprAction [("value" `FieldOf` Var "dial1") `Assign` 
                              (("value" `FieldOf` Var "dial1") `Plus`
                               Literal (IntegerV 1))] )
          onP = ( KeyPattern (S.empty, CharacterKey "p")
                , ExprAction [("value" `FieldOf` Var "dial1") `Assign` 
                              (("value" `FieldOf` Var "dial1") `Plus`
                               Literal (IntegerV (-1)))] )
          gui = GUI Nothing "Horizontally" (M.fromList [ ("maxheight", Literal $ IntegerV 400)
                                                       , ("maxwidth", Literal $ IntegerV 400)
                                                       , ("valign", Literal $ StringV "center")
                                                       , ("halign", Literal $ StringV "center") ])
                [ (Nothing, GUI (Just "dial1") "Dial" (M.fromList [ ("max", Literal $ IntegerV 120)
                                                                  , ("maxheight", Literal $ IntegerV 40)
                                                                  , ("valign", Literal $ StringV "center")]) [])
                , (Nothing, GUI (Just "dial2") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) [])
                , (Nothing, GUI (Just "dial3") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) [])
                ]
-}