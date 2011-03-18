{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  unportable
--
-- visp, a programming language for writing simple GUIs
--
-----------------------------------------------------------------------------

module Main(main)
    where

import Visp.Compiler
import Visp.Runtime
import Visp.Widgets
import Visp.Parser
import Visp.Visp
import Visp.Util
import Visp.X11

import System.Environment
import System.Exit
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  dstr <- getEnv "DISPLAY" `catch` const (return "")
  vispX11 dialProgram classMap dstr
  exitSuccess
  
classMap :: ClassMap VispX11M
classMap = M.fromList [ ("Dial", sizeable mkDial)
                      , ("Horizontally", mkHorizontally) 
                      , ("Vertically", mkVertically)
                      ]

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
                , ExprAction $ ("value" `FieldOf` Var "dial1") `Assign` 
                                 (("value" `FieldOf` Var "dial1") `Plus`
                                  Literal (IntegerV 1)) )
          onP = ( KeyPattern (S.empty, CharacterKey "p")
                , ExprAction $ ("value" `FieldOf` Var "dial1") `Assign` 
                                 (("value" `FieldOf` Var "dial1") `Plus`
                                  Literal (IntegerV (-1))) )
          gui = GUI Nothing "Horizontally" (M.fromList [ ("maxheight", Literal $ IntegerV 400) 
                                                       , ("maxwidth", Literal $ IntegerV 400)])
                [ GUI (Just "dial1") "Dial" (M.fromList [ ("max", Literal $ IntegerV 120)
                                                        , ("maxheight", Literal $ IntegerV 40) ]) []
                , GUI (Just "dial2") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                , GUI Nothing "Vertically" M.empty [
                            GUI (Just "dial4") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          , GUI (Just "dial5") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                          
                     ]
                , GUI (Just "dial3") "Dial" (M.singleton "max" $ Literal $ IntegerV 12) []
                ]
