module Lib
  ( runEditor
  ) where


import Commands (keySequenceToCommand)

import Control.Lens ((.~))

import Display (displayEditorState)

import Editor

import Terminal (clearScreen, getRawChar, RawChar(..), rawCharToKeySequence)


default (Int)


processInput :: EditorState -> IO ()
processInput editorState = do input@(RawChar rawChar) <- getRawChar
                              case rawCharToKeySequence input of
                                Just keySequence -> do case keySequenceToCommand keySequence of
                                                        Just command -> command editorState >>= mainLoop
                                                        Nothing      -> mainLoop $ (minibuffer.message .~ "Unbound key!") editorState
                                Nothing   -> mainLoop $ (minibuffer.message .~ ("Unrecognized key: " ++ show rawChar)) editorState


mainLoop :: EditorState -> IO ()
mainLoop editorState = clearScreen >> displayEditorState editorState >> processInput editorState


runEditor :: IO ()
runEditor = do clearScreen
               mainLoop initEditorState
