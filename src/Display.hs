module Display
  ( displayEditorState
  ) where


import Control.Lens ((%~), (^.))

import Cursor (cursorText)

import Editor


default (Int)



spliceIn :: String -> Int -> String -> String
spliceIn value position' input = take position' input ++ value ++ drop position' input

spliceInCursor :: EditorState -> EditorState
spliceInCursor editorState = (focusedInput.text %~ spliceIn cursorText (editorState^.focusedInput.cursor.position)) editorState
    where focusedInput :: FocusedInputLens
          focusedInput = findFocusedInputLens editorState

displayBuffer :: EditorState -> IO ()
displayBuffer = putStrLn . (^.buffer.text)


displayMinibuffer :: EditorState -> IO ()
displayMinibuffer editorState = putStr $ "> " ++ if not $ null $ editorState^.minibuffer.commandInput.text
                                                  then editorState^.minibuffer.commandInput.text
                                                  else editorState^.minibuffer.message


displayEditorState :: EditorState -> IO ()
displayEditorState editorState = displayBuffer editorStateWithCursor >> displayMinibuffer editorStateWithCursor
    where editorStateWithCursor :: EditorState
          editorStateWithCursor = spliceInCursor editorState
