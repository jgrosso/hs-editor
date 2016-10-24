module Cursor
  ( CursorMovement(..)
  , cursorText
  , moveCursor
  ) where


import Control.Lens ((%~), (^.))

import Editor

import Types (Command)

import Utils (clamp)


default (Int)


data CursorMovement = MoveDown
                    | MoveLeft
                    | MoveRight
                    | MoveUp


cursorText :: String
cursorText = "█"


clampCursor :: (Position -> Position) -> FocusedInputLens -> Command
clampCursor f focusedInput editorState = return $ (focusedInput.cursor.position %~ clamp 0 (length (editorState^.focusedInput.text)) f) editorState


moveCursor :: CursorMovement -> Command
moveCursor MoveDown _            = undefined
moveCursor MoveLeft editorState  = clampCursor (subtract 1) (findFocusedInputLens editorState) editorState
moveCursor MoveRight editorState = clampCursor (+ 1) (findFocusedInputLens editorState) editorState
moveCursor MoveUp _              = undefined
