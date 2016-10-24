module Types
  ( Command
  ) where


import Editor (EditorState)


default (Int)


type Command = EditorState -> IO EditorState
