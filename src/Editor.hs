module Editor where


import Control.Lens ((^.), Lens, makeLenses)


default (Int)


type Position = Int


data CursorState = CursorState
                   { _position :: Position
                   }

makeLenses ''CursorState


data InputState = InputState
                  { _cursor :: CursorState
                  , _text :: String
                  }

makeLenses ''InputState


data MinibufferState = MinibufferState
                       { _message :: String
                       , _commandInput :: InputState
                       }

makeLenses ''MinibufferState


type BufferState = InputState


data InputFocus = BufferFocused
                | MinibufferFocused


data EditorState = EditorState
                   { _buffer     :: BufferState
                   , _minibuffer :: MinibufferState
                   , _inputFocus :: InputFocus
                   }

makeLenses ''EditorState


initEditorState :: EditorState
initEditorState = EditorState (InputState (CursorState 0) "") (MinibufferState "Welcome!" (InputState (CursorState 0) [])) BufferFocused


type FocusedInputLens = Lens EditorState EditorState InputState InputState


findFocusedInputLens :: EditorState -> FocusedInputLens
findFocusedInputLens editorState = case editorState^.inputFocus of
                                     BufferFocused -> buffer
                                     MinibufferFocused -> minibuffer.commandInput
