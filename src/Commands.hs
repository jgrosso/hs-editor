module Commands
  ( Command
  , keySequenceToCommand
  ) where


import Control.Lens ((%~), (.~), (^.))

import Cursor (CursorMovement(..), moveCursor)

import Data.Char (toUpper)

import Editor

import Terminal (Key(..), KeySequence(..), ModifierKey(..))

import Types (Command)

import Utils (replicateM')


default (Int)


insertString :: String -> Command
insertString string editorState = insertString' string editorState >>= replicateM' (moveCursor MoveRight) (length string)
  where insertString' :: String -> Command
        insertString' string' = return . ((findFocusedInputLens editorState).text %~ (++ string'))


insertChar :: Char -> Command
insertChar = insertString . (:[])

insertTab :: Command
insertTab = insertString "    "


insertNewline :: Command
insertNewline = insertChar '\n'


handleReturn :: Command
handleReturn editorState = case editorState^.inputFocus of
                             MinibufferFocused -> clearMinibuffer editorState >>= focusBuffer
                             _                 -> insertNewline editorState


backspace :: Command
backspace editorState = if (editorState^.buffer.cursor.position) /= 0
                        then moveCursor MoveLeft editorState >>= deleteChar
                        else return editorState
  where deleteChar :: Command
        deleteChar = return . (buffer.text %~ init)


clearInput :: FocusedInputLens -> Command
clearInput focusedInput = return . (focusedInput .~ InputState { _text = "", _cursor = CursorState { _position = 0 } })


clearMinibuffer :: Command
clearMinibuffer = clearInput (minibuffer.commandInput)


focusMinibuffer :: Command
focusMinibuffer = return . (minibuffer.message .~ "") . (inputFocus .~ MinibufferFocused)


focusBuffer :: Command
focusBuffer = return . (inputFocus .~ BufferFocused)


keySequenceToCommand :: KeySequence -> Maybe Command
keySequenceToCommand keySequence = lookup keySequence $ topRow ++ secondRow ++ thirdRow ++ fourthRow ++ fifthRow
  where letterKey :: (Key, Char) -> [(KeySequence, Command)]
        letterKey (key', char) = [ (KeySequence [] key', insertChar char)
                                 , (KeySequence [ShiftKey] key', insertChar $ toUpper char)
                                 ]

        topRow :: [(KeySequence, Command)]
        topRow = [ (KeySequence [] BacktickKey, insertChar '`')
                 , (KeySequence [ShiftKey] BacktickKey, insertChar '~')

                 , (KeySequence [] OneKey, insertChar '1')
                 , (KeySequence [ShiftKey] OneKey, insertChar '!')

                 , (KeySequence [] TwoKey, insertChar '2')
                 , (KeySequence [ShiftKey] TwoKey, insertChar '@')

                 , (KeySequence [] ThreeKey, insertChar '3')
                 , (KeySequence [ShiftKey] ThreeKey, insertChar '#')

                 , (KeySequence [] FourKey, insertChar '4')
                 , (KeySequence [ShiftKey] FourKey, insertChar '$')

                 , (KeySequence [] FiveKey, insertChar '5')
                 , (KeySequence [ShiftKey] FiveKey, insertChar '%')

                 , (KeySequence [] SixKey, insertChar '6')
                 , (KeySequence [ShiftKey] SixKey, insertChar '^')

                 , (KeySequence [] SevenKey, insertChar '7')
                 , (KeySequence [ShiftKey] SevenKey, insertChar '&')

                 , (KeySequence [] EightKey, insertChar '8')
                 , (KeySequence [ShiftKey] EightKey, insertChar '*')

                 , (KeySequence [] NineKey, insertChar '9')
                 , (KeySequence [ShiftKey] NineKey, insertChar '(')

                 , (KeySequence [] ZeroKey, insertChar '0')
                 , (KeySequence [ShiftKey] ZeroKey, insertChar ')')

                 , (KeySequence [] DashKey, insertChar '-')
                 , (KeySequence [ShiftKey] DashKey, insertChar '_')

                 , (KeySequence [] EqualsKey, insertChar '=')
                 , (KeySequence [ShiftKey] EqualsKey, insertChar '+')

                 , (KeySequence [] BackspaceKey, backspace)
                 ]

        secondRow :: [(KeySequence, Command)]
        secondRow = [(KeySequence [] TabKey, insertTab)]
                    ++ (concatMap letterKey
                        [ (QKey, 'q')
                        , (WKey, 'w')
                        , (EKey, 'e')
                        , (RKey, 'r')
                        , (TKey, 't')
                        , (YKey, 'y')
                        , (UKey, 'u')
                        , (IKey, 'i')
                        , (OKey, 'o')
                        , (PKey, 'p')
                        ])
                    ++ [ (KeySequence [] OpenBracketKey, insertChar '[')
                       , (KeySequence [ShiftKey] OpenBracketKey, insertChar '{')

                       , (KeySequence [] CloseBracketKey, insertChar ']')
                       , (KeySequence [ShiftKey] CloseBracketKey, insertChar '}')

                       , (KeySequence [] BackslashKey, insertChar '\\')
                       , (KeySequence [ShiftKey] BackslashKey, insertChar '|')
                       ]

        thirdRow :: [(KeySequence, Command)]
        thirdRow = (concatMap letterKey
                    [ (AKey, 'a')
                    , (SKey, 's')
                    , (DKey, 'd')
                    , (FKey, 'f')
                    , (GKey, 'g')
                    , (HKey, 'h')
                    , (JKey, 'j')
                    , (KKey, 'k')
                    , (LKey, 'l')
                    ])
                   ++ [ (KeySequence [] SemicolonKey, insertChar ';')
                      , (KeySequence [ShiftKey] SemicolonKey, insertChar ':')

                      , (KeySequence [] SingleQuoteKey, insertChar '\'')
                      , (KeySequence [ShiftKey] SingleQuoteKey, insertChar '"')

                      , (KeySequence [] ReturnKey, handleReturn)
                      ]

        fourthRow :: [(KeySequence, Command)]
        fourthRow = (concatMap letterKey
                     [ (ZKey, 'z')
                     , (XKey, 'x')
                     , (CKey, 'c')
                     , (VKey, 'v')
                     , (BKey, 'b')
                     , (NKey, 'n')
                     , (MKey, 'm')
                     ])
                    ++ [ (KeySequence [] CommaKey, insertChar ',')
                       , (KeySequence [ShiftKey] CommaKey, insertChar '<')

                       , (KeySequence [] PeriodKey, insertChar '.')
                       , (KeySequence [ShiftKey] PeriodKey, insertChar '>')

                       , (KeySequence [] ForwardSlashKey, insertChar '/')
                       , (KeySequence [ShiftKey] ForwardSlashKey, insertChar '?')
                       ]
                    ++ [ (KeySequence [OptionKey] XKey, focusMinibuffer) ]

        fifthRow :: [(KeySequence, Command)]
        fifthRow = [ (KeySequence [] SpaceKey, insertChar ' ')
                   , (KeySequence [] LeftArrowKey, moveCursor MoveLeft)
                   , (KeySequence [] UpArrowKey, moveCursor MoveUp)
                   , (KeySequence [] DownArrowKey, moveCursor MoveDown)
                   , (KeySequence [] RightArrowKey, moveCursor MoveRight)
                   ]
