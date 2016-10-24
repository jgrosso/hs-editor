module Terminal where


import Control.Lens (makeLenses)
import Control.Monad (void)

import System.Process (system)

import System.IO (BufferMode(NoBuffering), Handle, hReady, hSetBuffering, hSetEcho, stdin, stdout)


default (Int)


modifyStdio :: (Handle -> a -> IO ()) -> a -> IO ()
modifyStdio f value = mapM_ (\h -> f h value) [stdin, stdout]


disableBuffering :: IO ()
disableBuffering = modifyStdio hSetBuffering NoBuffering


disableEcho :: IO ()
disableEcho = modifyStdio hSetEcho False


disableCursor :: IO ()
disableCursor = putStrLn "\ESC[?25l"


applyEditorConfig :: IO ()
applyEditorConfig = do disableBuffering
                       disableEcho
                       disableCursor


clearScreen :: IO ()
clearScreen = void $ system "clear"


newtype RawChar = RawChar String


getRawChar :: IO RawChar
getRawChar = getRawChar' []
  where getRawChar' :: String -> IO RawChar
        getRawChar' acc = do char <- getChar
                             ready <- hReady stdin
                             (if not ready then return . RawChar else getRawChar') $ acc ++ [char]


data Key = BacktickKey
         | OneKey
         | TwoKey
         | ThreeKey
         | FourKey
         | FiveKey
         | SixKey
         | SevenKey
         | EightKey
         | NineKey
         | ZeroKey
         | DashKey
         | EqualsKey
         | BackspaceKey
         | TabKey
         | QKey
         | WKey
         | EKey
         | RKey
         | TKey
         | YKey
         | UKey
         | IKey
         | OKey
         | PKey
         | OpenBracketKey
         | CloseBracketKey
         | BackslashKey
         | AKey
         | SKey
         | DKey
         | FKey
         | GKey
         | HKey
         | JKey
         | KKey
         | LKey
         | SemicolonKey
         | SingleQuoteKey
         | ReturnKey
         | ZKey
         | XKey
         | CKey
         | VKey
         | BKey
         | NKey
         | MKey
         | CommaKey
         | PeriodKey
         | ForwardSlashKey
         | SpaceKey
         | LeftArrowKey
         | UpArrowKey
         | DownArrowKey
         | RightArrowKey
         deriving (Eq)

data ModifierKey = CommandKey
                 | ControlKey
                 | OptionKey
                 | ShiftKey
                 deriving (Eq)

data KeySequence = KeySequence
                   { _modifiers :: [ModifierKey]
                   , _key       :: Key
                   }
                   deriving (Eq)

makeLenses ''KeySequence


rawCharToKeySequence :: RawChar -> Maybe KeySequence
rawCharToKeySequence (RawChar char) = lookup char $ firstRow ++ secondRow ++ thirdRow ++ fourthRow ++ fifthRow
  where firstRow :: [(String, KeySequence)]
        firstRow = [ ("`", KeySequence [] BacktickKey)
                   , ("~", KeySequence [ShiftKey] BacktickKey)

                   , ("1", KeySequence [] OneKey)
                   , ("!", KeySequence [ShiftKey] OneKey)

                   , ("2", KeySequence [] TwoKey)
                   , ("@", KeySequence [ShiftKey] TwoKey)

                   , ("3", KeySequence [] ThreeKey)
                   , ("#", KeySequence [ShiftKey] ThreeKey)

                   , ("4", KeySequence [] FourKey)
                   , ("$", KeySequence [ShiftKey] FourKey)

                   , ("5", KeySequence [] FiveKey)
                   , ("%", KeySequence [ShiftKey] FiveKey)

                   , ("6", KeySequence [] SixKey)
                   , ("^", KeySequence [ShiftKey] SixKey)

                   , ("7", KeySequence [] SevenKey)
                   , ("&", KeySequence [ShiftKey] SevenKey)

                   , ("8", KeySequence [] EightKey)
                   , ("*", KeySequence [ShiftKey] EightKey)

                   , ("9", KeySequence [] NineKey)
                   , ("(", KeySequence [ShiftKey] NineKey)

                   , ("0", KeySequence [] ZeroKey)
                   , (")", KeySequence [ShiftKey] ZeroKey)

                   , ("-", KeySequence [] DashKey)
                   , ("_", KeySequence [ShiftKey] DashKey)

                   , ("=", KeySequence [] EqualsKey)
                   , ("+", KeySequence [ShiftKey] EqualsKey)

                   , ("\DEL", KeySequence [] BackspaceKey)
                   ]

        secondRow :: [(String, KeySequence)]
        secondRow = [ ("\t", KeySequence [] TabKey)

                    , ("q", KeySequence [] QKey)
                    , ("Q", KeySequence [ShiftKey] QKey)

                    , ("w", KeySequence [] WKey)
                    , ("W", KeySequence [ShiftKey] WKey)

                    , ("e", KeySequence [] EKey)
                    , ("E", KeySequence [ShiftKey] EKey)

                    , ("r", KeySequence [] RKey)
                    , ("R", KeySequence [ShiftKey] RKey)

                    , ("t", KeySequence [] TKey)
                    , ("T", KeySequence [ShiftKey] TKey)

                    , ("y", KeySequence [] YKey)
                    , ("Y", KeySequence [ShiftKey] YKey)

                    , ("u", KeySequence [] UKey)
                    , ("U", KeySequence [ShiftKey] UKey)

                    , ("i", KeySequence [] IKey)
                    , ("I", KeySequence [ShiftKey] IKey)

                    , ("o", KeySequence [] OKey)
                    , ("O", KeySequence [ShiftKey] OKey)

                    , ("p", KeySequence [] PKey)
                    , ("P", KeySequence [ShiftKey] PKey)

                    , ("[", KeySequence [] OpenBracketKey)
                    , ("{", KeySequence [ShiftKey] OpenBracketKey)

                    , ("]", KeySequence [] CloseBracketKey)
                    , ("}", KeySequence [ShiftKey] CloseBracketKey)

                    , ("\\", KeySequence [] BackslashKey)
                    , ("|", KeySequence [ShiftKey] BackslashKey)
                    ]

        thirdRow :: [(String, KeySequence)]
        thirdRow = [ ("a", KeySequence [] AKey)
                   , ("A", KeySequence [ShiftKey] AKey)

                   , ("s", KeySequence [] SKey)
                   , ("S", KeySequence [ShiftKey] SKey)

                   , ("d", KeySequence [] DKey)
                   , ("D", KeySequence [ShiftKey] DKey)

                   , ("f", KeySequence [] FKey)
                   , ("F", KeySequence [ShiftKey] FKey)

                   , ("g", KeySequence [] GKey)
                   , ("G", KeySequence [ShiftKey] GKey)

                   , ("h", KeySequence [] HKey)
                   , ("H", KeySequence [ShiftKey] HKey)

                   , ("j", KeySequence [] JKey)
                   , ("J", KeySequence [ShiftKey] JKey)

                   , ("k", KeySequence [] KKey)
                   , ("K", KeySequence [ShiftKey] KKey)

                   , ("l", KeySequence [] LKey)
                   , ("L", KeySequence [ShiftKey] LKey)

                   , (";", KeySequence [] SemicolonKey)
                   , (":", KeySequence [ShiftKey] SemicolonKey)

                   , ("\'", KeySequence [] SingleQuoteKey)
                   , ("\"", KeySequence [ShiftKey] SingleQuoteKey)

                   , ("\n", KeySequence [] ReturnKey)
                   ]

        fourthRow :: [(String, KeySequence)]
        fourthRow = [ ("z", KeySequence [] ZKey)
                   , ("Z", KeySequence [ShiftKey] ZKey)

                   , ("x", KeySequence [] XKey)
                   , ("X", KeySequence [ShiftKey] XKey)
                   , ("\ESCx", KeySequence [OptionKey] XKey)

                   , ("c", KeySequence [] CKey)
                   , ("C", KeySequence [ShiftKey] CKey)

                   , ("v", KeySequence [] VKey)
                   , ("V", KeySequence [ShiftKey] VKey)

                   , ("b", KeySequence [] BKey)
                   , ("B", KeySequence [ShiftKey] BKey)

                   , ("n", KeySequence [] NKey)
                   , ("N", KeySequence [ShiftKey] NKey)

                   , ("m", KeySequence [] MKey)
                   , ("M", KeySequence [ShiftKey] MKey)

                   , (",", KeySequence [] CommaKey)
                   , ("<", KeySequence [ShiftKey] CommaKey)

                   , (".", KeySequence [] PeriodKey)
                   , (">", KeySequence [ShiftKey] PeriodKey)

                   , ("/", KeySequence [] ForwardSlashKey)
                   , ("?", KeySequence [ShiftKey] ForwardSlashKey)
                   ]

        fifthRow :: [(String, KeySequence)]
        fifthRow = [ (" ", KeySequence [] SpaceKey)
                   , ("\ESC[A", KeySequence [] UpArrowKey)
                   , ("\ESC[B", KeySequence [] DownArrowKey)
                   , ("\ESC[C", KeySequence [] RightArrowKey)
                   , ("\ESC[D", KeySequence [] LeftArrowKey)
                   ]
