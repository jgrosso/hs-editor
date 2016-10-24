module Main
  ( main
  ) where


import Lib (runEditor)

import Terminal (applyEditorConfig)

main :: IO ()
main = do applyEditorConfig
          runEditor
