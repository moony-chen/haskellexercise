-- file: ch05/Main.hs
module Main (main) where

import SimpleJSON
import PutJSON

main = print (renderJValue (JObject [("foo", JNumber 1), ("bar", JBool False)]))