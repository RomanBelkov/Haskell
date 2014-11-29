module Parsing where

import Tokenizer
import qualified Tokenizer

data T = X String | C Int | A T T | S T T | M T T | D T T

