module Derivation where

import Structures

calculate :: Expression -> Calculation
calculate exp = Calc exp []