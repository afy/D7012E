{- Test for Parser.hs -}
module TestParser where

import Prelude hiding (return, fail)
import Parser

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test result: ALL PASS
l1 = letter "abc"   {- Just('a',"bc") -}
l2 = letter "123"   {- Nothing -}
l3 = letter ""      {- Nothing -}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test result: ALL PASS
w1 = spaces "abc"  {- Just("","abc") -}
w2 = spaces "  \t abc"  {- Just("  \t ","abc") -}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test result: ALL PASS
c1 = chars 2 "abc"          {-  Just ("ab","c")  -}
c2 = chars 0 "ab"          {-  Just ("","ab")  -}
c3 = chars 3 "ab"          {-  Nothing)  -}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test result: ALL PASS
r1 = require ":=" ":= 1"     {- Just (":=","1") -}
r2 = require "else" "then"     {- Program error: expecting else near then -}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test result: ALL PASS
a4 = (accept "read" -# word) "read count" {-  Just ("count","") -}
