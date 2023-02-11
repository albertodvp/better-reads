module Common where

import Miso

data Action
    = NoOp

page404View :: View Action
page404View = text "Hey, this page does not exit :("
