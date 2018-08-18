module Main where

{-
General Help:

    purs - The PureScript compiler (transpiler?) itself.
    npm - The Node Package Manager, which will allow us to install the rest of our development tools.
    pulp - A command-line tool which automates many of the tasks associated with managing PureScript projects.
    bower - An alternative package manager to npm, the one preffered by purescript for its packages

    pulp commands:
    "pulp init" initialize project skeleton
    "pulp run" executes project
    "pulp browserify" prints the entire project and its dependencies as javascript which can be saved to a file, added to html, and run in a browser
    "pulp build" builds the project, including CommonJS modules and purescript modules, modules get placed in the "output" folder
      "-O" optimizes the javascript output by pulp build by applying dead code elimination to remove unnecessary javascript (i.e. from prelude and libraries)
      "--to FILENAME.js" saves this output into the given file
    "pulp repl" fires up PSCi

    psci commands:
    ":?" help
    ":t" type
    ":k" kind
    ":r" reload
    ":b MODULENAME" browse that module
    "TAB" list all available functions in your project
    "import MODULENAME" imports that module (you need to import Main)

    bower commands:
    "bower install PACKAGENAME"
      "--save" adds this package as a dependency to the bower.json config file
    "bower update" updates bower with newest package versions
-}

--Chapter 2 imports
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Math (sqrt, pi)

--Chapter 3 imports
import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

--Chapter 2

main :: Effect Unit
main = log $ show $ circleArea 5.0

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * r * r

--Chapter 3

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName
