module Pathy.Node.FS.Aff.Dir where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Node.FS.Aff.Dir as F
import Pathy.Node.FS.Dir (Dir(..)) as PathyFS
import Pathy.Node.FS.Dirent (Dirent(..)) as PathyFS

read :: PathyFS.Dir -> Aff (Maybe PathyFS.Dirent)
read (PathyFS.Dir dir) = F.read dir <#> map PathyFS.Dirent

close :: PathyFS.Dir -> Aff Unit
close (PathyFS.Dir dir) = F.close dir

entries :: PathyFS.Dir -> Aff (Array PathyFS.Dirent)
entries (PathyFS.Dir dir) = F.entries dir <#> map PathyFS.Dirent
