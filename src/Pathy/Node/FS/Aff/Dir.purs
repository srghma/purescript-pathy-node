module Pathy.Node.FS.Aff.Dir where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Node.FS.Aff.Dir as F
import Pathy.Node.FS.Dir (Dir(..)) as PathyFS
import Pathy.Node.FS.Dirent (Dirent(..)) as PathyFS

read :: forall relOrAbs. PathyFS.Dir relOrAbs -> Aff (Maybe (PathyFS.Dirent relOrAbs))
read (PathyFS.Dir dir) = F.read dir <#> map PathyFS.Dirent

close :: forall relOrAbs. PathyFS.Dir relOrAbs -> Aff Unit
close (PathyFS.Dir dir) = F.close dir

entries :: forall relOrAbs. PathyFS.Dir relOrAbs -> Aff (Array (PathyFS.Dirent relOrAbs))
entries (PathyFS.Dir dir) = F.entries dir <#> map PathyFS.Dirent
